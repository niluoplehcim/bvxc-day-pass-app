# app.R
# Bulkley Valley Cross Country Ski Club – passes + donations (Square)
# DAY, CHRISTMAS, SEASON, DONATION, with admin-set prices and limits.
# Logging + config via SQLite, no global <<- state for prices/limits.

library(shiny)
library(httr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(uuid)

# Alias for Shiny's validate/need
validate <- shiny::validate
need     <- shiny::need

# ---------- GLOBAL SETTINGS ----------

Sys.setenv(TZ = "America/Vancouver")
APP_VERSION <- "BVXC passes v2.2 – 2025-12-05"

# Load .Renviron if present (for local + shinyapps.io bundle)
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# Read required environment variables
SQUARE_ENV           <- Sys.getenv("SQUARE_ENV",           unset = NA_character_)
SQUARE_ACCESS_TOKEN  <- Sys.getenv("SQUARE_ACCESS_TOKEN",  unset = NA_character_)
SQUARE_LOCATION_ID   <- Sys.getenv("SQUARE_LOCATION_ID",   unset = NA_character_)
ADMIN_PASSWORD       <- Sys.getenv("BVXC_ADMIN_PASSWORD",  unset = NA_character_)

# Optional override for return URL base (useful on shinyapps.io / reverse proxy)
RETURN_BASE_URL <- Sys.getenv("BVXC_RETURN_BASE_URL", unset = NA_character_)

# ---- Validate Square environment ----
ALLOWED_SQUARE_ENVS <- c("sandbox", "production")

if (is.na(SQUARE_ENV) || !nzchar(SQUARE_ENV) || !(SQUARE_ENV %in% ALLOWED_SQUARE_ENVS)) {
  stop("SQUARE_ENV must be 'sandbox' or 'production'")
}

# Label for Square depending on environment
SQUARE_LABEL <- if (identical(SQUARE_ENV, "sandbox")) "Square sandbox" else "Square"

# Helper to build the redirect URL after payment
build_return_url <- function(session) {
  # If RETURN_BASE_URL is set, trust it.
  if (!is.na(RETURN_BASE_URL) && nzchar(RETURN_BASE_URL)) {
    base <- sub("/+$", "", RETURN_BASE_URL)
    path <- session$clientData$url_pathname %||% "/"
    if (!startsWith(path, "/")) {
      path <- paste0("/", path)
    }
    return(paste0(base, path, "?success=1"))
  }

  # Fallback: reconstruct from clientData (works fine for local use)
  paste0(
    session$clientData$url_protocol,
    "//",
    session$clientData$url_hostname,
    session$clientData$url_pathname,
    "?success=1"
  )
}

# Enforce that everything is set – fail fast if not
missing_vars <- c(
  if (is.na(SQUARE_ENV)          || SQUARE_ENV          == "") "SQUARE_ENV"          else NULL,
  if (is.na(SQUARE_ACCESS_TOKEN) || SQUARE_ACCESS_TOKEN == "") "SQUARE_ACCESS_TOKEN" else NULL,
  if (is.na(SQUARE_LOCATION_ID)  || SQUARE_LOCATION_ID  == "") "SQUARE_LOCATION_ID"  else NULL,
  if (is.na(ADMIN_PASSWORD)      || ADMIN_PASSWORD      == "") "BVXC_ADMIN_PASSWORD" else NULL
)

if (length(missing_vars) > 0) {
  stop(
    paste0(
      "Missing required environment variables: ",
      paste(missing_vars, collapse = ", "),
      ". Check your .Renviron in the app directory or your server configuration."
    )
  )
}

square_base_url <- if (identical(SQUARE_ENV, "production")) {
  "https://connect.squareup.com"
} else {
  "https://connect.squareupsandbox.com"
}

SQUARE_ENV_LABEL <- if (identical(SQUARE_ENV, "sandbox")) {
  " (Square sandbox)"
} else {
  ""
}

# ---------- HELPERS ----------

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

sanitize_for_storage <- function(x) {
  x <- trimws(as.character(x %||% ""))

  if (length(x) == 0) return(x)

  first_char <- substr(x, 1, 1)
  bad_start  <- first_char %in% c("=", "+", "-", "@")

  bad_idx <- which(bad_start & !is.na(x))
  if (length(bad_idx) > 0) {
    x[bad_idx] <- paste0("'", x[bad_idx])
  }

  x
}

compute_age_years <- function(dob, ref_date = Sys.Date()) {
  if (length(dob) == 0) return(integer(0))
  dob <- as.Date(dob)
  dif <- as.numeric(ref_date - dob)
  as.integer(floor(dif / 365.25))
}

YOUTH_MIN_AGE <- 9L
YOUTH_MAX_AGE <- 18L
ADULT_MIN_AGE <- 19L

get_db_connection <- function() {
  dbConnect(RSQLite::SQLite(), "bvxc.sqlite")
}


# ---------- DB INIT & HELPERS ----------

init_db <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS transactions (
      id               INTEGER PRIMARY KEY AUTOINCREMENT,
      created          TEXT,
      ski_date         TEXT,
      product_type     TEXT,
      adults           INTEGER,
      youths           INTEGER,
      under9           INTEGER,
      families         INTEGER,
      christmas_passes INTEGER,
      total_cents      INTEGER,
      donation_cents   INTEGER,
      name             TEXT,
      email            TEXT,
      checkout_id      TEXT,
      status           TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS registrations (
      id           INTEGER PRIMARY KEY AUTOINCREMENT,
      checkout_id  TEXT,
      product_type TEXT,
      holder_name  TEXT,
      holder_type  TEXT,
      holder_dob   TEXT,
      notes        TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS blocked_dates (
      date TEXT PRIMARY KEY
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS config (
      key   TEXT PRIMARY KEY,
      value TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS donation_details (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      checkout_id TEXT,
      address     TEXT,
      postal      TEXT,
      contact_ok  TEXT,
      public_ack  TEXT,
      notes       TEXT
    )
  ")
}

save_transaction <- function(row_df) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  dbWriteTable(con, "transactions", row_df, append = TRUE)
}

load_transactions <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  if (!"transactions" %in% dbListTables(con)) {
    return(data.frame())
  }
  dbReadTable(con, "transactions")
}

append_registrations <- function(regs_df) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  dbWriteTable(con, "registrations", regs_df, append = TRUE)
}

load_registrations <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  if (!"registrations" %in% dbListTables(con)) {
    return(data.frame())
  }
  dbReadTable(con, "registrations")
}

append_donation_details <- function(details_df) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  dbWriteTable(con, "donation_details", details_df, append = TRUE)
}

get_blocked_dates <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  if (!"blocked_dates" %in% dbListTables(con)) {
    return(as.Date(character(0)))
  }
  df <- dbReadTable(con, "blocked_dates")
  if (!"date" %in% names(df)) return(as.Date(character(0)))
  as.Date(df$date)
}

set_blocked_dates <- function(dates) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  dates <- as.Date(dates)
  df <- data.frame(date = as.character(dates), stringsAsFactors = FALSE)
  dbWriteTable(con, "blocked_dates", df, overwrite = TRUE)
}

# ---------- CONFIG (PRICES + LIMITS) ----------

DEFAULT_CONFIG <- list(
  # Day prices (cents)
  price_adult_day    = 1500L,
  price_youth_day    = 1000L,
  price_under9_day   = 0L,
  price_family_day   = 3000L,
  # Season + Christmas prices (cents)
  price_season_adult   = 25000L,
  price_season_youth   = 18000L,
  price_season_family  = 45000L,
  price_christmas_pass = 7500L,
  season_label         = "2025–2026 season",
  # Day limits
  max_day_adult    = 10L,
  max_day_youth    = 10L,
  max_day_under9   = 15L,
  max_day_family   = 5L,
  max_day_amount   = 300,   # dollars
  # Donation limit
  max_donation_amount = 1000,  # dollars
  # Christmas limits
  max_christmas_passes = 6L,
  max_christmas_amount = 500,  # dollars
  # Season limits
  max_season_adult   = 6L,
  max_season_youth   = 6L,
  max_season_family  = 4L,
  max_season_amount  = 2000   # dollars
)

load_config <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  if (!"config" %in% dbListTables(con)) {
    return(DEFAULT_CONFIG)
  }
  df <- dbReadTable(con, "config")
  cfg <- DEFAULT_CONFIG
  if (nrow(df) > 0) {
    for (i in seq_len(nrow(df))) {
      k <- df$key[i]
      v <- df$value[i]
      if (!nzchar(k)) next
      if (k %in% names(cfg)) {
        if (grepl("^price_", k) || grepl("^max_", k)) {
          num <- suppressWarnings(as.numeric(v))
          if (!is.na(num)) {
            if (grepl("^price_", k)) {
              cfg[[k]] <- as.integer(round(num))
            } else {
              cfg[[k]] <- num
            }
          }
        } else {
          cfg[[k]] <- v
        }
      }
    }
  }
  cfg
}

save_config <- function(cfg) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  df <- data.frame(
    key   = names(cfg),
    value = vapply(cfg, function(x) as.character(x %||% ""), character(1)),
    stringsAsFactors = FALSE
  )
  dbWriteTable(con, "config", df, overwrite = TRUE)
}

# ---------- TAB FLAGS (CSV, RARELY WRITTEN) ----------

TAB_FLAGS_FILE <- "tab_flags.csv"

load_tab_flags <- function() {
  flags <- list(
    day       = TRUE,
    christmas = TRUE,
    season    = TRUE,
    donation  = TRUE
  )
  if (!file.exists(TAB_FLAGS_FILE)) {
    df <- data.frame(
      tab     = names(flags),
      enabled = as.integer(unlist(flags)),
      stringsAsFactors = FALSE
    )
    write.csv(df, TAB_FLAGS_FILE, row.names = FALSE)
    return(flags)
  }
  df <- read.csv(TAB_FLAGS_FILE, stringsAsFactors = FALSE)
  if (!all(c("tab", "enabled") %in% names(df))) return(flags)
  for (i in seq_len(nrow(df))) {
    nm  <- df$tab[i]
    val <- as.logical(df$enabled[i])
    if (nm %in% names(flags) && !is.na(val)) {
      flags[[nm]] <- val
    }
  }
  flags
}

save_tab_flags <- function(flags) {
  df <- data.frame(
    tab     = c("day", "christmas", "season", "donation"),
    enabled = as.integer(c(flags$day, flags$christmas, flags$season, flags$donation)),
    stringsAsFactors = FALSE
  )
  write.csv(df, TAB_FLAGS_FILE, row.names = FALSE)
}

# ---------- SQUARE CHECKOUT HELPER ----------

create_square_checkout <- function(total_cents, item_name, return_url) {
  if (is.null(total_cents) || is.na(total_cents) || total_cents <= 0) {
    stop("Amount must be > 0.")
  }

  idempotency_key <- uuid::UUIDgenerate()

  body_list <- list(
    idempotency_key = idempotency_key,
    quick_pay = list(
      name        = item_name,
      location_id = SQUARE_LOCATION_ID,
      price_money = list(
        amount   = as.integer(total_cents),
        currency = "CAD"
      )
    ),
    checkout_options = list(
      redirect_url = return_url
    )
  )

  resp <- tryCatch(
    {
      httr::POST(
        url = paste0(square_base_url, "/v2/online-checkout/payment-links"),
        httr::add_headers(
          "Authorization" = paste("Bearer", SQUARE_ACCESS_TOKEN),
          "Content-Type"  = "application/json"
        ),
        body   = body_list,
        encode = "json"
      )
    },
    error = function(e) {
      msg <- sprintf("Square API call failed: %s", e$message)
      message(msg)
      stop(msg)
    }
  )

  if (!inherits(resp, "response")) {
    stop("Square API call did not return a valid HTTP response object.")
  }

  if (httr::http_error(resp)) {
    status  <- httr::http_status(resp)
    body    <- httr::content(resp, "text", encoding = "UTF-8")
    message("Square HTTP error: ", status$status_code, " ", status$reason)
    message("Square error body (first 500 chars): ", substr(body, 1, 500))

    if (grepl("^\\s*<", body)) {
      stop(
        sprintf(
          "Square or its network returned an internal error (HTTP %s). Please try again in a few minutes.",
          status$status_code
        )
      )
    } else {
      stop(sprintf("Square returned an error: %s", body))
    }
  }

  content_list <- httr::content(resp, as = "parsed", type = "application/json")

  if (is.null(content_list$payment_link$url) || is.null(content_list$payment_link$id)) {
    stop("Square response missing payment_link url or id.")
  }

  list(
    url = content_list$payment_link$url,
    id  = content_list$payment_link$id
  )
}

# ---------- CHRISTMAS DATE LIMITS ----------

CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))
CHRISTMAS_DAY_THIS_YEAR <- as.Date(paste0(CURRENT_YEAR, "-12-25"))
CHRISTMAS_START_MIN <- CHRISTMAS_DAY_THIS_YEAR - 13
CHRISTMAS_START_MAX <- CHRISTMAS_DAY_THIS_YEAR

MAX_SKI_DATE_OFFSET_DAYS <- 183L

# ---------- INIT DB ----------

init_db()

# ---------- UI ----------

sandbox_banner <- if (SQUARE_ENV == "sandbox") {
  div(
    style = "background:#fee; border:1px solid #f88; padding:6px; margin-bottom:12px; font-size:0.9em;",
    strong("SANDBOX – TEST MODE ONLY. NO REAL CARD CHARGES.")
  )
} else {
  NULL
}

day_tab <- tabPanel(
  "DAY PASS",
  fluidPage(
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title("BVXC – Day Passes")
    ),
    div(
      style = "max-width: 650px; margin: 0 auto; padding: 1.5rem;",
      sandbox_banner,
      conditionalPanel(
        condition = "output.day_tab_enabled",
        h2("Day passes"),
        p(
          "Buy single-day trail passes online. Show your Square email receipt if asked on the trails.",
          style = "color:#555; margin-bottom:0.75rem;"
        ),
        uiOutput("price_line"),
        uiOutput("day_limits_text"),
        dateInput(
          "ski_date",
          "Ski date",
          value  = Sys.Date(),
          min    = Sys.Date(),
          max    = Sys.Date() + MAX_SKI_DATE_OFFSET_DAYS,
          format = "yyyy-mm-dd"
        ),
        h4("Number of passes"),
numericInput("n_adult",  "Adults (19+)",      value = 1, min = 0, max = 100, step = 1),
  numericInput("n_youth",  "Youth (9–18)",      value = 0, min = 0, max = 100, step = 1),
        numericInput("n_under9", "Children under 9",  value = 0, min = 0, max = 100, step = 1),
        numericInput("n_family", "Family day passes", value = 0, min = 0, max = 100, step = 1),
        tags$hr(),
        uiOutput("summary"),
        textInput("name",  "Name (for receipt / records)"),
        textInput("email", "Email (for Square receipt)"),
        tags$hr(),
        actionButton(
          "pay",
          paste0("Pay for day passes", SQUARE_ENV_LABEL),
          class = "btn btn-primary btn-lg btn-block"
        ),
        tags$br(),
        uiOutput("status")
      ),
      conditionalPanel(
        condition = "!output.day_tab_enabled",
        h2("Day passes – temporarily offline"),
        p(
          "Online day-pass purchases are currently disabled. Please check back later or purchase passes on-site following club instructions.",
          style = "color:#555; margin-top:0.75rem;"
        )
      )
    )
  )
)

donation_tab <- tabPanel(
  "DONATION",
  fluidPage(
    div(
      style = "max-width: 650px; margin: 0 auto; padding: 1.5rem;",
      sandbox_banner,
      conditionalPanel(
        condition = "output.donation_tab_enabled",
        h2("Donations"),
        p(
          "Make a standalone donation to support trail operations and grooming. No passes are issued in this transaction.",
          style = "color:#555; margin-bottom:0.75rem;"
        ),
        div(
          style = "background:#f9f9f9; border-left:4px solid #999; padding:8px; margin-bottom:12px; font-size:0.9em;",
          strong("Important: "),
          span("The club currently does not have charitable status with the Canada Revenue Agency (CRA). Receipts from this form "),
          strong("cannot"),
          span(" be used for Canadian income tax purposes.")
        ),
        uiOutput("donation_limits_text"),
        numericInput("donation_only_amount", "Donation amount ($)", value = 10, min = 0, max = 100000, step = 1),
        textInput("donation_only_name",  "Donor name"),
        textInput("donation_only_email", "Donor email (for Square receipt)"),
        textInput("donation_only_address", "Mailing address (optional, for club records)"),
        textInput("donation_only_postal",  "Postal code (optional)"),
        selectInput(
          "donation_only_contact_ok",
          "May the club contact you about future news or fundraising?",
          choices = c("Yes", "No"),
          selected = "Yes"
        ),
        selectInput(
          "donation_only_public_ack",
          "May the club publicly acknowledge your donation (e.g., annual report, website)?",
          choices = c("Yes – use my name", "No – keep my donation anonymous"),
          selected = "Yes – use my name"
        ),
        textInput("donation_only_notes", "Notes or comments (optional)"),
        tags$hr(),
        uiOutput("donation_only_summary"),
        tags$hr(),
        actionButton(
          "donation_only_pay",
          paste0("Make donation", SQUARE_ENV_LABEL),
          class = "btn btn-primary btn-lg btn-block"
        ),
        tags$br(),
        uiOutput("donation_only_status")
      ),
      conditionalPanel(
        condition = "!output.donation_tab_enabled",
        h2("Donations – temporarily offline"),
        p(
          "Online donations are currently disabled. Please check back later or contact the club for other ways to donate.",
          style = "color:#555; margin-top:0.75rem;"
        )
      )
    )
  )
)

christmas_tab <- tabPanel(
  "CHRISTMAS PASS",
  fluidPage(
    div(
      style = "max-width: 650px; margin: 0 auto; padding: 1.5rem;",
      sandbox_banner,
      conditionalPanel(
        condition = "output.christmas_tab_enabled",
        h2("Christmas 2-week pass"),
        p(
          "Buy 14-day passes for individual skiers. Your chosen period must include Christmas Day and each pass is for one named person.",
          style = "color:#555; margin-bottom:0.75rem;"
        ),
        uiOutput("christmas_price_line"),
        uiOutput("christmas_limits_text"),
        numericInput("n_christmas", "Number of Christmas passes", value = 0, min = 0, max = 100, step = 1),
        uiOutput("christmas_holder_form"),
        dateInput(
          "christmas_start",
          "Start date of your 2-week period",
          value  = CHRISTMAS_START_MIN,
          min    = CHRISTMAS_START_MIN,
          max    = CHRISTMAS_START_MAX,
          format = "yyyy-mm-dd"
        ),
        tags$hr(),
        uiOutput("christmas_summary"),
        textInput("christmas_name",  "Purchaser name (for records)"),
        textInput("christmas_email", "Purchaser email (for Square receipt)"),
        tags$hr(),
        actionButton(
          "christmas_pay",
          paste0("Pay for Christmas passes", SQUARE_ENV_LABEL),
          class = "btn btn-primary btn-lg btn-block"
        ),
        tags$br(),
        uiOutput("christmas_status")
      ),
      conditionalPanel(
        condition = "!output.christmas_tab_enabled",
        h2("Christmas passes – coming soon"),
        p(
          "Online purchase of Christmas passes is currently disabled. Please check back closer to the season or contact the club.",
          style = "color:#555; margin-top:0.75rem;"
        )
      )
    )
  )
)

season_tab <- tabPanel(
  "SEASON PASS",
  fluidPage(
    div(
      style = "max-width: 650px; margin: 0 auto; padding: 1.5rem;",
      sandbox_banner,
      conditionalPanel(
        condition = "output.season_tab_enabled",
        h2(textOutput("season_header")),
        p(
          "Purchase adult, youth, or family season passes and register each pass holder by name and date of birth.",
          style = "color:#555; margin-bottom:0.75rem;"
        ),
        uiOutput("season_price_line"),
        uiOutput("season_limits_text"),
        h4("Number of season passes"),
        numericInput("n_season_adult",  "Adult season passes",  value = 1, min = 0, max = 100, step = 1),
        numericInput("n_season_youth",  "Youth season passes",  value = 0, min = 0, max = 100, step = 1),
        numericInput("n_season_family", "Family season passes", value = 0, min = 0, max = 100, step = 1),
        uiOutput("season_holder_form"),
        tags$hr(),
        uiOutput("season_summary"),
        textInput("season_name",  "Purchaser name (for records)"),
        textInput("season_email", "Purchaser email (for Square receipt)"),
        tags$hr(),
        actionButton(
          "season_pay",
          paste0("Pay for season passes", SQUARE_ENV_LABEL),
          class = "btn btn-primary btn-lg btn-block"
        ),
        tags$br(),
        uiOutput("season_status")
      ),
      conditionalPanel(
        condition = "!output.season_tab_enabled",
        h2("Season passes – coming soon"),
        p(
          "Online purchase of season passes is currently disabled. Please see the club website or contact the club directly.",
          style = "color:#555; margin-top:0.75rem;"
        )
      )
    )
  )
)

admin_tab <- tabPanel(
  "ADMIN",
  fluidPage(
    div(
      style = "max-width: 950px; margin: 0 auto; padding: 1.5rem;",
      sandbox_banner,
      h3("Bulkley Valley XC – Admin"),
      p(
        "View pass logs, adjust prices and limits, manage blocked dates, and control which public tabs are live.",
        style = "color:#555; margin-bottom:0.75rem;"
      ),
      h4("Admin access"),
      passwordInput("admin_password", "Admin password"),
      actionButton("admin_login", "Unlock admin"),
      verbatimTextOutput("admin_auth_message"),
      tags$hr(),
      uiOutput("admin_body"),
      tags$hr(),
      p(APP_VERSION, style = "font-size: 0.8em; color: #666;")
    )
  )
)

ui_core <- navbarPage(
  title = "Bulkley Valley Cross Country Ski Club – Passes",
  day_tab,
  christmas_tab,
  season_tab,
  donation_tab,
  admin_tab
)

# JS + CSS
js <- "
Shiny.addCustomMessageHandler('redirectToSquare', function(url) {
  if (url && typeof url === 'string') {
    window.location.href = url;
  }
});
"

app_ui <- tagList(
  tags$head(
    tags$script(HTML(js)),
    tags$style(HTML("
      .datepicker.dropdown-menu {
        font-size: 14px;
        padding: 4px;
        margin-top: 40px; /* push popup down so month/year are visible */
      }
      .datepicker .datepicker-switch {
        width: 190px;
      }
    "))
  ),
  ui_core
)

# ---------- SERVER ----------

server <- function(input, output, session) {

  # Config (prices + limits) from DB into reactiveValues
  cfg0 <- load_config()
  rv_config <- reactiveValues(
    price_adult_day      = cfg0$price_adult_day,
    price_youth_day      = cfg0$price_youth_day,
    price_under9_day     = cfg0$price_under9_day,
    price_family_day     = cfg0$price_family_day,
    price_season_adult   = cfg0$price_season_adult,
    price_season_youth   = cfg0$price_season_youth,
    price_season_family  = cfg0$price_season_family,
    price_christmas_pass = cfg0$price_christmas_pass,
    season_label         = cfg0$season_label,
    max_day_adult        = cfg0$max_day_adult,
    max_day_youth        = cfg0$max_day_youth,
    max_day_under9       = cfg0$max_day_under9,
    max_day_family       = cfg0$max_day_family,
    max_day_amount       = cfg0$max_day_amount,
    max_donation_amount  = cfg0$max_donation_amount,
    max_christmas_passes = cfg0$max_christmas_passes,
    max_christmas_amount = cfg0$max_christmas_amount,
    max_season_adult     = cfg0$max_season_adult,
    max_season_youth     = cfg0$max_season_youth,
    max_season_family    = cfg0$max_season_family,
    max_season_amount    = cfg0$max_season_amount
  )

  blocked_dates <- reactiveVal(get_blocked_dates())

  flags0 <- load_tab_flags()
  tab_flags <- reactiveValues(
    day       = isTRUE(flags0$day),
    christmas = isTRUE(flags0$christmas),
    season    = isTRUE(flags0$season),
    donation  = isTRUE(flags0$donation)
  )

  admin_ok <- reactiveVal(FALSE)

  observeEvent(input$admin_login, {
    if (identical(input$admin_password, ADMIN_PASSWORD)) {
      admin_ok(TRUE)
      output$admin_auth_message <- renderText("Admin unlocked for this session.")
    } else {
      admin_ok(FALSE)
      output$admin_auth_message <- renderText("Incorrect password.")
    }
  })

  # Expose tab flags
  output$day_tab_enabled       <- reactive({ tab_flags$day })
  output$christmas_tab_enabled <- reactive({ tab_flags$christmas })
  output$season_tab_enabled    <- reactive({ tab_flags$season })
  output$donation_tab_enabled  <- reactive({ tab_flags$donation })

  outputOptions(output, "day_tab_enabled",       suspendWhenHidden = FALSE)
  outputOptions(output, "christmas_tab_enabled", suspendWhenHidden = FALSE)
  outputOptions(output, "season_tab_enabled",    suspendWhenHidden = FALSE)
  outputOptions(output, "donation_tab_enabled",  suspendWhenHidden = FALSE)

  # Season header, price/limits lines
  output$season_header <- renderText({
    paste("Season passes –", rv_config$season_label)
  })

  output$price_line <- renderUI({
    tags$p(
      sprintf(
        "Adults $%0.2f · Youth (9–18) $%0.2f · Under 9 %s · Family $%0.2f",
        rv_config$price_adult_day / 100,
        rv_config$price_youth_day / 100,
        if (rv_config$price_under9_day > 0) {
          sprintf("$%0.2f", rv_config$price_under9_day / 100)
        } else {
          "FREE"
        },
        rv_config$price_family_day / 100
      )
    )
  })

  output$day_limits_text <- renderUI({
    tags$p(
      sprintf(
        "Per transaction limits: up to %d adult, %d youth, %d under-9, %d family day passes, and a maximum of $%0.2f total.",
        as.integer(rv_config$max_day_adult),
        as.integer(rv_config$max_day_youth),
        as.integer(rv_config$max_day_under9),
        as.integer(rv_config$max_day_family),
        rv_config$max_day_amount
      ),
      style = "font-size:0.9em; color:#666;"
    )
  })

  output$season_price_line <- renderUI({
    # Show "(sandbox demo)" only when using Square sandbox
    env_note <- if (identical(SQUARE_ENV, "sandbox")) " (sandbox demo)" else ""
    tags$p(
      sprintf(
        "Adult $%0.2f · Youth $%0.2f · Family $%0.2f%s",
        rv_config$price_season_adult / 100,
        rv_config$price_season_youth / 100,
        rv_config$price_season_family / 100,
        env_note
      )
    )
  })

  output$season_limits_text <- renderUI({
    tags$p(
      sprintf(
        "Per transaction limits: up to %d adult, %d youth, %d family season passes, and a maximum of $%0.2f total.",
        as.integer(rv_config$max_season_adult),
        as.integer(rv_config$max_season_youth),
        as.integer(rv_config$max_season_family),
        rv_config$max_season_amount
      ),
      style = "font-size:0.9em; color:#666;"
    )
  })

  output$christmas_price_line <- renderUI({
    # Same conditional note for Christmas passes
    env_note <- if (identical(SQUARE_ENV, "sandbox")) " (sandbox demo)" else ""
    tags$p(
      sprintf(
        "Christmas 2-week pass: $%0.2f%s per person",
        rv_config$price_christmas_pass / 100,
        env_note
      )
    )
  })

  output$christmas_limits_text <- renderUI({
    tags$p(
      sprintf(
        "Per transaction limits: up to %d Christmas passes, and a maximum of $%0.2f total.",
        as.integer(rv_config$max_christmas_passes),
        rv_config$max_christmas_amount
      ),
      style = "font-size:0.9em; color:#666;"
    )
  })

  # ---------- QUERYSTRING SUCCESS POPUP (UI ONLY) ----------

  observe({
    query <- shiny::getQueryString()
    success_flag <- isTRUE(query[["success"]] == "1")
    if (success_flag) {
      showModal(
        modalDialog(
          title = "Payment complete",
          "Thank you. Your payment appears to have completed successfully. Please keep your Square email receipt as proof of payment. (Note: the club may still verify final payment status via Square.)",
          easyClose = TRUE,
          footer = NULL
        )
      )
    }
  })

  # ---------- DAY PASSES ----------

  day_total <- reactive({
    adults_raw   <- input$n_adult
    youths_raw   <- input$n_youth
    under9_raw   <- input$n_under9
    families_raw <- input$n_family

    if (is.null(adults_raw)   || is.na(adults_raw))   adults_raw   <- 0
    if (is.null(youths_raw)   || is.na(youths_raw))   youths_raw   <- 0
    if (is.null(under9_raw)   || is.na(under9_raw))   under9_raw   <- 0
    if (is.null(families_raw) || is.na(families_raw)) families_raw <- 0

    adults   <- as.integer(adults_raw)
    youths   <- as.integer(youths_raw)
    under9   <- as.integer(under9_raw)
    families <- as.integer(families_raw)

    passes_cents <- adults   * rv_config$price_adult_day +
                    youths   * rv_config$price_youth_day +
                    under9   * rv_config$price_under9_day +
                    families * rv_config$price_family_day

    list(
      adults       = adults,
      youths       = youths,
      under9       = under9,
      families     = families,
      passes_cents = passes_cents,
      total_cents  = passes_cents
    )
  })

  output$summary <- renderUI({
    t <- day_total()

    if (t$adults == 0 && t$youths == 0 && t$families == 0 && t$under9 == 0) {
      return(tags$p("No passes selected.", style = "font-weight: 600;"))
    }

    total_dollars <- t$total_cents / 100
    lines <- list(
      tags$p(
        sprintf(
          "Adult: %d, Youth: %d, Under 9: %d, Family passes: %d",
          t$adults, t$youths, t$under9, t$families
        )
      )
    )

    if (t$passes_cents > 0) {
      lines <- c(lines, list(
        tags$p(sprintf("Passes subtotal: $%0.2f CAD", t$passes_cents / 100))
      ))
    }

    lines <- c(lines, list(
      tags$p(
        sprintf("Total to pay: $%0.2f CAD%s", total_dollars, SQUARE_ENV_LABEL),
        style = "font-weight: 700; font-size: 1.2rem;"
      )
    ))

    do.call(tagList, lines)
  })

  observeEvent(input$ski_date, {
    ski_date <- as.Date(input$ski_date)
    if (is.na(ski_date)) return()

    today    <- Sys.Date()
    max_date <- today + MAX_SKI_DATE_OFFSET_DAYS

    if (ski_date < today || ski_date > max_date) {
      showModal(
        modalDialog(
          title = "Invalid ski date",
          paste0(
            "Online passes are only available for dates from ",
            format(today, "%Y-%m-%d"),
            " to ",
            format(max_date, "%Y-%m-%d"),
            ". You selected ",
            format(ski_date, "%Y-%m-%d"),
            "."
          ),
          easyClose = TRUE,
          footer = NULL
        )
      )
      updateDateInput(session, "ski_date", value = today)
      return()
    }

    current_blocked <- blocked_dates()
    if (length(current_blocked) > 0 && ski_date %in% current_blocked) {
      showModal(
        modalDialog(
          title = "Online passes not available for this date",
          "Online day passes are disabled for this date (free day, closure, or off-season). Please choose another date or use cash / follow club instructions.",
          easyClose = TRUE,
          footer = NULL
        )
      )

      new_date  <- ski_date
      max_date  <- today + MAX_SKI_DATE_OFFSET_DAYS
      max_steps <- as.integer(MAX_SKI_DATE_OFFSET_DAYS)
      steps     <- 0L

      while (!is.na(new_date) &&
             new_date <= max_date &&
             new_date %in% current_blocked &&
             steps < max_steps) {
        new_date <- new_date + 1
        steps    <- steps + 1L
      }

      if (is.na(new_date) || new_date > max_date || new_date %in% current_blocked) {
        if (!(today %in% current_blocked)) {
          new_date <- today
        } else {
          new_date <- today
        }
      }

      updateDateInput(session, "ski_date", value = new_date)
    }
  })

  # Live limit enforcement for day passes
  observeEvent(input$n_adult, ignoreInit = TRUE, {
    val_raw <- input$n_adult
    if (is.null(val_raw) || is.na(val_raw)) {
      updateNumericInput(session, "n_adult", value = 0)
      return()
    }
    val <- as.integer(val_raw)
    if (val > rv_config$max_day_adult) {
      updateNumericInput(session, "n_adult", value = rv_config$max_day_adult)
      showNotification(
        sprintf("Maximum %d adult day passes per transaction.", as.integer(rv_config$max_day_adult)),
        type = "error", duration = 4
      )
    } else if (val < 0) {
      updateNumericInput(session, "n_adult", value = 0)
    }
  })

  observeEvent(input$n_youth, ignoreInit = TRUE, {
    val_raw <- input$n_youth
    if (is.null(val_raw) || is.na(val_raw)) {
      updateNumericInput(session, "n_youth", value = 0)
      return()
    }
    val <- as.integer(val_raw)
    if (val > rv_config$max_day_youth) {
      updateNumericInput(session, "n_youth", value = rv_config$max_day_youth)
      showNotification(
        sprintf("Maximum %d youth day passes per transaction.", as.integer(rv_config$max_day_youth)),
        type = "error", duration = 4
      )
    } else if (val < 0) {
      updateNumericInput(session, "n_youth", value = 0)
    }
  })

  observeEvent(input$n_under9, ignoreInit = TRUE, {
    val_raw <- input$n_under9
    if (is.null(val_raw) || is.na(val_raw)) {
      updateNumericInput(session, "n_under9", value = 0)
      return()
    }
    val <- as.integer(val_raw)
    if (val > rv_config$max_day_under9) {
      updateNumericInput(session, "n_under9", value = rv_config$max_day_under9)
      showNotification(
        sprintf("Maximum %d under-9 entries per transaction.", as.integer(rv_config$max_day_under9)),
        type = "error", duration = 4
      )
    } else if (val < 0) {
      updateNumericInput(session, "n_under9", value = 0)
    }
  })

  observeEvent(input$n_family, ignoreInit = TRUE, {
    val_raw <- input$n_family
    if (is.null(val_raw) || is.na(val_raw)) {
      updateNumericInput(session, "n_family", value = 0)
      return()
    }
    val <- as.integer(val_raw)
    if (val > rv_config$max_day_family) {
      updateNumericInput(session, "n_family", value = rv_config$max_day_family)
      showNotification(
        sprintf("Maximum %d family day passes per transaction.", as.integer(rv_config$max_day_family)),
        type = "error", duration = 4
      )
    } else if (val < 0) {
      updateNumericInput(session, "n_family", value = 0)
    }
  })

  observeEvent(input$pay, {
    tryCatch({

      if (!tab_flags$day) {
        output$status <- renderUI({
          tags$p(
            "Day-pass payments are currently disabled.",
            style = "color: #b00; white-space: pre-wrap;"
          )
        })
        return()
      }

      t <- day_total()
      d <- as.Date(input$ski_date)

      MAX_TOTAL_DOLL <- rv_config$max_day_amount

      validate(
        need(t$total_cents > 0, "Please select at least one paid pass."),
        need(!is.na(d), "Please select a ski date."),
        need(nchar(input$name)  > 0, "Please enter your name."),
        need(nchar(input$email) > 0, "Please enter your email."),
        need(t$adults   <= rv_config$max_day_adult,
             sprintf("Maximum %d adult day passes per transaction.",  as.integer(rv_config$max_day_adult))),
        need(t$youths   <= rv_config$max_day_youth,
             sprintf("Maximum %d youth day passes per transaction.",  as.integer(rv_config$max_day_youth))),
        need(t$under9   <= rv_config$max_day_under9,
             sprintf("Maximum %d under-9 entries per transaction.",   as.integer(rv_config$max_day_under9))),
        need(t$families <= rv_config$max_day_family,
             sprintf("Maximum %d family day passes per transaction.", as.integer(rv_config$max_day_family)))
      )

      if (t$total_cents > MAX_TOTAL_DOLL * 100) {
        showModal(
          modalDialog(
            title = "Transaction amount too large",
            paste0(
              "Online day-pass transactions are limited to $",
              MAX_TOTAL_DOLL,
              " per purchase. Your total was $",
              sprintf("%0.2f", t$total_cents / 100),
              ". For larger purchases, please contact the club."
            ),
            easyClose = TRUE,
            footer = NULL
          )
        )
        return()
      }

      today    <- Sys.Date()
      max_date <- today + MAX_SKI_DATE_OFFSET_DAYS

      if (d < today || d > max_date) {
        showModal(
          modalDialog(
            title = "Invalid ski date",
            paste0(
              "Online passes are only available for dates from ",
              format(today, "%Y-%m-%d"),
              " to ",
              format(max_date, "%Y-%m-%d"),
              ". You selected ",
              format(d, "%Y-%m-%d"),
              "."
            ),
            easyClose = TRUE,
            footer = NULL
          )
        )
        return()
      }

      current_blocked <- blocked_dates()
      if (length(current_blocked) > 0 && d %in% current_blocked) {
        showModal(
          modalDialog(
            title = "Online passes not available for this date",
            "Online day passes are disabled for this date (free day, closure, or off-season). Please use cash or follow club instructions.",
            easyClose = TRUE,
            footer = NULL
          )
        )
        return()
      }

      return_url <- build_return_url(session)

      info <- NULL
      err  <- NULL
      tryCatch({
        info <- create_square_checkout(
          total_cents = t$total_cents,
          item_name   = "Bulkley Valley XC day passes",
          return_url  = return_url
        )
      }, error = function(e) {
        err <<- e$message
      })

      if (!is.null(err) || is.null(info)) {
        output$status <- renderUI({
          tags$p(
            paste("Error creating Square checkout:", err %||% "Unknown error"),
            style = "color: #b00; white-space: pre-wrap;"
          )
        })
        return()
      }

      row_df <- data.frame(
        created          = as.character(Sys.time()),
        ski_date         = as.character(d),
        product_type     = "day",
        adults           = t$adults,
        youths           = t$youths,
        under9           = t$under9,
        families         = t$families,
        christmas_passes = 0L,
        total_cents      = as.integer(t$total_cents),
        donation_cents   = 0L,
        name             = sanitize_for_storage(input$name),
        email            = sanitize_for_storage(input$email),
        checkout_id      = info$id,
        status           = "day_link_created",
        stringsAsFactors = FALSE
      )
      save_transaction(row_df)

      session$sendCustomMessage("redirectToSquare", info$url)

      output$status <- renderUI({
        tags$p("Redirecting to Square payment page...", style = "margin-top: 1rem;")
      })

    }, error = function(e) {
      output$status <- renderUI({
        tags$p(
          paste("Unexpected error in day-pass Pay handler:", e$message),
          style = "color: #b00; white-space: pre-wrap;"
        )
      })
    })
  })

  # ---------- DONATIONS ----------

  donation_only_total <- reactive({
    donation_raw <- input$donation_only_amount
    if (is.null(donation_raw) || is.na(donation_raw)) {
      donation_raw <- 0
    }
    donation_dollars <- suppressWarnings(as.numeric(donation_raw))
    if (is.na(donation_dollars) || donation_dollars < 0) {
      donation_dollars <- 0
    }
    donation_cents <- as.integer(round(donation_dollars * 100))
    list(
      donation_dollars = donation_dollars,
      donation_cents   = donation_cents
    )
  })

  output$donation_only_summary <- renderUI({
    t <- donation_only_total()
    if (t$donation_cents <= 0) {
      return(tags$p("No donation amount entered.", style = "font-weight: 600;"))
    }
    total_dollars <- t$donation_cents / 100
    tagList(
      tags$p(sprintf("Donation amount: $%0.2f CAD", total_dollars)),
      tags$p(
        sprintf("Total to pay: $%0.2f CAD%s", total_dollars, SQUARE_ENV_LABEL),
        style = "font-weight: 700; font-size: 1.2rem;"
      )
    )
  })

  observeEvent(input$donation_only_amount, ignoreInit = TRUE, {
    val_raw <- input$donation_only_amount
    if (is.null(val_raw) || is.na(val_raw)) {
      updateNumericInput(session, "donation_only_amount", value = 0)
      return()
    }
    val <- suppressWarnings(as.numeric(val_raw))
    if (is.na(val) || val < 0) {
      updateNumericInput(session, "donation_only_amount", value = 0)
      return()
    }
    if (val > rv_config$max_donation_amount) {
      updateNumericInput(session, "donation_only_amount", value = rv_config$max_donation_amount)
      showNotification(
        sprintf("Maximum online donation is $%0.2f per transaction.", rv_config$max_donation_amount),
        type = "error", duration = 4
      )
    }
  })

  observeEvent(input$donation_only_pay, {
    tryCatch({

      if (!tab_flags$donation) {
        output$donation_only_status <- renderUI({
          tags$p(
            "Online donations are currently disabled.",
            style = "color: #b00; white-space: pre-wrap;"
          )
        })
        return()
      }

      t <- donation_only_total()
      MAX_DONATION_DOLL <- rv_config$max_donation_amount

      validate(
        need(t$donation_cents > 0, "Please enter a positive donation amount."),
        need(t$donation_dollars <= MAX_DONATION_DOLL,
             sprintf("Maximum online donation is $%0.2f per transaction.", MAX_DONATION_DOLL)),
        need(nchar(input$donation_only_name)  > 0, "Please enter a name."),
        need(nchar(input$donation_only_email) > 0, "Please enter an email.")
      )

      return_url <- build_return_url(session)

      info <- NULL
      err  <- NULL
      tryCatch({
        info <- create_square_checkout(
          total_cents = t$donation_cents,
          item_name   = "Bulkley Valley XC donation",
          return_url  = return_url
        )
      }, error = function(e) {
        err <<- e$message
      })

      if (!is.null(err) || is.null(info)) {
        output$donation_only_status <- renderUI({
          tags$p(
            paste("Error creating Square checkout for donation:", err %||% "Unknown error"),
            style = "color: #b00; white-space: pre-wrap;"
          )
        })
        return()
      }

      row_df <- data.frame(
        created          = as.character(Sys.time()),
        ski_date         = as.character(Sys.Date()),
        product_type     = "donation",
        adults           = 0L,
        youths           = 0L,
        under9           = 0L,
        families         = 0L,
        christmas_passes = 0L,
        total_cents      = as.integer(t$donation_cents),
        donation_cents   = as.integer(t$donation_cents),
        name             = sanitize_for_storage(input$donation_only_name),
        email            = sanitize_for_storage(input$donation_only_email),
        checkout_id      = info$id,
        status           = "donation_link_created",
        stringsAsFactors = FALSE
      )
      save_transaction(row_df)

      details_df <- data.frame(
        checkout_id = info$id,
        address     = sanitize_for_storage(input$donation_only_address),
        postal      = sanitize_for_storage(input$donation_only_postal),
        contact_ok  = input$donation_only_contact_ok %||% "",
        public_ack  = input$donation_only_public_ack %||% "",
        notes       = sanitize_for_storage(input$donation_only_notes),
        stringsAsFactors = FALSE
      )
      append_donation_details(details_df)

      session$sendCustomMessage("redirectToSquare", info$url)

      output$donation_only_status <- renderUI({
        tags$p("Redirecting to Square payment page for donation...", style = "margin-top: 1rem;")
      })

    }, error = function(e) {
      output$donation_only_status <- renderUI({
        tags$p(
          e$message,
          style = "color: #b00; white-space: pre-wrap;"
        )
      })
    })
  })

  # ---------- CHRISTMAS PASSES ----------

  christmas_total <- reactive({
    christmas_raw <- input$n_christmas
    if (is.null(christmas_raw) || is.na(christmas_raw)) {
      christmas_raw <- 0
    }
    christmas <- as.integer(christmas_raw)
    total_cents <- christmas * rv_config$price_christmas_pass
    list(
      christmas   = christmas,
      total_cents = total_cents
    )
  })

  output$christmas_summary <- renderUI({
    t <- christmas_total()
    if (t$christmas == 0) {
      return(tags$p("No Christmas passes selected.", style = "font-weight: 600;"))
    }
    total_dollars <- t$total_cents / 100
    tagList(
      tags$p(sprintf("Christmas 2-week passes: %d", t$christmas)),
      tags$p(
        sprintf("Total for Christmas passes: $%0.2f CAD%s", total_dollars, SQUARE_ENV_LABEL),
        style = "font-weight: 700; font-size: 1.2rem;"
      )
    )
  })

  output$christmas_holder_form <- renderUI({
    n_raw <- input$n_christmas
    if (is.null(n_raw) || is.na(n_raw)) n_raw <- 0
    n <- as.integer(n_raw)
    if (n <= 0L) return(NULL)

    tagList(
      h4("Pass holder details"),
      lapply(seq_len(n), function(i) {
        tagList(
          h5(paste("Pass", i)),
          textInput(
            inputId = paste0("christmas_holder_name_", i),
            label   = "Full name",
            value   = ""
          ),
          tags$hr()
        )
      })
    )
  })

  observeEvent(input$n_christmas, ignoreInit = TRUE, {
    val_raw <- input$n_christmas
    if (is.null(val_raw) || is.na(val_raw)) {
      updateNumericInput(session, "n_christmas", value = 0)
      return()
    }
    val <- as.integer(val_raw)
    if (val > rv_config$max_christmas_passes) {
      updateNumericInput(session, "n_christmas", value = rv_config$max_christmas_passes)
      showNotification(
        sprintf("Maximum %d Christmas passes per transaction.", as.integer(rv_config$max_christmas_passes)),
        type = "error", duration = 4
      )
    } else if (val < 0) {
      updateNumericInput(session, "n_christmas", value = 0)
    }
  })

  observeEvent(input$christmas_pay, {
    tryCatch({

      if (!tab_flags$christmas) {
        output$christmas_status <- renderUI({
          tags$p(
            "Christmas pass payments are currently disabled.",
            style = "color: #b00; white-space: pre-wrap;"
          )
        })
        return()
      }

      t <- christmas_total()

      MAX_CHRISTMAS_PASSES <- rv_config$max_christmas_passes
      MAX_CHRISTMAS_DOLL   <- rv_config$max_christmas_amount

      n_passes <- t$christmas
      holder_names <- character(0)

      if (!is.na(n_passes) && n_passes > 0L) {
        holder_names <- vapply(
          seq_len(n_passes),
          function(i) {
            val <- input[[paste0("christmas_holder_name_", i)]] %||% ""
            sanitize_for_storage(val)
          },
          FUN.VALUE = character(1)
        )
      }

      validate(
        need(t$christmas > 0, "Please select at least one Christmas pass."),
        need(t$christmas <= MAX_CHRISTMAS_PASSES,
             sprintf("Maximum %d Christmas passes per transaction.", as.integer(MAX_CHRISTMAS_PASSES))),
        need(nchar(input$christmas_name)  > 0, "Please enter a name."),
        need(nchar(input$christmas_email) > 0, "Please enter an email."),
        need(all(nzchar(holder_names)), "Please enter a name for each Christmas pass.")
      )

      if (t$total_cents > MAX_CHRISTMAS_DOLL * 100) {
        showModal(
          modalDialog(
            title = "Transaction amount too large",
            paste0(
              "Christmas pass transactions are limited to $",
              MAX_CHRISTMAS_DOLL,
              " per purchase. Your total was $",
              sprintf("%0.2f", t$total_cents / 100),
              ". For larger group purchases, please contact the club."
            ),
            easyClose = TRUE,
            footer = NULL
          )
        )
        return()
      }

      christmas_start <- as.Date(input$christmas_start)
      validate(
        need(!is.na(christmas_start), "Please select a start date for the Christmas 2-week pass.")
      )

      year          <- as.integer(format(christmas_start, "%Y"))
      christmas_day <- as.Date(paste0(year, "-12-25"))
      period_end    <- christmas_start + 13

      if (!(christmas_start <= christmas_day && christmas_day <= period_end)) {
        showModal(
          modalDialog(
            title = "Christmas pass period must include Christmas Day",
            paste0(
              "For a Christmas 2-week pass, the 14-day period from ",
              format(christmas_start, "%Y-%m-%d"), " to ",
              format(period_end, "%Y-%m-%d"),
              " must include Christmas Day (", format(christmas_day, "%Y-%m-%d"), ")."
            ),
            easyClose = TRUE,
            footer = NULL
          )
        )
        return()
      }

      return_url <- build_return_url(session)

      info <- NULL
      err  <- NULL
      tryCatch({
        info <- create_square_checkout(
          total_cents = t$total_cents,
          item_name   = "Bulkley Valley XC Christmas 2-week pass",
          return_url  = return_url
        )
      }, error = function(e) {
        err <<- e$message
      })

      if (!is.null(err) || is.null(info)) {
        output$christmas_status <- renderUI({
          tags$p(
            paste("Error creating Square checkout for Christmas passes:", err %||% "Unknown error"),
            style = "color: #b00; white-space: pre-wrap;"
          )
        })
        return()
      }

      row_df <- data.frame(
        created          = as.character(Sys.time()),
        ski_date         = as.character(Sys.Date()),
        product_type     = "christmas",
        adults           = 0L,
        youths           = 0L,
        under9           = 0L,
        families         = 0L,
        christmas_passes = t$christmas,
        total_cents      = as.integer(t$total_cents),
        donation_cents   = 0L,
        name             = sanitize_for_storage(input$christmas_name),
        email            = sanitize_for_storage(input$christmas_email),
        checkout_id      = info$id,
        status           = "christmas_link_created",
        stringsAsFactors = FALSE
      )
      save_transaction(row_df)

      if (length(holder_names) > 0L) {
        regs_df <- data.frame(
          checkout_id  = info$id,
          product_type = "christmas",
          holder_name  = holder_names,
          holder_type  = "christmas",
          holder_dob   = NA_character_,
          notes        = "",
          stringsAsFactors = FALSE
        )
        append_registrations(regs_df)
      }

      session$sendCustomMessage("redirectToSquare", info$url)

      output$christmas_status <- renderUI({
        tags$p("Redirecting to Square payment page for Christmas passes...", style = "margin-top: 1rem;")
      })
    }, error = function(e) {
      output$christmas_status <- renderUI({
        tags$p(
          e$message,
          style = "color: #b00; white-space: pre-wrap;"
        )
      })
    })
  })

  # ---------- SEASON PASSES ----------

  season_total <- reactive({
    na_raw <- input$n_season_adult
    ny_raw <- input$n_season_youth
    nf_raw <- input$n_season_family

    if (is.null(na_raw) || is.na(na_raw)) na_raw <- 0
    if (is.null(ny_raw) || is.na(ny_raw)) ny_raw <- 0
    if (is.null(nf_raw) || is.na(nf_raw)) nf_raw <- 0

    adults   <- as.integer(na_raw)
    youths   <- as.integer(ny_raw)
    families <- as.integer(nf_raw)

    passes_cents <- adults   * rv_config$price_season_adult +
                    youths   * rv_config$price_season_youth +
                    families * rv_config$price_season_family

    list(
      adults       = adults,
      youths       = youths,
      families     = families,
      passes_cents = passes_cents
    )
  })

  output$season_holder_form <- renderUI({
    na_raw <- input$n_season_adult
    ny_raw <- input$n_season_youth
    nf_raw <- input$n_season_family

    if (is.null(na_raw) || is.na(na_raw)) na_raw <- 0
    if (is.null(ny_raw) || is.na(ny_raw)) ny_raw <- 0
    if (is.null(nf_raw) || is.na(nf_raw)) nf_raw <- 0

    na <- as.integer(na_raw)
    ny <- as.integer(ny_raw)
    nf <- as.integer(nf_raw)

    if (na <= 0L && ny <= 0L && nf <= 0L) return(NULL)

    out <- list(h4("Season pass holder details"))

    if (na > 0L) {
      out <- c(out, list(
        h5("Adult passes"),
        lapply(seq_len(na), function(i) {
          tagList(
            textInput(
              paste0("season_adult_name_", i),
              paste("Adult pass", i, "- full name")
            ),
            dateInput(
              paste0("season_adult_dob_", i),
              "Date of birth",
              value  = NULL,
              format = "yyyy-mm-dd"
            ),
            tags$hr()
          )
        })
      ))
    }

    if (ny > 0L) {
      out <- c(out, list(
        h5("Youth passes"),
        lapply(seq_len(ny), function(i) {
          tagList(
            textInput(
              paste0("season_youth_name_", i),
              paste("Youth pass", i, "- full name")
            ),
            dateInput(
              paste0("season_youth_dob_", i),
              "Date of birth",
              value  = NULL,
              format = "yyyy-mm-dd"
            ),
            tags$hr()
          )
        })
      ))
    }

    if (nf > 0L) {
      out <- c(out, list(
        h5("Family passes"),
        lapply(seq_len(nf), function(i) {
          tagList(
            textInput(
              paste0("season_family_name_", i),
              paste("Family pass", i, "- family / household name")
            ),
            dateInput(
              paste0("season_family_dob_", i),
              "Date of birth (main holder)",
              value  = NULL,
              format = "yyyy-mm-dd"
            ),
            tags$hr()
          )
        })
      ))
    }

    do.call(tagList, out)
  })

  output$season_summary <- renderUI({
    t <- season_total()

    if (t$adults == 0 && t$youths == 0 && t$families == 0) {
      return(tags$p("No season passes selected.", style = "font-weight: 600;"))
    }

    total_dollars <- t$passes_cents / 100

    tagList(
      tags$p(
        sprintf(
          "Adult season: %d, Youth season: %d, Family season: %d",
          t$adults, t$youths, t$families
        )
      ),
      tags$p(
        sprintf("Total for season passes: $%0.2f CAD%s", total_dollars, SQUARE_ENV_LABEL),
        style = "font-weight: 700; font-size: 1.2rem;"
      )
    )
  })

  observeEvent(input$n_season_adult, ignoreInit = TRUE, {
    val_raw <- input$n_season_adult
    if (is.null(val_raw) || is.na(val_raw)) {
      updateNumericInput(session, "n_season_adult", value = 0)
      return()
    }
    val <- as.integer(val_raw)
    if (val > rv_config$max_season_adult) {
      updateNumericInput(session, "n_season_adult", value = rv_config$max_season_adult)
      showNotification(
        sprintf("Maximum %d adult season passes per transaction.", as.integer(rv_config$max_season_adult)),
        type = "error", duration = 4
      )
    } else if (val < 0) {
      updateNumericInput(session, "n_season_adult", value = 0)
    }
  })

  observeEvent(input$n_season_youth, ignoreInit = TRUE, {
    val_raw <- input$n_season_youth
    if (is.null(val_raw) || is.na(val_raw)) {
      updateNumericInput(session, "n_season_youth", value = 0)
      return()
    }
    val <- as.integer(val_raw)
    if (val > rv_config$max_season_youth) {
      updateNumericInput(session, "n_season_youth", value = rv_config$max_season_youth)
      showNotification(
        sprintf("Maximum %d youth season passes per transaction.", as.integer(rv_config$max_season_youth)),
        type = "error", duration = 4
      )
    } else if (val < 0) {
      updateNumericInput(session, "n_season_youth", value = 0)
    }
  })

  observeEvent(input$n_season_family, ignoreInit = TRUE, {
    val_raw <- input$n_season_family
    if (is.null(val_raw) || is.na(val_raw)) {
      updateNumericInput(session, "n_season_family", value = 0)
      return()
    }
    val <- as.integer(val_raw)
    if (val > rv_config$max_season_family) {
      updateNumericInput(session, "n_season_family", value = rv_config$max_season_family)
      showNotification(
        sprintf("Maximum %d family season passes per transaction.", as.integer(rv_config$max_season_family)),
        type = "error", duration = 4
      )
    } else if (val < 0) {
      updateNumericInput(session, "n_season_family", value = 0)
    }
  })

  observeEvent(input$season_pay, {
    tryCatch({

      if (!tab_flags$season) {
        output$season_status <- renderUI({
          tags$p(
            "Season pass payments are currently disabled.",
            style = "color: #b00; white-space: pre-wrap;"
          )
        })
        return()
      }

      t <- season_total()

      MAX_SEASON_ADULT   <- rv_config$max_season_adult
      MAX_SEASON_YOUTH   <- rv_config$max_season_youth
      MAX_SEASON_FAMILY  <- rv_config$max_season_family
      MAX_SEASON_DOLL    <- rv_config$max_season_amount

      na <- t$adults
      ny <- t$youths
      nf <- t$families

      adult_names <- if (!is.na(na) && na > 0L) vapply(
        seq_len(na),
        function(i) sanitize_for_storage(input[[paste0("season_adult_name_", i)]]),
        FUN.VALUE = character(1)
      ) else character(0)

      youth_names <- if (!is.na(ny) && ny > 0L) vapply(
        seq_len(ny),
        function(i) sanitize_for_storage(input[[paste0("season_youth_name_", i)]]),
        FUN.VALUE = character(1)
      ) else character(0)

      family_names <- if (!is.na(nf) && nf > 0L) vapply(
        seq_len(nf),
        function(i) sanitize_for_storage(input[[paste0("season_family_name_", i)]]),
        FUN.VALUE = character(1)
      ) else character(0)

      adult_dobs <- if (!is.na(na) && na > 0L) vapply(
        seq_len(na),
        function(i) as.Date(input[[paste0("season_adult_dob_", i)]]),
        FUN.VALUE = as.Date(NA)
      ) else as.Date(character(0))

      youth_dobs <- if (!is.na(ny) && ny > 0L) vapply(
        seq_len(ny),
        function(i) as.Date(input[[paste0("season_youth_dob_", i)]]),
        FUN.VALUE = as.Date(NA)
      ) else as.Date(character(0))

      family_dobs <- if (!is.na(nf) && nf > 0L) vapply(
        seq_len(nf),
        function(i) as.Date(input[[paste0("season_family_dob_", i)]]),
        FUN.VALUE = as.Date(NA)
      ) else as.Date(character(0))

      ref_date   <- Sys.Date()
      adult_ages <- compute_age_years(adult_dobs, ref_date)
      youth_ages <- compute_age_years(youth_dobs, ref_date)

      invalid_youth <- length(youth_ages) > 0 &&
        any(youth_ages < YOUTH_MIN_AGE | youth_ages > YOUTH_MAX_AGE, na.rm = TRUE)

      invalid_adult <- length(adult_ages) > 0 &&
        any(adult_ages < ADULT_MIN_AGE, na.rm = TRUE)

      validate(
        need(na > 0 || ny > 0 || nf > 0,
             "Please select at least one season pass."),
        need(nchar(input$season_name)  > 0, "Please enter a name."),
        need(nchar(input$season_email) > 0, "Please enter an email."),
        need(na <= MAX_SEASON_ADULT,
             sprintf("Maximum %d adult season passes per transaction.",  as.integer(MAX_SEASON_ADULT))),
        need(ny <= MAX_SEASON_YOUTH,
             sprintf("Maximum %d youth season passes per transaction.",  as.integer(MAX_SEASON_YOUTH))),
        need(nf <= MAX_SEASON_FAMILY,
             sprintf("Maximum %d family season passes per transaction.", as.integer(MAX_SEASON_FAMILY))),
        need(all(nzchar(adult_names)),  "Please enter a name for each adult season pass."),
        need(all(nzchar(youth_names)),  "Please enter a name for each youth season pass."),
        need(all(nzchar(family_names)), "Please enter a name for each family season pass."),
        need(length(adult_dobs)  == 0 || all(!is.na(adult_dobs)),
             "Please enter a date of birth for each adult season pass holder."),
        need(length(youth_dobs)  == 0 || all(!is.na(youth_dobs)),
             "Please enter a date of birth for each youth season pass holder."),
        need(length(family_dobs) == 0 || all(!is.na(family_dobs)),
             "Please enter a date of birth for each family season pass holder (main holder)."),
        need(!invalid_youth,
             sprintf("One or more youth season passes have a DOB outside the youth age range (%d–%d). Please adjust the category or DOB.",
                     YOUTH_MIN_AGE, YOUTH_MAX_AGE)),
        need(!invalid_adult,
             sprintf("One or more adult season passes have a DOB under %d years. Please change to a youth pass if appropriate.", ADULT_MIN_AGE))
      )

      if (t$passes_cents > MAX_SEASON_DOLL * 100) {
        showModal(
          modalDialog(
            title = "Transaction amount too large",
            paste0(
              "Season-pass transactions are limited to $",
              MAX_SEASON_DOLL,
              " per purchase. Your total was $",
              sprintf("%0.2f", t$passes_cents / 100),
              ". For larger purchases, please contact the club."
            ),
            easyClose = TRUE,
            footer = NULL
          )
        )
        return()
      }

      return_url <- build_return_url(session)

      info <- NULL
      err  <- NULL
      tryCatch({
        info <- create_square_checkout(
          total_cents = t$passes_cents,
          item_name   = paste("Bulkley Valley XC season passes –", rv_config$season_label),
          return_url  = return_url
        )
      }, error = function(e) {
        err <<- e$message
      })

      if (!is.null(err) || is.null(info)) {
        output$season_status <- renderUI({
          tags$p(
            paste("Error creating Square checkout for season passes:", err %||% "Unknown error"),
            style = "color: #b00; white-space: pre-wrap;"
          )
        })
        return()
      }

      row_df <- data.frame(
        created          = as.character(Sys.time()),
        ski_date         = as.character(Sys.Date()),
        product_type     = "season",
        adults           = na,
        youths           = ny,
        under9           = 0L,
        families         = nf,
        christmas_passes = 0L,
        total_cents      = as.integer(t$passes_cents),
        donation_cents   = 0L,
        name             = sanitize_for_storage(input$season_name),
        email            = sanitize_for_storage(input$season_email),
        checkout_id      = info$id,
        status           = "season_link_created",
        stringsAsFactors = FALSE
      )
      save_transaction(row_df)

      regs_rows <- list()

      if (length(adult_names) > 0L) {
        regs_rows[[length(regs_rows) + 1]] <- data.frame(
          checkout_id  = info$id,
          product_type = "season",
          holder_name  = adult_names,
          holder_type  = "adult",
          holder_dob   = as.character(adult_dobs),
          notes        = "",
          stringsAsFactors = FALSE
        )
      }

      if (length(youth_names) > 0L) {
        regs_rows[[length(regs_rows) + 1]] <- data.frame(
          checkout_id  = info$id,
          product_type = "season",
          holder_name  = youth_names,
          holder_type  = "youth",
          holder_dob   = as.character(youth_dobs),
          notes        = "",
          stringsAsFactors = FALSE
        )
      }

      if (length(family_names) > 0L) {
        regs_rows[[length(regs_rows) + 1]] <- data.frame(
          checkout_id  = info$id,
          product_type = "season",
          holder_name  = family_names,
          holder_type  = "family",
          holder_dob   = as.character(family_dobs),
          notes        = "",
          stringsAsFactors = FALSE
        )
      }

      if (length(regs_rows) > 0L) {
        append_registrations(do.call(rbind, regs_rows))
      }

      session$sendCustomMessage("redirectToSquare", info$url)

      output$season_status <- renderUI({
        tags$p("Redirecting to Square payment page for season passes...", style = "margin-top: 1rem;")
      })

    }, error = function(e) {
      output$season_status <- renderUI({
        tags$p(
          e$message,
          style = "color: #b00; white-space: pre-wrap;"
        )
      })
    })
  })

  # ---------- ADMIN: REPORTS + SETTINGS ----------

  filtered_log <- reactive({
    req(admin_ok())
    df <- load_transactions()
    if (nrow(df) == 0) return(df)

    if (!is.null(input$admin_daterange) && length(input$admin_daterange) == 2) {
      start_date <- as.Date(input$admin_daterange[1])
      end_date   <- as.Date(input$admin_daterange[2])
      if (!is.na(start_date) && !is.na(end_date)) {
        df_date <- as.Date(df$ski_date)
        df <- df[!is.na(df_date) & df_date >= start_date & df_date <= end_date, , drop = FALSE]
      }
    }

    if (!is.null(input$admin_name) && nchar(trimws(input$admin_name)) > 0) {
      pattern <- tolower(trimws(input$admin_name))
      df <- df[grepl(pattern, tolower(df$name)), , drop = FALSE]
    }

    if (!is.null(input$admin_status) && nchar(trimws(input$admin_status)) > 0) {
      pattern <- tolower(trimws(input$admin_status))
      df <- df[grepl(pattern, tolower(df$status)), , drop = FALSE]
    }

    df[order(df$created, decreasing = TRUE), , drop = FALSE]
  })

  admin_registrations <- reactive({
    req(admin_ok())
    df_log <- filtered_log()
    if (nrow(df_log) == 0) return(data.frame())
    regs <- load_registrations()
    if (nrow(regs) == 0) return(regs)
    regs[regs$checkout_id %in% df_log$checkout_id, , drop = FALSE]
  })

  output$admin_table <- renderTable({
    req(admin_ok())
    df <- filtered_log()
    if (nrow(df) == 0) return(NULL)

    df$total_dollars    <- df$total_cents    / 100
    df$donation_dollars <- df$donation_cents / 100

    df$created  <- as.character(df$created)
    df$ski_date <- as.character(df$ski_date)

    df[, c("id", "created", "ski_date",
           "product_type",
           "adults", "youths", "under9", "families", "christmas_passes",
           "total_dollars", "donation_dollars",
           "name", "email", "status", "checkout_id")]
  })

  output$admin_overall_summary <- renderTable({
    req(admin_ok())
    df <- filtered_log()
    if (nrow(df) == 0) return(NULL)

    n_tx            <- nrow(df)
    total_amount    <- sum(df$total_cents,    na.rm = TRUE) / 100
    total_donations <- sum(df$donation_cents, na.rm = TRUE) / 100
    tx_with_don     <- sum(df$donation_cents > 0, na.rm = TRUE)

    avg_tx             <- if (n_tx > 0) total_amount / n_tx else NA_real_
    avg_don_per_tx     <- if (n_tx > 0) total_donations / n_tx else NA_real_
    avg_don_per_don_tx <- if (tx_with_don > 0) total_donations / tx_with_don else NA_real_
    donation_share_pct <- if (total_amount > 0) 100 * total_donations / total_amount else NA_real_

    data.frame(
      metric = c(
        "Transactions",
        "Total amount ($)",
        "Total donations ($)",
        "Transactions with any donation",
        "Average transaction value ($)",
        "Average donation per transaction ($)",
        "Average donation per donating transaction ($)",
        "Donations as % of total revenue"
      ),
      value = c(
        n_tx,
        round(total_amount, 2),
        round(total_donations, 2),
        tx_with_don,
        round(avg_tx, 2),
        round(avg_don_per_tx, 2),
        round(avg_don_per_don_tx, 2),
        round(donation_share_pct, 1)
      ),
      stringsAsFactors = FALSE
    )
  })

  output$admin_product_summary <- renderTable({
    req(admin_ok())
    df <- filtered_log()
    if (nrow(df) == 0) return(NULL)

    agg <- aggregate(
      cbind(adults, youths, under9, families, christmas_passes,
            total_cents, donation_cents) ~ product_type,
      data = df,
      FUN  = sum,
      na.rm = TRUE
    )

    agg$total_amount    <- agg$total_cents    / 100
    agg$total_donations <- agg$donation_cents / 100

    agg[, c("product_type",
            "adults", "youths", "under9", "families", "christmas_passes",
            "total_amount", "total_donations")]
  })

  output$admin_totals <- renderTable({
    req(admin_ok())
    df <- filtered_log()
    if (nrow(df) == 0) return(NULL)

    agg <- aggregate(
      cbind(adults, youths, under9, families, christmas_passes,
            total_cents, donation_cents) ~ ski_date,
      data = df,
      FUN  = sum,
      na.rm = TRUE
    )

    agg$total_amount    <- agg$total_cents    / 100
    agg$total_donations <- agg$donation_cents / 100

    agg$ski_date <- as.character(agg$ski_date)

    agg[, c("ski_date",
            "adults", "youths", "under9", "families", "christmas_passes",
            "total_donations", "total_amount")]
  })

  output$admin_registrations <- renderTable({
    regs <- admin_registrations()
    if (nrow(regs) == 0) return(NULL)
    regs[order(regs$checkout_id, regs$holder_type, regs$holder_name), ]
  })

  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("bvxc_passes_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(admin_ok())
      df <- filtered_log()
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_all <- downloadHandler(
    filename = function() {
      paste0("bvxc_passes_full_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(admin_ok())
      df <- load_transactions()
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Blocked dates display + controls
  output$blocked_dates_table <- renderTable({
    req(admin_ok())
    bd <- blocked_dates()
    if (length(bd) == 0) return(NULL)
    data.frame(
      blocked_date = as.character(bd),
      stringsAsFactors = FALSE
    )
  })

  observe({
    bd <- blocked_dates()
    updateSelectInput(
      session,
      "blocked_date_to_remove",
      choices = as.character(bd)
    )
  })

  observeEvent(input$add_blocked_date, {
    req(admin_ok())
    d <- as.Date(input$admin_block_date)
    if (is.na(d)) return()
    bd <- blocked_dates()
    if (d %in% bd) return()
    new_bd <- sort(unique(c(bd, d)))
    blocked_dates(new_bd)
    set_blocked_dates(new_bd)
  })

  observeEvent(input$remove_blocked_date, {
    req(admin_ok())
    d_str <- input$blocked_date_to_remove
    if (is.null(d_str) || d_str == "") return()
    d  <- as.Date(d_str)
    bd <- blocked_dates()
    new_bd <- bd[bd != d]
    blocked_dates(new_bd)
    set_blocked_dates(new_bd)
  })

  # Admin manual status update (for reconciliation)
  observeEvent(input$admin_update_status_btn, {
    req(admin_ok())

    tx_id <- input$admin_update_tx_id
    new_status <- input$admin_update_status_value %||% ""

    if (is.null(tx_id) || is.na(tx_id) || tx_id <= 0) {
      output$admin_update_status_message <- renderText("Please enter a valid transaction ID.")
      return()
    }
    if (!nzchar(new_status)) {
      output$admin_update_status_message <- renderText("Please choose a new status.")
      return()
    }

    con <- get_db_connection()
    on.exit(dbDisconnect(con), add = TRUE)

    n_updated <- dbExecute(
      con,
      "UPDATE transactions SET status = ? WHERE id = ?",
      params = list(new_status, as.integer(tx_id))
    )

    if (n_updated > 0) {
      output$admin_update_status_message <- renderText(
        sprintf("Updated transaction ID %d to status '%s'.", as.integer(tx_id), new_status)
      )
    } else {
      output$admin_update_status_message <- renderText(
        sprintf("No transaction found with ID %d.", as.integer(tx_id))
      )
    }
  })

  # Admin body UI
  output$admin_body <- renderUI({
    if (!admin_ok()) return(NULL)

    tabsetPanel(
      id = "admin_tabs",
      tabPanel(
        "Reports",
        br(),
        h4("Filters"),
        fluidRow(
          column(
            4,
            dateRangeInput(
              "admin_daterange",
              "Date range",
              start = Sys.Date(),
              end   = Sys.Date()
            )
          ),
          column(
            4,
            textInput("admin_name", "Filter by name (optional)")
          ),
          column(
            4,
            textInput(
              "admin_status",
              "Filter by status (optional, e.g. day_link_created)"
            )
          )
        ),
        tags$hr(),
        h4("Overall summary (filtered range)"),
        tableOutput("admin_overall_summary"),
        tags$hr(),
        h4("By product type"),
        tableOutput("admin_product_summary"),
        tags$hr(),
        h4("Daily totals by date"),
        tableOutput("admin_totals"),
        tags$hr(),
        h4("Transactions (filtered)"),
        tableOutput("admin_table"),
        tags$hr(),
        h4("Registered pass holders (filtered)"),
        tableOutput("admin_registrations"),
        tags$hr(),
        h4("Export"),
        p("Download the current filtered view or the full log as CSV for Excel/Sheets."),
        downloadButton("download_filtered", "Download filtered CSV"),
        tags$span(" "),
        downloadButton("download_all", "Download full CSV"),
        tags$hr(),
        h4("Update transaction status (manual reconciliation)"),
        p("After confirming payment status in Square, you can manually update the status field here."),
        fluidRow(
          column(
            4,
            numericInput(
              "admin_update_tx_id",
              "Transaction ID (from table above)",
              value = NA,
              min   = 1,
              step  = 1
            )
          ),
          column(
            4,
            selectInput(
              "admin_update_status_value",
              "New status",
              choices = c(
                "day_link_created",
                "season_link_created",
                "christmas_link_created",
                "donation_link_created",
                "paid_confirmed",
                "cancelled",
                "refunded"
              ),
              selected = "paid_confirmed"
            )
          ),
          column(
            4,
            actionButton(
              "admin_update_status_btn",
              "Update status"
            )
          )
        ),
        verbatimTextOutput("admin_update_status_message")
      ),
      tabPanel(
        "Prices / limits / tabs",
        br(),
        h4("Day-pass prices (CAD)"),
        p("Adjust prices here and click \"Save day-pass prices\". Changes apply to new online day-pass purchases."),
        fluidRow(
          column(3, numericInput("price_adult",   "Adult ($)",   value = rv_config$price_adult_day  / 100, min = 0, step = 1)),
          column(3, numericInput("price_youth",   "Youth ($)",   value = rv_config$price_youth_day  / 100, min = 0, step = 1)),
          column(3, numericInput("price_family",  "Family ($)",  value = rv_config$price_family_day / 100, min = 0, step = 1)),
          column(3, numericInput("price_under9",  "Under-9 ($)", value = rv_config$price_under9_day / 100, min = 0, step = 1))
        ),
        actionButton("save_prices", "Save day-pass prices"),

        tags$hr(),
        h4("Season / Christmas prices (CAD)"),
        fluidRow(
          column(3, numericInput("season_price_adult",     "Adult season ($)",     value = rv_config$price_season_adult   / 100, min = 0, step = 5)),
          column(3, numericInput("season_price_youth",     "Youth season ($)",     value = rv_config$price_season_youth   / 100, min = 0, step = 5)),
          column(3, numericInput("season_price_family",    "Family season ($)",    value = rv_config$price_season_family  / 100, min = 0, step = 5)),
          column(3, numericInput("season_price_christmas", "Christmas 2-week ($)", value = rv_config$price_christmas_pass / 100, min = 0, step = 5))
        ),
        textInput("season_label_edit", "Season label", value = rv_config$season_label),
        actionButton("save_season_prices", "Save season/Christmas prices"),

        tags$hr(),
        h4("Transaction limits"),
        p("Adjust maximum number of passes per transaction and maximum dollar amounts."),
        h5("Day passes"),
        fluidRow(
          column(3, numericInput("max_day_adult_edit",   "Max adult day passes",   value = rv_config$max_day_adult,   min = 0, step = 1)),
          column(3, numericInput("max_day_youth_edit",   "Max youth day passes",   value = rv_config$max_day_youth,   min = 0, step = 1)),
          column(3, numericInput("max_day_under9_edit",  "Max under-9 entries",    value = rv_config$max_day_under9,  min = 0, step = 1)),
          column(3, numericInput("max_day_family_edit",  "Max family day passes",  value = rv_config$max_day_family,  min = 0, step = 1))
        ),
        numericInput("max_day_amount_edit", "Max day-pass transaction amount ($)", value = rv_config$max_day_amount, min = 0, step = 10),

        h5("Donations"),
        numericInput("max_donation_amount_edit", "Max donation per transaction ($)", value = rv_config$max_donation_amount, min = 0, step = 10),

        h5("Christmas passes"),
        fluidRow(
          column(6, numericInput("max_christmas_passes_edit", "Max Christmas passes per transaction", value = rv_config$max_christmas_passes, min = 0, step = 1)),
          column(6, numericInput("max_christmas_amount_edit", "Max Christmas transaction amount ($)", value = rv_config$max_christmas_amount,   min = 0, step = 10))
        ),

        h5("Season passes"),
        fluidRow(
          column(3, numericInput("max_season_adult_edit",   "Max adult season passes",   value = rv_config$max_season_adult,   min = 0, step = 1)),
          column(3, numericInput("max_season_youth_edit",   "Max youth season passes",   value = rv_config$max_season_youth,   min = 0, step = 1)),
          column(3, numericInput("max_season_family_edit",  "Max family season passes",  value = rv_config$max_season_family,  min = 0, step = 1)),
          column(3, numericInput("max_season_amount_edit",  "Max season transaction ($)", value = rv_config$max_season_amount, min = 0, step = 10))
        ),
        actionButton("save_limits", "Save all limits"),

        tags$hr(),
        h4("Tab availability (live / disabled)"),
        fluidRow(
          column(3, checkboxInput("enable_day_tab",       "DAY PASS tab live",       value = isTRUE(tab_flags$day))),
          column(3, checkboxInput("enable_christmas_tab", "CHRISTMAS PASS tab live", value = isTRUE(tab_flags$christmas))),
          column(3, checkboxInput("enable_season_tab",    "SEASON PASS tab live",    value = isTRUE(tab_flags$season))),
          column(3, checkboxInput("enable_donation_tab",  "DONATION tab live",       value = isTRUE(tab_flags$donation)))
        ),
        actionButton("save_tab_flags", "Save tab availability"),

        tags$hr(),
        h4("Blocked dates (no online day-pass sales)"),
        fluidRow(
          column(6, dateInput("admin_block_date", "Add blocked date", value = Sys.Date())),
          column(6, actionButton("add_blocked_date", "Add blocked date", style = "margin-top: 25px;"))
        ),
        tags$br(),
        fluidRow(
          column(6, selectInput("blocked_date_to_remove", "Unblock date", choices = character(0))),
          column(6, actionButton("remove_blocked_date", "Remove selected date", style = "margin-top: 25px;"))
        ),
        tags$br(),
        tableOutput("blocked_dates_table")
      )
    )
  })

  # Save day prices
  observeEvent(input$save_prices, {
    req(admin_ok())

    pa <- suppressWarnings(as.numeric(input$price_adult))
    py <- suppressWarnings(as.numeric(input$price_youth))
    pf <- suppressWarnings(as.numeric(input$price_family))
    pu <- suppressWarnings(as.numeric(input$price_under9))

    if (!is.na(pa) && pa >= 0) rv_config$price_adult_day  <- as.integer(round(pa * 100))
    if (!is.na(py) && py >= 0) rv_config$price_youth_day  <- as.integer(round(py * 100))
    if (!is.na(pf) && pf >= 0) rv_config$price_family_day <- as.integer(round(pf * 100))
    if (!is.na(pu) && pu >= 0) rv_config$price_under9_day <- as.integer(round(pu * 100))

    cfg <- load_config()
    cfg$price_adult_day   <- rv_config$price_adult_day
    cfg$price_youth_day   <- rv_config$price_youth_day
    cfg$price_under9_day  <- rv_config$price_under9_day
    cfg$price_family_day  <- rv_config$price_family_day
    save_config(cfg)

    output$price_line <- renderUI({
      tags$p(
        sprintf(
          "Adults $%0.2f · Youth (9–18) $%0.2f · Under 9 %s · Family $%0.2f",
          rv_config$price_adult_day / 100,
          rv_config$price_youth_day / 100,
          if (rv_config$price_under9_day > 0) {
            sprintf("$%0.2f", rv_config$price_under9_day / 100)
          } else {
            "FREE"
          },
          rv_config$price_family_day / 100
        )
      )
    })

    showModal(
      modalDialog(
        title = "Day-pass prices updated",
        "New day-pass prices will apply to future online purchases.",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  # Save season / Christmas prices + label
  observeEvent(input$save_season_prices, {
    req(admin_ok())

    sa <- suppressWarnings(as.numeric(input$season_price_adult))
    sy <- suppressWarnings(as.numeric(input$season_price_youth))
    sf <- suppressWarnings(as.numeric(input$season_price_family))
    sc <- suppressWarnings(as.numeric(input$season_price_christmas))
    label <- input$season_label_edit %||% rv_config$season_label

    if (!is.na(sa) && sa >= 0) rv_config$price_season_adult   <- as.integer(round(sa * 100))
    if (!is.na(sy) && sy >= 0) rv_config$price_season_youth   <- as.integer(round(sy * 100))
    if (!is.na(sf) && sf >= 0) rv_config$price_season_family  <- as.integer(round(sf * 100))
    if (!is.na(sc) && sc >= 0) rv_config$price_christmas_pass <- as.integer(round(sc * 100))
    rv_config$season_label <- label

    cfg <- load_config()
    cfg$price_season_adult   <- rv_config$price_season_adult
    cfg$price_season_youth   <- rv_config$price_season_youth
    cfg$price_season_family  <- rv_config$price_season_family
    cfg$price_christmas_pass <- rv_config$price_christmas_pass
    cfg$season_label         <- rv_config$season_label
    save_config(cfg)

    output$season_price_line <- renderUI({
      tags$p(
        sprintf(
          "Adult $%0.2f · Youth $%0.2f · Family $%0.2f (sandbox demo)",
          rv_config$price_season_adult / 100,
          rv_config$price_season_youth / 100,
          rv_config$price_season_family / 100
        )
      )
    })

    output$christmas_price_line <- renderUI({
      tags$p(
        sprintf(
          "Christmas 2-week pass: $%0.2f (sandbox demo, per person)",
          rv_config$price_christmas_pass / 100
        )
      )
    })

    showModal(
      modalDialog(
        title = "Season / Christmas prices updated",
        "New season and Christmas prices will apply to future purchases.",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  # Save limits
  observeEvent(input$save_limits, {
    req(admin_ok())

    # Day limits
    mda     <- suppressWarnings(as.numeric(input$max_day_adult_edit))
    mdy     <- suppressWarnings(as.numeric(input$max_day_youth_edit))
    mdu     <- suppressWarnings(as.numeric(input$max_day_under9_edit))
    mdf     <- suppressWarnings(as.numeric(input$max_day_family_edit))
    mda_amt <- suppressWarnings(as.numeric(input$max_day_amount_edit))

    if (!is.na(mda)     && mda >= 0)     rv_config$max_day_adult      <- mda
    if (!is.na(mdy)     && mdy >= 0)     rv_config$max_day_youth      <- mdy
    if (!is.na(mdu)     && mdu >= 0)     rv_config$max_day_under9     <- mdu
    if (!is.na(mdf)     && mdf >= 0)     rv_config$max_day_family     <- mdf
    if (!is.na(mda_amt) && mda_amt >= 0) rv_config$max_day_amount     <- mda_amt

    # Donation limit
    mdon <- suppressWarnings(as.numeric(input$max_donation_amount_edit))
    if (!is.na(mdon) && mdon >= 0) rv_config$max_donation_amount <- mdon

    # Christmas
    mcp <- suppressWarnings(as.numeric(input$max_christmas_passes_edit))
    mca <- suppressWarnings(as.numeric(input$max_christmas_amount_edit))
    if (!is.na(mcp) && mcp >= 0) rv_config$max_christmas_passes <- mcp
    if (!is.na(mca) && mca >= 0) rv_config$max_christmas_amount <- mca

    # Season
    msa     <- suppressWarnings(as.numeric(input$max_season_adult_edit))
    msy     <- suppressWarnings(as.numeric(input$max_season_youth_edit))
    msf     <- suppressWarnings(as.numeric(input$max_season_family_edit))
    msa_amt <- suppressWarnings(as.numeric(input$max_season_amount_edit))

    if (!is.na(msa)     && msa >= 0)     rv_config$max_season_adult   <- msa
    if (!is.na(msy)     && msy >= 0)     rv_config$max_season_youth   <- msy
    if (!is.na(msf)     && msf >= 0)     rv_config$max_season_family  <- msf
    if (!is.na(msa_amt) && msa_amt >= 0) rv_config$max_season_amount  <- msa_amt

    cfg <- load_config()
    cfg$max_day_adult        <- rv_config$max_day_adult
    cfg$max_day_youth        <- rv_config$max_day_youth
    cfg$max_day_under9       <- rv_config$max_day_under9
    cfg$max_day_family       <- rv_config$max_day_family
    cfg$max_day_amount       <- rv_config$max_day_amount
    cfg$max_donation_amount  <- rv_config$max_donation_amount
    cfg$max_christmas_passes <- rv_config$max_christmas_passes
    cfg$max_christmas_amount <- rv_config$max_christmas_amount
    cfg$max_season_adult     <- rv_config$max_season_adult
    cfg$max_season_youth     <- rv_config$max_season_youth
    cfg$max_season_family    <- rv_config$max_season_family
    cfg$max_season_amount    <- rv_config$max_season_amount
    save_config(cfg)

    showModal(
      modalDialog(
        title = "Limits updated",
        "New limits will apply immediately to new transactions.",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  # Save tab flags
  observeEvent(input$save_tab_flags, {
    req(admin_ok())

    flags <- list(
      day       = isTRUE(input$enable_day_tab),
      christmas = isTRUE(input$enable_christmas_tab),
      season    = isTRUE(input$enable_season_tab),
      donation  = isTRUE(input$enable_donation_tab)
    )

    tab_flags$day       <- flags$day
    tab_flags$christmas <- flags$christmas
    tab_flags$season    <- flags$season
    tab_flags$donation  <- flags$donation

    save_tab_flags(flags)

    showModal(
      modalDialog(
        title = "Tab availability updated",
        "Public payments now respect the new ON/OFF settings.",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
}

shinyApp(
  ui     = app_ui,
  server = server
)
