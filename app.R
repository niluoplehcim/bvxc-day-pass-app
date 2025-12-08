# app.R
# Bulkley Valley Cross Country Ski Club – passes + donations (Square)
# DAY, CHRISTMAS, SEASON, DONATION, with admin-set prices and limits.
# Logging + config via SQLite.
#
# CHANGES in this version:
# - Money handling centralized; all dollar displays and inputs support decimals (e.g., $10.50).
# - Reduced UI duplication with ui_when_enabled().
# - Extracted validators per product flow.
# - Collected constants in CONST list.
# - Standardized config access via get_cfg().
# - Simplified ui_bindings to uniform closures.
# - Centralized error handling with with_ui_error().
# - Upgraded section headers and early-return patterns.
# - Documented blocked-date UX strategy.
#
# Prior retained changes:
# - Added admin rate limiter (5 attempts / 5 minutes, 5-minute lock).
# - Added optional hashed admin password support (prefix "sha256:").
# - Added real-time UX hints for day-pass name, email, and total.
# - Added inline warning for blocked day-pass dates.
# - Removed transactions$status column and all use.
# - Moved tab flags into SQLite config (removed tab_flags.csv).
# - Removed donation_details table + donation detail inputs + writes.
# - Extracted small UI helper functions and removed redundant output reassignments.
# - Reduced repetitive NULL/NA-to-zero logic.
# - Automatically mapped config into reactiveValues.
# - Centralized renderUI bindings.
# - Extracted a generic enforce-max numeric input observer.
# - Consolidated “create checkout + write transaction + redirect”.

library(shiny)
library(httr)
library(DBI)
library(RSQLite)
library(uuid)

# Alias for Shiny's validate/need
validate <- shiny::validate
need     <- shiny::need

# =============================================================================
# GLOBAL SETTINGS
# =============================================================================

Sys.setenv(TZ = "America/Vancouver")
APP_VERSION <- "BVXC passes v2.7 – 2025-12-08"

# Load .Renviron if present (for local + shinyapps.io bundle)
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# Read required environment variables
SQUARE_ENV           <- Sys.getenv("SQUARE_ENV",           unset = NA_character_)
SQUARE_ACCESS_TOKEN  <- Sys.getenv("SQUARE_ACCESS_TOKEN", unset = NA_character_)
SQUARE_LOCATION_ID   <- Sys.getenv("SQUARE_LOCATION_ID",  unset = NA_character_)
ADMIN_PASSWORD       <- Sys.getenv("BVXC_ADMIN_PASSWORD", unset = NA_character_)

# Optional override for return URL base (useful on shinyapps.io / reverse proxy)
RETURN_BASE_URL <- Sys.getenv("BVXC_RETURN_BASE_URL", unset = NA_character_)

# ---- Validate Square environment ----
ALLOWED_SQUARE_ENVS <- c("sandbox", "production")

if (is.na(SQUARE_ENV) || !nzchar(SQUARE_ENV) || !(SQUARE_ENV %in% ALLOWED_SQUARE_ENVS)) {
  stop("SQUARE_ENV must be 'sandbox' or 'production'")
}

square_base_url <- if (identical(SQUARE_ENV, "production")) {
  "https://connect.squareup.com"
} else {
  "https://connect.squareupsandbox.com"
}

# Enforce that everything is set – fail fast if not
missing_vars <- c(
  if (is.na(SQUARE_ENV)           || SQUARE_ENV           == "") "SQUARE_ENV"           else NULL,
  if (is.na(SQUARE_ACCESS_TOKEN) || SQUARE_ACCESS_TOKEN  == "") "SQUARE_ACCESS_TOKEN" else NULL,
  if (is.na(SQUARE_LOCATION_ID)  || SQUARE_LOCATION_ID   == "") "SQUARE_LOCATION_ID"  else NULL,
  if (is.na(ADMIN_PASSWORD)      || ADMIN_PASSWORD       == "") "BVXC_ADMIN_PASSWORD" else NULL
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

# =============================================================================
# CONSTANTS
# =============================================================================

CONST <- list(
  youth_min_age = 9L,
  youth_max_age = 18L,
  adult_min_age = 19L,

  admin_max_attempts = 5L,
  admin_window_secs  = 5 * 60,
  admin_lock_secs    = 5 * 60,

  max_ski_date_offset_days = 183L
)

CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))
CHRISTMAS_DAY_THIS_YEAR <- as.Date(paste0(CURRENT_YEAR, "-12-25"))
CHRISTMAS_START_MIN <- CHRISTMAS_DAY_THIS_YEAR - 13
CHRISTMAS_START_MAX <- CHRISTMAS_DAY_THIS_YEAR

# =============================================================================
# SMALL UTILS
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

as_int0 <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(0L)
  suppressWarnings(as.integer(x))
}

as_num0 <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(0)
  suppressWarnings(as.numeric(x))
}

# ---- Money helpers (all human-facing dollars show 2 decimals) ----
to_cents <- function(dollars) {
  as.integer(round(as_num0(dollars) * 100))
}

to_dollars <- function(cents) {
  as_num0(cents) / 100
}

fmt_cad <- function(dollars) {
  sprintf("$%0.2f CAD", as_num0(dollars))
}

fmt_cad_cents <- function(cents) {
  fmt_cad(to_dollars(cents))
}

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

is_valid_email <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return(FALSE)
  grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", x)
}

# Helper to build the redirect URL after payment
build_return_url <- function(session) {
  if (!is.na(RETURN_BASE_URL) && nzchar(RETURN_BASE_URL)) {
    base <- sub("/+$", "", RETURN_BASE_URL)
    path <- session$clientData$url_pathname %||% "/"
    if (!startsWith(path, "/")) path <- paste0("/", path)
    return(paste0(base, path, "?success=1"))
  }

  paste0(
    session$clientData$url_protocol,
    "//",
    session$clientData$url_hostname,
    session$clientData$url_pathname,
    "?success=1"
  )
}

# =============================================================================
# ADMIN AUTH HELPERS
# =============================================================================

# ADMIN_PASSWORD may be either:
# 1) plain text, OR
# 2) "sha256:<hex>"
admin_password_matches <- function(input_pw, stored_pw) {
  if (is.na(stored_pw) || !nzchar(stored_pw)) return(FALSE)

  input_pw  <- as.character(input_pw %||% "")
  stored_pw <- as.character(stored_pw)

  if (startsWith(stored_pw, "sha256:")) {
    if (!requireNamespace("digest", quietly = TRUE)) {
      stop("Hashed admin password detected (sha256:...), but the 'digest' package is not installed.")
    }
    target <- sub("^sha256:", "", stored_pw)
    cand   <- digest::digest(input_pw, algo = "sha256")
    return(identical(tolower(cand), tolower(target)))
  }

  identical(input_pw, stored_pw)
}

# =============================================================================
# DB
# =============================================================================

get_db_connection <- function() {
  con <- dbConnect(RSQLite::SQLite(), "bvxc.sqlite")
  dbExecute(con, "PRAGMA journal_mode = WAL;")
  dbExecute(con, "PRAGMA busy_timeout = 5000;")
  con
}

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
      checkout_id      TEXT
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
}

save_transaction <- function(row_df) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  dbWriteTable(con, "transactions", row_df, append = TRUE)
}

load_transactions <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  if (!"transactions" %in% dbListTables(con)) return(data.frame())
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
  if (!"registrations" %in% dbListTables(con)) return(data.frame())
  dbReadTable(con, "registrations")
}

get_blocked_dates <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  if (!"blocked_dates" %in% dbListTables(con)) return(as.Date(character(0)))
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

# =============================================================================
# CONFIG (PRICES + LIMITS + TAB FLAGS)
# =============================================================================

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
  max_day_amount   = 300,     # dollars

  # Donation limit
  max_donation_amount = 1000, # dollars

  # Christmas limits
  max_christmas_passes = 6L,
  max_christmas_amount = 500, # dollars

  # Season limits
  max_season_adult   = 6L,
  max_season_youth   = 6L,
  max_season_family  = 4L,
  max_season_amount  = 2000,  # dollars

  # Tab flags (1 = enabled, 0 = disabled)
  tab_day_enabled       = 1L,
  tab_christmas_enabled = 1L,
  tab_season_enabled    = 1L,
  tab_donation_enabled  = 1L
)

load_config <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  if (!"config" %in% dbListTables(con)) return(DEFAULT_CONFIG)

  df <- dbReadTable(con, "config")
  cfg <- DEFAULT_CONFIG

  if (nrow(df) > 0) {
    for (i in seq_len(nrow(df))) {
      k <- df$key[i]
      v <- df$value[i]
      if (!nzchar(k)) next

      if (k %in% names(cfg)) {
        if (grepl("^price_", k) || grepl("^max_", k) || grepl("^tab_", k)) {
          num <- suppressWarnings(as.numeric(v))
          if (!is.na(num)) {
            if (grepl("^price_", k)) {
              cfg[[k]] <- as.integer(round(num))
            } else if (grepl("^tab_", k)) {
              cfg[[k]] <- as.integer(ifelse(num > 0, 1, 0))
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

load_tab_flags <- function() {
  cfg <- load_config()
  list(
    day       = isTRUE(as.integer(cfg$tab_day_enabled) == 1L),
    christmas = isTRUE(as.integer(cfg$tab_christmas_enabled) == 1L),
    season    = isTRUE(as.integer(cfg$tab_season_enabled) == 1L),
    donation  = isTRUE(as.integer(cfg$tab_donation_enabled) == 1L)
  )
}

save_tab_flags <- function(flags, cfg = NULL) {
  if (is.null(cfg)) cfg <- load_config()

  cfg$tab_day_enabled       <- as.integer(isTRUE(flags$day))
  cfg$tab_christmas_enabled <- as.integer(isTRUE(flags$christmas))
  cfg$tab_season_enabled    <- as.integer(isTRUE(flags$season))
  cfg$tab_donation_enabled  <- as.integer(isTRUE(flags$donation))

  save_config(cfg)
}

# =============================================================================
# SQUARE CHECKOUT
# =============================================================================

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

# =============================================================================
# INIT DB
# =============================================================================

init_db()

# =============================================================================
# SMALL UI HELPERS
# =============================================================================

ui_price_line_day <- function(cfg) {
  tags$p(
    sprintf(
      "Adults %s · Youth (9–18) %s · Under 9 %s · Family %s",
      fmt_cad_cents(cfg$price_adult_day),
      fmt_cad_cents(cfg$price_youth_day),
      if (cfg$price_under9_day > 0) fmt_cad_cents(cfg$price_under9_day) else "FREE",
      fmt_cad_cents(cfg$price_family_day)
    )
  )
}

ui_limits_day <- function(cfg) {
  tags$p(
    sprintf(
      "Per transaction limits: up to %d adult, %d youth, %d under-9, %d family day passes, and a maximum of %s total.",
      as.integer(cfg$max_day_adult),
      as.integer(cfg$max_day_youth),
      as.integer(cfg$max_day_under9),
      as.integer(cfg$max_day_family),
      fmt_cad(cfg$max_day_amount)
    ),
    style = "font-size:0.9em; color:#666;"
  )
}

ui_price_line_season <- function(cfg) {
  tags$p(
    sprintf(
      "Adult %s · Youth %s · Family %s",
      fmt_cad_cents(cfg$price_season_adult),
      fmt_cad_cents(cfg$price_season_youth),
      fmt_cad_cents(cfg$price_season_family)
    )
  )
}

ui_limits_season <- function(cfg) {
  tags$p(
    sprintf(
      "Per transaction limits: up to %d adult, %d youth, %d family season passes, and a maximum of %s total.",
      as.integer(cfg$max_season_adult),
      as.integer(cfg$max_season_youth),
      as.integer(cfg$max_season_family),
      fmt_cad(cfg$max_season_amount)
    ),
    style = "font-size:0.9em; color:#666;"
  )
}

ui_price_line_christmas <- function(cfg) {
  tags$p(
    sprintf(
      "Christmas 2-week pass: %s per person",
      fmt_cad_cents(cfg$price_christmas_pass)
    )
  )
}

ui_limits_christmas <- function(cfg) {
  tags$p(
    sprintf(
      "Per transaction limits: up to %d Christmas passes, and a maximum of %s total.",
      as.integer(cfg$max_christmas_passes),
      fmt_cad(cfg$max_christmas_amount)
    ),
    style = "font-size:0.9em; color:#666;"
  )
}

ui_limits_donation <- function(cfg) {
  tags$p(
    sprintf(
      "Per transaction limit: maximum online donation of %s.",
      fmt_cad(cfg$max_donation_amount)
    ),
    style = "font-size:0.9em; color:#666;"
  )
}

# Reduce duplicated enabled/disabled UI
ui_when_enabled <- function(output_flag, enabled_ui, disabled_title, disabled_msg) {
  tagList(
    conditionalPanel(
      condition = sprintf("output.%s", output_flag),
      enabled_ui
    ),
    conditionalPanel(
      condition = sprintf("!output.%s", output_flag),
      h2(disabled_title),
      p(disabled_msg, style = "color:#555; margin-top:0.75rem;")
    )
  )
}

# =============================================================================
# UI
# =============================================================================

sandbox_banner <- if (SQUARE_ENV == "sandbox") {
  div(
    style = "background:#fee; border:1px solid #f88; padding:6px; margin-bottom:12px; font-size:0.9em;",
    strong("SANDBOX – TEST MODE ONLY. NO REAL CARD CHARGES.")
  )
} else {
  NULL
}

# JS + CSS
js <- "
Shiny.addCustomMessageHandler('redirectToSquare', function(url) {
  if (url && typeof url === 'string') {
    window.location.href = url;
  }
});
"

base_head <- tags$head(
  tags$script(HTML(js)),
  tags$style(HTML("
    /* Datepicker tweaks */
    .datepicker.dropdown-menu {
      font-size: 14px;
      padding: 4px;
      margin-top: 40px;
    }
    .datepicker .datepicker-switch {
      width: 190px;
    }

    /* Make the top nav tabs more obviously clickable */
    .navbar-default {
      background-color: #f8f8f8;
      border-bottom: 2px solid #ddd;
    }

    .navbar-nav > li > a {
      font-weight: 600;
      padding-top: 14px;
      padding-bottom: 14px;
      border-bottom: 3px solid transparent;
      cursor: pointer;
    }

    .navbar-nav > li > a:hover,
    .navbar-nav > .active > a,
    .navbar-nav > .active > a:focus,
    .navbar-nav > .active > a:hover {
      border-bottom-color: #337ab7;
      background-color: #f5f5f5;
    }

    .navbar-brand {
      font-size: 16px;
      font-weight: 600;
    }
  "))
)

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
      ui_when_enabled(
        "day_tab_enabled",
        tagList(
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
            max    = Sys.Date() + CONST$max_ski_date_offset_days,
            format = "yyyy-mm-dd"
          ),
          uiOutput("ski_date_warning"),
          h4("Number of passes"),
          numericInput("n_adult",  "Adults (19+)",      value = 1, min = 0, max = 100, step = 1),
          numericInput("n_youth",  "Youth (9–18)",      value = 0, min = 0, max = 100, step = 1),
          numericInput("n_under9", "Children under 9",  value = 0, min = 0, max = 100, step = 1),
          numericInput("n_family", "Family day passes", value = 0, min = 0, max = 100, step = 1),
          tags$hr(),
          uiOutput("summary"),
          textInput("name",  "Name (for receipt / records)"),
          uiOutput("day_name_hint"),
          textInput("email", "Email (for Square receipt)"),
          uiOutput("day_email_hint"),
          uiOutput("day_total_hint"),
          tags$hr(),
          actionButton(
            "pay",
            "Pay for day passes",
            class = "btn btn-primary btn-lg btn-block"
          ),
          tags$br(),
          uiOutput("status")
        ),
        "Day passes – temporarily offline",
        "Online day-pass purchases are currently disabled. Please check back later or purchase passes on-site following club instructions."
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
      ui_when_enabled(
        "donation_tab_enabled",
        tagList(
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
          numericInput("donation_only_amount", "Donation amount ($)", value = 10, min = 0, max = 100000, step = 0.5),
          textInput("donation_only_name",  "Donor name"),
          textInput("donation_only_email", "Donor email (for Square receipt)"),
          tags$hr(),
          uiOutput("donation_only_summary"),
          tags$hr(),
          actionButton(
            "donation_only_pay",
            "Make donation",
            class = "btn btn-primary btn-lg btn-block"
          ),
          tags$br(),
          uiOutput("donation_only_status")
        ),
        "Donations – temporarily offline",
        "Online donations are currently disabled. Please check back later or contact the club for other ways to donate."
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
      ui_when_enabled(
        "christmas_tab_enabled",
        tagList(
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
            "Pay for Christmas passes",
            class = "btn btn-primary btn-lg btn-block"
          ),
          tags$br(),
          uiOutput("christmas_status")
        ),
        "Christmas passes – coming soon",
        "Online purchase of Christmas passes is currently disabled. Please check back closer to the season or contact the club."
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
      ui_when_enabled(
        "season_tab_enabled",
        tagList(
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
            "Pay for season passes",
            class = "btn btn-primary btn-lg btn-block"
          ),
          tags$br(),
          uiOutput("season_status")
        ),
        "Season passes – coming soon",
        "Online purchase of season passes is currently disabled. Please see the club website or contact the club directly."
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
      uiOutput("admin_login_ui"),
      verbatimTextOutput("admin_auth_message"),
      tags$hr(),
      uiOutput("admin_body"),
      tags$hr(),
      p(APP_VERSION, style = "font-size: 0.8em; color: #666;")
    )
  )
)

ui_core <- navbarPage(
  title = "Bulkley Valley Cross Country Ski Club",
  day_tab,
  christmas_tab,
  season_tab,
  donation_tab,
  admin_tab
)

app_ui <- tagList(
  base_head,
  ui_core
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Config + flags + blocked dates
  # ---------------------------------------------------------------------------

  config_initial <- load_config()
  config_rv <- do.call(reactiveValues, config_initial)

  get_cfg <- function() reactiveValuesToList(config_rv)

  blocked_dates <- reactiveVal(get_blocked_dates())

  flags_initial <- load_tab_flags()
  tab_flags <- reactiveValues(
    day       = isTRUE(flags_initial$day),
    christmas = isTRUE(flags_initial$christmas),
    season    = isTRUE(flags_initial$season),
    donation  = isTRUE(flags_initial$donation)
  )

  admin_ok <- reactiveVal(FALSE)

  # ---------------------------------------------------------------------------
  # Admin rate limiter
  # ---------------------------------------------------------------------------

  admin_rl <- reactiveValues(
    fail_times = as.POSIXct(character(0)),
    lock_until = as.POSIXct(NA)
  )

  prune_fail_times <- function(times, now) {
    if (length(times) == 0) return(times)
    times[as.numeric(difftime(now, times, units = "secs")) <= CONST$admin_window_secs]
  }

  admin_is_locked <- reactive({
    lu <- admin_rl$lock_until
    !is.na(lu) && Sys.time() < lu
  })

  output$admin_login_ui <- renderUI({
    locked <- isTRUE(admin_is_locked())

    tagList(
      passwordInput("admin_password", "Admin password"),
      if (locked) {
        tags$button(
          "Unlock admin",
          class = "btn btn-default",
          disabled = "disabled",
          style = "opacity: 0.6; cursor: not-allowed;"
        )
      } else {
        actionButton("admin_login", "Unlock admin")
      }
    )
  })

  observeEvent(input$admin_login, {
    now <- Sys.time()

    if (isTRUE(admin_is_locked())) {
      remaining <- ceiling(as.numeric(difftime(admin_rl$lock_until, now, units = "secs")))
      output$admin_auth_message <- renderText(
        sprintf("Too many failed attempts. Try again in ~%d seconds.", max(0, remaining))
      )
      admin_ok(FALSE)
      return()
    }

    admin_rl$fail_times <- prune_fail_times(admin_rl$fail_times, now)

    err_msg <- NULL
    ok <- tryCatch(
      admin_password_matches(input$admin_password, ADMIN_PASSWORD),
      error = function(e) {
        err_msg <<- e$message
        FALSE
      }
    )

    if (!is.null(err_msg)) {
      admin_ok(FALSE)
      output$admin_auth_message <- renderText(err_msg)
      return()
    }

    if (ok) {
      admin_ok(TRUE)
      admin_rl$fail_times <- as.POSIXct(character(0))
      admin_rl$lock_until <- as.POSIXct(NA)
      output$admin_auth_message <- renderText("Admin unlocked for this session.")
      return()
    }

    admin_rl$fail_times <- c(admin_rl$fail_times, now)
    n_fails <- length(admin_rl$fail_times)

    if (n_fails >= CONST$admin_max_attempts) {
      admin_rl$lock_until <- now + CONST$admin_lock_secs
      admin_ok(FALSE)
      output$admin_auth_message <- renderText(
        "Too many failed attempts. Admin login is temporarily locked."
      )
      return()
    }

    admin_ok(FALSE)
    output$admin_auth_message <- renderText(
      sprintf("Incorrect password. Attempt %d of %d.", n_fails, CONST$admin_max_attempts)
    )
  })

  # ---------------------------------------------------------------------------
  # Expose tab flags
  # ---------------------------------------------------------------------------

  output$day_tab_enabled       <- reactive({ tab_flags$day })
  output$christmas_tab_enabled <- reactive({ tab_flags$christmas })
  output$season_tab_enabled    <- reactive({ tab_flags$season })
  output$donation_tab_enabled  <- reactive({ tab_flags$donation })

  outputOptions(output, "day_tab_enabled",       suspendWhenHidden = FALSE)
  outputOptions(output, "christmas_tab_enabled", suspendWhenHidden = FALSE)
  outputOptions(output, "season_tab_enabled",    suspendWhenHidden = FALSE)
  outputOptions(output, "donation_tab_enabled",  suspendWhenHidden = FALSE)

  # ---------------------------------------------------------------------------
  # Season header
  # ---------------------------------------------------------------------------

  output$season_header <- renderText({
    paste("Season passes –", config_rv$season_label)
  })

  # ---------------------------------------------------------------------------
  # Centralized price/limits UI bindings
  # ---------------------------------------------------------------------------

  ui_bindings <- list(
    price_line            = function() ui_price_line_day(get_cfg()),
    day_limits_text       = function() ui_limits_day(get_cfg()),
    season_price_line     = function() ui_price_line_season(get_cfg()),
    season_limits_text    = function() ui_limits_season(get_cfg()),
    christmas_price_line  = function() ui_price_line_christmas(get_cfg()),
    christmas_limits_text = function() ui_limits_christmas(get_cfg()),
    donation_limits_text  = function() ui_limits_donation(get_cfg())
  )

  lapply(names(ui_bindings), function(id) {
    output[[id]] <- renderUI(ui_bindings[[id]]())
  })

  # ---------------------------------------------------------------------------
  # Querystring success popup (UI only)
  # ---------------------------------------------------------------------------

  observe({
    query <- shiny::getQueryString()
    if (isTRUE(query[["success"]] == "1")) {
      showModal(
        modalDialog(
          title = "Payment return received",
          "If your payment completed, you will receive a Square email receipt. Please keep it as proof of payment.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    }
  })

  # ---------------------------------------------------------------------------
  # Generic enforce-max observer
  # ---------------------------------------------------------------------------

  enforce_max <- function(input_id, max_getter, msg_fmt) {
    observeEvent(input[[input_id]], ignoreInit = TRUE, {
      val  <- as_int0(input[[input_id]])
      maxv <- as.integer(max_getter())

      if (val > maxv) {
        updateNumericInput(session, input_id, value = maxv)
        showNotification(sprintf(msg_fmt, maxv), type = "error", duration = 4)
        return()
      }

      if (val < 0) {
        updateNumericInput(session, input_id, value = 0)
      }
    })
  }

  # ---------------------------------------------------------------------------
  # Shared helpers: checkout + tx write + redirect + error wrapper
  # ---------------------------------------------------------------------------

  with_ui_error <- function(output_id, expr) {
    tryCatch(
      expr,
      error = function(e) {
        output[[output_id]] <- renderUI(
          tags$p(e$message, style = "color: #b00; white-space: pre-wrap;")
        )
      }
    )
  }

  safe_checkout <- function(total_cents, item_name) {
    return_url <- build_return_url(session)
    create_square_checkout(
      total_cents = total_cents,
      item_name   = item_name,
      return_url  = return_url
    )
  }

  write_tx <- function(info_id, product_type, total_cents,
                       name, email,
                       ski_date = Sys.Date(),
                       adults = 0L, youths = 0L, under9 = 0L, families = 0L,
                       christmas_passes = 0L,
                       donation_cents = 0L) {

    row_df <- data.frame(
      created          = as.character(Sys.time()),
      ski_date         = as.character(ski_date),
      product_type     = product_type,
      adults           = as.integer(adults),
      youths           = as.integer(youths),
      under9           = as.integer(under9),
      families         = as.integer(families),
      christmas_passes = as.integer(christmas_passes),
      total_cents      = as.integer(total_cents),
      donation_cents   = as.integer(donation_cents),
      name             = sanitize_for_storage(name),
      email            = sanitize_for_storage(email),
      checkout_id      = info_id,
      stringsAsFactors = FALSE
    )

    save_transaction(row_df)
  }

  redirect_square <- function(info_url, status_output_id, msg) {
    session$sendCustomMessage("redirectToSquare", info_url)
    output[[status_output_id]] <- renderUI({
      tags$p(msg, style = "margin-top: 1rem;")
    })
  }

  # =============================================================================
  # DAY PASSES
  # =============================================================================

  day_total <- reactive({
    adults   <- as_int0(input$n_adult)
    youths   <- as_int0(input$n_youth)
    under9   <- as_int0(input$n_under9)
    families <- as_int0(input$n_family)

    passes_cents <- adults   * config_rv$price_adult_day +
      youths   * config_rv$price_youth_day +
      under9   * config_rv$price_under9_day +
      families * config_rv$price_family_day

    list(
      adults       = adults,
      youths       = youths,
      under9       = under9,
      families     = families,
      passes_cents = passes_cents,
      total_cents  = passes_cents
    )
  })

  # Blocked-date UX strategy:
  # - Show inline warning immediately.
  # - If user selects a blocked date, show a modal and auto-advance to next valid date.
  output$ski_date_warning <- renderUI({
    d  <- as.Date(input$ski_date)
    bd <- blocked_dates()

    if (is.na(d) || length(bd) == 0) return(NULL)

    if (d %in% bd) {
      tags$p(
        "This date is blocked for online day-pass sales. Please choose another date.",
        style = "color:#b00; font-weight:600; margin-top: -6px;"
      )
    } else {
      NULL
    }
  })

  output$day_name_hint <- renderUI({
    nm <- trimws(input$name %||% "")
    if (!nzchar(nm)) tags$p("Name is required.", style = "color:#b00; margin-top:-6px;") else NULL
  })

  output$day_email_hint <- renderUI({
    em <- input$email %||% ""
    if (!nzchar(trimws(em))) {
      tags$p("Email is required.", style = "color:#b00; margin-top:-6px;")
    } else if (!is_valid_email(em)) {
      tags$p("Email format looks invalid.", style = "color:#b00; margin-top:-6px;")
    } else NULL
  })

  output$day_total_hint <- renderUI({
    t <- day_total()
    if (is.null(t$total_cents)) return(NULL)

    if (t$total_cents <= 0) {
      tags$p("Select at least one paid pass.", style = "color:#b00;")
    } else if (t$total_cents > config_rv$max_day_amount * 100) {
      tags$p(
        sprintf("Total exceeds the %s day-pass limit.", fmt_cad(config_rv$max_day_amount)),
        style = "color:#b00; font-weight:600;"
      )
    } else {
      NULL
    }
  })

  output$summary <- renderUI({
    t <- day_total()

    if (t$adults == 0 && t$youths == 0 && t$families == 0 && t$under9 == 0) {
      return(tags$p("No passes selected.", style = "font-weight: 600;"))
    }

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
        tags$p(sprintf("Passes subtotal: %s", fmt_cad_cents(t$passes_cents)))
      ))
    }

    lines <- c(lines, list(
      tags$p(
        sprintf("Total to pay: %s", fmt_cad_cents(t$total_cents)),
        style = "font-weight: 700; font-size: 1.2rem;"
      )
    ))

    do.call(tagList, lines)
  })

  observeEvent(input$ski_date, {
    ski_date <- as.Date(input$ski_date)
    if (is.na(ski_date)) return()

    today    <- Sys.Date()
    max_date <- today + CONST$max_ski_date_offset_days

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
    if (length(current_blocked) == 0 || !(ski_date %in% current_blocked)) return()

    showModal(
      modalDialog(
        title = "Online passes not available for this date",
        "Online day passes are disabled for this date (free day, closure, or off-season). Please choose another date or use cash / follow club instructions.",
        easyClose = TRUE,
        footer = NULL
      )
    )

    new_date  <- ski_date
    max_steps <- as.integer(CONST$max_ski_date_offset_days)
    steps     <- 0L

    while (!is.na(new_date) &&
           new_date <= max_date &&
           new_date %in% current_blocked &&
           steps < max_steps) {
      new_date <- new_date + 1
      steps    <- steps + 1L
    }

    if (is.na(new_date) || new_date > max_date || new_date %in% current_blocked) {
      new_date <- today
    }

    updateDateInput(session, "ski_date", value = new_date)
  })

  enforce_max("n_adult",  function() config_rv$max_day_adult,
              "Maximum %d adult day passes per transaction.")
  enforce_max("n_youth",  function() config_rv$max_day_youth,
              "Maximum %d youth day passes per transaction.")
  enforce_max("n_under9", function() config_rv$max_day_under9,
              "Maximum %d under-9 entries per transaction.")
  enforce_max("n_family", function() config_rv$max_day_family,
              "Maximum %d family day passes per transaction.")

  validate_day_purchase <- function(calc, ski_date, name, email, cfg, blocked, today, max_date) {
    validate(
      need(calc$total_cents > 0, "Please select at least one paid pass."),
      need(!is.na(ski_date), "Please select a ski date."),
      need(nchar(trimws(name)) > 0, "Please enter your name."),
      need(is_valid_email(email), "Please enter a valid email address."),
      need(calc$adults   <= cfg$max_day_adult,  sprintf("Maximum %d adult day passes per transaction.",  as.integer(cfg$max_day_adult))),
      need(calc$youths   <= cfg$max_day_youth,  sprintf("Maximum %d youth day passes per transaction.",  as.integer(cfg$max_day_youth))),
      need(calc$under9   <= cfg$max_day_under9, sprintf("Maximum %d under-9 entries per transaction.",   as.integer(cfg$max_day_under9))),
      need(calc$families <= cfg$max_day_family, sprintf("Maximum %d family day passes per transaction.", as.integer(cfg$max_day_family)))
    )

    if (calc$total_cents > cfg$max_day_amount * 100) {
      stop(sprintf("Online day-pass transactions are limited to %s per purchase.", fmt_cad(cfg$max_day_amount)))
    }

    if (ski_date < today || ski_date > max_date) {
      stop("Selected ski date is outside the allowed online purchase window.")
    }

    if (length(blocked) > 0 && ski_date %in% blocked) {
      stop("Online day passes are disabled for this date.")
    }
  }

  observeEvent(input$pay, {
    with_ui_error("status", {

      if (!tab_flags$day) {
        output$status <- renderUI(tags$p("Day-pass payments are currently disabled.", style = "color:#b00;"))
        return()
      }

      cfg <- get_cfg()
      calc <- day_total()
      d <- as.Date(input$ski_date)

      today    <- Sys.Date()
      max_date <- today + CONST$max_ski_date_offset_days

      validate_day_purchase(
        calc       = calc,
        ski_date   = d,
        name       = input$name,
        email      = input$email,
        cfg        = cfg,
        blocked    = blocked_dates(),
        today      = today,
        max_date   = max_date
      )

      info <- safe_checkout(
        total_cents = calc$total_cents,
        item_name   = "Bulkley Valley XC day passes"
      )

      write_tx(
        info_id = info$id,
        product_type = "day",
        total_cents = calc$total_cents,
        name = input$name,
        email = input$email,
        ski_date = d,
        adults = calc$adults,
        youths = calc$youths,
        under9 = calc$under9,
        families = calc$families
      )

      redirect_square(info$url, "status", "Redirecting to Square payment page...")
    })
  })

  # =============================================================================
  # DONATIONS
  # =============================================================================

  donation_only_total <- reactive({
    donation_dollars <- as_num0(input$donation_only_amount)
    if (is.na(donation_dollars) || donation_dollars < 0) donation_dollars <- 0
    list(
      donation_dollars = donation_dollars,
      donation_cents   = to_cents(donation_dollars)
    )
  })

  output$donation_only_summary <- renderUI({
    t <- donation_only_total()

    if (t$donation_cents <= 0) {
      return(tags$p("No donation amount entered.", style = "font-weight: 600;"))
    }

    tagList(
      tags$p(sprintf("Donation amount: %s", fmt_cad(t$donation_dollars))),
      tags$p(
        sprintf("Total to pay: %s", fmt_cad(t$donation_dollars)),
        style = "font-weight: 700; font-size: 1.2rem;"
      )
    )
  })

  observeEvent(input$donation_only_amount, ignoreInit = TRUE, {
    val <- as_num0(input$donation_only_amount)
    if (is.na(val) || val < 0) {
      updateNumericInput(session, "donation_only_amount", value = 0)
      return()
    }
    if (val > config_rv$max_donation_amount) {
      updateNumericInput(session, "donation_only_amount", value = config_rv$max_donation_amount)
      showNotification(
        sprintf("Maximum online donation is %s per transaction.", fmt_cad(config_rv$max_donation_amount)),
        type = "error", duration = 4
      )
    }
  })

  validate_donation_purchase <- function(calc, name, email, cfg) {
    validate(
      need(calc$donation_cents > 0, "Please enter a positive donation amount."),
      need(calc$donation_dollars <= cfg$max_donation_amount,
           sprintf("Maximum online donation is %s per transaction.", fmt_cad(cfg$max_donation_amount))),
      need(nchar(trimws(name))  > 0, "Please enter a name."),
      need(is_valid_email(email), "Please enter a valid email address.")
    )
  }

  observeEvent(input$donation_only_pay, {
    with_ui_error("donation_only_status", {

      if (!tab_flags$donation) {
        output$donation_only_status <- renderUI(tags$p("Online donations are currently disabled.", style = "color:#b00;"))
        return()
      }

      cfg <- get_cfg()
      calc <- donation_only_total()

      validate_donation_purchase(
        calc  = calc,
        name  = input$donation_only_name,
        email = input$donation_only_email,
        cfg   = cfg
      )

      info <- safe_checkout(
        total_cents = calc$donation_cents,
        item_name   = "Bulkley Valley XC donation"
      )

      write_tx(
        info_id = info$id,
        product_type = "donation",
        total_cents = calc$donation_cents,
        name = input$donation_only_name,
        email = input$donation_only_email,
        donation_cents = calc$donation_cents
      )

      redirect_square(info$url, "donation_only_status",
                      "Redirecting to Square payment page for donation...")
    })
  })

  # =============================================================================
  # CHRISTMAS PASSES
  # =============================================================================

  christmas_total <- reactive({
    christmas <- as_int0(input$n_christmas)
    total_cents <- christmas * config_rv$price_christmas_pass
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

    tagList(
      tags$p(sprintf("Christmas 2-week passes: %d", t$christmas)),
      tags$p(
        sprintf("Total for Christmas passes: %s", fmt_cad_cents(t$total_cents)),
        style = "font-weight: 700; font-size: 1.2rem;"
      )
    )
  })

  output$christmas_holder_form <- renderUI({
    n <- as_int0(input$n_christmas)
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

  enforce_max("n_christmas", function() config_rv$max_christmas_passes,
              "Maximum %d Christmas passes per transaction.")

  validate_christmas_purchase <- function(calc, holder_names, start_date, purchaser_name, purchaser_email, cfg) {
    validate(
      need(calc$christmas > 0, "Please select at least one Christmas pass."),
      need(calc$christmas <= cfg$max_christmas_passes,
           sprintf("Maximum %d Christmas passes per transaction.", as.integer(cfg$max_christmas_passes))),
      need(nchar(trimws(purchaser_name)) > 0, "Please enter a name."),
      need(is_valid_email(purchaser_email), "Please enter a valid email address."),
      need(all(nzchar(holder_names)), "Please enter a name for each Christmas pass."),
      need(!is.na(start_date), "Please select a start date for the Christmas 2-week pass.")
    )

    if (calc$total_cents > cfg$max_christmas_amount * 100) {
      stop(sprintf("Christmas pass transactions are limited to %s per purchase.", fmt_cad(cfg$max_christmas_amount)))
    }
  }

  observeEvent(input$christmas_pay, {
    with_ui_error("christmas_status", {

      if (!tab_flags$christmas) {
        output$christmas_status <- renderUI(tags$p("Christmas pass payments are currently disabled.", style = "color:#b00;"))
        return()
      }

      cfg  <- get_cfg()
      calc <- christmas_total()

      n_passes <- calc$christmas

      holder_names <- if (n_passes > 0L) {
        vapply(
          seq_len(n_passes),
          function(i) sanitize_for_storage(input[[paste0("christmas_holder_name_", i)]] %||% ""),
          FUN.VALUE = character(1)
        )
      } else character(0)

      christmas_start <- as.Date(input$christmas_start)

      validate_christmas_purchase(
        calc = calc,
        holder_names = holder_names,
        start_date = christmas_start,
        purchaser_name = input$christmas_name,
        purchaser_email = input$christmas_email,
        cfg = cfg
      )

      year          <- as.integer(format(christmas_start, "%Y"))
      christmas_day <- as.Date(paste0(year, "-12-25"))
      period_end    <- christmas_start + 13

      if (!(christmas_start <= christmas_day && christmas_day <= period_end)) {
        stop(
          paste0(
            "For a Christmas 2-week pass, the 14-day period must include Christmas Day (",
            format(christmas_day, "%Y-%m-%d"), ")."
          )
        )
      }

      info <- safe_checkout(
        total_cents = calc$total_cents,
        item_name   = "Bulkley Valley XC Christmas 2-week pass"
      )

      write_tx(
        info_id = info$id,
        product_type = "christmas",
        total_cents = calc$total_cents,
        name = input$christmas_name,
        email = input$christmas_email,
        christmas_passes = calc$christmas
      )

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

      redirect_square(info$url, "christmas_status",
                      "Redirecting to Square payment page for Christmas passes...")
    })
  })

  # =============================================================================
  # SEASON PASSES
  # =============================================================================

  season_total <- reactive({
    adults   <- as_int0(input$n_season_adult)
    youths   <- as_int0(input$n_season_youth)
    families <- as_int0(input$n_season_family)

    passes_cents <- adults   * config_rv$price_season_adult +
      youths   * config_rv$price_season_youth +
      families * config_rv$price_season_family

    list(
      adults       = adults,
      youths       = youths,
      families     = families,
      passes_cents = passes_cents
    )
  })

  output$season_holder_form <- renderUI({
    na <- as_int0(input$n_season_adult)
    ny <- as_int0(input$n_season_youth)
    nf <- as_int0(input$n_season_family)

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
    calc <- season_total()

    if (calc$adults == 0 && calc$youths == 0 && calc$families == 0) {
      return(tags$p("No season passes selected.", style = "font-weight: 600;"))
    }

    tagList(
      tags$p(
        sprintf(
          "Adult season: %d, Youth season: %d, Family season: %d",
          calc$adults, calc$youths, calc$families
        )
      ),
      tags$p(
        sprintf("Total for season passes: %s", fmt_cad_cents(calc$passes_cents)),
        style = "font-weight: 700; font-size: 1.2rem;"
      )
    )
  })

  enforce_max("n_season_adult",  function() config_rv$max_season_adult,
              "Maximum %d adult season passes per transaction.")
  enforce_max("n_season_youth",  function() config_rv$max_season_youth,
              "Maximum %d youth season passes per transaction.")
  enforce_max("n_season_family", function() config_rv$max_season_family,
              "Maximum %d family season passes per transaction.")

  validate_season_purchase <- function(calc, adult_names, youth_names, family_names,
                                       adult_dobs, youth_dobs, family_dobs,
                                       purchaser_name, purchaser_email, cfg) {

    ref_date   <- Sys.Date()
    adult_ages <- compute_age_years(adult_dobs, ref_date)
    youth_ages <- compute_age_years(youth_dobs, ref_date)

    invalid_youth <- length(youth_ages) > 0 &&
      any(youth_ages < CONST$youth_min_age | youth_ages > CONST$youth_max_age, na.rm = TRUE)

    invalid_adult <- length(adult_ages) > 0 &&
      any(adult_ages < CONST$adult_min_age, na.rm = TRUE)

    validate(
      need(calc$adults > 0 || calc$youths > 0 || calc$families > 0, "Please select at least one season pass."),
      need(nchar(trimws(purchaser_name))  > 0, "Please enter a name."),
      need(is_valid_email(purchaser_email), "Please enter a valid email address."),
      need(calc$adults <= cfg$max_season_adult,
           sprintf("Maximum %d adult season passes per transaction.", as.integer(cfg$max_season_adult))),
      need(calc$youths <= cfg$max_season_youth,
           sprintf("Maximum %d youth season passes per transaction.", as.integer(cfg$max_season_youth))),
      need(calc$families <= cfg$max_season_family,
           sprintf("Maximum %d family season passes per transaction.", as.integer(cfg$max_season_family))),
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
           sprintf("One or more youth season passes have a DOB outside the youth age range (%d–%d).",
                   CONST$youth_min_age, CONST$youth_max_age)),
      need(!invalid_adult,
           sprintf("One or more adult season passes have a DOB under %d years.",
                   CONST$adult_min_age))
    )

    if (calc$passes_cents > cfg$max_season_amount * 100) {
      stop(sprintf("Season-pass transactions are limited to %s per purchase.", fmt_cad(cfg$max_season_amount)))
    }
  }

  observeEvent(input$season_pay, {
    with_ui_error("season_status", {

      if (!tab_flags$season) {
        output$season_status <- renderUI(tags$p("Season pass payments are currently disabled.", style = "color:#b00;"))
        return()
      }

      cfg  <- get_cfg()
      calc <- season_total()

      na <- calc$adults
      ny <- calc$youths
      nf <- calc$families

      adult_names <- if (na > 0L) vapply(
        seq_len(na),
        function(i) sanitize_for_storage(input[[paste0("season_adult_name_", i)]] %||% ""),
        FUN.VALUE = character(1)
      ) else character(0)

      youth_names <- if (ny > 0L) vapply(
        seq_len(ny),
        function(i) sanitize_for_storage(input[[paste0("season_youth_name_", i)]] %||% ""),
        FUN.VALUE = character(1)
      ) else character(0)

      family_names <- if (nf > 0L) vapply(
        seq_len(nf),
        function(i) sanitize_for_storage(input[[paste0("season_family_name_", i)]] %||% ""),
        FUN.VALUE = character(1)
      ) else character(0)

      adult_dobs <- if (na > 0L) vapply(
        seq_len(na),
        function(i) as.Date(input[[paste0("season_adult_dob_", i)]]),
        FUN.VALUE = as.Date(NA)
      ) else as.Date(character(0))

      youth_dobs <- if (ny > 0L) vapply(
        seq_len(ny),
        function(i) as.Date(input[[paste0("season_youth_dob_", i)]]),
        FUN.VALUE = as.Date(NA)
      ) else as.Date(character(0))

      family_dobs <- if (nf > 0L) vapply(
        seq_len(nf),
        function(i) as.Date(input[[paste0("season_family_dob_", i)]]),
        FUN.VALUE = as.Date(NA)
      ) else as.Date(character(0))

      validate_season_purchase(
        calc = calc,
        adult_names = adult_names,
        youth_names = youth_names,
        family_names = family_names,
        adult_dobs = adult_dobs,
        youth_dobs = youth_dobs,
        family_dobs = family_dobs,
        purchaser_name = input$season_name,
        purchaser_email = input$season_email,
        cfg = cfg
      )

      info <- safe_checkout(
        total_cents = calc$passes_cents,
        item_name   = paste("Bulkley Valley XC season passes –", config_rv$season_label)
      )

      write_tx(
        info_id = info$id,
        product_type = "season",
        total_cents = calc$passes_cents,
        name = input$season_name,
        email = input$season_email,
        adults = na,
        youths = ny,
        families = nf
      )

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

      redirect_square(info$url, "season_status",
                      "Redirecting to Square payment page for season passes...")
    })
  })

  # =============================================================================
  # ADMIN: REPORTS + SETTINGS
  # =============================================================================

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

    if (!is.null(input$admin_product_filter) && input$admin_product_filter != "Any") {
      df <- df[df$product_type == input$admin_product_filter, , drop = FALSE]
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

    df$total_dollars    <- to_dollars(df$total_cents)
    df$donation_dollars <- to_dollars(df$donation_cents)

    df$created  <- as.character(df$created)
    df$ski_date <- as.character(df$ski_date)

    df[, c("id", "created", "ski_date",
           "product_type",
           "adults", "youths", "under9", "families", "christmas_passes",
           "total_dollars", "donation_dollars",
           "name", "email", "checkout_id")]
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

  # ---------------------------------------------------------------------------
  # Blocked dates display + controls
  # ---------------------------------------------------------------------------

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

  # ---------------------------------------------------------------------------
  # Admin body UI
  # ---------------------------------------------------------------------------

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
            6,
            dateRangeInput(
              "admin_daterange",
              "Date range",
              start = Sys.Date() - 30,
              end   = Sys.Date()
            )
          ),
          column(
            6,
            selectInput(
              "admin_product_filter",
              "Product type",
              choices  = c("Any", "day", "season", "christmas", "donation"),
              selected = "Any"
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
        downloadButton("download_all", "Download full CSV")
      ),
      tabPanel(
        "Prices / limits / tabs",
        br(),
        h4("Day-pass prices (CAD)"),
        p("Adjust prices here and click \"Save day-pass prices\". Changes apply to new online day-pass purchases."),
        fluidRow(
          column(3, numericInput("price_adult",   "Adult ($)",   value = to_dollars(config_rv$price_adult_day),  min = 0, step = 0.5)),
          column(3, numericInput("price_youth",   "Youth ($)",   value = to_dollars(config_rv$price_youth_day),  min = 0, step = 0.5)),
          column(3, numericInput("price_family",  "Family ($)",  value = to_dollars(config_rv$price_family_day), min = 0, step = 0.5)),
          column(3, numericInput("price_under9",  "Under-9 ($)", value = to_dollars(config_rv$price_under9_day), min = 0, step = 0.5))
        ),
        actionButton("save_prices", "Save day-pass prices"),

        tags$hr(),
        h4("Season / Christmas prices (CAD)"),
        fluidRow(
          column(3, numericInput("season_price_adult",     "Adult season ($)",     value = to_dollars(config_rv$price_season_adult),   min = 0, step = 1)),
          column(3, numericInput("season_price_youth",     "Youth season ($)",     value = to_dollars(config_rv$price_season_youth),   min = 0, step = 1)),
          column(3, numericInput("season_price_family",    "Family season ($)",    value = to_dollars(config_rv$price_season_family),  min = 0, step = 1)),
          column(3, numericInput("season_price_christmas", "Christmas 2-week ($)", value = to_dollars(config_rv$price_christmas_pass), min = 0, step = 1))
        ),
        textInput("season_label_edit", "Season label", value = config_rv$season_label),
        actionButton("save_season_prices", "Save season/Christmas prices"),

        tags$hr(),
        h4("Transaction limits"),
        p("Adjust maximum number of passes per transaction and maximum dollar amounts."),
        h5("Day passes"),
        fluidRow(
          column(3, numericInput("max_day_adult_edit",   "Max adult day passes",   value = config_rv$max_day_adult,   min = 0, step = 1)),
          column(3, numericInput("max_day_youth_edit",   "Max youth day passes",   value = config_rv$max_day_youth,   min = 0, step = 1)),
          column(3, numericInput("max_day_under9_edit",  "Max under-9 entries",    value = config_rv$max_day_under9,  min = 0, step = 1)),
          column(3, numericInput("max_day_family_edit",  "Max family day passes",  value = config_rv$max_day_family,  min = 0, step = 1))
        ),
        numericInput("max_day_amount_edit", "Max day-pass transaction amount ($)", value = config_rv$max_day_amount, min = 0, step = 1),

        h5("Donations"),
        numericInput("max_donation_amount_edit", "Max donation per transaction ($)", value = config_rv$max_donation_amount, min = 0, step = 1),

        h5("Christmas passes"),
        fluidRow(
          column(6, numericInput("max_christmas_passes_edit", "Max Christmas passes per transaction", value = config_rv$max_christmas_passes, min = 0, step = 1)),
          column(6, numericInput("max_christmas_amount_edit", "Max Christmas transaction amount ($)", value = config_rv$max_christmas_amount,   min = 0, step = 1))
        ),

        h5("Season passes"),
        fluidRow(
          column(3, numericInput("max_season_adult_edit",   "Max adult season passes",   value = config_rv$max_season_adult,   min = 0, step = 1)),
          column(3, numericInput("max_season_youth_edit",   "Max youth season passes",   value = config_rv$max_season_youth,   min = 0, step = 1)),
          column(3, numericInput("max_season_family_edit",  "Max family season passes",  value = config_rv$max_season_family,  min = 0, step = 1)),
          column(3, numericInput("max_season_amount_edit",  "Max season transaction ($)", value = config_rv$max_season_amount, min = 0, step = 1))
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

  # ---------------------------------------------------------------------------
  # Save day prices
  # ---------------------------------------------------------------------------

  observeEvent(input$save_prices, {
    req(admin_ok())

    pa <- as_num0(input$price_adult)
    py <- as_num0(input$price_youth)
    pf <- as_num0(input$price_family)
    pu <- as_num0(input$price_under9)

    if (!is.na(pa) && pa >= 0) config_rv$price_adult_day  <- to_cents(pa)
    if (!is.na(py) && py >= 0) config_rv$price_youth_day  <- to_cents(py)
    if (!is.na(pf) && pf >= 0) config_rv$price_family_day <- to_cents(pf)
    if (!is.na(pu) && pu >= 0) config_rv$price_under9_day <- to_cents(pu)

    cfg <- load_config()
    cfg$price_adult_day   <- config_rv$price_adult_day
    cfg$price_youth_day   <- config_rv$price_youth_day
    cfg$price_under9_day  <- config_rv$price_under9_day
    cfg$price_family_day  <- config_rv$price_family_day
    save_config(cfg)

    showModal(
      modalDialog(
        title = "Day-pass prices updated",
        "New day-pass prices will apply to future online purchases.",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Save season / Christmas prices + label
  # ---------------------------------------------------------------------------

  observeEvent(input$save_season_prices, {
    req(admin_ok())

    sa <- as_num0(input$season_price_adult)
    sy <- as_num0(input$season_price_youth)
    sf <- as_num0(input$season_price_family)
    sc <- as_num0(input$season_price_christmas)
    label <- input$season_label_edit %||% config_rv$season_label

    if (!is.na(sa) && sa >= 0) config_rv$price_season_adult   <- to_cents(sa)
    if (!is.na(sy) && sy >= 0) config_rv$price_season_youth   <- to_cents(sy)
    if (!is.na(sf) && sf >= 0) config_rv$price_season_family  <- to_cents(sf)
    if (!is.na(sc) && sc >= 0) config_rv$price_christmas_pass <- to_cents(sc)
    config_rv$season_label <- label

    cfg <- load_config()
    cfg$price_season_adult   <- config_rv$price_season_adult
    cfg$price_season_youth   <- config_rv$price_season_youth
    cfg$price_season_family  <- config_rv$price_season_family
    cfg$price_christmas_pass <- config_rv$price_christmas_pass
    cfg$season_label         <- config_rv$season_label
    save_config(cfg)

    showModal(
      modalDialog(
        title = "Season / Christmas prices updated",
        "New season and Christmas prices will apply to future purchases.",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Save limits
  # ---------------------------------------------------------------------------

  observeEvent(input$save_limits, {
    req(admin_ok())

    mda     <- as_num0(input$max_day_adult_edit)
    mdy     <- as_num0(input$max_day_youth_edit)
    mdu     <- as_num0(input$max_day_under9_edit)
    mdf     <- as_num0(input$max_day_family_edit)
    mda_amt <- as_num0(input$max_day_amount_edit)

    if (!is.na(mda)      && mda >= 0)      config_rv$max_day_adult   <- mda
    if (!is.na(mdy)      && mdy >= 0)      config_rv$max_day_youth   <- mdy
    if (!is.na(mdu)      && mdu >= 0)      config_rv$max_day_under9  <- mdu
    if (!is.na(mdf)      && mdf >= 0)      config_rv$max_day_family  <- mdf
    if (!is.na(mda_amt)  && mda_amt >= 0) config_rv$max_day_amount  <- mda_amt

    mdon <- as_num0(input$max_donation_amount_edit)
    if (!is.na(mdon) && mdon >= 0) config_rv$max_donation_amount <- mdon

    mcp <- as_num0(input$max_christmas_passes_edit)
    mca <- as_num0(input$max_christmas_amount_edit)
    if (!is.na(mcp) && mcp >= 0) config_rv$max_christmas_passes <- mcp
    if (!is.na(mca) && mca >= 0) config_rv$max_christmas_amount <- mca

    msa     <- as_num0(input$max_season_adult_edit)
    msy     <- as_num0(input$max_season_youth_edit)
    msf     <- as_num0(input$max_season_family_edit)
    msa_amt <- as_num0(input$max_season_amount_edit)

    if (!is.na(msa)     && msa >= 0)      config_rv$max_season_adult  <- msa
    if (!is.na(msy)     && msy >= 0)      config_rv$max_season_youth  <- msy
    if (!is.na(msf)     && msf >= 0)      config_rv$max_season_family <- msf
    if (!is.na(msa_amt) && msa_amt >= 0) config_rv$max_season_amount <- msa_amt

    cfg <- load_config()
    cfg$max_day_adult        <- config_rv$max_day_adult
    cfg$max_day_youth        <- config_rv$max_day_youth
    cfg$max_day_under9       <- config_rv$max_day_under9
    cfg$max_day_family       <- config_rv$max_day_family
    cfg$max_day_amount       <- config_rv$max_day_amount
    cfg$max_donation_amount  <- config_rv$max_donation_amount
    cfg$max_christmas_passes <- config_rv$max_christmas_passes
    cfg$max_christmas_amount <- config_rv$max_christmas_amount
    cfg$max_season_adult     <- config_rv$max_season_adult
    cfg$max_season_youth     <- config_rv$max_season_youth
    cfg$max_season_family    <- config_rv$max_season_family
    cfg$max_season_amount    <- config_rv$max_season_amount
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

  # ---------------------------------------------------------------------------
  # Save tab flags
  # ---------------------------------------------------------------------------

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

    cfg <- load_config()
    save_tab_flags(flags, cfg = cfg)

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
