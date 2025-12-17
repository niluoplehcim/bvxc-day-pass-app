# app.R
# BVXC – Passes, Programs, Events + Cart + Admin Controls (Square)
# v5.6 – Connect Cloud + Supabase-ready (SQLite fallback) – 2025-12-17

library(shiny)
library(httr)
library(DBI)
library(RSQLite)
library(uuid)
library(jsonlite)
library(qrcode)
library(pool)
library(RPostgres)

# ---- renv -------------------------------------------------------------------
if (file.exists("renv/activate.R")) {
  try(source("renv/activate.R"), silent = TRUE)
}

# -----------------------------------------------------------------------------
# GLOBAL SETTINGS / ENV
# -----------------------------------------------------------------------------

Sys.setenv(TZ = "America/Vancouver")
APP_VERSION <- "BVXC v5.6 – Connect Cloud + Supabase-ready – 2025-12-17"

if (file.exists(".Renviron")) readRenviron(".Renviron")

ALLOWED_SQUARE_ENVS   <- c("sandbox", "production")
ALLOWED_SANDBOX_MODES <- c("fake", "square")

SQUARE_ENV          <- tolower(Sys.getenv("SQUARE_ENV", unset = "sandbox"))
SANDBOX_MODE        <- tolower(Sys.getenv("BVXC_SANDBOX_MODE", unset = "fake"))
SQUARE_ACCESS_TOKEN <- Sys.getenv("SQUARE_ACCESS_TOKEN", unset = "")
SQUARE_LOCATION_ID  <- Sys.getenv("SQUARE_LOCATION_ID",  unset = "")

ADMIN_PASSWORD  <- Sys.getenv("BVXC_ADMIN_PASSWORD",  unset = NA_character_)
RETURN_BASE_URL <- Sys.getenv("BVXC_RETURN_BASE_URL", unset = NA_character_)

SQUARE_VERSION <- Sys.getenv("SQUARE_VERSION", unset = "2025-10-16")

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
trim_trailing_slash <- function(x) sub("/+$", "", x)

if (!SQUARE_ENV %in% ALLOWED_SQUARE_ENVS) SQUARE_ENV <- "sandbox"
if (!SANDBOX_MODE %in% ALLOWED_SANDBOX_MODES) SANDBOX_MODE <- "fake"

HAVE_SQUARE_CREDS <- nzchar(SQUARE_ACCESS_TOKEN) && nzchar(SQUARE_LOCATION_ID)

if ((SQUARE_ENV == "production" || (SQUARE_ENV == "sandbox" && SANDBOX_MODE == "square")) && !HAVE_SQUARE_CREDS) {
  SQUARE_ENV   <- "sandbox"
  SANDBOX_MODE <- "fake"
}

if (!is.na(RETURN_BASE_URL) && nzchar(RETURN_BASE_URL)) {
  RETURN_BASE_URL <- trim_trailing_slash(RETURN_BASE_URL)
}

ENV_LABEL <- if (SQUARE_ENV == "sandbox") {
  if (SANDBOX_MODE == "fake") "[SANDBOX – FAKE PAYMENTS]" else "[SANDBOX – SQUARE CHECKOUT]"
} else {
  "[LIVE – PRODUCTION]"
}

# -----------------------------------------------------------------------------
# SEASON WINDOW HELPERS (Nov 1 → May 1)
# -----------------------------------------------------------------------------

get_season_window <- function(today = Sys.Date()) {
  y <- as.integer(format(today, "%Y"))
  m <- as.integer(format(today, "%m"))

  if (m >= 11) {
    start <- as.Date(sprintf("%d-11-01", y))
    end   <- as.Date(sprintf("%d-05-01", y + 1))
  } else if (m <= 5) {
    start <- as.Date(sprintf("%d-11-01", y - 1))
    end   <- as.Date(sprintf("%d-05-01", y))
  } else {
    start <- as.Date(sprintf("%d-11-01", y))
    end   <- as.Date(sprintf("%d-05-01", y + 1))
  }

  list(start = start, end = end)
}

season_label <- function(w) paste0("Season: ", format(w$start), " to ", format(w$end))

# -----------------------------------------------------------------------------
# DB POOL
# -----------------------------------------------------------------------------

is_true <- function(x) tolower(trimws(x %||% "")) %in% c("1","true","yes","y","on")

is_connect_runtime <- function() {
  nzchar(Sys.getenv("RSCONNECT_SERVER", "")) ||
    nzchar(Sys.getenv("CONNECT_SERVER", "")) ||
    nzchar(Sys.getenv("POSIT_CONNECT", "")) ||
    nzchar(Sys.getenv("CONNECT_CONTENT_GUID", ""))
}

DB_URL  <- Sys.getenv("BVXC_DB_URL",  unset = "")
DB_PATH <- Sys.getenv("BVXC_DB_PATH", unset = "bvxc.sqlite")

ALLOW_SQLITE_FALLBACK <- is_true(Sys.getenv("BVXC_ALLOW_SQLITE_FALLBACK", "0"))
db_is_postgres <- function() nzchar(trimws(DB_URL))

if (is_connect_runtime() && !db_is_postgres() && !ALLOW_SQLITE_FALLBACK) {
  stop(
    "BVXC_DB_URL is not set. Refusing to run with SQLite on Connect Cloud.\n",
    "Set BVXC_DB_URL to your Supabase/Postgres connection string.\n",
    "If you *really* want SQLite fallback, set BVXC_ALLOW_SQLITE_FALLBACK=1."
  )
}

parse_kv_conn <- function(s) {
  s <- trimws(s)
  re <- gregexpr("(\\w+)=(?:'([^']*)'|\"([^\"]*)\"|(\\S+))", s, perl = TRUE)
  m  <- regmatches(s, re)[[1]]
  out <- list()
  for (tok in m) {
    g <- regmatches(tok, regexec("(\\w+)=(?:'([^']*)'|\"([^\"]*)\"|(\\S+))", tok, perl = TRUE))[[1]]
    k <- g[2]
    v <- if (nzchar(g[3])) g[3] else if (nzchar(g[4])) g[4] else g[5]
    out[[k]] <- v
  }
  if (!is.null(out$db) && is.null(out$dbname)) out$dbname <- out$db
  out
}

make_pool <- function() {
  if (db_is_postgres()) {
    s <- trimws(DB_URL)

    if (grepl("^postgres(ql)?://", s, ignore.case = TRUE)) {
      return(pool::dbPool(RPostgres::Postgres(), dbname = s))
    }

    args <- parse_kv_conn(s)
    need <- c("host", "port", "dbname", "user", "password")
    missing <- setdiff(need, names(args))
    if (length(missing) > 0) {
      stop(
        "BVXC_DB_URL is missing: ", paste(missing, collapse = ", "),
        "\nUse URI form (recommended): postgresql://USER:PASSWORD@HOST:5432/DBNAME?sslmode=require"
      )
    }

    return(do.call(pool::dbPool, c(list(RPostgres::Postgres()), args)))
  }

  pool::dbPool(RSQLite::SQLite(), dbname = DB_PATH)
}

DB_POOL <- make_pool()

# Proper cleanup for pooled resources
onStop(function() {
  try(pool::poolClose(DB_POOL), silent = TRUE)
})

with_db <- function(fun) {
  con <- pool::poolCheckout(DB_POOL)
  on.exit(pool::poolReturn(con), add = TRUE)
  fun(con)
}

now_ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

db_get  <- function(con, sql, ...) DBI::dbGetQuery(con, DBI::sqlInterpolate(con, sql, ...))
db_exec <- function(con, sql, ...) DBI::dbExecute(con, DBI::sqlInterpolate(con, sql, ...))

db_get1  <- function(sql, ...) with_db(function(con) db_get(con, sql, ...))
db_exec1 <- function(sql, ...) with_db(function(con) db_exec(con, sql, ...))

# -----------------------------------------------------------------------------
# DB INIT
# -----------------------------------------------------------------------------

init_db <- function() {
  with_db(function(con) {

    db_exec(con, "
      CREATE TABLE IF NOT EXISTS config (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL
      )
    ")

    db_exec(con, "
      CREATE TABLE IF NOT EXISTS transactions (
        id TEXT PRIMARY KEY,
        created_at TEXT NOT NULL,
        buyer_name TEXT,
        buyer_email TEXT,
        total_amount_cents INTEGER NOT NULL,
        currency TEXT NOT NULL,
        cart_json TEXT NOT NULL,
        square_checkout_id TEXT,
        square_order_id TEXT,
        receipt_token TEXT,
        status TEXT
      )
    ")

    if (db_is_postgres()) {
      db_exec(con, '
        CREATE TABLE IF NOT EXISTS blocked_dates (
          id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
          "date" TEXT NOT NULL UNIQUE,
          reason TEXT
        )
      ')
    } else {
      db_exec(con, '
        CREATE TABLE IF NOT EXISTS blocked_dates (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          "date" TEXT NOT NULL UNIQUE,
          reason TEXT
        )
      ')
    }

    if (db_is_postgres()) {
      db_exec(con, "
        CREATE TABLE IF NOT EXISTS special_events (
          id TEXT PRIMARY KEY,
          name TEXT NOT NULL,
          event_date TEXT NOT NULL,
          price_cad NUMERIC(10,2),
          capacity INTEGER,
          enabled INTEGER NOT NULL DEFAULT 1,
          created_at TEXT NOT NULL
        )
      ")
    } else {
      db_exec(con, "
        CREATE TABLE IF NOT EXISTS special_events (
          id TEXT PRIMARY KEY,
          name TEXT NOT NULL,
          event_date TEXT NOT NULL,
          price_cad REAL,
          capacity INTEGER,
          enabled INTEGER NOT NULL DEFAULT 1,
          created_at TEXT NOT NULL
        )
      ")
    }

  })
}

init_db()

# -----------------------------------------------------------------------------
# CONFIG HELPERS
# -----------------------------------------------------------------------------

cfg_get <- function(key, default = "") {
  x <- db_get1("SELECT value FROM config WHERE key = ?key LIMIT 1", key = key)
  if (nrow(x) == 0) return(default)
  x$value[1]
}

cfg_set <- function(key, value) {
  db_exec1(
    "INSERT INTO config(key,value) VALUES(?key, ?value)
     ON CONFLICT(key) DO UPDATE SET value = excluded.value",
    key   = key,
    value = as.character(value %||% "")
  )
}

cfg_bool <- function(key, default = FALSE) {
  v <- tolower(trimws(cfg_get(key, "")))
  if (!nzchar(v)) return(default)
  v %in% c("1", "true", "yes", "on")
}

cfg_num <- function(key, default = NA_real_) {
  s <- trimws(cfg_get(key, ""))
  if (!nzchar(s) || toupper(s) %in% c("N/A", "NA")) return(default)
  s <- gsub("\\$", "", s)
  s <- gsub(",", "", s)
  v <- suppressWarnings(as.numeric(s))
  if (is.na(v) || v < 0) return(default)
  v
}

cfg_int <- function(key, default = NA_integer_) {
  s <- trimws(cfg_get(key, ""))
  if (!nzchar(s) || toupper(s) %in% c("N/A", "NA")) return(default)
  v <- suppressWarnings(as.integer(s))
  if (is.na(v) || v < 0) return(default)
  v
}

cfg_date <- function(key, default = as.Date(NA)) {
  s <- trimws(cfg_get(key, ""))
  if (!nzchar(s)) return(default)
  d <- suppressWarnings(as.Date(s))
  if (is.na(d)) default else d
}

fmt_price <- function(x) if (is.na(x)) "N/A" else paste0("$", sprintf("%.2f", x))

# -----------------------------------------------------------------------------
# BUSINESS DATA (PRICES / LISTS)
# -----------------------------------------------------------------------------

get_early_bird_cutoff <- function() cfg_date("early_bird_cutoff", as.Date(NA))

cfg_num_under9 <- function() cfg_num("price_day_under9", NA_real_)

get_day_prices <- function() {
  data.frame(
    type  = c("Adult", "Youth", "Under 9", "Family"),
    price = c(
      cfg_num("price_day_adult", NA_real_),
      cfg_num("price_day_youth", NA_real_),
      cfg_num_under9(),
      cfg_num("price_day_family", NA_real_)
    ),
    stringsAsFactors = FALSE
  )
}

get_season_prices <- function(is_early_bird = FALSE) {
  if (is_early_bird) {
    data.frame(
      type  = c("Adult", "Youth"),
      price = c(
        cfg_num("price_season_eb_adult", NA_real_),
        cfg_num("price_season_eb_youth", NA_real_)
      ),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      type  = c("Adult", "Youth"),
      price = c(
        cfg_num("price_season_reg_adult", NA_real_),
        cfg_num("price_season_reg_youth", NA_real_)
      ),
      stringsAsFactors = FALSE
    )
  }
}

get_christmas_pass_price <- function() cfg_num("price_christmas_pass", NA_real_)

get_program_list <- function() {
  data.frame(
    id    = c("kids_ski", "masters", "biathlon_intro"),
    name  = c("Kids Ski Program", "Masters Training", "Biathlon Intro"),
    price = c(
      cfg_num("price_program_kids_ski", NA_real_),
      cfg_num("price_program_masters", NA_real_),
      cfg_num("price_program_biathlon_intro", NA_real_)
    ),
    stringsAsFactors = FALSE
  )
}

get_special_events <- function(enabled_only = TRUE) {
  q <- "SELECT id, name, event_date, price_cad, capacity, enabled, created_at FROM special_events"
  if (enabled_only) q <- paste(q, "WHERE enabled = 1")
  q <- paste(q, "ORDER BY event_date ASC")
  db_get1(q)
}

get_blocked_dates <- function() {
  db_get1('SELECT "date" AS date, reason FROM blocked_dates ORDER BY "date" ASC')
}

# -----------------------------------------------------------------------------
# LIMITS / VALIDATION
# -----------------------------------------------------------------------------

validate_cart_limits <- function(cart_df) {
  max_total <- cfg_num("limit_max_total_cad", NA_real_)
  max_items <- cfg_int("limit_max_items_total", NA_integer_)

  total <- sum(cart_df$quantity * cart_df$unit_price)
  items <- sum(cart_df$quantity)

  if (!is.na(max_total) && total > max_total) {
    return(paste0("Transaction exceeds max total: $", sprintf("%.2f", max_total)))
  }
  if (!is.na(max_items) && items > max_items) {
    return(paste0("Transaction exceeds max items: ", max_items))
  }
  NULL
}

# -----------------------------------------------------------------------------
# SQUARE CHECKOUT (CART)
# -----------------------------------------------------------------------------

square_base_url <- function() {
  if (identical(SQUARE_ENV, "sandbox")) "https://connect.squareupsandbox.com" else "https://connect.squareup.com"
}

square_headers <- function(include_version = TRUE) {
  h <- httr::add_headers(
    "Authorization" = paste("Bearer", SQUARE_ACCESS_TOKEN),
    "Content-Type"  = "application/json"
  )
  if (include_version) {
    h <- c(h, httr::add_headers("Square-Version" = SQUARE_VERSION))
  }
  h
}

create_square_checkout_from_cart <- function(cart_df,
                                            buyer_name   = NULL,
                                            buyer_email  = NULL,
                                            note         = NULL,
                                            redirect_url = NULL) {
  if (nrow(cart_df) == 0) return(NULL)

  line_items <- lapply(seq_len(nrow(cart_df)), function(i) {
    row <- cart_df[i, ]
    list(
      name     = row$description,
      quantity = as.character(row$quantity),
      base_price_money = list(
        amount   = as.integer(round(row$unit_price * 100)),
        currency = "CAD"
      )
    )
  })

  body <- list(
    idempotency_key = UUIDgenerate(),
    order = list(
      location_id = SQUARE_LOCATION_ID,
      line_items  = line_items
    ),
    payment_note = note %||% "",
    checkout_options = list(
      ask_for_shipping_address = FALSE,
      merchant_support_email   = "info@bvnordic.ca",
      redirect_url             = redirect_url
    ),
    pre_populated_data = list(
      buyer_email = buyer_email %||% ""
    )
  )

  res <- httr::POST(
    url    = paste0(square_base_url(), "/v2/online-checkout/payment-links"),
    square_headers(include_version = TRUE),
    body   = body,
    encode = "json"
  )

  if (httr::status_code(res) >= 300) {
    warning("Square payment link error: ", httr::content(res, as = "text", encoding = "UTF-8"))
    return(NULL)
  }

  content <- httr::content(res, as = "parsed", type = "application/json")
  pl <- content$payment_link

  list(
    checkout_url = pl$url %||% NULL,
    checkout_id  = pl$id %||% NA_character_,
    square_order = pl$order_id %||% NA_character_
  )
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

css_tabs <- "
.navbar-nav > li > a { font-weight: 600; }
.navbar-nav > li.active > a,
.navbar-nav > li > a:hover {
  border-bottom: 3px solid #0d6efd !important;
  padding-bottom: 10px;
}
.cart-hot { color: #0d6efd !important; font-weight: 800 !important; font-size: 1.07em !important; }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(css_tabs))),
  uiOutput("main_nav_ui"),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('redirect', function(message) {
      try { window.top.location.href = message.url; }
      catch(e) { window.location.href = message.url; }
    });
  "))
)

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    cart = data.frame(
      id          = character(),
      category    = character(),
      description = character(),
      quantity    = integer(),
      unit_price  = numeric(),
      meta_json   = character(),
      stringsAsFactors = FALSE
    ),
    admin_logged_in  = FALSE,
    admin_fail_count = 0L,
    admin_lock_until = as.POSIXct(NA)
  )

  receipt_tx <- reactiveVal(NULL)

  day_date_ui_nonce     <- reactiveVal(0L)
  last_valid_day_date   <- reactiveVal(Sys.Date())
  blocked_nonce         <- reactiveVal(0L)
  events_nonce          <- reactiveVal(0L)

  clear_cart <- function() rv$cart <- rv$cart[0, , drop = FALSE]
  cart_total_cents <- function(df) as.integer(round(sum(df$quantity * df$unit_price) * 100))

  qty_int <- function(x, label = "Quantity") {
    if (is.null(x) || length(x) == 0) return(0L)
    v <- suppressWarnings(as.numeric(x))
    if (is.na(v) || v <= 0) return(0L)
    if (abs(v - round(v)) > 1e-9) {
      showNotification(
        paste0(label, " must be a whole number. Rounding to ", as.integer(round(v)), "."),
        type = "warning"
      )
    }
    as.integer(round(v))
  }

  build_redirect_url <- function(receipt_token) {
    if (!is.na(RETURN_BASE_URL) && nzchar(RETURN_BASE_URL)) {
      paste0(RETURN_BASE_URL, "/?receipt=", receipt_token)
    } else {
      NULL
    }
  }

  bump_day_date_ui <- function() day_date_ui_nonce(isolate(day_date_ui_nonce()) + 1L)

  add_to_cart <- function(category, description, quantity, unit_price, meta = list()) {
    q <- as.integer(quantity %||% 0)
    p <- as.numeric(unit_price %||% NA_real_)

    if (q <= 0) return(invisible(NULL))

    if (is.na(p)) {
      showNotification("Price is N/A. Admin must set prices first.", type = "error")
      return(invisible(NULL))
    }
    if (p < 0) return(invisible(NULL))

    rv$cart <- rbind(
      rv$cart,
      data.frame(
        id          = UUIDgenerate(),
        category    = category,
        description = description,
        quantity    = q,
        unit_price  = p,
        meta_json   = jsonlite::toJSON(meta, auto_unbox = TRUE),
        stringsAsFactors = FALSE
      )
    )
    showNotification("Added to cart.", type = "message")
  }

  # -----------------------------------------------------------------------------
  # REACTIVE DATA SOURCES (with refresh triggers)
  # -----------------------------------------------------------------------------

  season_win <- reactive(get_season_window(Sys.Date()))

  blocked_df <- reactive({
    blocked_nonce()
    get_blocked_dates()
  })

  blocked_chr <- reactive({
    bd <- blocked_df()
    if (nrow(bd) == 0) return(character())
    x <- as.character(bd$date)
    x <- x[!is.na(x) & nzchar(x)]
    x
  })

  # -----------------------------------------------------------------------------
  # CHRISTMAS PASS: constrain dateInput to season range (only when tab visible)
  # -----------------------------------------------------------------------------

  observe({
    if (!identical(input$main_nav, "Christmas Pass")) return()

    w <- season_win()
    season_dec_year <- as.integer(format(w$start, "%Y"))

    min_start <- as.Date(sprintf("%d-12-12", season_dec_year))
    max_start <- as.Date(sprintf("%d-12-25", season_dec_year))

    raw <- input$xmas_start
    cur <- if (is.null(raw) || length(raw) == 0 || !nzchar(as.character(raw))) NA else suppressWarnings(as.Date(raw))
    if (is.na(cur) || cur < min_start || cur > max_start) cur <- min_start

    updateDateInput(session, "xmas_start", min = min_start, max = max_start, value = cur)
  })

  # -----------------------------------------------------------------------------
  # BLOCKED DATES: revert if user picks blocked day
  # -----------------------------------------------------------------------------

  observeEvent(input$day_date, {
    picked <- suppressWarnings(as.Date(input$day_date))
    if (is.null(picked) || is.na(picked)) return()

    blk <- blocked_chr()
    if (length(blk) > 0 && (as.character(picked) %in% blk)) {
      showNotification("This date has been blocked from paying. Please choose another date.", type = "error")
      bump_day_date_ui()
      return()
    }
    last_valid_day_date(picked)
  }, ignoreInit = TRUE)

  # -----------------------------------------------------------------------------
  # DYNAMIC NAVBAR
  # -----------------------------------------------------------------------------

  output$main_nav_ui <- renderUI({
    tab_on <- function(key, default = TRUE) cfg_bool(key, default)

    cart_items <- if (nrow(rv$cart) == 0) 0 else sum(rv$cart$quantity %||% 0)
    has_cart   <- cart_items > 0
    cart_label <- if (has_cart) paste0("Cart (", cart_items, ")") else "Cart"

    env_diag <- tagList(
      if (db_is_postgres()) {
        tags$div(style="margin:8px 0; padding:10px; border:1px solid #ddd; border-radius:6px; background:#fafafa;",
                 tags$strong("Database: "), "Postgres (BVXC_DB_URL set)")
      } else {
        tags$div(style="margin:8px 0; padding:10px; border:1px solid #ffc107; border-radius:6px; background:#fff8e1;",
                 tags$strong("Database: "), "SQLite (BVXC_DB_URL NOT set). Settings will not persist across redeployments on Connect Cloud.")
      },
      if (!HAVE_SQUARE_CREDS && SANDBOX_MODE == "square") {
        tags$div(style="margin:8px 0; padding:10px; border:1px solid #dc3545; border-radius:6px; background:#fff5f5;",
                 tags$strong("Square: "), "SANDBOX_MODE is 'square' but Square credentials are missing. Falling back to fake mode.")
      } else NULL
    )

    tabs <- list()

    if (tab_on("tab_daypass_enabled", TRUE)) {
      tabs <- c(tabs, list(
        tabPanel(
          title = "Day Pass",
          value = "Day Pass",
          fluidPage(
            h3("Day Passes"),
            tags$p(season_label(season_win())),
            p("Choose your ski day and passes, then add to cart. Payment happens on the Cart tab."),
            fluidRow(
              column(
                4,
                uiOutput("day_date_ui"),
                numericInput("day_adult",  "Adult",   value = 0, min = 0, step = 1),
                numericInput("day_youth",  "Youth",   value = 0, min = 0, step = 1),
                numericInput("day_under9", "Under 9", value = 0, min = 0, step = 1),
                numericInput("day_family", "Family",  value = 0, min = 0, step = 1),
                br(),
                actionButton("day_add_to_cart", "Add to cart")
              ),
              column(8, h4("Price summary"), verbatimTextOutput("day_price_summary"))
            )
          )
        )
      ))
    }

    if (tab_on("tab_christmas_enabled", TRUE)) {
      tabs <- c(tabs, list(
        tabPanel(
          title = "Christmas Pass",
          value = "Christmas Pass",
          fluidPage(
            h3("Christmas Pass"),
            tags$p(season_label(season_win())),
            p("Choose a 14-day window that must include Dec 25. Add to cart, then pay on the Cart tab."),
            fluidRow(
              column(
                4,
                dateInput("xmas_start", "Start date (14-day window)", value = Sys.Date()),
                numericInput("xmas_qty", "Number of passes", value = 0, min = 0, step = 1),
                br(),
                actionButton("xmas_add_to_cart", "Add to cart")
              ),
              column(8, h4("Summary"), verbatimTextOutput("xmas_summary"))
            )
          )
        )
      ))
    }

    if (tab_on("tab_season_enabled", TRUE)) {
      tabs <- c(tabs, list(
        tabPanel(
          title = "Season Pass",
          value = "Season Pass",
          fluidPage(
            h3("Season Passes"),
            uiOutput("season_info"),
            fluidRow(
              column(
                4,
                numericInput("season_adult",  "Adult", value = 0, min = 0, step = 1),
                numericInput("season_youth",  "Youth", value = 0, min = 0, step = 1),
                br(),
                actionButton("season_add_to_cart", "Add to cart")
              ),
              column(8, h4("Price summary"), verbatimTextOutput("season_price_summary"))
            )
          )
        )
      ))
    }

    if (tab_on("tab_programs_enabled", TRUE)) {
      tabs <- c(tabs, list(
        tabPanel(
          title = "Programs",
          value = "Programs",
          fluidPage(
            h3("Programs"),
            p("Select a program and how many participants, then add to cart."),
            fluidRow(
              column(
                4,
                selectInput("program_choice", "Program",
                            choices = setNames(get_program_list()$id, get_program_list()$name)),
                numericInput("program_qty", "Number of participants", value = 0, min = 0, step = 1),
                br(),
                actionButton("program_add_to_cart", "Add to cart")
              ),
              column(8, h4("Program details"), verbatimTextOutput("program_summary"))
            )
          )
        )
      ))
    }

    if (tab_on("tab_events_enabled", TRUE)) {
      tabs <- c(tabs, list(
        tabPanel(
          title = "Special Events",
          value = "Special Events",
          fluidPage(
            h3("Special Events"),
            p("Add one or more special event registrations to the cart."),
            fluidRow(
              column(
                4,
                uiOutput("event_picker_ui"),
                numericInput("event_qty", "Number of participants", value = 0, min = 0, step = 1),
                br(),
                actionButton("event_add_to_cart", "Add to cart")
              ),
              column(8, h4("Event details"), verbatimTextOutput("event_summary"))
            )
          )
        )
      ))
    }

    if (tab_on("tab_donation_enabled", TRUE)) {
      tabs <- c(tabs, list(
        tabPanel(
          title = "Donation",
          value = "Donation",
          fluidPage(
            h3("Donation"),
            p("Add a donation to the cart and pay later under the Cart tab."),
            tags$div(
              style = "margin-top: 8px; padding: 10px; border: 1px solid #ddd; border-radius: 6px; background: #fafafa;",
              tags$strong("Important: "),
              "The club is not a registered charity at this time. Donations are not tax-deductible."
            ),
            br(),
            fluidRow(
              column(
                4,
                textInput("donor_name",  "Name (optional)", ""),
                textInput("donor_email", "Email (optional)", ""),
                numericInput("donation_amount", "Donation amount (CAD)", value = 0, min = 0, step = 1),
                br(),
                actionButton("donate_add_to_cart", "Add donation to cart")
              ),
              column(8, verbatimTextOutput("donation_status"))
            )
          )
        )
      ))
    }

    tabs <- c(tabs, list(
      tabPanel(
        title = tags$span(class = if (has_cart) "cart-hot" else NULL, cart_label),
        value = "Cart",
        fluidPage(
          h3("Cart – review and pay"),
          env_diag,
          fluidRow(
            column(
              6,
              textInput("buyer_name",  "Name for receipt", ""),
              textInput("buyer_email", "Email for receipt", ""),
              br(),
              tableOutput("cart_table"),
              uiOutput("cart_remove_ui"),
              actionButton("cart_remove_btn", "Remove selected item"),
              br(),
              strong(textOutput("cart_total")),
              br(), br(),
              actionButton("cart_clear",    "Clear cart"),
              actionButton("cart_checkout", "Pay now"),
              br(), br(),
              uiOutput("receipt_panel")
            ),
            column(
              6,
              h4("Notes"),
              p("1. All items you added from other tabs are listed here."),
              p("2. When you click Pay now, you’ll be redirected to a secure Square checkout page."),
              p("3. After successful payment, you will receive a Square receipt at the email you provided."),
              tags$div(
                style = if (SQUARE_ENV == "sandbox") {
                  "margin-top: 10px; padding: 10px; border: 1px solid #0d6efd; border-radius: 6px; background: #f3f8ff;"
                } else {
                  "margin-top: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 6px; background: #fafafa;"
                },
                tags$strong("Test card (sandbox): "),
                tags$div("Card number: 4111 1111 1111 1111"),
                tags$div("Expiry: 12/26"),
                tags$div("CVV: 111"),
                tags$div("Name / email / ZIP: any values")
              ),
              br(),
              tags$p(
                style = "padding: 5px 10px; border-radius: 6px; font-weight: 700;",
                if (SQUARE_ENV == "sandbox") "background: #ffc107;" else "background: #28a745; color: white;",
                if (SQUARE_ENV == "sandbox") "SANDBOX – TEST MODE – NO REAL CHARGES" else "LIVE – PRODUCTION PAYMENTS ENABLED"
              ),
              tags$p(APP_VERSION),
              tags$p(ENV_LABEL)
            )
          )
        )
      ),
      tabPanel(
        title = "Admin",
        value = "Admin",
        fluidPage(
          h3("Admin"),
          passwordInput("admin_password", "Admin password"),
          actionButton("admin_login", "Log in"),
          br(), br(),
          uiOutput("admin_content")
        )
      )
    ))

    do.call(navbarPage, c(list(title = "BVXC", id = "main_nav"), tabs))
  })

  # -----------------------------------------------------------------------------
  # DAY PASS date UI
  # -----------------------------------------------------------------------------

  output$day_date_ui <- renderUI({
    day_date_ui_nonce()
    w <- season_win()

    min_d <- max(Sys.Date(), w$start)
    max_d <- w$end

    if (min_d > max_d) {
      return(tags$div(
        style = "padding:10px; border:1px solid #ddd; border-radius:6px; background:#fafafa; color:#555;",
        "Day passes are not available right now. ",
        season_label(w)
      ))
    }

    v <- last_valid_day_date()
    if (is.null(v) || is.na(v)) v <- min_d
    if (v < min_d) v <- min_d
    if (v > max_d) v <- max_d

    blk <- blocked_chr()
    if (length(blk) > 0 && as.character(v) %in% blk) {
      d <- v
      for (i in 1:200) {
        d <- d + 1
        if (d > max_d) break
        if (!(as.character(d) %in% blk)) { v <- d; break }
      }
    }
    last_valid_day_date(v)

    dateInput("day_date", "Ski date", value = v, min = min_d, max = max_d)
  })

  # -----------------------------------------------------------------------------
  # DAY PASS
  # -----------------------------------------------------------------------------

  output$day_price_summary <- renderText({
    date <- suppressWarnings(as.Date(input$day_date))
    if (is.null(date) || length(date) == 0 || is.na(date)) date <- last_valid_day_date()

    prices <- get_day_prices()
    pr <- setNames(prices$price, prices$type)

    getp <- function(name) {
      v <- pr[[name]]
      if (is.null(v) || length(v) == 0) return(NA_real_)
      v <- suppressWarnings(as.numeric(v[1]))
      if (is.na(v) || v < 0) NA_real_ else v
    }

    pA <- getp("Adult")
    pY <- getp("Youth")
    pU <- getp("Under 9")
    pF <- getp("Family")

    qa <- qty_int(input$day_adult,  "Adult")
    qy <- qty_int(input$day_youth,  "Youth")
    qu <- qty_int(input$day_under9, "Under 9")
    qf <- qty_int(input$day_family, "Family")

    ok <- !any(is.na(c(pA, pY, pU, pF)))
    total <- if (ok) qa*pA + qy*pY + qu*pU + qf*pF else NA_real_

    paste0(
      "Date: ", as.character(date), "\n",
      "Adult:   ", qa, " x ", fmt_price(pA), "\n",
      "Youth:   ", qy, " x ", fmt_price(pY), "\n",
      "Under 9: ", qu, " x ", fmt_price(pU), "\n",
      "Family:  ", qf, " x ", fmt_price(pF), "\n",
      "-------------------------\n",
      "Total: ", if (ok) paste0("$", sprintf("%.2f", total)) else "N/A (set prices in Admin)"
    )
  })

  observeEvent(input$day_add_to_cart, {
    w <- season_win()
    d <- suppressWarnings(as.Date(input$day_date))
    if (is.na(d)) { showNotification("Please select a valid ski date.", type="error"); return() }

    if (d < w$start || d > w$end) {
      showNotification("That date is outside the current season window.", type="error")
      return()
    }

    if (as.character(d) %in% blocked_chr()) {
      showNotification("That date is blocked. Choose another date.", type = "error")
      bump_day_date_ui()
      return()
    }

    prices <- get_day_prices()
    pr <- setNames(prices$price, prices$type)

    qa <- qty_int(input$day_adult,  "Adult")
    qy <- qty_int(input$day_youth,  "Youth")
    qu <- qty_int(input$day_under9, "Under 9")
    qf <- qty_int(input$day_family, "Family")

    if (qa > 0) add_to_cart("day_pass", paste("Day pass – Adult –", as.character(d)),  qa, pr[["Adult"]],   list(type="Adult", date=as.character(d)))
    if (qy > 0) add_to_cart("day_pass", paste("Day pass – Youth –", as.character(d)),  qy, pr[["Youth"]],   list(type="Youth", date=as.character(d)))
    if (qu > 0) add_to_cart("day_pass", paste("Day pass – Under 9 –", as.character(d)),qu, pr[["Under 9"]], list(type="Under 9", date=as.character(d)))
    if (qf > 0) add_to_cart("day_pass", paste("Day pass – Family –", as.character(d)), qf, pr[["Family"]],  list(type="Family", date=as.character(d)))
  })

  # -----------------------------------------------------------------------------
  # CHRISTMAS PASS
  # -----------------------------------------------------------------------------

  output$xmas_summary <- renderText({
    start_raw <- input$xmas_start
    if (is.null(start_raw) || length(start_raw) == 0 || !nzchar(as.character(start_raw))) {
      return("Choose a start date.")
    }

    start <- suppressWarnings(as.Date(start_raw))
    if (is.na(start)) return("Choose a valid start date.")

    qty <- qty_int(input$xmas_qty, "Passes")

    end <- start + 13
    dec25 <- as.Date(sprintf("%d-12-25", as.integer(format(start, "%Y"))))
    includes_dec25 <- (dec25 >= start) && (dec25 <= end)

    price <- get_christmas_pass_price()
    ok <- !is.na(price)
    total <- if (ok) qty * price else NA_real_

    paste0(
      "Window: ", format(start), " to ", format(end), "\n",
      "Includes Dec 25: ", ifelse(includes_dec25, "YES", "NO"), "\n",
      "Price: ", fmt_price(price), " per pass\n",
      "Quantity: ", qty, "\n",
      "-------------------------\n",
      "Total: ", if (ok) paste0("$", sprintf("%.2f", total)) else "N/A (set price in Admin)"
    )
  })

  observeEvent(input$xmas_add_to_cart, {
    start_raw <- input$xmas_start
    if (is.null(start_raw) || length(start_raw) == 0 || !nzchar(as.character(start_raw))) {
      showNotification("Choose a start date.", type = "error")
      return()
    }

    start <- suppressWarnings(as.Date(start_raw))
    if (is.na(start)) {
      showNotification("Choose a valid start date.", type = "error")
      return()
    }

    qty <- qty_int(input$xmas_qty, "Passes")
    if (qty <= 0) {
      showNotification("Enter a quantity greater than zero.", type = "error")
      return()
    }

    end <- start + 13
    dec25 <- as.Date(sprintf("%d-12-25", as.integer(format(start, "%Y"))))
    includes_dec25 <- (dec25 >= start) && (dec25 <= end)

    if (!includes_dec25) {
      showNotification("The 14-day window must include Dec 25.", type = "error")
      return()
    }

    add_to_cart(
      category    = "christmas_pass",
      description = paste0("Christmas Pass – ", format(start), " to ", format(end)),
      quantity    = qty,
      unit_price  = get_christmas_pass_price(),
      meta        = list(start = as.character(start), end = as.character(end))
    )
  })

  # -----------------------------------------------------------------------------
  # SEASON PASS
  # -----------------------------------------------------------------------------

  output$season_info <- renderUI({
    cutoff <- get_early_bird_cutoff()
    if (is.na(cutoff)) return(p("Early-bird cutoff is not set. Admin can set it in the Admin tab."))
    is_eb <- Sys.Date() <= cutoff
    p(if (is_eb) paste("Early-bird pricing in effect until", format(cutoff, "%Y-%m-%d"))
      else paste("Regular pricing (early-bird ended", format(cutoff, "%Y-%m-%d"), ")"))
  })

  output$season_price_summary <- renderText({
    cutoff <- get_early_bird_cutoff()
    is_eb <- if (is.na(cutoff)) FALSE else (Sys.Date() <= cutoff)

    prices <- get_season_prices(is_eb)
    pr <- setNames(prices$price, prices$type)

    qa <- qty_int(input$season_adult, "Adult")
    qy <- qty_int(input$season_youth, "Youth")

    ok <- !(is.na(pr[["Adult"]]) || is.na(pr[["Youth"]]))
    total <- if (ok) qa*pr[["Adult"]] + qy*pr[["Youth"]] else NA_real_

    paste0(
      "Adult:  ", qa, " x ", fmt_price(pr[["Adult"]]), "\n",
      "Youth:  ", qy, " x ", fmt_price(pr[["Youth"]]), "\n",
      "-------------------------\n",
      "Total: ", if (ok) paste0("$", sprintf("%.2f", total)) else "N/A (set prices in Admin)"
    )
  })

  observeEvent(input$season_add_to_cart, {
    cutoff <- get_early_bird_cutoff()
    is_eb <- if (is.na(cutoff)) FALSE else (Sys.Date() <= cutoff)

    prices <- get_season_prices(is_eb)
    pr <- setNames(prices$price, prices$type)

    qa <- qty_int(input$season_adult, "Adult")
    qy <- qty_int(input$season_youth, "Youth")

    if (qa > 0) add_to_cart("season_pass", "Season pass – Adult", qa, pr[["Adult"]], list(type="Adult", early_bird=is_eb))
    if (qy > 0) add_to_cart("season_pass", "Season pass – Youth", qy, pr[["Youth"]], list(type="Youth", early_bird=is_eb))
  })

  # -----------------------------------------------------------------------------
  # PROGRAMS
  # -----------------------------------------------------------------------------

  output$program_summary <- renderText({
    programs <- get_program_list()
    id  <- input$program_choice
    qty <- qty_int(input$program_qty, "Participants")
    row <- programs[programs$id == id, , drop = FALSE]
    if (nrow(row) == 0) return("No program selected.")

    ok <- !is.na(row$price[1])
    total <- if (ok) qty * row$price[1] else NA_real_

    paste0(
      "Program: ", row$name[1], "\n",
      "Price: ", fmt_price(row$price[1]), " per participant\n",
      "Quantity: ", qty, "\n",
      "-------------------------\n",
      "Total: ", if (ok) paste0("$", sprintf("%.2f", total)) else "N/A (set price in Admin)"
    )
  })

  observeEvent(input$program_add_to_cart, {
    programs <- get_program_list()
    id  <- input$program_choice
    qty <- qty_int(input$program_qty, "Participants")
    row <- programs[programs$id == id, , drop = FALSE]
    if (nrow(row) == 0 || qty <= 0) return()

    add_to_cart("program", paste("Program –", row$name[1]), qty, row$price[1],
                list(program_id=row$id[1], program_name=row$name[1]))
  })

  # -----------------------------------------------------------------------------
  # SPECIAL EVENTS (refreshable)
  # -----------------------------------------------------------------------------

  output$event_picker_ui <- renderUI({
    events_nonce()
    ev <- get_special_events(enabled_only = TRUE)
    if (nrow(ev) == 0) return(tags$div(style="color:#666;", "No events available right now."))
    choices <- setNames(ev$id, paste0(ev$name, " (", ev$event_date, ")"))
    selectInput("event_choice", "Event", choices = choices)
  })

  output$event_summary <- renderText({
    events_nonce()
    ev <- get_special_events(enabled_only = TRUE)

    id  <- input$event_choice
    qty <- qty_int(input$event_qty, "Participants")
    if (is.null(id) || !nzchar(id) || nrow(ev) == 0) return("No event selected.")

    row <- ev[ev$id == id, , drop = FALSE]
    if (nrow(row) == 0) return("No event selected.")

    price <- row$price_cad[1]
    ok <- !is.na(price)
    total <- if (ok) qty * price else NA_real_

    paste0(
      "Event: ", row$name[1], "\n",
      "Date: ", row$event_date[1], "\n",
      "Price: ", fmt_price(price), " per participant\n",
      "Quantity: ", qty, "\n",
      "Capacity (info): ", ifelse(is.na(row$capacity[1]), "N/A", row$capacity[1]), "\n",
      "-------------------------\n",
      "Total: ", if (ok) paste0("$", sprintf("%.2f", total)) else "N/A (set price in Admin)"
    )
  })

  observeEvent(input$event_add_to_cart, {
    events_nonce()
    ev <- get_special_events(enabled_only = TRUE)

    id  <- input$event_choice
    qty <- qty_int(input$event_qty, "Participants")
    if (is.null(id) || !nzchar(id) || qty <= 0 || nrow(ev) == 0) return()

    row <- ev[ev$id == id, , drop = FALSE]
    if (nrow(row) == 0) return()

    add_to_cart(
      category    = "event",
      description = paste0("Event – ", row$name[1], " (", row$event_date[1], ")"),
      quantity    = qty,
      unit_price  = row$price_cad[1],
      meta        = list(event_id=row$id[1], event_name=row$name[1], event_date=row$event_date[1])
    )
  })

  # -----------------------------------------------------------------------------
  # DONATION (add to cart)
  # -----------------------------------------------------------------------------

  output$donation_status <- renderText({ "" })

  observeEvent(input$donate_add_to_cart, {
    amt   <- suppressWarnings(as.numeric(input$donation_amount %||% 0))
    name  <- trimws(input$donor_name %||% "")
    email <- trimws(input$donor_email %||% "")

    if (is.na(amt) || amt <= 0) {
      output$donation_status <- renderText("Please enter a donation amount greater than zero.")
      return()
    }

    add_to_cart(
      category    = "donation",
      description = "Donation – Bulkley Valley Cross Country Ski Club",
      quantity    = 1,
      unit_price  = amt,
      meta        = list(donor_name = name, donor_email = email)
    )

    output$donation_status <- renderText(paste0("Donation of $", sprintf("%.2f", amt), " added to cart."))
  })

  # -----------------------------------------------------------------------------
  # RECEIPT RETURN / STATUS POLL
  # -----------------------------------------------------------------------------

  square_retrieve_order <- function(order_id) {
    if (!nzchar(order_id %||% "")) return(NULL)
    if (!nzchar(SQUARE_ACCESS_TOKEN) || !nzchar(SQUARE_LOCATION_ID)) return(NULL)

    url <- paste0(square_base_url(), "/v2/orders/", order_id)

    res <- httr::GET(url, square_headers(include_version = TRUE), httr::timeout(4))
    if (httr::status_code(res) >= 300) {
      warning("Square RetrieveOrder error: ", httr::content(res, as = "text", encoding = "UTF-8"))
      return(NULL)
    }
    httr::content(res, as = "parsed", type = "application/json")
  }

  square_get_payment <- function(payment_id) {
    if (!nzchar(payment_id %||% "")) return(NULL)
    if (!nzchar(SQUARE_ACCESS_TOKEN)) return(NULL)

    url <- paste0(square_base_url(), "/v2/payments/", payment_id)

    res <- httr::GET(url, square_headers(include_version = TRUE), httr::timeout(4))
    if (httr::status_code(res) >= 300) {
      warning("Square GetPayment error: ", httr::content(res, as = "text", encoding = "UTF-8"))
      return(NULL)
    }
    httr::content(res, as = "parsed", type = "application/json")
  }

  load_receipt_token <- function(token) {
    if (is.null(token) || !nzchar(token)) return(NULL)
    x <- db_get1(
      "SELECT id, created_at, buyer_name, buyer_email, total_amount_cents, currency, cart_json,
              square_checkout_id, square_order_id, receipt_token, status
       FROM transactions
       WHERE receipt_token = ?token",
      token = token
    )
    if (nrow(x) == 1) x else NULL
  }

  refresh_tx_status_from_square <- function(receipt_token) {
    if (!nzchar(receipt_token %||% "")) return(NULL)

    tx <- db_get1(
      "SELECT id, created_at, buyer_name, buyer_email, total_amount_cents, currency, cart_json,
              square_checkout_id, square_order_id, receipt_token, status
       FROM transactions
       WHERE receipt_token = ?token
       LIMIT 1",
      token = receipt_token
    )
    if (nrow(tx) != 1) return(NULL)

    st <- toupper(trimws(tx$status[1] %||% ""))
    if (!st %in% c("PENDING", "PENDING_SANDBOX")) return(tx)

    order_id <- tx$square_order_id[1] %||% ""
    if (!nzchar(order_id) || is.na(order_id)) return(tx)

    payload <- tryCatch(square_retrieve_order(order_id), error = function(e) NULL)
    if (is.null(payload) || is.null(payload$order)) return(tx)

    order <- payload$order
    order_state <- toupper(order$state %||% "")

    if (order_state == "CANCELED") {
      db_exec1("UPDATE transactions SET status = 'CANCELED' WHERE receipt_token = ?token", token = receipt_token)
      tx$status[1] <- "CANCELED"
      return(tx)
    }

    tenders <- order$tenders %||% list()
    payment_id <- NULL
    if (length(tenders) > 0) {
      payment_id <- tenders[[1]]$payment_id %||% tenders[[1]]$paymentId %||% NULL
    }

    if (!nzchar(payment_id %||% "")) return(tx)

    pay <- tryCatch(square_get_payment(payment_id), error = function(e) NULL)
    if (is.null(pay) || is.null(pay$payment)) return(tx)

    p <- pay$payment
    p_status <- toupper(p$status %||% "")

    expected_amt <- as.integer(tx$total_amount_cents[1] %||% NA_integer_)
    expected_cur <- toupper(tx$currency[1] %||% "CAD")

    got_amt <- tryCatch(as.integer(p$amount_money$amount %||% NA_integer_), error = function(e) NA_integer_)
    got_cur <- tryCatch(toupper(p$amount_money$currency %||% ""), error = function(e) "")

    new_status <- st
    if (p_status == "COMPLETED") {
      if (!is.na(expected_amt) && !is.na(got_amt) && expected_amt == got_amt && expected_cur == got_cur) {
        new_status <- "COMPLETED"
      } else {
        new_status <- "AMOUNT_MISMATCH"
      }
    } else if (p_status %in% c("CANCELED", "FAILED")) {
      new_status <- p_status
    }

    if (!identical(new_status, st)) {
      db_exec1(
        "UPDATE transactions SET status = ?status WHERE receipt_token = ?token",
        status = new_status,
        token  = receipt_token
      )
      tx$status[1] <- new_status
    }

    tx
  }

  session$onFlushed(function() {
    isolate({
      qs <- session$clientData$url_search %||% ""
      token <- parseQueryString(qs)[["receipt"]]

      tx <- refresh_tx_status_from_square(token)
      if (is.null(tx)) tx <- load_receipt_token(token)

      if (!is.null(tx)) {
        receipt_tx(tx)
        updateTabsetPanel(session, "main_nav", selected = "Cart")
      }
    })
  }, once = TRUE)

  observeEvent(session$clientData$url_search, {
    qs <- session$clientData$url_search %||% ""
    token <- parseQueryString(qs)[["receipt"]]

    tx <- refresh_tx_status_from_square(token)
    if (is.null(tx)) tx <- load_receipt_token(token)

    if (!is.null(tx)) {
      receipt_tx(tx)
      updateTabsetPanel(session, "main_nav", selected = "Cart")
    }
  }, ignoreInit = TRUE)

  output$receipt_panel <- renderUI({
    tx <- receipt_tx()
    if (is.null(tx)) return(NULL)

    total   <- tx$total_amount_cents[1] / 100
    name    <- tx$buyer_name[1]
    status  <- toupper(tx$status[1] %||% "")
    created <- tx$created_at[1]

    title <- switch(status,
      "COMPLETED"        = "Payment complete",
      "SANDBOX_TEST_OK"  = "Test payment recorded",
      "PENDING"          = "Payment not yet confirmed",
      "PENDING_SANDBOX"  = "Payment not yet confirmed (sandbox)",
      "FAILED"           = "Payment failed",
      "CANCELED"         = "Payment canceled",
      "AMOUNT_MISMATCH"  = "Payment amount mismatch",
      paste0("Payment status: ", status)
    )

    msg <- switch(status,
      "COMPLETED"        = "Your payment has been confirmed.",
      "SANDBOX_TEST_OK"  = "This is a sandbox fake-mode transaction. No real payment was processed.",
      "PENDING"          = "We have not confirmed payment yet. If you just paid, refresh this page in 10–30 seconds.",
      "PENDING_SANDBOX"  = "We have not confirmed payment yet (sandbox). Refresh this page in 10–30 seconds.",
      "FAILED"           = "Square reported the payment as FAILED. Please try again.",
      "CANCELED"         = "Square reported the order as CANCELED. No payment was taken.",
      "AMOUNT_MISMATCH"  = "Square reported a completed payment, but the amount/currency did not match what we expected. Contact the club.",
      "Status recorded. If unsure, contact the club."
    )

    tagList(
      tags$hr(),
      h4(title),
      p(sprintf(
        "Thank you, %s, for supporting Bulkley Valley Cross Country Ski Club.",
        ifelse(is.na(name) || name == "", "skier", name)
      )),
      p(sprintf("Payment date/time: %s", created)),
      p(sprintf("Amount: $%.2f %s", total, tx$currency[1])),
      p(sprintf("Status: %s", status)),
      tags$div(style="margin-top:8px; padding:10px; border:1px solid #ddd; border-radius:6px; background:#fafafa;", msg),
      br(),
      h5("Receipt QR code"),
      plotOutput("receipt_qr", height = "260px", width = "260px")
    )
  })

  output$receipt_qr <- renderPlot({
    tx <- receipt_tx()
    if (is.null(tx)) return(invisible(NULL))

    token <- tx$receipt_token[1]
    if (is.null(token) || is.na(token) || !nzchar(token)) return(invisible(NULL))

    qr_payload <- if (!is.na(RETURN_BASE_URL) && nzchar(RETURN_BASE_URL)) {
      paste0(RETURN_BASE_URL, "/?receipt=", token)
    } else {
      token
    }

    m <- qrcode::qr_code(qr_payload)
    if (is.null(m) || length(m) == 0) return(invisible(NULL))
    m <- m[nrow(m):1, , drop = FALSE]

    op <- par(mar = c(0, 0, 0, 0))
    on.exit(par(op), add = TRUE)

    plot.new()
    plot.window(xlim = c(0, ncol(m)), ylim = c(0, nrow(m)), asp = 1)

    for (r in seq_len(nrow(m))) {
      for (c in seq_len(ncol(m))) {
        rect(c - 1, r - 1, c, r, col = if (m[r, c] == 1) "black" else "white", border = NA)
      }
    }
  }, res = 120)

  # -----------------------------------------------------------------------------
  # CART TAB
  # -----------------------------------------------------------------------------

  output$cart_table <- renderTable({
    df <- rv$cart
    if (nrow(df) == 0) return(NULL)
    df$line_total <- df$quantity * df$unit_price
    df[, c("category", "description", "quantity", "unit_price", "line_total")]
  }, digits = 2)

  output$cart_remove_ui <- renderUI({
    df <- rv$cart
    if (nrow(df) == 0) return(NULL)
    choices <- setNames(df$id, paste0(df$description, " (x", df$quantity, ")"))
    selectInput("cart_remove_id", "Select item to remove", choices = choices)
  })

  observeEvent(input$cart_remove_btn, {
    id <- input$cart_remove_id %||% ""
    if (!nzchar(id)) return()
    rv$cart <- rv$cart[rv$cart$id != id, , drop = FALSE]
  })

  output$cart_total <- renderText({
    df <- rv$cart
    if (nrow(df) == 0) return("Cart is empty.")
    paste("Total:", sprintf("$%.2f", sum(df$quantity * df$unit_price)))
  })

  observeEvent(input$cart_clear, { clear_cart() })

  observeEvent(input$cart_checkout, {
    df <- rv$cart
    if (nrow(df) == 0) {
      showNotification("Cart is empty.", type = "warning")
      return()
    }

    buyer_name  <- input$buyer_name %||% ""
    buyer_email <- input$buyer_email %||% ""
    if (!nzchar(buyer_email)) {
      showNotification("Please enter an email for the receipt.", type = "warning")
      return()
    }

    msg <- validate_cart_limits(df)
    if (!is.null(msg)) {
      showNotification(msg, type = "error")
      return()
    }

    total_cents <- cart_total_cents(df)

    if (SQUARE_ENV == "sandbox" && SANDBOX_MODE == "fake") {
      receipt_token <- UUIDgenerate()

      db_exec1(
        "INSERT INTO transactions (
           id, created_at, buyer_name, buyer_email, total_amount_cents, currency, cart_json,
           square_checkout_id, square_order_id, receipt_token, status
         ) VALUES (
           ?id, ?created_at, ?buyer_name, ?buyer_email, ?total_cents, 'CAD', ?cart_json,
           NULL, NULL, ?receipt_token, 'SANDBOX_TEST_OK'
         )",
        id            = UUIDgenerate(),
        created_at    = now_ts(),
        buyer_name    = buyer_name,
        buyer_email   = buyer_email,
        total_cents   = total_cents,
        cart_json     = jsonlite::toJSON(df, auto_unbox = TRUE),
        receipt_token = receipt_token
      )

      receipt_tx(load_receipt_token(receipt_token))
      clear_cart()

      showModal(modalDialog(
        title = "Sandbox test payment simulated",
        "No real payment was processed. This is a TEST ONLY transaction in sandbox fake mode.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    receipt_token <- UUIDgenerate()
    redirect_url  <- build_redirect_url(receipt_token)

    res <- create_square_checkout_from_cart(
      cart_df      = df,
      buyer_name   = buyer_name,
      buyer_email  = buyer_email,
      note         = if (SQUARE_ENV == "sandbox") "BVXC sandbox cart checkout" else "BVXC production cart checkout",
      redirect_url = redirect_url
    )
    if (is.null(res) || is.null(res$checkout_url)) {
      showNotification("Error creating Square checkout.", type = "error")
      return()
    }

    db_exec1(
      "INSERT INTO transactions (
         id, created_at, buyer_name, buyer_email, total_amount_cents, currency, cart_json,
         square_checkout_id, square_order_id, receipt_token, status
       ) VALUES (
         ?id, ?created_at, ?buyer_name, ?buyer_email, ?total_cents, 'CAD', ?cart_json,
         ?checkout_id, ?order_id, ?receipt_token, ?status
       )",
      id            = UUIDgenerate(),
      created_at    = now_ts(),
      buyer_name    = buyer_name,
      buyer_email   = buyer_email,
      total_cents   = total_cents,
      cart_json     = jsonlite::toJSON(df, auto_unbox = TRUE),
      checkout_id   = res$checkout_id %||% NA_character_,
      order_id      = res$square_order %||% NA_character_,
      receipt_token = receipt_token,
      status        = if (SQUARE_ENV == "sandbox") "PENDING_SANDBOX" else "PENDING"
    )

    session$sendCustomMessage("redirect", list(url = res$checkout_url))
    clear_cart()
  })

  # -----------------------------------------------------------------------------
  # ADMIN
  # -----------------------------------------------------------------------------

  observeEvent(input$admin_login, {
    now <- Sys.time()

    if (!is.na(rv$admin_lock_until) && now < rv$admin_lock_until) {
      secs <- ceiling(as.numeric(difftime(rv$admin_lock_until, now, units = "secs")))
      showNotification(paste0("Admin login locked. Try again in ", secs, " seconds."), type = "error")
      return()
    }

    ok <- (!is.na(ADMIN_PASSWORD) && nzchar(ADMIN_PASSWORD) && identical(input$admin_password, ADMIN_PASSWORD))

    if (ok) {
      rv$admin_logged_in  <- TRUE
      rv$admin_fail_count <- 0L
      rv$admin_lock_until <- as.POSIXct(NA)
      updateTextInput(session, "admin_password", value = "")
    } else {
      rv$admin_logged_in  <- FALSE
      rv$admin_fail_count <- rv$admin_fail_count + 1L
      showNotification("Invalid admin password.", type = "error")

      if (rv$admin_fail_count >= 5L) {
        rv$admin_lock_until <- now + 60
        rv$admin_fail_count <- 0L
        showNotification("Too many failed attempts. Locked for 60 seconds.", type = "error")
      }
    }
  })

  output$admin_content <- renderUI({
    if (!rv$admin_logged_in) return(p("Please log in to see admin options."))

    eb_val <- cfg_date("early_bird_cutoff", as.Date(NA))
    eb_val <- if (is.na(eb_val)) NULL else eb_val

    tagList(
      h4("Global settings"),
      fluidRow(
        column(4, dateInput("adm_early_bird_cutoff", "Early-bird cutoff", value = eb_val)),
        column(4, textInput("adm_limit_max_total", "Max transaction total CAD (blank = no limit)", value = cfg_get("limit_max_total_cad", ""))),
        column(4, textInput("adm_limit_max_items", "Max total items (blank = no limit)", value = cfg_get("limit_max_items_total", "")))
      ),
      hr(),

      h4("Tabs (enable/disable)"),
      fluidRow(
        column(3, checkboxInput("adm_tab_daypass",    "Day Pass",       value = cfg_bool("tab_daypass_enabled", TRUE))),
        column(3, checkboxInput("adm_tab_christmas",  "Christmas Pass", value = cfg_bool("tab_christmas_enabled", TRUE))),
        column(3, checkboxInput("adm_tab_season",     "Season Pass",    value = cfg_bool("tab_season_enabled", TRUE))),
        column(3, checkboxInput("adm_tab_programs",   "Programs",       value = cfg_bool("tab_programs_enabled", TRUE)))
      ),
      fluidRow(
        column(3, checkboxInput("adm_tab_events",     "Special Events", value = cfg_bool("tab_events_enabled", TRUE))),
        column(3, checkboxInput("adm_tab_donation",   "Donation",       value = cfg_bool("tab_donation_enabled", TRUE)))
      ),
      br(),
      actionButton("admin_save_toggles_limits", "Save settings / tabs / limits"),
      hr(),

      h4("Prices (CAD) — leave blank for N/A"),
      tags$h5("Day Pass"),
      fluidRow(
        column(3, textInput("adm_price_day_adult",  "Adult",   value = cfg_get("price_day_adult", ""))),
        column(3, textInput("adm_price_day_youth",  "Youth",   value = cfg_get("price_day_youth", ""))),
        column(3, textInput("adm_price_day_under9", "Under 9", value = cfg_get("price_day_under9", ""))),
        column(3, textInput("adm_price_day_family", "Family",  value = cfg_get("price_day_family", "")))
      ),

      tags$h5("Season Pass — early bird"),
      fluidRow(
        column(6, textInput("adm_price_seb_adult", "Adult", value = cfg_get("price_season_eb_adult", ""))),
        column(6, textInput("adm_price_seb_youth", "Youth", value = cfg_get("price_season_eb_youth", "")))
      ),

      tags$h5("Season Pass — regular"),
      fluidRow(
        column(6, textInput("adm_price_sreg_adult", "Adult", value = cfg_get("price_season_reg_adult", ""))),
        column(6, textInput("adm_price_sreg_youth", "Youth", value = cfg_get("price_season_reg_youth", "")))
      ),

      tags$h5("Christmas Pass"),
      fluidRow(
        column(6, textInput("adm_price_christmas", "Christmas pass price", value = cfg_get("price_christmas_pass", "")))
      ),

      tags$h5("Programs"),
      fluidRow(
        column(4, textInput("adm_price_prog_kids",     "Kids Ski Program", value = cfg_get("price_program_kids_ski", ""))),
        column(4, textInput("adm_price_prog_masters",  "Masters Training", value = cfg_get("price_program_masters", ""))),
        column(4, textInput("adm_price_prog_biathlon", "Biathlon Intro",   value = cfg_get("price_program_biathlon_intro", "")))
      ),
      br(),
      actionButton("admin_save_prices", "Save prices"),
      hr(),

      h4("Special events (create / edit / enable / delete)"),
      fluidRow(
        column(
          6,
          textInput("adm_ev_id",   "Event ID (unique, no spaces)", value = ""),
          textInput("adm_ev_name", "Event name", value = ""),
          dateInput("adm_ev_date", "Event date", value = Sys.Date()),
          textInput("adm_ev_price","Price CAD (blank = N/A)", value = ""),
          textInput("adm_ev_cap",  "Capacity (blank = N/A)", value = ""),
          checkboxInput("adm_ev_enabled", "Enabled (visible to public)", value = TRUE),
          br(),
          actionButton("admin_ev_create", "Create new event"),
          actionButton("admin_ev_update", "Update selected event"),
          actionButton("admin_ev_delete", "Delete selected event")
        ),
        column(
          6,
          h5("Existing events"),
          tableOutput("admin_events_table"),
          p("Tip: copy an existing Event ID into the editor to update/delete.", style="color:#666;")
        )
      ),
      hr(),

      h4("Blocked dates"),
      fluidRow(
        column(
          6,
          dateInput("adm_block_date", "Date to block", value = Sys.Date()),
          textInput("adm_block_reason", "Reason (optional)", value = ""),
          br(),
          actionButton("admin_block_add", "Add blocked date"),
          actionButton("admin_block_remove", "Remove blocked date")
        ),
        column(
          6,
          h5("Blocked dates list"),
          tableOutput("admin_blocked_table")
        )
      ),
      hr(),

      h4("Recent transactions (last 10)"),
      tableOutput("admin_recent_tx")
    )
  })

  observeEvent(input$admin_save_toggles_limits, {
    req(rv$admin_logged_in)

    eb <- suppressWarnings(as.Date(input$adm_early_bird_cutoff))
    cfg_set("early_bird_cutoff", if (is.na(eb)) "" else as.character(eb))

    cfg_set("limit_max_total_cad",  input$adm_limit_max_total %||% "")
    cfg_set("limit_max_items_total", input$adm_limit_max_items %||% "")

    cfg_set("tab_daypass_enabled",   if (isTRUE(input$adm_tab_daypass)) "1" else "0")
    cfg_set("tab_christmas_enabled", if (isTRUE(input$adm_tab_christmas)) "1" else "0")
    cfg_set("tab_season_enabled",    if (isTRUE(input$adm_tab_season)) "1" else "0")
    cfg_set("tab_programs_enabled",  if (isTRUE(input$adm_tab_programs)) "1" else "0")
    cfg_set("tab_events_enabled",    if (isTRUE(input$adm_tab_events)) "1" else "0")
    cfg_set("tab_donation_enabled",  if (isTRUE(input$adm_tab_donation)) "1" else "0")

    showNotification("Saved settings/tabs/limits. Refresh tabs if needed.", type = "message")
  })

  observeEvent(input$admin_save_prices, {
    req(rv$admin_logged_in)

    cfg_set("price_day_adult",   input$adm_price_day_adult %||% "")
    cfg_set("price_day_youth",   input$adm_price_day_youth %||% "")
    cfg_set("price_day_under9",  input$adm_price_day_under9 %||% "")
    cfg_set("price_day_family",  input$adm_price_day_family %||% "")

    cfg_set("price_season_eb_adult",  input$adm_price_seb_adult %||% "")
    cfg_set("price_season_eb_youth",  input$adm_price_seb_youth %||% "")

    cfg_set("price_season_reg_adult", input$adm_price_sreg_adult %||% "")
    cfg_set("price_season_reg_youth", input$adm_price_sreg_youth %||% "")

    cfg_set("price_christmas_pass", input$adm_price_christmas %||% "")

    cfg_set("price_program_kids_ski",       input$adm_price_prog_kids %||% "")
    cfg_set("price_program_masters",        input$adm_price_prog_masters %||% "")
    cfg_set("price_program_biathlon_intro", input$adm_price_prog_biathlon %||% "")

    showNotification("Prices saved.", type = "message")
  })

  output$admin_events_table <- renderTable({
    if (!rv$admin_logged_in) return(NULL)
    ev <- get_special_events(enabled_only = FALSE)
    if (nrow(ev) == 0) return(NULL)
    ev[, c("id","name","event_date","price_cad","capacity","enabled")]
  }, digits = 2)

  observeEvent(input$admin_ev_create, {
    req(rv$admin_logged_in)

    id   <- trimws(input$adm_ev_id %||% "")
    name <- trimws(input$adm_ev_name %||% "")
    d    <- suppressWarnings(as.Date(input$adm_ev_date))

    if (!nzchar(id) || !nzchar(name) || is.na(d)) {
      showNotification("Event ID, name, and a valid date are required.", type="error")
      return()
    }

    price <- {
      s <- trimws(input$adm_ev_price %||% "")
      if (!nzchar(s) || toupper(s) %in% c("N/A","NA")) NA_real_ else suppressWarnings(as.numeric(gsub("[\\$,]", "", s)))
    }
    if (!is.na(price) && price < 0) { showNotification("Invalid price.", type="error"); return() }

    cap <- {
      s <- trimws(input$adm_ev_cap %||% "")
      if (!nzchar(s) || toupper(s) %in% c("N/A","NA")) NA_integer_ else suppressWarnings(as.integer(s))
    }
    if (!is.na(cap) && cap < 0) { showNotification("Invalid capacity.", type="error"); return() }

    en <- if (isTRUE(input$adm_ev_enabled)) 1L else 0L

    ok <- tryCatch({
      db_exec1(
        "INSERT INTO special_events (id, name, event_date, price_cad, capacity, enabled, created_at)
         VALUES (?id, ?name, ?event_date, ?price_cad, ?capacity, ?enabled, ?created_at)",
        id         = id,
        name       = name,
        event_date = as.character(d),
        price_cad  = ifelse(is.na(price), NA, price),
        capacity   = ifelse(is.na(cap), NA, cap),
        enabled    = en,
        created_at = now_ts()
      )
      TRUE
    }, error = function(e) FALSE)

    if (!ok) showNotification("Create failed. That ID may already exist.", type="error")
    else {
      showNotification("Event created.", type="message")
      events_nonce(events_nonce() + 1L)
    }
  })

  observeEvent(input$admin_ev_update, {
    req(rv$admin_logged_in)
    id <- trimws(input$adm_ev_id %||% "")
    if (!nzchar(id)) { showNotification("Enter the event ID to update.", type="error"); return() }

    name <- trimws(input$adm_ev_name %||% "")
    d    <- suppressWarnings(as.Date(input$adm_ev_date))
    if (!nzchar(name) || is.na(d)) { showNotification("Name and a valid date are required.", type="error"); return() }

    price <- {
      s <- trimws(input$adm_ev_price %||% "")
      if (!nzchar(s) || toupper(s) %in% c("N/A","NA")) NA_real_ else suppressWarnings(as.numeric(gsub("[\\$,]", "", s)))
    }
    cap <- {
      s <- trimws(input$adm_ev_cap %||% "")
      if (!nzchar(s) || toupper(s) %in% c("N/A","NA")) NA_integer_ else suppressWarnings(as.integer(s))
    }
    en <- if (isTRUE(input$adm_ev_enabled)) 1L else 0L

    n <- db_exec1(
      "UPDATE special_events
       SET name = ?name, event_date = ?event_date, price_cad = ?price_cad, capacity = ?capacity, enabled = ?enabled
       WHERE id = ?id",
      name       = name,
      event_date = as.character(d),
      price_cad  = ifelse(is.na(price), NA, price),
      capacity   = ifelse(is.na(cap), NA, cap),
      enabled    = en,
      id         = id
    )

    if (n == 0) showNotification("No event found with that ID.", type="error")
    else {
      showNotification("Event updated.", type="message")
      events_nonce(events_nonce() + 1L)
    }
  })

  observeEvent(input$admin_ev_delete, {
    req(rv$admin_logged_in)
    id <- trimws(input$adm_ev_id %||% "")
    if (!nzchar(id)) { showNotification("Enter the event ID to delete.", type="error"); return() }

    n <- db_exec1("DELETE FROM special_events WHERE id = ?id", id = id)
    if (n == 0) showNotification("No event found with that ID.", type="error")
    else {
      showNotification("Event deleted.", type="message")
      events_nonce(events_nonce() + 1L)
    }
  })

  output$admin_blocked_table <- renderTable({
    if (!rv$admin_logged_in) return(NULL)
    get_blocked_dates()
  })

  observeEvent(input$admin_block_add, {
    req(rv$admin_logged_in)
    d <- suppressWarnings(as.Date(input$adm_block_date))
    rs <- trimws(input$adm_block_reason %||% "")
    if (is.na(d)) { showNotification("Invalid date.", type="error"); return() }

    ok <- tryCatch({
      db_exec1(
        'INSERT INTO blocked_dates("date", reason) VALUES (?date, ?reason)',
        date   = as.character(d),
        reason = ifelse(nzchar(rs), rs, NA_character_)
      )
      TRUE
    }, error = function(e) FALSE)

    if (!ok) {
      showNotification("Block failed. Date may already be blocked.", type="error")
    } else {
      showNotification("Date blocked.", type="message")
      blocked_nonce(blocked_nonce() + 1L)
      bump_day_date_ui()
    }
  })

  observeEvent(input$admin_block_remove, {
    req(rv$admin_logged_in)
    d <- suppressWarnings(as.Date(input$adm_block_date))
    if (is.na(d)) { showNotification("Invalid date.", type="error"); return() }

    n <- db_exec1('DELETE FROM blocked_dates WHERE "date" = ?date', date = as.character(d))
    if (n == 0) {
      showNotification("That date was not blocked.", type="warning")
    } else {
      showNotification("Blocked date removed.", type="message")
      blocked_nonce(blocked_nonce() + 1L)
      bump_day_date_ui()
    }
  })

  output$admin_recent_tx <- renderTable({
    if (!rv$admin_logged_in) return(NULL)
    db_get1(
      "SELECT created_at, buyer_name, buyer_email, total_amount_cents, status
       FROM transactions
       ORDER BY created_at DESC
       LIMIT 10"
    )
  })
}

shinyApp(ui = ui, server = server)
