# app.R
# BVXC – Passes, Programs, Events + Admin Controls (Square)
# v6.2 – Per-tab checkout panel (no Cart tab) + merged cart lines + Admin subtabs + hidden Receipt tab – 2025-12-18

# ---- renv -------------------------------------------------------------------
if (file.exists("renv/activate.R")) {
  try(source("renv/activate.R"), silent = TRUE)
}

suppressPackageStartupMessages({
  library(shiny)
  library(httr)
  library(DBI)
  library(RSQLite)
  library(uuid)
  library(jsonlite)
  library(qrcode)
  library(pool)
  library(RPostgres)
})

# -----------------------------------------------------------------------------
# GLOBAL SETTINGS / ENV
# -----------------------------------------------------------------------------

Sys.setenv(TZ = "America/Vancouver")
APP_VERSION <- "BVXC v6.2 – Per-tab checkout + merged cart + Admin subtabs + hidden Receipt tab – 2025-12-18"

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
is_true <- function(x) tolower(trimws(x %||% "")) %in% c("1","true","yes","y","on")

if (!SQUARE_ENV %in% ALLOWED_SQUARE_ENVS) SQUARE_ENV <- "sandbox"
if (!SANDBOX_MODE %in% ALLOWED_SANDBOX_MODES) SANDBOX_MODE <- "fake"

HAVE_SQUARE_CREDS <- nzchar(SQUARE_ACCESS_TOKEN) && nzchar(SQUARE_LOCATION_ID)

# If user tries "sandbox+square" without creds, force fake mode
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

DB_URL  <- Sys.getenv("BVXC_DB_URL",  unset = "")
DB_PATH <- Sys.getenv("BVXC_DB_PATH", unset = "bvxc.sqlite")

ALLOW_SQLITE_FALLBACK <- is_true(Sys.getenv("BVXC_ALLOW_SQLITE_FALLBACK", "0"))
db_is_postgres <- function() nzchar(trimws(DB_URL))

if (!db_is_postgres() && !ALLOW_SQLITE_FALLBACK) {
  stop(
    "BVXC_DB_URL is not set. Refusing to run without Postgres.\n",
    "Set BVXC_DB_URL to your Supabase/Postgres connection string.\n",
    "If you *really* want SQLite fallback (dev only), set BVXC_ALLOW_SQLITE_FALLBACK=1."
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

    # URI form (recommended): postgresql://USER:PASSWORD@HOST:5432/DB?sslmode=require
    if (grepl("^postgres(ql)?://", s, ignore.case = TRUE)) {
      return(pool::dbPool(RPostgres::Postgres(), dbname = s))
    }

    # KV form: host=... port=... dbname=... user=... password=...
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

    if (db_is_postgres()) {
      try(db_exec(con, "CREATE INDEX IF NOT EXISTS idx_transactions_created_at ON transactions(created_at)"), silent = TRUE)
      try(db_exec(con, "CREATE INDEX IF NOT EXISTS idx_transactions_receipt_token ON transactions(receipt_token)"), silent = TRUE)
      try(db_exec(con, "CREATE INDEX IF NOT EXISTS idx_events_event_date ON special_events(event_date)"), silent = TRUE)
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
  key   <- as.character(key %||% "")
  value <- as.character(value %||% "")

  with_db(function(con) {
    DBI::dbWithTransaction(con, {
      n <- db_exec(con,
        "UPDATE config SET value = ?value WHERE key = ?key",
        key = key, value = value
      )
      if (isTRUE(n == 0)) {
        tryCatch({
          db_exec(con,
            "INSERT INTO config(key, value) VALUES (?key, ?value)",
            key = key, value = value
          )
        }, error = function(e) {
          db_exec(con,
            "UPDATE config SET value = ?value WHERE key = ?key",
            key = key, value = value
          )
        })
      }
    })
  })
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

# -----------------------------------------------------------------------------
# BUSINESS DATA
# -----------------------------------------------------------------------------

get_early_bird_cutoff <- function() cfg_date("early_bird_cutoff", as.Date(NA))

get_day_prices <- function() {
  data.frame(
    type  = c("Adult", "Youth", "Under 9", "Family"),
    price = c(
      cfg_num("price_day_adult", NA_real_),
      cfg_num("price_day_youth", NA_real_),
      cfg_num("price_day_under9", NA_real_),
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

program_catalog <- function() {
  data.frame(
    id = c(
      "bunnies","rabbits","u10","u12","u14","u16","u18","u20",
      "biathlon_adp","biathlon_rifle_rental_adp","biathlon_youth_intro",
      "masters_2x","masters_1x","biathlon_masters","biathlon_masters_rifle_rental"
    ),
    name = c(
      "Bunnies","Rabbits","U10","U12","U14","U16","U18","U20",
      "Biathlon ADP","Biathlon Rifle Rental ADP","Biathlon Youth Intro",
      "Masters 2X","Masters 1X","Biathlon Masters","Biathlon Masters Rifle Rental"
    ),
    price_key = c(
      "price_program_bunnies","price_program_rabbits","price_program_u10","price_program_u12",
      "price_program_u14","price_program_u16","price_program_u18","price_program_u20",
      "price_program_biathlon_adp","price_program_biathlon_rifle_rental_adp","price_program_biathlon_youth_intro",
      "price_program_masters_2x","price_program_masters_1x","price_program_biathlon_masters","price_program_biathlon_masters_rifle_rental"
    ),
    stringsAsFactors = FALSE
  )
}

get_program_list <- function() {
  cat <- program_catalog()
  cat$price <- vapply(cat$price_key, function(k) cfg_num(k, NA_real_), numeric(1))
  cat[, c("id","name","price","price_key")]
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
  if (is.null(cart_df) || nrow(cart_df) == 0) return(NULL)

  max_total <- cfg_num("limit_max_total_cad", NA_real_)
  max_items <- cfg_int("limit_max_items_total", NA_integer_)

  total <- sum(cart_df$quantity * cart_df$unit_price, na.rm = TRUE)
  items <- sum(cart_df$quantity, na.rm = TRUE)

  if (!is.na(max_total) && total > max_total) {
    return(paste0("Transaction exceeds max total: $", sprintf("%.2f", max_total)))
  }
  if (!is.na(max_items) && items > max_items) {
    return(paste0("Transaction exceeds max items: ", max_items))
  }
  NULL
}

# -----------------------------------------------------------------------------
# SQUARE CHECKOUT (Payment Links)
# -----------------------------------------------------------------------------

square_base_url <- function() {
  if (identical(SQUARE_ENV, "sandbox")) "https://connect.squareupsandbox.com" else "https://connect.squareup.com"
}

square_headers <- function(include_version = TRUE) {
  h <- httr::add_headers(
    "Authorization" = paste("Bearer", SQUARE_ACCESS_TOKEN),
    "Content-Type"  = "application/json"
  )
  if (include_version) h <- c(h, httr::add_headers("Square-Version" = SQUARE_VERSION))
  h
}

create_square_checkout_from_cart <- function(cart_df,
                                            buyer_name   = NULL,
                                            buyer_email  = NULL,
                                            note         = NULL,
                                            redirect_url = NULL) {
  if (is.null(cart_df) || nrow(cart_df) == 0) return(NULL)

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
# UI HELPERS
# -----------------------------------------------------------------------------

checkout_panel_ui <- function(prefix, title = "Checkout") {
  tagList(
    tags$div(
      class = "mini-cart-box",
      h4(title),
      uiOutput(paste0(prefix, "_cart_list")),
      br(),
      textInput(paste0(prefix, "_buyer_name"),  "Name for receipt",  value = ""),
      textInput(paste0(prefix, "_buyer_email"), "Email for receipt", value = ""),
      br(),
      strong(textOutput(paste0(prefix, "_cart_total"))),
      br(), br(),
      actionButton(paste0(prefix, "_cart_clear"), "Clear cart"),
      uiOutput(paste0(prefix, "_cart_pay_ui"))
    )
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
.mini-cart-box { margin-top: 14px; padding: 10px; border: 1px solid #ddd; border-radius: 8px; background: #fafafa; }

.cart-line {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 6px 0;
  border-bottom: 1px solid #eee;
}
.cart-desc { flex: 1; font-weight: 600; }
.cart-controls { display: flex; align-items: center; gap: 12px; }
.cart-qty-input {
  font-size: 20px;
  height: 44px;
  padding: 6px 12px;
  min-width: 120px;
}
/* ---- Receipt status banners ---- */
.receipt-card{
  padding: 16px;
  border-radius: 10px;
  margin-top: 10px;
  border: 1px solid #ddd;
  background: #fafafa;
}

.receipt-title{
  font-size: 34px;
  font-weight: 800;
}

.receipt-sub{
  margin-top: 6px;
  font-weight: 600;
}

/* Status variants */
.receipt-ok{
  border-color: #badbcc;
  background: #d1e7dd;
  color: #0f5132;
}
.receipt-ok .receipt-title,
.receipt-ok .receipt-sub{ color:#0f5132; }

.receipt-pending{
  border-color: #ffecb5;
  background: #fff3cd;
  color: #664d03;
}
.receipt-pending .receipt-title,
.receipt-pending .receipt-sub{ color:#664d03; }

.receipt-bad{
  border-color: #f5c2c7;
  background: #f8d7da;
  color: #842029;
}
.receipt-bad .receipt-title,
.receipt-bad .receipt-sub{ color:#842029; }

.receipt-neutral{
  border-color: #dee2e6;
  background: #f8f9fa;
  color: #495057;
}

.receipt-spinner{
  display:inline-block;
  width:14px; height:14px;
  border:2px solid currentColor;
  border-right-color: transparent;
  border-radius: 50%;
  animation: receiptSpin .8s linear infinite;
  vertical-align: -2px;
}
@keyframes receiptSpin { to { transform: rotate(360deg); } }

/* ---- Pay button emphasis (ONLY when cart has items) ---- */
.pay-now-hot {
  font-weight: 800;
  font-size: 22px;
  color: #0d6efd !important;
  text-decoration: underline;
  text-decoration-thickness: 3px;
  text-underline-offset: 6px;
}
"

ui <- fluidPage(
  tags$head(tags$style(HTML(css_tabs))),
  uiOutput("main_nav_ui"),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('redirect', function(message) {
      try { window.top.location.href = message.url; }
      catch(e) { window.location.href = message.url; }
    });

    // Hide/show the Receipt navbar item without removing the tabPanel (keeps routing stable)
    Shiny.addCustomMessageHandler('toggleReceiptNav', function(message) {
      var show = !!message.show;
      var a = $('a[data-value=\"Receipt\"]');
      if (a.length) a.closest('li').toggle(show);
    });
  "))
)

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  empty_cart_df <- function() {
    data.frame(
      id          = character(),
      category    = character(),
      description = character(),
      quantity    = integer(),
      unit_price  = numeric(),
      meta_json   = character(),
      merge_key   = character(),
      stringsAsFactors = FALSE
    )
  }

  rv <- reactiveValues(
    cart = empty_cart_df(),
    buyer_name   = "",
    buyer_email  = "",
    admin_logged_in  = FALSE,
    admin_fail_count = 0L,
    admin_lock_until = as.POSIXct(NA)
  )

  receipt_tx <- reactiveVal(NULL)

  day_date_ui_nonce   <- reactiveVal(0L)
  last_valid_day_date <- reactiveVal(Sys.Date())
  blocked_nonce       <- reactiveVal(0L)
  events_nonce        <- reactiveVal(0L)

  poll_count <- reactiveVal(0L)
  report_state <- reactiveVal(NULL)

  clear_cart <- function() rv$cart <- empty_cart_df()

  cart_total_cents <- function(df) {
    as.integer(round(sum(df$quantity * df$unit_price, na.rm = TRUE) * 100))
  }

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

  # -----------------------------------------------------------------------------
  # CART MERGE / NORMALIZE
  # -----------------------------------------------------------------------------

  sort_list_recursive <- function(x) {
    if (is.list(x) && !is.data.frame(x)) {
      nm <- names(x)
      if (!is.null(nm)) {
        o <- order(nm)
        x <- x[o]
        names(x) <- nm[o]
      }
      x <- lapply(x, sort_list_recursive)
    }
    x
  }

  canonical_meta_json <- function(s) {
    s <- as.character(s %||% "")
    if (!nzchar(s)) return("{}")
    obj <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(obj)) return(s)
    obj <- sort_list_recursive(obj)
    as.character(jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null"))
  }

  cart_merge_key <- function(category, description, unit_price, meta_json) {
    p <- suppressWarnings(as.numeric(unit_price))
    if (is.na(p)) p <- NA_real_
    paste(
      as.character(category %||% ""),
      as.character(description %||% ""),
      sprintf("%.4f", p),
      canonical_meta_json(meta_json),
      sep = "|||"
    )
  }

  normalize_cart <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)

    df$merge_key <- mapply(
      cart_merge_key,
      df$category, df$description, df$unit_price, df$meta_json,
      USE.NAMES = FALSE
    )

    df$quantity <- suppressWarnings(as.integer(df$quantity))
    df <- df[!is.na(df$quantity) & df$quantity > 0L, , drop = FALSE]
    if (nrow(df) == 0) return(df)

    split_idx <- split(seq_len(nrow(df)), df$merge_key)
    out <- lapply(split_idx, function(ix) {
      r <- df[ix[1], , drop = FALSE]
      r$quantity <- sum(df$quantity[ix], na.rm = TRUE)
      r
    })
    out <- do.call(rbind, out)
    rownames(out) <- NULL
    out
  }

  # -----------------------------------------------------------------------------
  # CART QTY UPDATE (and Donation Amount Edit)
  # -----------------------------------------------------------------------------

  set_cart_qty <- function(item_id, new_qty) {
    item_id <- as.character(item_id %||% "")
    if (!nzchar(item_id)) return(invisible(NULL))

    q <- suppressWarnings(as.integer(new_qty))
    if (is.na(q)) return(invisible(NULL))

    df <- rv$cart
    if (is.null(df) || nrow(df) == 0) return(invisible(NULL))

    idx <- which(df$id == item_id)
    if (length(idx) != 1) return(invisible(NULL))

    cat <- as.character(df$category[idx] %||% "")
    if (identical(cat, "donation")) {
      # Donation is amount-editable, not quantity-editable
      return(invisible(NULL))
    }

    max_q <- 20L

    if (q <= 0L) {
      cand <- df[df$id != item_id, , drop = FALSE]
    } else {
      q <- min(q, max_q)
      cand <- df
      cand$quantity[idx] <- q
    }

    cand <- normalize_cart(cand)

    msg <- validate_cart_limits(cand)
    if (!is.null(msg)) {
      showNotification(msg, type = "error")
      return(invisible(NULL))
    }

    rv$cart <- cand
    invisible(NULL)
  }

  set_cart_amount <- function(item_id, new_amt) {
    item_id <- as.character(item_id %||% "")
    if (!nzchar(item_id)) return(invisible(NULL))

    amt <- suppressWarnings(as.numeric(new_amt))
    if (is.na(amt)) return(invisible(NULL))

    df <- rv$cart
    if (is.null(df) || nrow(df) == 0) return(invisible(NULL))

    idx <- which(df$id == item_id)
    if (length(idx) != 1) return(invisible(NULL))

    cat <- as.character(df$category[idx] %||% "")
    if (!identical(cat, "donation")) {
      # Only donation is amount-editable
      return(invisible(NULL))
    }

    if (amt <= 0) {
      cand <- df[df$id != item_id, , drop = FALSE]  # set to 0 => remove line
    } else {
      cand <- df
      cand$quantity[idx]   <- 1L
      cand$unit_price[idx] <- amt
    }

    cand <- normalize_cart(cand)

    msg <- validate_cart_limits(cand)
    if (!is.null(msg)) {
      showNotification(msg, type = "error")
      return(invisible(NULL))
    }

    rv$cart <- cand
    invisible(NULL)
  }

  render_cart_list_ui <- function(df, change_input_id, max_qty = 20L, show_category = FALSE) {
    if (is.null(df) || nrow(df) == 0) {
      return(tags$div(style = "color:#666;", "No items."))
    }

    box_id <- paste0(change_input_id, "_box")

    row_ui <- lapply(seq_len(nrow(df)), function(i) {
      r <- df[i, , drop = FALSE]
      desc  <- as.character(r$description[1] %||% "")
      cat   <- as.character(r$category[1] %||% "")
      qty   <- suppressWarnings(as.integer(r$quantity[1] %||% 0L))
      price <- suppressWarnings(as.numeric(r$unit_price[1] %||% NA_real_))

      if (is.na(qty) || qty < 0L) qty <- 0L

      # Donation: force qty=1, edit amount instead
      if (identical(cat, "donation")) qty <- 1L
      if (!identical(cat, "donation") && qty > max_qty) qty <- max_qty

      line_total <- if (!is.na(price)) qty * price else NA_real_

      left_text <- if (isTRUE(show_category) && nzchar(cat)) {
        tags$div(
          tags$div(desc),
          tags$div(style = "color:#777; font-size: 0.9em;", cat)
        )
      } else {
        tags$div(desc)
      }

      controls <- if (identical(cat, "donation")) {
        tagList(
          tags$span(style = "font-weight:600;", "Amount:"),
          tags$input(
            type          = "text",
            class         = "cart-amt-input",
            `data-itemid` = as.character(r$id[1]),
            value         = if (!is.na(price)) sprintf("%.2f", price) else "",
            placeholder   = "0.00"
          ),
          tags$span(
            class = "cart-line-total",
            if (!is.na(line_total)) sprintf("$%.2f", line_total) else ""
          )
        )
      } else {
        tagList(
          tags$input(
            type          = "number",
            class         = "cart-qty-input",
            `data-itemid` = as.character(r$id[1]),
            value         = qty,
            min           = 0,
            max           = max_qty,
            step          = 1
          ),
          tags$span(
            class = "cart-line-total",
            if (!is.na(line_total)) sprintf("$%.2f", line_total) else ""
          )
        )
      }

      tags$div(
        class = "cart-line",
        tags$div(class = "cart-desc", left_text),
        tags$div(class = "cart-controls", controls)
      )
    })

    tagList(
      tags$div(id = box_id, class = "cart-list", row_ui),
      tags$script(HTML(sprintf(
        "
        // Quantity edits (non-donation)
        $(document).off('input change', '#%s .cart-qty-input');
        $(document).on('input change', '#%s .cart-qty-input', function() {
          var id = $(this).data('itemid');
          var qty = parseInt($(this).val(), 10);
          if (isNaN(qty)) qty = 0;
          Shiny.setInputValue('%s', {id: id, qty: qty, nonce: Math.random()}, {priority: 'event'});
        });

        // Amount edits (donation)
        $(document).off('input change', '#%s .cart-amt-input');
        $(document).on('input change', '#%s .cart-amt-input', function() {
          var id = $(this).data('itemid');
          var amt = $(this).val();
          Shiny.setInputValue('%s', {id: id, amt: amt, nonce: Math.random()}, {priority: 'event'});
        });
        ",
        box_id, box_id, change_input_id,
        box_id, box_id, change_input_id
      )))
    )
  }

  add_rows_to_cart <- function(rows_df) {
    if (is.null(rows_df) || nrow(rows_df) == 0) return(invisible(FALSE))

    df <- rv$cart

    rows_df$merge_key <- mapply(
      cart_merge_key,
      rows_df$category, rows_df$description, rows_df$unit_price, rows_df$meta_json,
      USE.NAMES = FALSE
    )

    if (!is.null(df) && nrow(df) > 0) {
      df$merge_key <- mapply(
        cart_merge_key,
        df$category, df$description, df$unit_price, df$meta_json,
        USE.NAMES = FALSE
      )
    }

    for (i in seq_len(nrow(rows_df))) {
      r <- rows_df[i, , drop = FALSE]
      q <- suppressWarnings(as.integer(r$quantity[1] %||% 0L))
      if (is.na(q) || q <= 0L) next

      # Donation: always quantity 1
      if (identical(as.character(r$category[1] %||% ""), "donation")) q <- 1L

      k <- r$merge_key[1] %||% ""
      hit <- which(df$merge_key == k)

      if (length(hit) >= 1) {
        j <- hit[1]

        if (identical(as.character(df$category[j] %||% ""), "donation")) {
          # If same donation merges, sum the amounts
          df$quantity[j]   <- 1L
          df$unit_price[j] <- as.numeric(df$unit_price[j] %||% 0) + as.numeric(r$unit_price[1] %||% 0)
        } else {
          df$quantity[j] <- as.integer(df$quantity[j] %||% 0L) + q
        }

      } else {
        if (identical(as.character(r$category[1] %||% ""), "donation")) r$quantity[1] <- 1L
        df <- rbind(df, r)
      }
    }

    df <- normalize_cart(df)

    msg <- validate_cart_limits(df)
    if (!is.null(msg)) {
      showNotification(msg, type = "error")
      return(invisible(FALSE))
    }

    rv$cart <- df
    showNotification("Added to cart.", type = "message")
    invisible(TRUE)
  }

  add_to_cart <- function(category, description, quantity, unit_price, meta = list()) {
    cat <- as.character(category %||% "")
    q <- as.integer(quantity %||% 0)
    p <- as.numeric(unit_price %||% NA_real_)

    if (cat != "donation" && q <= 0) return(invisible(FALSE))

    if (is.na(p)) {
      showNotification("Price is N/A. Admin must set prices first.", type = "error")
      return(invisible(FALSE))
    }
    if (p < 0) return(invisible(FALSE))

    # Donation behaves as a single editable amount line
    if (identical(cat, "donation")) q <- 1L
    q <- min(q, 20L)

    rows_df <- data.frame(
      id          = UUIDgenerate(),
      category    = cat,
      description = description,
      quantity    = q,
      unit_price  = p,
      meta_json   = as.character(jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null")),
      merge_key   = "",
      stringsAsFactors = FALSE
    )

    add_rows_to_cart(rows_df)
  }

  # -----------------------------------------------------------------------------
  # SPECIAL EVENT CAPACITY ENFORCEMENT
  # -----------------------------------------------------------------------------

  parse_event_id_from_meta <- function(meta_json) {
    s <- as.character(meta_json %||% "")
    if (!nzchar(s)) return("")
    obj <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(obj)) return("")
    as.character(obj$event_id %||% "")
  }

  cart_event_qty_in_session <- function(event_id) {
    if (!nzchar(event_id %||% "")) return(0L)
    df <- rv$cart
    if (is.null(df) || nrow(df) == 0) return(0L)

    ix <- which(df$category == "event")
    if (length(ix) == 0) return(0L)

    q <- 0L
    for (i in ix) {
      eid <- parse_event_id_from_meta(df$meta_json[i] %||% "")
      if (identical(eid, event_id)) {
        q <- q + as.integer(df$quantity[i] %||% 0L)
      }
    }
    as.integer(q)
  }

  event_sold_qty_completed <- function(event_id) {
    if (!nzchar(event_id %||% "")) return(0L)

    tx <- db_get1(
      "SELECT cart_json
       FROM transactions
       WHERE status IN ('COMPLETED','SANDBOX_TEST_OK')"
    )
    if (nrow(tx) == 0) return(0L)

    sold <- 0L
    for (i in seq_len(nrow(tx))) {
      cart_df <- tryCatch(jsonlite::fromJSON(tx$cart_json[i] %||% ""), error = function(e) NULL)
      if (is.null(cart_df) || nrow(cart_df) == 0) next
      if (!all(c("category","quantity","meta_json") %in% names(cart_df))) next

      ix <- which(cart_df$category == "event")
      if (length(ix) == 0) next

      for (j in ix) {
        eid <- parse_event_id_from_meta(cart_df$meta_json[j] %||% "")
        if (identical(eid, event_id)) {
          sold <- sold + as.integer(cart_df$quantity[j] %||% 0L)
        }
      }
    }
    as.integer(sold)
  }

  validate_event_capacities_for_cart <- function(cart_df) {
    if (is.null(cart_df) || nrow(cart_df) == 0) return(NULL)

    ix <- which(cart_df$category == "event")
    if (length(ix) == 0) return(NULL)

    # aggregate requested qty by event_id
    req_by_event <- list()
    for (i in ix) {
      eid <- parse_event_id_from_meta(cart_df$meta_json[i] %||% "")
      if (!nzchar(eid)) next
      req_by_event[[eid]] <- (req_by_event[[eid]] %||% 0L) + as.integer(cart_df$quantity[i] %||% 0L)
    }
    if (length(req_by_event) == 0) return(NULL)

    for (eid in names(req_by_event)) {
      cap_row <- db_get1("SELECT capacity, name, event_date FROM special_events WHERE id = ?id LIMIT 1", id = eid)
      if (nrow(cap_row) != 1) next

      cap <- suppressWarnings(as.integer(cap_row$capacity[1]))
      if (is.na(cap)) next  # no capacity = unlimited

      sold <- event_sold_qty_completed(eid)
      remaining <- cap - sold

      requested <- as.integer(req_by_event[[eid]] %||% 0L)

      if (remaining <= 0L) {
        return(paste0("Event is sold out: ", cap_row$name[1], " (", cap_row$event_date[1], ")."))
      }
      if (requested > remaining) {
        return(paste0(
          "Not enough remaining capacity for event: ",
          cap_row$name[1], " (", cap_row$event_date[1], "). ",
          "Remaining: ", remaining, ", requested: ", requested, "."
        ))
      }
    }

    NULL
  }

  # -----------------------------------------------------------------------------
  # REACTIVE DATA SOURCES
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
  # CHRISTMAS PASS: constrain dateInput (only when tab visible)
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

    tabs <- list()

    if (tab_on("tab_daypass_enabled", TRUE)) {
      tabs <- c(tabs, list(
        tabPanel(
          title = "Day Pass",
          value = "Day Pass",
          fluidPage(
            h3("Day Passes"),
            tags$p(season_label(season_win())),
            p("Choose your ski day and passes, then add to cart. Review and pay on the right."),
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
              column(8, checkout_panel_ui("day", "Checkout"))
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
            p("Choose a 14-day window that must include Dec 25. Add to cart, then pay on the right."),
            fluidRow(
              column(
                4,
                dateInput("xmas_start", "Start date (14-day window)", value = Sys.Date()),
                numericInput("xmas_qty", "Number of passes", value = 0, min = 0, step = 1),
                br(),
                actionButton("xmas_add_to_cart", "Add to cart")
              ),
              column(8, checkout_panel_ui("xmas", "Checkout"))
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
                numericInput("season_adult", "Adult", value = 0, min = 0, step = 1),
                numericInput("season_youth", "Youth", value = 0, min = 0, step = 1),
                br(),
                actionButton("season_add_to_cart", "Add to cart")
              ),
              column(8, checkout_panel_ui("season", "Checkout"))
            )
          )
        )
      ))
    }

    if (tab_on("tab_programs_enabled", TRUE)) {
      prog <- get_program_list()
      tabs <- c(tabs, list(
        tabPanel(
          title = "Programs",
          value = "Programs",
          fluidPage(
            h3("Programs"),
            p("Select a program and number of participants, then add to cart. Review and pay on the right."),
            fluidRow(
              column(
                4,
                selectInput(
                  "program_choice", "Program",
                  choices = setNames(prog$id, prog$name)
                ),
                numericInput("program_qty", "Number of participants", value = 0, min = 0, step = 1),
                br(),
                actionButton("program_add_to_cart", "Add to cart")
              ),
              column(8, checkout_panel_ui("prog", "Checkout"))
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
            p("Add one or more special event registrations to the cart. Review and pay on the right."),
            fluidRow(
              column(
                4,
                uiOutput("event_picker_ui"),
                numericInput("event_qty", "Number of participants", value = 0, min = 0, step = 1),
                br(),
                actionButton("event_add_to_cart", "Add to cart")
              ),
              column(8, checkout_panel_ui("event", "Checkout"))
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
            p("Add a donation to the cart and pay on the right."),
            tags$div(
              style = "margin-top: 8px; padding: 10px; border: 1px solid #ddd; border-radius: 6px; background: #fafafa;",
              tags$strong("Important: "),
              "The club is not a registered charity at this time. Donations are not tax-deductible."
            ),
            br(),
            fluidRow(
                           column(
                4,
                numericInput(
                  "donation_amount",
                  "Donation amount (CAD)",
                  value = 0,
                  min   = 0,
                  step  = 10
                ),
                br(),
                actionButton("donate_add_to_cart", "Add donation to cart")
              ),
              column(8, checkout_panel_ui("don", "Checkout"))
            )
          )
        )
      ))
    }

    # Receipt always present (nav item hidden until we show it)
    tabs <- c(tabs, list(
      tabPanel(
        title = "Receipt",
        value = "Receipt",
        fluidPage(
          h3("Payment receipt"),
          uiOutput("receipt_panel")
        )
      )
    ))

    tabs <- c(tabs, list(
      tabPanel(
        title = "Admin",
        value = "Admin",
        fluidPage(
          h3("Admin"),
          tags$p(APP_VERSION),
          tags$p(ENV_LABEL),
          if (db_is_postgres()) {
            tags$div(
              style="margin:8px 0; padding:10px; border:1px solid #ddd; border-radius:6px; background:#fafafa;",
              tags$strong("Database: "), "Postgres (BVXC_DB_URL set)"
            )
          } else {
            tags$div(
              style="margin:8px 0; padding:10px; border:1px solid #ffc107; border-radius:6px; background:#fff8e1;",
              tags$strong("Database: "), "SQLite fallback (dev only). Do not use this on Connect Cloud for persistence."
            )
          },
          if (!HAVE_SQUARE_CREDS && SANDBOX_MODE == "square") {
            tags$div(
              style="margin:8px 0; padding:10px; border:1px solid #dc3545; border-radius:6px; background:#fff5f5;",
              tags$strong("Square: "),
              "SANDBOX_MODE is 'square' but Square credentials are missing. Falling back to fake mode."
            )
          } else NULL,
          hr(),
          passwordInput("admin_password", "Admin password"),
          actionButton("admin_login", "Log in"),
          br(), br(),
          uiOutput("admin_content")
        )
      )
    ))

    do.call(navbarPage, c(list(title = "BVXC", id = "main_nav"), tabs))
  })

  # Hide receipt tab in navbar by default (we show it only after a receipt exists)
  session$onFlushed(function() {
    session$sendCustomMessage("toggleReceiptNav", list(show = FALSE))
  }, once = TRUE)

  observeEvent(receipt_tx(), {
    session$sendCustomMessage("toggleReceiptNav", list(show = !is.null(receipt_tx())))
  }, ignoreInit = TRUE)

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
  # PER-TAB CHECKOUT WIRING (cart shown on every tab)
  # -----------------------------------------------------------------------------

  checkout_prefixes <- c("day","xmas","season","prog","event","don")

  for (p in checkout_prefixes) {
    local({
      prefix    <- p
      change_id <- paste0(prefix, "_cart_qty_change")

      output[[paste0(prefix, "_cart_list")]] <- renderUI({
        df <- rv$cart
        render_cart_list_ui(
          df,
          change_input_id = change_id,
          max_qty         = 20L,
          show_category   = TRUE
        )
      })

      output[[paste0(prefix, "_cart_total")]] <- renderText({
        df <- rv$cart
        if (is.null(df) || nrow(df) == 0) return("Cart is empty.")
        tot <- sum(df$quantity * df$unit_price, na.rm = TRUE)
        paste0("Total: $", sprintf("%.2f", tot))
      })

      output[[paste0(prefix, "_cart_pay_ui")]] <- renderUI({
        has_items <- !is.null(rv$cart) && nrow(rv$cart) > 0
        actionButton(
          inputId = paste0(prefix, "_cart_pay"),
          label   = "Pay now",
          class   = paste("btn", if (has_items) "pay-now-hot" else ""),
          disabled = if (!has_items) "disabled" else NULL
        )
      })

      observeEvent(input[[change_id]], {
        x <- input[[change_id]]

        if (!is.null(x$qty)) {
          set_cart_qty(x$id, x$qty)
        } else if (!is.null(x$amt)) {
          set_cart_amount(x$id, x$amt)
        }
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0(prefix, "_buyer_name")]], {
        rv$buyer_name <- trimws(input[[paste0(prefix, "_buyer_name")]] %||% "")
        for (other in setdiff(checkout_prefixes, prefix)) {
          try(updateTextInput(session, paste0(other, "_buyer_name"), value = rv$buyer_name), silent = TRUE)
        }
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0(prefix, "_buyer_email")]], {
        rv$buyer_email <- trimws(input[[paste0(prefix, "_buyer_email")]] %||% "")
        for (other in setdiff(checkout_prefixes, prefix)) {
          try(updateTextInput(session, paste0(other, "_buyer_email"), value = rv$buyer_email), silent = TRUE)
        }
      }, ignoreInit = TRUE)
    })
  }

  # -----------------------------------------------------------------------------
  # CHECKOUT (single path used by all Pay buttons)
  # -----------------------------------------------------------------------------

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

  do_checkout <- function(source = "tab") {
    df <- rv$cart
    if (is.null(df) || nrow(df) == 0) {
      showNotification("Cart is empty.", type = "warning")
      return()
    }

    buyer_name  <- trimws(rv$buyer_name %||% "")
    buyer_email <- trimws(rv$buyer_email %||% "")

    if (!nzchar(buyer_email)) {
      showNotification("Please enter an email for the receipt.", type = "warning")
      return()
    }

    df <- normalize_cart(df)

    msg <- validate_cart_limits(df)
    if (!is.null(msg)) {
      showNotification(msg, type = "error")
      return()
    }

    cap_msg <- validate_event_capacities_for_cart(df)
    if (!is.null(cap_msg)) {
      showNotification(cap_msg, type = "error")
      return()
    }

    total_cents <- cart_total_cents(df)

    # Sandbox fake mode
    if (SQUARE_ENV == "sandbox" && SANDBOX_MODE == "fake") {
      receipt_token <- UUIDgenerate()

      ok <- tryCatch({
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
          cart_json     = as.character(jsonlite::toJSON(df, auto_unbox = TRUE, null = "null")),
          receipt_token = receipt_token
        )
        TRUE
      }, error = function(e) {
        showNotification(paste("DB error saving sandbox transaction:", conditionMessage(e)), type = "error")
        FALSE
      })

      if (!ok) return()

      receipt_tx(load_receipt_token(receipt_token))
      poll_count(0L)
      clear_cart()
      updateTabsetPanel(session, "main_nav", selected = "Receipt")

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
      note         = paste("BVXC", if (SQUARE_ENV == "sandbox") "sandbox" else "production", source, "checkout"),
      redirect_url = redirect_url
    )
    if (is.null(res) || is.null(res$checkout_url)) {
      showNotification("Error creating Square checkout.", type = "error")
      return()
    }

    ok <- tryCatch({
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
        cart_json     = as.character(jsonlite::toJSON(df, auto_unbox = TRUE, null = "null")),
        checkout_id   = res$checkout_id %||% NA_character_,
        order_id      = res$square_order %||% NA_character_,
        receipt_token = receipt_token,
        status        = if (SQUARE_ENV == "sandbox") "PENDING_SANDBOX" else "PENDING"
      )
      TRUE
    }, error = function(e) {
      showNotification(paste("DB error saving transaction:", conditionMessage(e)), type = "error")
      FALSE
    })

    if (!ok) return()

    session$sendCustomMessage("redirect", list(url = res$checkout_url))
    clear_cart()
  }

  for (p in checkout_prefixes) {
    local({
      prefix <- p

      observeEvent(input[[paste0(prefix, "_cart_clear")]], {
        clear_cart()
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0(prefix, "_cart_pay")]], {
        rv$buyer_name  <- trimws(input[[paste0(prefix, "_buyer_name")]]  %||% rv$buyer_name %||% "")
        rv$buyer_email <- trimws(input[[paste0(prefix, "_buyer_email")]] %||% rv$buyer_email %||% "")
        do_checkout(source = prefix)
      }, ignoreInit = TRUE)
    })
  }

  # -----------------------------------------------------------------------------
  # DAY PASS
  # -----------------------------------------------------------------------------

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

    rows <- list()

    add_row <- function(type, qty, price) {
      if (qty <= 0) return()
      p <- suppressWarnings(as.numeric(price))
      if (is.na(p)) {
        showNotification("Price is N/A. Admin must set prices first.", type = "error")
        rows <<- NULL
        return()
      }
      rows[[length(rows) + 1L]] <<- data.frame(
        id          = UUIDgenerate(),
        category    = "day_pass",
        description = paste("Day pass –", type, "–", as.character(d)),
        quantity    = as.integer(min(qty, 20L)),
        unit_price  = p,
        meta_json   = as.character(jsonlite::toJSON(list(type = type, date = as.character(d)), auto_unbox = TRUE, null = "null")),
        merge_key   = "",
        stringsAsFactors = FALSE
      )
    }

    add_row("Adult",   qa, pr[["Adult"]]);   if (is.null(rows)) return()
    add_row("Youth",   qy, pr[["Youth"]]);   if (is.null(rows)) return()
    add_row("Under 9", qu, pr[["Under 9"]]); if (is.null(rows)) return()
    add_row("Family",  qf, pr[["Family"]]);  if (is.null(rows)) return()

    if (length(rows) == 0) {
      showNotification("Nothing to add.", type = "warning")
      return()
    }

    add_rows_to_cart(do.call(rbind, rows))
  })

  # -----------------------------------------------------------------------------
  # CHRISTMAS PASS
  # -----------------------------------------------------------------------------

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

  observeEvent(input$season_add_to_cart, {
    cutoff <- get_early_bird_cutoff()
    is_eb  <- if (is.na(cutoff)) FALSE else (Sys.Date() <= cutoff)

    prices <- get_season_prices(is_eb)
    pr <- setNames(prices$price, prices$type)

    qa <- qty_int(input$season_adult, "Adult")
    qy <- qty_int(input$season_youth, "Youth")

    rows <- list()

    add_row <- function(type, qty, price) {
      if (qty <= 0) return(TRUE)

      p <- suppressWarnings(as.numeric(price))
      if (is.na(p) || p < 0) {
        showNotification("Price is N/A. Admin must set prices first.", type = "error")
        return(FALSE)
      }

      rows[[length(rows) + 1L]] <<- data.frame(
        id          = UUIDgenerate(),
        category    = "season_pass",
        description = paste("Season pass –", type),
        quantity    = as.integer(min(qty, 20L)),
        unit_price  = p,
        meta_json   = as.character(jsonlite::toJSON(list(type = type, early_bird = is_eb), auto_unbox = TRUE, null = "null")),
        merge_key   = "",
        stringsAsFactors = FALSE
      )
      TRUE
    }

    ok <- add_row("Adult", qa, pr[["Adult"]]); if (!ok) return()
    ok <- add_row("Youth", qy, pr[["Youth"]]); if (!ok) return()

    if (length(rows) == 0) {
      showNotification("Nothing to add.", type = "warning")
      return()
    }

    add_rows_to_cart(do.call(rbind, rows))
  })

  # -----------------------------------------------------------------------------
  # PROGRAMS
  # -----------------------------------------------------------------------------

  observeEvent(input$program_add_to_cart, {
    programs <- get_program_list()
    id  <- input$program_choice %||% ""
    qty <- qty_int(input$program_qty, "Participants")
    row <- programs[programs$id == id, , drop = FALSE]
    if (nrow(row) == 0 || qty <= 0) return()

    add_to_cart(
      "program",
      paste("Program –", row$name[1]),
      qty,
      row$price[1],
      list(program_id = row$id[1], program_name = row$name[1])
    )
  })

  # -----------------------------------------------------------------------------
  # SPECIAL EVENTS
  # -----------------------------------------------------------------------------

  output$event_picker_ui <- renderUI({
    events_nonce()
    ev <- get_special_events(enabled_only = TRUE)
    if (nrow(ev) == 0) return(tags$div(style="color:#666;", "No events available right now."))
    choices <- setNames(ev$id, paste0(ev$name, " (", ev$event_date, ")"))
    selectInput("event_choice", "Event", choices = choices)
  })

  observeEvent(input$event_add_to_cart, {
    events_nonce()
    ev <- get_special_events(enabled_only = TRUE)

    id  <- input$event_choice %||% ""
    qty <- qty_int(input$event_qty, "Participants")
    if (!nzchar(id) || qty <= 0 || nrow(ev) == 0) return()

    row <- ev[ev$id == id, , drop = FALSE]
    if (nrow(row) == 0) return()

    cap <- suppressWarnings(as.integer(row$capacity[1]))
    if (!is.na(cap)) {
      sold    <- event_sold_qty_completed(id)
      in_cart <- cart_event_qty_in_session(id)
      remaining <- cap - sold - in_cart

      if (remaining <= 0L) {
        showNotification("This event is sold out.", type = "error")
        return()
      }
      if (qty > remaining) {
        showNotification(paste0("Only ", remaining, " spots remaining. Adjusting quantity."), type = "warning")
        qty <- remaining
      }
    }

    add_to_cart(
      category    = "event",
      description = paste0("Event – ", row$name[1], " (", row$event_date[1], ")"),
      quantity    = qty,
      unit_price  = row$price_cad[1],
      meta        = list(event_id = row$id[1], event_name = row$name[1], event_date = row$event_date[1])
    )
  })

  # -----------------------------------------------------------------------------
  # DONATION
  # -----------------------------------------------------------------------------

  observeEvent(input$donate_add_to_cart, {
    amt <- suppressWarnings(as.numeric(input$donation_amount %||% 0))

    if (is.na(amt) || amt <= 0) {
      showNotification("Please enter a donation amount greater than zero.", type = "error")
      return()
    }

    add_to_cart(
      category    = "donation",
      description = "Donation – Bulkley Valley Cross Country Ski Club",
      quantity    = 1,
      unit_price  = amt,
      meta        = list()  # buyer name/email captured at checkout
    )

    showNotification(paste0("Donation of $", sprintf("%.2f", amt), " added to cart."), type = "message")
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
      for (t in tenders) {
        pid <- t$payment_id %||% t$paymentId %||% NULL
        if (nzchar(pid %||% "")) { payment_id <- pid; break }
      }
    }

    if (!nzchar(payment_id %||% "")) {
      pid2 <- order$payment_id %||% order$paymentId %||% NULL
      if (nzchar(pid2 %||% "")) payment_id <- pid2
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
        poll_count(0L)
        updateTabsetPanel(session, "main_nav", selected = "Receipt")
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
      poll_count(0L)
      updateTabsetPanel(session, "main_nav", selected = "Receipt")
    }
  }, ignoreInit = TRUE)

  observe({
    tx <- receipt_tx()
    if (is.null(tx)) return()

    st <- toupper(tx$status[1] %||% "")
    if (!st %in% c("PENDING", "PENDING_SANDBOX")) return()

    token <- tx$receipt_token[1] %||% ""
    if (!nzchar(token)) return()
    if (poll_count() >= 60L) return()

    invalidateLater(4000, session)
    poll_count(poll_count() + 1L)

    updated <- refresh_tx_status_from_square(token)
    if (!is.null(updated)) receipt_tx(updated)
  })

  cart_from_tx <- function(tx) {
    cj <- tx$cart_json[1] %||% ""
    df <- tryCatch(jsonlite::fromJSON(cj), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    if (!all(c("description","quantity","unit_price") %in% names(df))) return(NULL)
    df$line_total <- df$quantity * df$unit_price
    df[, c("description","quantity","unit_price","line_total")]
  }

  output$receipt_panel <- renderUI({
    tx <- receipt_tx()

    if (is.null(tx)) {
      return(tags$div(
        style="padding:10px; border:1px solid #ddd; border-radius:8px; background:#fafafa; color:#555;",
        "No receipt loaded yet. After payment, you will be redirected here automatically."
      ))
    }

    total   <- (tx$total_amount_cents[1] %||% 0) / 100
    name    <- tx$buyer_name[1] %||% ""
    email   <- tx$buyer_email[1] %||% ""
    status  <- toupper(tx$status[1] %||% "")
    created <- tx$created_at[1] %||% ""
    cur     <- toupper(tx$currency[1] %||% "CAD")

    status_class <- if (status %in% c("COMPLETED", "SANDBOX_TEST_OK")) {
      "receipt-ok"
    } else if (status %in% c("PENDING", "PENDING_SANDBOX")) {
      "receipt-pending"
    } else if (status %in% c("FAILED", "CANCELED", "AMOUNT_MISMATCH")) {
      "receipt-bad"
    } else {
      "receipt-neutral"
    }

    title <- switch(status,
      "COMPLETED"        = "\u2705 Payment complete",
      "SANDBOX_TEST_OK"  = "\u2705 Test payment recorded",
      "PENDING"          = "\u23F3 Payment pending",
      "PENDING_SANDBOX"  = "\u23F3 Payment pending (sandbox)",
      "FAILED"           = "\u274C Payment failed",
      "CANCELED"         = "\u274C Payment canceled",
      "AMOUNT_MISMATCH"  = "\u26A0\uFE0F Payment amount mismatch",
      paste0("Payment status: ", status)
    )

    sub <- switch(status,
      "COMPLETED"        = "Thank you for supporting Bulkley Valley Cross Country Ski Club.",
      "SANDBOX_TEST_OK"  = "Sandbox fake-mode transaction. No real payment was processed.",
      "PENDING"          = "Please wait while we confirm payment with Square. This page updates automatically.",
      "PENDING_SANDBOX"  = "Please wait while we confirm payment with Square (sandbox). This page updates automatically.",
      "FAILED"           = "Square reported FAILED. Please try again.",
      "CANCELED"         = "Square reported CANCELED. No payment was taken.",
      "AMOUNT_MISMATCH"  = "Completed, but amount/currency mismatch. Contact the club.",
      "Status recorded."
    )

    pending_ui <- NULL
    if (status %in% c("PENDING", "PENDING_SANDBOX")) {
      pending_ui <- tags$div(
        style="margin-top:10px;",
        tags$span(class="receipt-spinner"),
        tags$span(style="margin-left:10px; font-weight:600;", "Checking Square… this page updates automatically.")
      )
    }

    items <- cart_from_tx(tx)

    tagList(
      tags$div(
        class = paste("receipt-card", status_class),
        tags$div(class = "receipt-title", title),
        tags$div(class = "receipt-sub", sub),
        pending_ui
      ),
      br(),
      h4("Details"),
      tags$div(
        style="padding:10px; border:1px solid #ddd; border-radius:8px; background:#fafafa;",
        tags$div(tags$strong("Date/time: "), created),
        tags$div(tags$strong("Name: "),  if (nzchar(name)) name else "N/A"),
        tags$div(tags$strong("Email: "), if (nzchar(email)) email else "N/A"),
        tags$div(tags$strong("Amount: "), sprintf("$%.2f %s", total, cur)),
        tags$div(tags$strong("Status: "), status),
        tags$div(tags$strong("Receipt token: "), tx$receipt_token[1] %||% "")
      ),
      if (!is.null(items)) tagList(
        br(),
        h4("Items purchased"),
        tableOutput("receipt_items")
      ) else NULL,
      br(),
      h4("Receipt QR code"),
      plotOutput("receipt_qr", height = "260px", width = "260px")
    )
  })

  output$receipt_items <- renderTable({
    tx <- receipt_tx()
    if (is.null(tx)) return(NULL)
    cart_from_tx(tx)
  }, digits = 2)

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

  output$adm_program_prices_ui <- renderUI({
    cat <- program_catalog()
    inputs <- lapply(seq_len(nrow(cat)), function(i) {
      rid <- cat$id[i]
      nm  <- cat$name[i]
      key <- cat$price_key[i]
      input_id <- paste0("adm_prog_", rid)
      column(4, textInput(input_id, nm, value = cfg_get(key, "")))
    })

    rows <- list()
    for (i in seq(1, length(inputs), by = 3)) {
      rows[[length(rows) + 1L]] <- fluidRow(inputs[i:min(i+2, length(inputs))])
    }
    do.call(tagList, rows)
  })

  output$admin_content <- renderUI({
    if (!rv$admin_logged_in) return(p("Please log in to see admin options."))

    eb_val <- cfg_date("early_bird_cutoff", as.Date(NA))
    eb_val <- if (is.na(eb_val)) NULL else eb_val

    today <- Sys.Date()
    def_start <- today - 30
    def_end   <- today

    tabsetPanel(
      id = "admin_subtabs",
      tabPanel(
        "Settings",
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
          uiOutput("adm_program_prices_ui"),
          br(),
          actionButton("admin_save_prices", "Save prices"),
          hr(),

          h4("Special events (create / edit / enable / delete)"),
          fluidRow(
            column(
              6,
              uiOutput("admin_event_select_ui"),
              br(),
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
              p("Tip: select an event above to load it, then update/delete.", style="color:#666;")
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
          )
        )
      ),
      tabPanel(
        "Reports",
        tagList(
          h4("Reports"),
          p("Generate a summary and export CSV for a date range. Uses transaction created_at timestamps."),
          fluidRow(
            column(6, dateRangeInput("adm_report_range", "Date range", start = def_start, end = def_end)),
            column(6,
              br(),
              actionButton("admin_run_report", "Run report"),
              br(), br(),
              downloadButton("admin_dl_tx_csv", "Download transactions CSV"),
              downloadButton("admin_dl_items_csv", "Download line-items CSV")
            )
          ),
          br(),
          h5("Report summary"),
          tableOutput("admin_report_summary"),
          br(),
          h5("Line items (aggregated)"),
          tableOutput("admin_report_items"),
          hr(),
          h4("Recent transactions (last 10)"),
          tableOutput("admin_recent_tx")
        )
      )
    )
  })

  observeEvent(input$admin_save_toggles_limits, {
    req(rv$admin_logged_in)

    eb <- suppressWarnings(as.Date(input$adm_early_bird_cutoff))
    cfg_set("early_bird_cutoff", if (is.na(eb)) "" else as.character(eb))

    cfg_set("limit_max_total_cad",   input$adm_limit_max_total %||% "")
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

    cat <- program_catalog()
    for (i in seq_len(nrow(cat))) {
      rid <- cat$id[i]
      key <- cat$price_key[i]
      inp <- paste0("adm_prog_", rid)
      cfg_set(key, input[[inp]] %||% "")
    }

    showNotification("Prices saved.", type = "message")
  })

  # -----------------------------------------------------------------------------
  # REPORTS
  # -----------------------------------------------------------------------------

  parse_cart_json_to_df <- function(cart_json) {
    if (!nzchar(cart_json %||% "")) return(NULL)
    out <- tryCatch(jsonlite::fromJSON(cart_json), error = function(e) NULL)
    if (is.null(out)) return(NULL)
    if (!all(c("category","description","quantity","unit_price") %in% names(out))) return(NULL)
    out
  }

  run_report <- function(start_date, end_date) {
    start_date <- suppressWarnings(as.Date(start_date))
    end_date   <- suppressWarnings(as.Date(end_date))
    if (is.na(start_date) || is.na(end_date) || end_date < start_date) return(NULL)

    start_ts <- paste0(format(start_date), " 00:00:00")
    end_ts   <- paste0(format(end_date),   " 23:59:59")

    tx <- db_get1(
      "SELECT id, created_at, buyer_name, buyer_email, total_amount_cents, currency, cart_json, status
       FROM transactions
       WHERE created_at >= ?start_ts AND created_at <= ?end_ts
       ORDER BY created_at DESC",
      start_ts = start_ts,
      end_ts   = end_ts
    )

    items_all <- data.frame(
      tx_id = character(),
      created_at = character(),
      status = character(),
      category = character(),
      description = character(),
      quantity = integer(),
      unit_price = numeric(),
      line_total = numeric(),
      stringsAsFactors = FALSE
    )

    if (nrow(tx) > 0) {
      for (i in seq_len(nrow(tx))) {
        cart_df <- parse_cart_json_to_df(tx$cart_json[i] %||% "")
        if (is.null(cart_df) || nrow(cart_df) == 0) next

        tmp <- data.frame(
          tx_id       = tx$id[i],
          created_at  = tx$created_at[i],
          status      = tx$status[i],
          category    = as.character(cart_df$category),
          description = as.character(cart_df$description),
          quantity    = as.integer(cart_df$quantity),
          unit_price  = as.numeric(cart_df$unit_price),
          stringsAsFactors = FALSE
        )
        tmp$line_total <- tmp$quantity * tmp$unit_price
        items_all <- rbind(items_all, tmp)
      }
    }

n_tx <- nrow(tx)

# Totals
total_all_cad <- if (n_tx == 0) 0 else sum(tx$total_amount_cents, na.rm = TRUE) / 100

completed_statuses <- c("COMPLETED", "SANDBOX_TEST_OK")
tx_completed <- if (n_tx == 0) tx[0, , drop = FALSE] else tx[tx$status %in% completed_statuses, , drop = FALSE]
total_completed_cad <- if (nrow(tx_completed) == 0) 0 else sum(tx_completed$total_amount_cents, na.rm = TRUE) / 100

# Status breakdown
by_status <- if (n_tx == 0) {
  data.frame(status = character(), n = integer(), stringsAsFactors = FALSE)
} else {
  as.data.frame(table(tx$status), stringsAsFactors = FALSE)
}
names(by_status) <- c("status","n")

    items_agg <- data.frame()
    if (nrow(items_all) > 0) {
      key <- paste(items_all$category, items_all$description, sep="|||")
      agg_qty <- tapply(items_all$quantity, key, sum, na.rm = TRUE)
      agg_rev <- tapply(items_all$line_total, key, sum, na.rm = TRUE)

      parts <- strsplit(names(agg_qty), "\\|\\|\\|", perl = TRUE)
      items_agg <- data.frame(
        category = vapply(parts, `[[`, character(1), 1),
        description = vapply(parts, `[[`, character(1), 2),
        quantity = as.integer(agg_qty),
        revenue = as.numeric(agg_rev),
        stringsAsFactors = FALSE
      )
      items_agg <- items_agg[order(items_agg$revenue, decreasing = TRUE), , drop = FALSE]
      rownames(items_agg) <- NULL
    }

summary_tbl <- data.frame(
  metric = c("Start date","End date","Transactions","Total completed (CAD)","Total all statuses (CAD)"),
  value  = c(
    as.character(start_date),
    as.character(end_date),
    as.character(n_tx),
    sprintf("$%.2f", total_completed_cad),
    sprintf("$%.2f", total_all_cad)
  ),
  stringsAsFactors = FALSE
)

    list(
      tx = tx,
      items_all = items_all,
      items_agg = items_agg,
      summary_tbl = summary_tbl,
      by_status = by_status
    )
  }

  observeEvent(input$admin_run_report, {
    req(rv$admin_logged_in)
    dr <- input$adm_report_range
    if (is.null(dr) || length(dr) != 2) {
      showNotification("Select a valid report date range.", type="error")
      return()
    }
    rep <- run_report(dr[1], dr[2])
    if (is.null(rep)) {
      showNotification("Report failed. Check date range.", type="error")
      return()
    }
    report_state(rep)
    showNotification("Report generated.", type="message")
  })

  output$admin_report_summary <- renderTable({
    if (!rv$admin_logged_in) return(NULL)
    rep <- report_state()
    if (is.null(rep)) return(NULL)

    out <- rep$summary_tbl
    if (nrow(rep$by_status) > 0) {
      out <- rbind(out, data.frame(metric="---", value="---", stringsAsFactors=FALSE))
      for (i in seq_len(nrow(rep$by_status))) {
        out <- rbind(out, data.frame(
          metric = paste0("Status: ", rep$by_status$status[i]),
          value  = as.character(rep$by_status$n[i]),
          stringsAsFactors = FALSE
        ))
      }
    }
    out
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$admin_report_items <- renderTable({
    if (!rv$admin_logged_in) return(NULL)
    rep <- report_state()
    if (is.null(rep)) return(NULL)
    if (nrow(rep$items_agg) == 0) return(NULL)
    rep$items_agg
  }, digits = 2)

  output$admin_dl_tx_csv <- downloadHandler(
    filename = function() {
      dr <- input$adm_report_range
      if (is.null(dr) || length(dr) != 2) return("transactions.csv")
      paste0("transactions_", dr[1], "_to_", dr[2], ".csv")
    },
    content = function(file) {
      rep <- report_state()
      if (is.null(rep)) {
        write.csv(data.frame(), file, row.names = FALSE)
        return()
      }
      tx <- rep$tx
      if (nrow(tx) > 0) tx$total_amount_cad <- tx$total_amount_cents / 100
      write.csv(tx, file, row.names = FALSE)
    }
  )

  output$admin_dl_items_csv <- downloadHandler(
    filename = function() {
      dr <- input$adm_report_range
      if (is.null(dr) || length(dr) != 2) return("line_items.csv")
      paste0("line_items_", dr[1], "_to_", dr[2], ".csv")
    },
    content = function(file) {
      rep <- report_state()
      if (is.null(rep)) {
        write.csv(data.frame(), file, row.names = FALSE)
        return()
      }
      write.csv(rep$items_all, file, row.names = FALSE)
    }
  )

  # ---- Admin event CRUD ------------------------------------------------------

  output$admin_event_select_ui <- renderUI({
    if (!rv$admin_logged_in) return(NULL)

    events_nonce()
    ev <- get_special_events(enabled_only = FALSE)

    if (nrow(ev) == 0) {
      return(tags$div(style="color:#666;", "No events yet. Create one below."))
    }

    choices <- setNames(ev$id, paste0(ev$name, " (", ev$event_date, ")"))
    selectInput("adm_ev_selected_id", "Select existing event (for update/delete)", choices = choices)
  })

  observeEvent(input$adm_ev_selected_id, {
    req(rv$admin_logged_in)

    id <- input$adm_ev_selected_id %||% ""
    if (!nzchar(id)) return()

    ev <- get_special_events(enabled_only = FALSE)
    row <- ev[ev$id == id, , drop = FALSE]
    if (nrow(row) != 1) return()

    updateTextInput(session, "adm_ev_name",  value = row$name[1] %||% "")
    updateDateInput(session, "adm_ev_date",  value = suppressWarnings(as.Date(row$event_date[1])))
    updateTextInput(session, "adm_ev_price", value = ifelse(is.na(row$price_cad[1]), "", as.character(row$price_cad[1])))
    updateTextInput(session, "adm_ev_cap",   value = ifelse(is.na(row$capacity[1]),  "", as.character(row$capacity[1])))
    updateCheckboxInput(session, "adm_ev_enabled", value = isTRUE(as.integer(row$enabled[1] %||% 0) == 1L))
  }, ignoreInit = TRUE)

  output$admin_events_table <- renderTable({
    if (!rv$admin_logged_in) return(NULL)
    ev <- get_special_events(enabled_only = FALSE)
    if (nrow(ev) == 0) return(NULL)
    ev[, c("id","name","event_date","price_cad","capacity","enabled")]
  }, digits = 2)

  observeEvent(input$admin_ev_create, {
    req(rv$admin_logged_in)

    name <- trimws(input$adm_ev_name %||% "")
    d    <- suppressWarnings(as.Date(input$adm_ev_date))

    if (!nzchar(name) || is.na(d)) {
      showNotification("Event name and a valid date are required.", type="error")
      return()
    }

    id <- UUIDgenerate()

    price <- {
      s <- trimws(input$adm_ev_price %||% "")
      if (!nzchar(s) || toupper(s) %in% c("N/A","NA")) NA_real_
      else suppressWarnings(as.numeric(gsub("[\\$,]", "", s)))
    }
    if (!is.na(price) && price < 0) { showNotification("Invalid price.", type="error"); return() }

    cap <- {
      s <- trimws(input$adm_ev_cap %||% "")
      if (!nzchar(s) || toupper(s) %in% c("N/A","NA")) NA_integer_
      else suppressWarnings(as.integer(s))
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

    id <- input$adm_ev_selected_id %||% ""
    if (!nzchar(id)) { showNotification("Select an existing event to update.", type="error"); return() }

    name <- trimws(input$adm_ev_name %||% "")
    d    <- suppressWarnings(as.Date(input$adm_ev_date))
    if (!nzchar(name) || is.na(d)) { showNotification("Name and a valid date are required.", type="error"); return() }

    price <- {
      s <- trimws(input$adm_ev_price %||% "")
      if (!nzchar(s) || toupper(s) %in% c("N/A","NA")) NA_real_
      else suppressWarnings(as.numeric(gsub("[\\$,]", "", s)))
    }
    cap <- {
      s <- trimws(input$adm_ev_cap %||% "")
      if (!nzchar(s) || toupper(s) %in% c("N/A","NA")) NA_integer_
      else suppressWarnings(as.integer(s))
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
    id <- input$adm_ev_selected_id %||% ""
    if (!nzchar(id)) { showNotification("Select an existing event to delete.", type="error"); return() }

    n <- db_exec1("DELETE FROM special_events WHERE id = ?id", id = id)
    if (n == 0) showNotification("No event found with that ID.", type="error")
    else {
      showNotification("Event deleted.", type="message")
      events_nonce(events_nonce() + 1L)
    }
  })

  # ---- Blocked dates ---------------------------------------------------------

  output$admin_blocked_table <- renderTable({
    if (!rv$admin_logged_in) return(NULL)
    get_blocked_dates()
  })

  observeEvent(input$admin_block_add, {
    req(rv$admin_logged_in)
    d <- suppressWarnings(as.Date(input$adm_block_date))
    rs <- trimws(input$adm_block_reason %||% "")
    if (is.na(d)) { showNotification("Invalid date.", type = "error"); return() }

    ok <- tryCatch({
      db_exec1(
        'INSERT INTO blocked_dates("date", reason) VALUES (?date, ?reason)',
        date   = as.character(d),
        reason = ifelse(nzchar(rs), rs, NA_character_)
      )
      TRUE
    }, error = function(e) FALSE)

    if (!ok) {
      showNotification("Block failed. Date may already be blocked.", type = "error")
    } else {
      showNotification("Date blocked.", type = "message")
      blocked_nonce(blocked_nonce() + 1L)
      bump_day_date_ui()
    }
  })

  observeEvent(input$admin_block_remove, {
    req(rv$admin_logged_in)
    d <- suppressWarnings(as.Date(input$adm_block_date))
    if (is.na(d)) { showNotification("Invalid date.", type = "error"); return() }

    n <- db_exec1('DELETE FROM blocked_dates WHERE "date" = ?date', date = as.character(d))
    if (n == 0) {
      showNotification("That date was not blocked.", type = "warning")
    } else {
      showNotification("Blocked date removed.", type = "message")
      blocked_nonce(blocked_nonce() + 1L)
      bump_day_date_ui()
    }
  })

  output$admin_recent_tx <- renderTable({
    if (!rv$admin_logged_in) return(NULL)
    x <- db_get1(
      "SELECT created_at, buyer_name, buyer_email, total_amount_cents, status
       FROM transactions
       ORDER BY created_at DESC
       LIMIT 10"
    )
    if (nrow(x) > 0) x$total_amount_cad <- x$total_amount_cents / 100
    x
  })
}

shinyApp(ui = ui, server = server)
