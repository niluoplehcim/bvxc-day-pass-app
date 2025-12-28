# app.R
# BVXC – Passes, Programs, Events + Admin Controls (Square)
# v6.3 – 2025-12-23

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
  library(htmltools)
  library(base64enc)
})

# DT is optional on Connect Cloud (prevents app startup crash)
HAVE_DT <- requireNamespace("DT", quietly = TRUE)

# -----------------------------------------------------------------------------
# GLOBAL SETTINGS / ENV
# -----------------------------------------------------------------------------

Sys.setenv(TZ = "America/Vancouver")
APP_VERSION <- "BVXC v6.3 – Age enforcement + receipt labels + meta-unique per person – 2025-12-23"

CFG_TX_ITEMS_BACKFILL_V1_DONE <- "tx_items_backfill_v1_done"

if (file.exists(".Renviron")) readRenviron(".Renviron")

ALLOWED_SQUARE_ENVS <- c("sandbox", "production")
ALLOWED_SANDBOX_MODES <- c("fake", "square")

SQUARE_ENV <- tolower(Sys.getenv("SQUARE_ENV", unset = "sandbox"))
SANDBOX_MODE <- tolower(Sys.getenv("BVXC_SANDBOX_MODE", unset = "fake"))
SQUARE_ACCESS_TOKEN <- Sys.getenv("SQUARE_ACCESS_TOKEN", unset = "")
SQUARE_LOCATION_ID <- Sys.getenv("SQUARE_LOCATION_ID", unset = "")

ADMIN_PASSWORD <- Sys.getenv("BVXC_ADMIN_PASSWORD", unset = NA_character_)
RETURN_BASE_URL <- Sys.getenv("BVXC_RETURN_BASE_URL", unset = NA_character_)

# Pin the Square API version you want to use (Square supports an explicit header)
SQUARE_VERSION <- Sys.getenv("SQUARE_VERSION", unset = "2025-10-16")

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
trim_trailing_slash <- function(x) sub("/+$", "", x)
is_true <- function(x) tolower(trimws(x %||% "")) %in% c("1", "true", "yes", "y", "on")

if (!SQUARE_ENV %in% ALLOWED_SQUARE_ENVS) SQUARE_ENV <- "sandbox"
if (!SANDBOX_MODE %in% ALLOWED_SANDBOX_MODES) SANDBOX_MODE <- "fake"

HAVE_SQUARE_CREDS <- nzchar(SQUARE_ACCESS_TOKEN) && nzchar(SQUARE_LOCATION_ID)

# If user tries "sandbox+square" without creds, force fake mode
if ((SQUARE_ENV == "production" || (SQUARE_ENV == "sandbox" && SANDBOX_MODE == "square")) && !HAVE_SQUARE_CREDS) {
  SQUARE_ENV <- "sandbox"
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

# After HAVE_SQUARE_CREDS is set and after the "force fake mode if no creds" block
IS_SQUARE_MODE <- (SQUARE_ENV == "production") ||
  (SQUARE_ENV == "sandbox" && SANDBOX_MODE == "square")

IS_FAKE_MODE <- (SQUARE_ENV == "sandbox" && SANDBOX_MODE == "fake")

# -----------------------------------------------------------------------------
# SEASON WINDOW HELPERS (Nov 1 → May 1)
# -----------------------------------------------------------------------------

get_season_window <- function(today = Sys.Date()) {
  y <- as.integer(format(today, "%Y"))
  m <- as.integer(format(today, "%m"))

  if (m >= 11) {
    start <- as.Date(sprintf("%d-11-01", y))
    end <- as.Date(sprintf("%d-05-01", y + 1))
  } else if (m <= 5) {
    start <- as.Date(sprintf("%d-11-01", y - 1))
    end <- as.Date(sprintf("%d-05-01", y))
  } else {
    start <- as.Date(sprintf("%d-11-01", y))
    end <- as.Date(sprintf("%d-05-01", y + 1))
  }
  list(start = start, end = end)
}

season_label <- function(w) paste0("Season: ", format(w$start), " to ", format(w$end))

# -----------------------------------------------------------------------------
# DB POOL
# -----------------------------------------------------------------------------

DB_URL <- Sys.getenv("BVXC_DB_URL", unset = "")
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
  m <- regmatches(s, re)[[1]]
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

# -----------------------------------------------------------------------------
# PERF TIMING (startup + Square calls)
# -----------------------------------------------------------------------------
tlog <- function(label, t0) {
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("[PERF] %-28s %6.3fs\n", label, dt))
  flush.console()
  invisible(dt)
}

timed <- function(label, expr) {
  t0 <- Sys.time()
  on.exit(tlog(label, t0), add = TRUE)
  force(expr)
}

make_pool <- function() {
  if (db_is_postgres()) {
    s <- trimws(DB_URL)

    # URI form: postgresql://USER:PASSWORD@HOST:5432/DB?sslmode=require
    if (grepl("^postgres(ql)?://", s, ignore.case = TRUE)) {
      return(pool::dbPool(
        drv = RPostgres::Postgres(),
        dbname = s,
        minSize = 1,
        maxSize = 2,
        idleTimeout = 600000, # 10 minutes (ms)
        validationInterval = 60000 # 1 minute (ms)
      ))
    }

    # KV form: host=... port=... dbname=... user=... password=...
    args <- parse_kv_conn(s)

    # Required fields
    required <- c("host", "port", "dbname", "user", "password")
    missing <- setdiff(required, names(args))

    if (length(missing) > 0) {
      stop(
        "BVXC_DB_URL is missing required fields: ",
        paste(missing, collapse = ", "),
        "\nExample KV form:\n",
        "  host=HOST port=5432 dbname=DBNAME user=USER password=PASSWORD sslmode=require\n",
        "Or URI form:\n",
        "  postgresql://USER:PASSWORD@HOST:5432/DBNAME?sslmode=require"
      )
    }

    return(do.call(
      pool::dbPool,
      c(
        list(drv = RPostgres::Postgres()),
        args,
        list(
          minSize = 1,
          maxSize = 2,
          idleTimeout = 600000, # 10 minutes (ms)
          validationInterval = 60000 # 1 minute (ms)
        )
      )
    ))
  }

  pool::dbPool(RSQLite::SQLite(), dbname = DB_PATH)
}

DB_POOL <- timed("make_pool()", make_pool())

onStop(function() {
  try(pool::poolClose(DB_POOL), silent = TRUE)
})

# -----------------------------------------------------------------------------
# DB HELPERS (robust checkout/return; force result before returning connection)
# -----------------------------------------------------------------------------

with_db <- function(fun) {
  stopifnot(is.function(fun))

  con <- pool::poolCheckout(DB_POOL)
  on.exit(try(pool::poolReturn(con), silent = TRUE), add = TRUE)

  res <- fun(con)

  # Ensure nothing lazy still references `con`
  force(res)

  res
}

now_ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

db_get <- function(con, sql, ...) DBI::dbGetQuery(con, DBI::sqlInterpolate(con, sql, ...))
db_exec <- function(con, sql, ...) DBI::dbExecute(con, DBI::sqlInterpolate(con, sql, ...))

db_get1 <- function(sql, ...) with_db(function(con) db_get(con, sql, ...))
db_exec1 <- function(sql, ...) with_db(function(con) db_exec(con, sql, ...))

# -----------------------------------------------------------------------------
# ADMIN: Transactions helper (query)
# -----------------------------------------------------------------------------

fetch_transactions <- function(limit = 50L,
                               start_date = NULL,
                               end_date = NULL,
                               name = "",
                               email = "",
                               status = "",
                               tx_type = "",
                               sort_by = "created_at") {
  limit <- max(1L, min(500L, as.integer(limit %||% 50L)))

  name <- trimws(as.character(name %||% ""))
  email <- trimws(as.character(email %||% ""))
  status <- trimws(as.character(status %||% ""))
  tx_type <- trimws(as.character(tx_type %||% ""))

  sd <- suppressWarnings(as.Date(start_date))
  ed <- suppressWarnings(as.Date(end_date))

  # Whitelist sorting (avoid SQL injection)
  sort_by <- if (sort_by %in% c("created_at", "total_amount_cents")) sort_by else "created_at"
  sort_dir <- "DESC"

  where <- character()
  args <- list()

  # created_at is stored as ISO string "YYYY-MM-DD HH:MM:SS" -> string compare works
  if (!is.na(sd)) {
    where <- c(where, "created_at >= ?start_ts")
    args$start_ts <- paste0(as.character(sd), " 00:00:00")
  }
  if (!is.na(ed)) {
    where <- c(where, "created_at <= ?end_ts")
    args$end_ts <- paste0(as.character(ed), " 23:59:59")
  }

  if (nzchar(name)) {
    where <- c(where, "LOWER(buyer_name) LIKE LOWER(?name)")
    args$name <- paste0("%", name, "%")
  }
  if (nzchar(email)) {
    where <- c(where, "LOWER(buyer_email) LIKE LOWER(?email)")
    args$email <- paste0("%", email, "%")
  }
  if (nzchar(status)) {
    where <- c(where, "status = ?status")
    args$status <- status
  }
  if (nzchar(tx_type)) {
    where <- c(where, "tx_type = ?tx_type")
    args$tx_type <- tx_type
  }

  q <- "
    SELECT created_at, buyer_name, buyer_email, tx_type,
           total_amount_cents, currency, receipt_token, status
    FROM transactions
  "

  if (length(where) > 0) {
    q <- paste(q, "WHERE", paste(where, collapse = " AND "))
  }

  q <- paste0(q, " ORDER BY ", sort_by, " ", sort_dir, " LIMIT ", limit)

  do.call(db_get1, c(list(sql = q), args))
}

# -----------------------------------------------------------------------------
# DB INIT
# -----------------------------------------------------------------------------

init_db <- function() {
  with_db(function(con) {
    # -----------------------------
    # config
    # -----------------------------
    db_exec(con, "
      CREATE TABLE IF NOT EXISTS config (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL
      )
    ")

    # -----------------------------
    # transactions
    # -----------------------------
    db_exec(con, "
      CREATE TABLE IF NOT EXISTS transactions (
        id TEXT PRIMARY KEY,
        created_at TEXT NOT NULL,
        buyer_name TEXT,
        buyer_email TEXT,
        tx_type TEXT,
        total_amount_cents INTEGER NOT NULL,
        currency TEXT NOT NULL,
        cart_json TEXT NOT NULL,
        square_checkout_id TEXT,
        square_order_id TEXT,
        receipt_token TEXT,
        status TEXT
      )
    ")

    # --- migrate existing DBs: ensure tx_type exists ---
    if (db_is_postgres()) {
      try(db_exec(con, "ALTER TABLE transactions ADD COLUMN IF NOT EXISTS tx_type TEXT"), silent = TRUE)
    } else {
      cols <- tryCatch(DBI::dbGetQuery(con, "PRAGMA table_info(transactions)"), error = function(e) data.frame())
      if (!("name" %in% names(cols)) || !("tx_type" %in% cols$name)) {
        try(db_exec(con, "ALTER TABLE transactions ADD COLUMN tx_type TEXT"), silent = TRUE)
      }
    }

    # -----------------------------
    # blocked_dates
    # -----------------------------
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

    # -----------------------------
    # special_events
    # -----------------------------
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

    # -----------------------------
    # tx_items (for fast capacity counts)
    # -----------------------------
    db_exec(con, "
      CREATE TABLE IF NOT EXISTS tx_items (
        id TEXT PRIMARY KEY,
        tx_id TEXT NOT NULL,
        category TEXT NOT NULL,
        item_id TEXT NOT NULL,
        qty INTEGER NOT NULL,
        status TEXT NOT NULL,
        created_at TEXT NOT NULL
      )
    ")

    try(db_exec(con, "CREATE INDEX IF NOT EXISTS idx_tx_items_item_cat_status ON tx_items(item_id, category, status)"), silent = TRUE)
    try(db_exec(con, "CREATE INDEX IF NOT EXISTS idx_tx_items_tx_id          ON tx_items(tx_id)"), silent = TRUE)

    # -----------------------------
    # indexes + constraints
    # -----------------------------
    try(db_exec(con, "CREATE INDEX IF NOT EXISTS idx_transactions_created_at     ON transactions(created_at)"), silent = TRUE)
    try(db_exec(con, "CREATE INDEX IF NOT EXISTS idx_transactions_receipt_token  ON transactions(receipt_token)"), silent = TRUE)
    try(db_exec(con, "CREATE INDEX IF NOT EXISTS idx_transactions_status         ON transactions(status)"), silent = TRUE)
    try(db_exec(con, "CREATE INDEX IF NOT EXISTS idx_transactions_tx_type        ON transactions(tx_type)"), silent = TRUE)
    try(db_exec(con, "CREATE INDEX IF NOT EXISTS idx_transactions_buyer_email    ON transactions(buyer_email)"), silent = TRUE)
    try(db_exec(con, "CREATE INDEX IF NOT EXISTS idx_transactions_buyer_name     ON transactions(buyer_name)"), silent = TRUE)

    try(db_exec(con, "CREATE INDEX IF NOT EXISTS idx_events_event_date           ON special_events(event_date)"), silent = TRUE)

    try(db_exec(con, "CREATE UNIQUE INDEX IF NOT EXISTS ux_transactions_receipt_token ON transactions(receipt_token)"), silent = TRUE)

    invisible(NULL)
  })

  invisible(NULL)
}

# Call it ONCE (timed)
timed("init_db()", init_db())

# -----------------------------------------------------------------------------
# CONFIG HELPERS (CACHED)
# -----------------------------------------------------------------------------

CFG_CACHE <- new.env(parent = emptyenv())
CFG_CACHE$map <- NULL
CFG_CACHE$loaded_at <- as.POSIXct(0, origin = "1970-01-01")

cfg_refresh_cache <- function(force = FALSE, ttl_secs = 60) {
  if (!force && !is.null(CFG_CACHE$map)) {
    age <- as.numeric(difftime(Sys.time(), CFG_CACHE$loaded_at, units = "secs"))
    if (!is.na(age) && age <= ttl_secs) {
      return(invisible(TRUE))
    }
  }

  x <- db_get1("SELECT key, value FROM config")
  m <- list()
  if (nrow(x) > 0) {
    for (i in seq_len(nrow(x))) {
      k <- as.character(x$key[i] %||% "")
      if (nzchar(k)) m[[k]] <- as.character(x$value[i] %||% "")
    }
  }

  CFG_CACHE$map <- m
  CFG_CACHE$loaded_at <- Sys.time()
  invisible(TRUE)
}

# Always-hit-DB read (used ONLY by reactivePoll marker)
cfg_get_db <- function(key, default = "") {
  key <- as.character(key %||% "")
  if (!nzchar(trimws(key))) {
    return(default)
  }

  x <- db_get1("SELECT value FROM config WHERE key = ?key LIMIT 1", key = key)
  if (nrow(x) == 0) {
    return(default)
  }
  as.character(x$value[1] %||% default)
}

cfg_get <- function(key, default = "") {
  key <- as.character(key %||% "")
  if (!nzchar(trimws(key))) {
    return(default)
  }

  cfg_refresh_cache(force = FALSE, ttl_secs = 60)

  v <- CFG_CACHE$map[[key]]
  if (is.null(v) || is.na(v)) default else v
}

cfg_set <- function(key, value) {
  key <- as.character(key %||% "")
  value <- as.character(value %||% "")
  if (!nzchar(trimws(key))) {
    return(invisible(FALSE))
  }

  with_db(function(con) {
    DBI::dbWithTransaction(con, {
      upsert_config <- function(k, v) {
        n <- db_exec(con, "UPDATE config SET value = ?value WHERE key = ?key", key = k, value = v)
        if (isTRUE(n == 0)) {
          tryCatch(
            db_exec(con, "INSERT INTO config(key, value) VALUES (?key, ?value)", key = k, value = v),
            error = function(e) {
              db_exec(con, "UPDATE config SET value = ?value WHERE key = ?key", key = k, value = v)
            }
          )
        }
        invisible(TRUE)
      }

      upsert_config(key, value)

      if (!identical(key, "__config_updated_at")) {
        upsert_config("__config_updated_at", now_ts())
      }
    })
  })

  # Refresh local cache immediately after writes
  cfg_refresh_cache(force = TRUE)

  invisible(TRUE)
}

cfg_bool <- function(key, default = FALSE) {
  v <- tolower(trimws(cfg_get(key, "")))
  if (!nzchar(v)) {
    return(default)
  }
  v %in% c("1", "true", "yes", "on")
}

cfg_num <- function(key, default = NA_real_) {
  s <- trimws(cfg_get(key, ""))
  if (!nzchar(s) || toupper(s) %in% c("N/A", "NA")) {
    return(default)
  }
  s <- gsub("\\$", "", s)
  s <- gsub(",", "", s)
  v <- suppressWarnings(as.numeric(s))
  if (is.na(v) || v < 0) {
    return(default)
  }
  v
}

cfg_int <- function(key, default = NA_integer_) {
  s <- trimws(cfg_get(key, ""))
  if (!nzchar(s) || toupper(s) %in% c("N/A", "NA")) {
    return(default)
  }
  v <- suppressWarnings(as.integer(s))
  if (is.na(v) || v < 0) {
    return(default)
  }
  v
}

cfg_date <- function(key, default = as.Date(NA)) {
  s <- trimws(cfg_get(key, ""))
  if (!nzchar(s)) {
    return(default)
  }
  d <- suppressWarnings(as.Date(s))
  if (is.na(d)) default else d
}

cfg_get_sentinel <- function(key) {
  v <- tolower(trimws(cfg_get(key, default = "")))
  v %in% c("1", "true", "yes", "on")
}

cfg_set_sentinel <- function(key, done = TRUE) {
  cfg_set(key, if (isTRUE(done)) "1" else "0")
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# BUSINESS DATA
# -----------------------------------------------------------------------------

get_early_bird_cutoff <- function() cfg_date("early_bird_cutoff", as.Date(NA))

get_day_prices <- function() {
  data.frame(
    type = c("Adult", "Youth", "Under 9", "Family"),
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
      type = c("Adult", "Youth"),
      price = c(
        cfg_num("price_season_eb_adult", NA_real_),
        cfg_num("price_season_eb_youth", NA_real_)
      ),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      type = c("Adult", "Youth"),
      price = c(
        cfg_num("price_season_reg_adult", NA_real_),
        cfg_num("price_season_reg_youth", NA_real_)
      ),
      stringsAsFactors = FALSE
    )
  }
}

get_christmas_pass_price <- function() cfg_num("price_christmas_pass", NA_real_)

PROGRAM_CATALOG <- data.frame(
  id = c(
    "bunnies", "rabbits", "u10", "u12", "u14", "u16", "u18", "u20",
    "biathlon_adp", "biathlon_rifle_rental_adp", "biathlon_youth_intro",
    "masters_2x", "masters_1x", "biathlon_masters", "biathlon_masters_rifle_rental"
  ),
  name = c(
    "Bunnies", "Rabbits", "U10", "U12", "U14", "U16", "U18", "U20",
    "Biathlon ADP", "Biathlon Rifle Rental ADP", "Biathlon Youth Intro",
    "Masters 2X", "Masters 1X", "Biathlon Masters", "Biathlon Masters Rifle Rental"
  ),
  price_key = c(
    "price_program_bunnies", "price_program_rabbits", "price_program_u10", "price_program_u12",
    "price_program_u14", "price_program_u16", "price_program_u18", "price_program_u20",
    "price_program_biathlon_adp", "price_program_biathlon_rifle_rental_adp", "price_program_biathlon_youth_intro",
    "price_program_masters_2x", "price_program_masters_1x", "price_program_biathlon_masters", "price_program_biathlon_masters_rifle_rental"
  ),
  cap_key = c(
    "cap_program_bunnies", "cap_program_rabbits", "cap_program_u10", "cap_program_u12",
    "cap_program_u14", "cap_program_u16", "cap_program_u18", "cap_program_u20",
    "cap_program_biathlon_adp", "cap_program_biathlon_rifle_rental_adp", "cap_program_biathlon_youth_intro",
    "cap_program_masters_2x", "cap_program_masters_1x", "cap_program_biathlon_masters", "cap_program_biathlon_masters_rifle_rental"
  ),
  stringsAsFactors = FALSE
)

program_catalog <- function() PROGRAM_CATALOG

get_program_list <- function() {
  # Warm config cache first (prevents repeated DB reads when cfg_num/cfg_int are called many times)
  if (exists("cfg_refresh_cache", mode = "function")) {
    cfg_refresh_cache(force = FALSE)
  }

  cat <- program_catalog()
  cat$price <- vapply(cat$price_key, function(k) cfg_num(k, NA_real_), numeric(1))
  cat$capacity <- vapply(cat$cap_key, function(k) cfg_int(k, NA_integer_), integer(1))
  cat[, c("id", "name", "price", "capacity", "price_key", "cap_key")]
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
  if (is.null(cart_df) || nrow(cart_df) == 0) {
    return(NULL)
  }

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
# AGE HELPERS
# BVXC day + season buckets: Child 0–8, Youth 9–18, Adult 19+.
# Programs: age is determined as-of Dec 31 of the ski season (season start year).
# Season pass enforcement uses the same Dec 31 reference (easy to change if needed).
# -----------------------------------------------------------------------------

age_years_on <- function(dob, ref_date) {
  dob <- suppressWarnings(as.Date(dob))
  ref_date <- suppressWarnings(as.Date(ref_date))
  if (is.na(dob) || is.na(ref_date)) {
    return(NA_integer_)
  }
  y <- as.integer(format(ref_date, "%Y")) - as.integer(format(dob, "%Y"))
  if (format(ref_date, "%m-%d") < format(dob, "%m-%d")) y <- y - 1L
  as.integer(y)
}

# Age reference is Dec 31 of the season start year (Nov 1)
season_age_ref_date <- function(today = Sys.Date()) {
  w <- get_season_window(today)
  y <- as.integer(format(w$start, "%Y"))
  as.Date(sprintf("%d-12-31", y))
}

bvxc_age_bucket <- function(age_years) {
  a <- suppressWarnings(as.integer(age_years))
  if (is.na(a) || a < 0L) {
    return(NA_character_)
  }
  if (a <= 8L) {
    return("Child")
  }
  if (a <= 18L) {
    return("Youth")
  }
  "Adult"
}

# Returns list(min=..., max=..., label=...) based on program id
program_age_rule <- function(program_id, program_name = "") {
  pid <- tolower(trimws(program_id %||% ""))

  # U10/U12/U14/... => "under N" => max age N-1 as-of Dec 31
  if (grepl("^u\\d+$", pid)) {
    n <- suppressWarnings(as.integer(sub("^u", "", pid)))
    if (!is.na(n) && n > 0) {
      return(list(min = 0L, max = as.integer(n - 1L), label = toupper(pid)))
    }
  }

  # Masters / ADP => Adult (19+)
  if (pid %in% c(
    "masters_2x", "masters_1x", "biathlon_masters", "biathlon_masters_rifle_rental",
    "biathlon_adp", "biathlon_rifle_rental_adp"
  )) {
    return(list(min = 19L, max = 120L, label = "Adult"))
  }

  # Youth Intro => Youth (9–18)
  if (pid %in% c("biathlon_youth_intro")) {
    return(list(min = 9L, max = 18L, label = "Youth"))
  }

  # Bunnies / Rabbits defaults
  if (pid == "bunnies") {
    return(list(min = 0L, max = 6L, label = "Bunnies"))
  }
  if (pid == "rabbits") {
    return(list(min = 0L, max = 8L, label = "Rabbits"))
  }

  # Unknown => do not enforce
  list(min = NA_integer_, max = NA_integer_, label = program_name %||% pid)
}

# -----------------------------------------------------------------------------
# CART / RECEIPT LABEL HELPERS (global)
# -----------------------------------------------------------------------------

# Parse meta JSON safely (returns list or NULL)
parse_meta_obj_safe <- function(meta_json) {
  if (is.null(meta_json)) return(NULL)
  if (is.list(meta_json)) return(meta_json)

  s <- as.character(meta_json %||% "")
  if (!nzchar(s)) return(NULL)

  tryCatch(
    jsonlite::fromJSON(s, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

# Format "Name — DOB: ..." (or just name) from a parsed meta object
fmt_person_from_meta_obj <- function(obj, name_key, dob_key) {
  if (is.null(obj) || !is.list(obj)) return("")

  nm  <- trimws(as.character(obj[[name_key]] %||% ""))
  dob <- trimws(as.character(obj[[dob_key]] %||% ""))

  if (!nzchar(nm) && !nzchar(dob)) return("")
  if (!nzchar(dob)) return(nm)

  paste0(nm, " — DOB: ", dob)
}

# Backward-compatible wrapper (keeps your old function name/signature)
fmt_person_from_meta <- function(meta_json, name_key, dob_key) {
  obj <- parse_meta_obj_safe(meta_json)
  fmt_person_from_meta_obj(obj, name_key, dob_key)
}

# Build a line item label when you already have the parsed meta object
line_item_label_from_obj <- function(category, description, obj) {
  desc <- as.character(description %||% "")
  cat  <- as.character(category %||% "")

  if (identical(cat, "program")) {
    who <- fmt_person_from_meta_obj(obj, "participant_name", "participant_dob")
    if (nzchar(who)) return(paste0(desc, " — ", who))
  }

  if (identical(cat, "season_pass")) {
    who <- fmt_person_from_meta_obj(obj, "holder_name", "holder_dob")
    if (nzchar(who)) return(paste0(desc, " — ", who))
  }

  desc
}

# Primary entry point (same signature as before)
line_item_label <- function(category, description, meta_json) {
  obj <- parse_meta_obj_safe(meta_json)
  line_item_label_from_obj(category, description, obj)
}

# -----------------------------------------------------------------------------
# SQUARE API HELPERS
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

square_safe_json <- function(res) {
  tryCatch(httr::content(res, as = "parsed", type = "application/json"), error = function(e) NULL)
}

square_http_get <- function(path, query = list()) {
  res <- timed(paste0("square_http_get() ", path), {
    httr::GET(
      url = paste0(square_base_url(), path),
      square_headers(include_version = TRUE),
      query = query,
      httr::timeout(15)
    )
  })

  body <- timed(paste0("square_safe_json() ", path), {
    square_safe_json(res)
  })

  list(status = httr::status_code(res), body = body, raw = res)
}

square_http_post <- function(path, body) {
  res <- timed(paste0("square_http_post() ", path), {
    httr::POST(
      url = paste0(square_base_url(), path),
      square_headers(include_version = TRUE),
      httr::content_type_json(),
      body = body,
      encode = "json",
      httr::timeout(15)
    )
  })

  body_parsed <- timed(paste0("square_safe_json() ", path), {
    square_safe_json(res)
  })

  list(status = httr::status_code(res), body = body_parsed, raw = res)
}

create_square_checkout_from_cart <- function(cart_df,
                                             buyer_email = NULL,
                                             note = NULL,
                                             redirect_url = NULL) {
  if (is.null(cart_df) || nrow(cart_df) == 0) {
    return(NULL)
  }

  line_items <- lapply(seq_len(nrow(cart_df)), function(i) {
    row <- cart_df[i, , drop = FALSE]
    list(
      name = line_item_label(as.character(row$category[1]), as.character(row$description[1]), as.character(row$meta_json[1] %||% "")),
      quantity = as.character(row$quantity[1]),
      base_price_money = list(
        amount   = as.integer(round(as.numeric(row$unit_price[1]) * 100)),
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
      redirect_url             = redirect_url
    )
  )

  # Only include buyer_email if it is non-empty (prevents Square rejecting "")
  buyer_email <- trimws(as.character(buyer_email %||% ""))
  if (nzchar(buyer_email)) {
    body$pre_populated_data <- list(buyer_email = buyer_email)
  }

  r <- square_http_post("/v2/online-checkout/payment-links", body = body)
  if (r$status >= 300 || is.null(r$body$payment_link$url)) {
    warning("Square payment link error: ", httr::content(r$raw, as = "text", encoding = "UTF-8"))
    return(NULL)
  }

  pl <- r$body$payment_link
  list(
    checkout_url = pl$url %||% NULL,
    checkout_id  = pl$id %||% NA_character_,
    square_order = pl$order_id %||% NA_character_
  )
}

square_get_order <- function(order_id) {
  if (!nzchar(order_id %||% "")) {
    return(NULL)
  }
  r <- square_http_get(paste0("/v2/orders/", order_id))
  if (r$status >= 300) {
    return(NULL)
  }
  r$body$order %||% NULL
}

square_list_payments_by_order <- function(order_id) {
  if (!nzchar(order_id %||% "")) {
    return(NULL)
  }
  r <- square_http_get("/v2/payments", query = list(order_id = order_id))
  if (r$status >= 300) {
    return(NULL)
  }
  r$body$payments %||% NULL
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
      textInput(paste0(prefix, "_buyer_name"), "Name for receipt", value = ""),
      textInput(paste0(prefix, "_buyer_email"), "Email for receipt", value = ""),
      br(),
      strong(textOutput(paste0(prefix, "_cart_total"))),
      br(), br(),
      actionButton(paste0(prefix, "_cart_clear"), "Clear cart"),
      uiOutput(paste0(prefix, "_cart_pay_ui"))
    )
  )
}

qty_stepper_input <- function(id, label, value = 0L, min = 0L, max = 20L) {
  v <- suppressWarnings(as.integer(value))
  if (is.na(v)) v <- 0L
  v <- max(as.integer(min), min(as.integer(max), v))

  tags$div(
    class = "form-group shiny-input-container",
    tags$label(label, `for` = id),
    tags$div(
      class = "qty-stepper qty-stepper-global",
      `data-target` = id,
      `data-min` = as.integer(min),
      `data-max` = as.integer(max),
      tags$button(type = "button", class = "btn btn-outline-secondary btn-sm qty-dec", "−"),
      tags$span(class = "qty-value", as.character(v)),
      tags$button(type = "button", class = "btn btn-outline-secondary btn-sm qty-inc", "+")
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

.mini-cart-box {
  margin-top: 14px;
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 8px;
  background: #fafafa;
}

.cart-line {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 6px 0;
  border-bottom: 1px solid #eee;
}
.cart-desc { flex: 1; font-weight: 600; }

.cart-controls { display: flex; align-items: center; gap: 12px; }

.qty-stepper { display:flex; align-items:center; gap:12px; }
.qty-stepper .qty-value { min-width:34px; text-align:center; font-weight:800; font-size:20px; }

.qty-stepper .btn {
  min-width:56px;
  height:56px;
  font-weight:900;
  font-size:28px;
  line-height: 1;
  cursor: pointer;
}

.qty-stepper .btn:active { transform: scale(0.98); }
.qty-stepper .btn:focus { outline: 2px solid rgba(13,110,253,0.35); outline-offset: 2px; }

.cart-qty-input {
  font-size: 20px;
  height: 44px;
  padding: 6px 12px;
  min-width: 120px;
}
.cart-amt-input {
  font-size: 20px;
  height: 44px;
  padding: 6px 12px;
  min-width: 160px;
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

/* ---- Receipt QR rendering (pixelated) ---- */
#receipt_qr img { image-rendering: pixelated; }

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

.admin-block { margin-bottom: 14px; }
.admin-block .panel-heading { font-weight: 700; }
.admin-block .panel-body { padding-top: 12px; }
"

ui <- fluidPage(
  tags$head(
    tags$style(HTML(css_tabs)),

    # --- Global quantity stepper (click-only, no typing) ---
    tags$script(HTML("
      (function() {
        function clamp(v, mn, mx) {
          v = parseInt(v, 10); mn = parseInt(mn, 10); mx = parseInt(mx, 10);
          if (isNaN(v)) v = 0;
          if (isNaN(mn)) mn = 0;
          if (isNaN(mx)) mx = 20;
          if (v < mn) v = mn;
          if (v > mx) v = mx;
          return v;
        }

        function push($stepper, v) {
          var target = $stepper.data('target');
          if (!target || !window.Shiny) return;
          Shiny.setInputValue(target, v, {priority: 'event'});
        }

        $(document).off('click', '.qty-stepper-global .qty-dec, .qty-stepper-global .qty-inc');
        $(document).on('click', '.qty-stepper-global .qty-dec, .qty-stepper-global .qty-inc', function(e) {
          e.preventDefault();

          var $stepper = $(this).closest('.qty-stepper-global');
          var mn = $stepper.data('min');
          var mx = $stepper.data('max');

          var $val = $stepper.find('.qty-value');
          var v = clamp($val.text(), mn, mx);

          v = v + ($(this).hasClass('qty-inc') ? 1 : -1);
          v = clamp(v, mn, mx);

          $val.text(v);
          push($stepper, v);
        });

        // Initialize: push displayed values into Shiny once on load
        $(function() {
          $('.qty-stepper-global').each(function() {
            var $s = $(this);
            var mn = $s.data('min');
            var mx = $s.data('max');
            var v = clamp($s.find('.qty-value').text(), mn, mx);
            $s.find('.qty-value').text(v);
            push($s, v);
          });
        });
      })();
    ")),

    # --- Existing BVXC bindings (keep as-is) ---
    tags$script(HTML("
(function() {
  function bindBVXC() {
    if (!window.jQuery || !window.Shiny) return;

    // Prevent double-binding of custom message handlers
    if (window.__bvxcBound) return;
    window.__bvxcBound = true;

    // --- Custom message handlers ---
    Shiny.addCustomMessageHandler('redirect', function(message) {
      try { window.top.location.href = message.url; }
      catch(e) { window.location.href = message.url; }
    });

    Shiny.addCustomMessageHandler('toggleReceiptNav', function(message) {
      var show = !!message.show;
      var a = $('a[data-value=\"Receipt\"]');
      if (a.length) a.closest('li').toggle(show);
    });

    Shiny.addCustomMessageHandler('disableBuyerAutofill', function(message) {
      try {
        $('input[id$=\"_buyer_name\"], input[id$=\"_buyer_email\"]').each(function() {
          $(this)
            .attr('autocomplete','off')
            .attr('autocorrect','off')
            .attr('autocapitalize','off')
            .attr('spellcheck','false')
            .attr('data-lpignore','true')
            .attr('data-1p-ignore','true');
        });
      } catch(e) {}
    });

    // --- Admin table actions (delegated handlers) ---

    // Blocked dates delete
    $(document).off('click.bvxc', 'a.admin-block-del');
    $(document).on('click.bvxc', 'a.admin-block-del', function(e){
      e.preventDefault();
      var d = $(this).data('date');
      Shiny.setInputValue(
        'admin_block_del',
        { date: d, nonce: Math.random() },
        { priority: 'event' }
      );
    });

    // Transactions: load receipt
    $(document).off('click.bvxc', 'a.admin-tx-load');
    $(document).on('click.bvxc', 'a.admin-tx-load', function(e){
      e.preventDefault();
      var tok = $(this).data('token');
      Shiny.setInputValue(
        'admin_tx_load',
        { token: String(tok || ''), nonce: Math.random() },
        { priority: 'event' }
      );
    });
  }

  // Bind when ready, and retry shortly in case Shiny/jQuery load slightly later
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', bindBVXC);
  } else {
    bindBVXC();
  }
  setTimeout(bindBVXC, 50);
  setTimeout(bindBVXC, 250);
})();
"))
  ),
  uiOutput("main_nav_ui")
)

# -----------------------------------------------------------------------------
# tx_items helpers (fast capacity counts)
# -----------------------------------------------------------------------------

extract_item_id_for_tx_item <- function(category, meta_json) {
  cat <- as.character(category %||% "")
  s <- as.character(meta_json %||% "")
  if (!nzchar(cat) || !nzchar(s)) {
    return("")
  }

  obj <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj)) {
    return("")
  }

  if (identical(cat, "event")) {
    return(as.character(obj$event_id %||% ""))
  }
  if (identical(cat, "program")) {
    return(as.character(obj$program_id %||% ""))
  }

  ""
}

# Backfill only if you have old COMPLETED tx without tx_items rows (prevents oversell)
ensure_tx_items_backfill_completed <- function(batch = 200L, max_batches = 20L) {

  # If already done (DB sentinel), skip
  if (cfg_get_sentinel(CFG_TX_ITEMS_BACKFILL_V1_DONE)) {
    return(invisible(TRUE))
  }

  for (b in seq_len(max_batches)) {

    # Find completed tx that have no tx_items yet
    # (ONLY those likely to contain capacity-relevant items)
    tx <- db_get1(
      "
      SELECT id, created_at, status, cart_json
      FROM transactions t
      WHERE t.status IN ('COMPLETED','SANDBOX_TEST_OK')
        AND (
          t.cart_json LIKE '%\"category\":\"event\"%'
          OR t.cart_json LIKE '%\"category\":\"program\"%'
        )
        AND NOT EXISTS (SELECT 1 FROM tx_items x WHERE x.tx_id = t.id)
      LIMIT ?n
      ",
      n = as.integer(batch)
    )
 
    # Nothing left to backfill -> mark done (only if truly none remain)
    if (nrow(tx) == 0) {

      remaining <- db_get1(
        "
        SELECT COUNT(*) AS n
        FROM transactions t
        WHERE t.status IN ('COMPLETED','SANDBOX_TEST_OK')
          AND (
            t.cart_json LIKE '%\"category\":\"event\"%'
            OR t.cart_json LIKE '%\"category\":\"program\"%'
          )
          AND NOT EXISTS (SELECT 1 FROM tx_items x WHERE x.tx_id = t.id)
        "
      )$n[1]

      if (!is.na(remaining) && remaining == 0) {
        cfg_set_sentinel(CFG_TX_ITEMS_BACKFILL_V1_DONE, TRUE)
      }

      return(invisible(TRUE))
    }

    # Insert tx_items for each tx
    for (i in seq_len(nrow(tx))) {

      cart_df <- tryCatch(
        jsonlite::fromJSON(tx$cart_json[i] %||% ""),
        error = function(e) NULL
      )

      if (is.null(cart_df) || !is.data.frame(cart_df) || nrow(cart_df) == 0) next
      if (!all(c("category", "quantity", "meta_json") %in% names(cart_df))) next

      st  <- as.character(tx$status[i] %||% "")
      tid <- as.character(tx$id[i] %||% "")
      ca  <- as.character(tx$created_at[i] %||% now_ts())
      if (!nzchar(tid)) next

      # Only event/program matter for capacity; store only those
      ix <- which(cart_df$category %in% c("event", "program"))
      if (length(ix) == 0) next

      with_db(function(con) {
        DBI::dbWithTransaction(con, {

          for (j in ix) {
            item_id <- extract_item_id_for_tx_item(cart_df$category[j], cart_df$meta_json[j])
            if (!nzchar(item_id)) next

            q <- suppressWarnings(as.integer(cart_df$quantity[j] %||% 0L))
            if (is.na(q) || q <= 0L) next

            db_exec(
              con,
              "INSERT INTO tx_items (id, tx_id, category, item_id, qty, status, created_at)
               VALUES (?id, ?tx_id, ?category, ?item_id, ?qty, ?status, ?created_at)",
              id         = UUIDgenerate(),
              tx_id      = tid,
              category   = as.character(cart_df$category[j]),
              item_id    = item_id,
              qty        = q,
              status     = st,
              created_at = ca
            )
          }

        })
      })
    }
  }

  # Hit max_batches; do NOT set sentinel. We'll continue next time.
  invisible(TRUE)
}

insert_tx_items_for_cart <- function(tx_id, created_at, status, cart_df) {
  if (!nzchar(tx_id %||% "")) {
    return(invisible(TRUE))
  }
  if (is.null(cart_df) || nrow(cart_df) == 0) {
    return(invisible(TRUE))
  }

  ix <- which(cart_df$category %in% c("event", "program"))
  if (length(ix) == 0) {
    return(invisible(TRUE))
  }

  with_db(function(con) {
    DBI::dbWithTransaction(con, {
      for (j in ix) {
        item_id <- extract_item_id_for_tx_item(cart_df$category[j], cart_df$meta_json[j])
        if (!nzchar(item_id)) next

        q <- suppressWarnings(as.integer(cart_df$quantity[j] %||% 0L))
        if (is.na(q) || q <= 0L) next

        db_exec(
          con,
          "INSERT INTO tx_items (id, tx_id, category, item_id, qty, status, created_at)
           VALUES (?id, ?tx_id, ?category, ?item_id, ?qty, ?status, ?created_at)",
          id         = UUIDgenerate(),
          tx_id      = tx_id,
          category   = as.character(cart_df$category[j]),
          item_id    = item_id,
          qty        = q,
          status     = as.character(status %||% ""),
          created_at = as.character(created_at %||% now_ts())
        )
      }
    })
  })

  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  empty_cart_df <- function() {
    data.frame(
      id = character(),
      category = character(),
      description = character(),
      quantity = integer(),
      unit_price = numeric(),
      meta_json = character(),
      merge_key = character(),
      stringsAsFactors = FALSE
    )
  }

  rv <- reactiveValues(
    cart             = empty_cart_df(),
    buyer_name       = "",
    buyer_email      = "",
    admin_logged_in  = FALSE,
    admin_fail_count = 0L,
    admin_lock_until = as.POSIXct(NA),
    checkout_started = FALSE,
    checkout_token   = "",
    checkout_lock    = FALSE,
    receipt_qr_token = "",
    receipt_qr_file  = ""
  )

  # One-time DB maintenance (cross-process): backfill tx_items if needed
  # DB sentinel controls "done" state across sessions/processes
  try({
    if (!cfg_get_sentinel(CFG_TX_ITEMS_BACKFILL_V1_DONE)) {
      ensure_tx_items_backfill_completed()
    }
  }, silent = TRUE)

  # ---- tiny cache so reactivePoll doesn't hit DB twice every interval ----
  CFG_UPDATED_CACHE <- new.env(parent = emptyenv())
  CFG_UPDATED_CACHE$val <- ""
  CFG_UPDATED_CACHE$t <- as.POSIXct(0, origin = "1970-01-01")

cfg_get_updated_at_cached <- function(ttl_secs = 1) {
  age <- as.numeric(difftime(Sys.time(), CFG_UPDATED_CACHE$t, units = "secs"))
  if (!is.na(age) && age <= ttl_secs) {
    return(CFG_UPDATED_CACHE$val)
  }

  v <- cfg_get_db("__config_updated_at", "")
  CFG_UPDATED_CACHE$val <- as.character(v %||% "")
  CFG_UPDATED_CACHE$t <- Sys.time()
  CFG_UPDATED_CACHE$val
}

  # -----------------------------------------------------------------------------
  # CONFIG REACTIVITY: re-render nav when config changes
  # -----------------------------------------------------------------------------

  config_updated_at <- reactivePoll(
    intervalMillis = 60000,
    session        = session,
    checkFunc      = function() cfg_get_updated_at_cached(1),
    valueFunc      = function() cfg_get_updated_at_cached(1)
  )

# Run once after the UI is ready
  session$onFlushed(function() {
    session$sendCustomMessage("disableBuyerAutofill", list())

    # Warm config cache after first paint (avoids first-load blocking)
    try(cfg_refresh_cache(force = FALSE), silent = TRUE)
  }, once = TRUE)

  receipt_tx <- reactiveVal(NULL)
  receipt_cart_df <- reactiveVal(NULL)

  # Cache parsed cart + parsed meta + final labels for the current receipt
  observeEvent(receipt_tx(), {
    tx <- receipt_tx()
    if (is.null(tx) || nrow(tx) != 1) {
      receipt_cart_df(NULL)
      return()
    }

    cart_df <- tryCatch(
      jsonlite::fromJSON(tx$cart_json[1] %||% ""),
      error = function(e) NULL
    )

    if (is.null(cart_df) || nrow(cart_df) == 0) {
      receipt_cart_df(cart_df)
      return()
    }

    # Ensure expected columns exist
    if (!("meta_json" %in% names(cart_df))) cart_df$meta_json <- rep("", nrow(cart_df))
    if (!("category" %in% names(cart_df)))  cart_df$category  <- rep("", nrow(cart_df))
    if (!("description" %in% names(cart_df))) cart_df$description <- rep("", nrow(cart_df))

    # Parse each meta_json ONCE
    cart_df$meta_obj <- lapply(cart_df$meta_json, parse_meta_obj_safe)

    # Build the final display label ONCE
    cart_df$label <- mapply(
      line_item_label_from_obj,
      cart_df$category,
      cart_df$description,
      cart_df$meta_obj,
      USE.NAMES = FALSE
    )

    receipt_cart_df(cart_df)
  }, ignoreInit = TRUE)

  day_date_ui_nonce <- reactiveVal(0L)
  last_valid_day_date <- reactiveVal(Sys.Date())
  blocked_nonce <- reactiveVal(0L)
  events_nonce <- reactiveVal(0L)
  admin_nonce <- reactiveVal(0L)
  tx_nonce <- reactiveVal(0L)

  # -----------------------------------------------------------------------------
  # ADMIN LOGIN + ADMIN UI OUTPUTS (RESTORED)
  # -----------------------------------------------------------------------------

  observeEvent(input$admin_login,
    {
      # Must be configured
      if (is.na(ADMIN_PASSWORD) || !nzchar(ADMIN_PASSWORD)) {
        rv$admin_logged_in <- FALSE
        showNotification("Admin password is not configured on this host.", type = "error")
        admin_nonce(admin_nonce() + 1L)
        return()
      }

      # Lockout check
      now <- Sys.time()
      if (!is.na(rv$admin_lock_until) && now < rv$admin_lock_until) {
        showNotification("Admin login temporarily locked. Try again shortly.", type = "error")
        admin_nonce(admin_nonce() + 1L)
        return()
      }

      entered <- trimws(as.character(input$admin_password %||% ""))

      if (identical(entered, ADMIN_PASSWORD)) {
        rv$admin_logged_in <- TRUE
        rv$admin_fail_count <- 0L
        rv$admin_lock_until <- as.POSIXct(NA)

        showNotification("Admin unlocked.", type = "message")
        admin_nonce(admin_nonce() + 1L)
      } else {
        rv$admin_logged_in <- FALSE
        rv$admin_fail_count <- as.integer(rv$admin_fail_count %||% 0L) + 1L

        if (rv$admin_fail_count >= 5L) {
          rv$admin_lock_until <- Sys.time() + 60
          rv$admin_fail_count <- 0L
          showNotification("Too many attempts. Locked for 60 seconds.", type = "error")
        } else {
          showNotification("Incorrect admin password.", type = "error")
        }

        admin_nonce(admin_nonce() + 1L)
      }
    },
    ignoreInit = TRUE
  )


  output$admin_lock_msg <- renderUI({
    admin_nonce()

    now <- Sys.time()
    if (!is.na(rv$admin_lock_until) && now < rv$admin_lock_until) {
      secs <- ceiling(as.numeric(difftime(rv$admin_lock_until, now, units = "secs")))
      return(tags$div(
        class = "alert alert-warning",
        paste("Login locked. Try again in", secs, "seconds.")
      ))
    }

    if (isTRUE(rv$admin_logged_in)) {
      return(tags$div(class = "alert alert-success", "Admin unlocked."))
    }

    if (isTRUE(rv$admin_fail_count > 0L)) {
      return(tags$div(
        class = "alert alert-warning",
        paste0("Incorrect password. Attempts: ", rv$admin_fail_count, "/5")
      ))
    }

    NULL
  })

  output$admin_content <- renderUI({
    admin_nonce()
    if (!isTRUE(rv$admin_logged_in)) {
      return(NULL)
    }

    tagList(
      uiOutput("admin_prices_ui"),
      hr(),
      uiOutput("admin_blocked_ui"),
      hr(),
      uiOutput("admin_events_ui"),
      hr(),
      uiOutput("admin_tx_ui")
    )
  })

  poll_count <- reactiveVal(0L)

  clear_cart <- function() rv$cart <- empty_cart_df()

  cart_total_cents <- function(df) {
    as.integer(round(sum(df$quantity * df$unit_price, na.rm = TRUE) * 100))
  }

  infer_tx_type <- function(cart_df) {
    if (is.null(cart_df) || nrow(cart_df) == 0) {
      return("unknown")
    }
    cats <- unique(as.character(cart_df$category %||% ""))
    cats <- cats[nzchar(cats)]
    if (length(cats) == 0) {
      return("unknown")
    }
    if (length(cats) == 1) {
      return(cats[1])
    }
    "mixed"
  }

  qty_int <- function(x, label = "Quantity") {
    if (is.null(x) || length(x) == 0) {
      return(0L)
    }
    v <- suppressWarnings(as.numeric(x))
    if (is.na(v) || v <= 0) {
      return(0L)
    }
    if (abs(v - round(v)) > 1e-9) {
      showNotification(
        paste0(label, " must be a whole number. Rounding to ", as.integer(round(v)), "."),
        type = "warning"
      )
    }
    as.integer(round(v))
  }

  # -----------------------------------------------------------------------------
  # BUYER VALIDATION (centralized)
  # -----------------------------------------------------------------------------

  is_valid_email <- function(x) {
    x <- trimws(as.character(x %||% ""))
    nzchar(x) && grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", x)
  }

  validate_buyer_or_notify <- function(name, email) {
    name <- trimws(as.character(name %||% ""))
    email <- trimws(as.character(email %||% ""))

    if (!nzchar(name) && !nzchar(email)) {
      showNotification("Please enter your name and email address.", type = "warning")
      return(FALSE)
    }
    if (!nzchar(name)) {
      showNotification("Please enter your name.", type = "warning")
      return(FALSE)
    }
    if (!is_valid_email(email)) {
      showNotification("Please enter a valid email address for the receipt.", type = "warning")
      return(FALSE)
    }
    TRUE
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
    if (!nzchar(s)) {
      return("{}")
    }
    obj <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(obj)) {
      return(s)
    }
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
    if (is.null(df) || nrow(df) == 0) {
      return(df)
    }

    df$merge_key <- mapply(
      cart_merge_key,
      df$category, df$description, df$unit_price, df$meta_json,
      USE.NAMES = FALSE
    )

    df$quantity <- suppressWarnings(as.integer(df$quantity))
    df <- df[!is.na(df$quantity) & df$quantity > 0L, , drop = FALSE]
    if (nrow(df) == 0) {
      return(df)
    }

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
    if (!nzchar(item_id)) {
      return(invisible(NULL))
    }

    q <- suppressWarnings(as.integer(new_qty))
    if (is.na(q)) {
      return(invisible(NULL))
    }

    df <- rv$cart
    if (is.null(df) || nrow(df) == 0) {
      return(invisible(NULL))
    }

    idx <- which(df$id == item_id)
    if (length(idx) != 1) {
      return(invisible(NULL))
    }

    cat <- as.character(df$category[idx] %||% "")
    if (identical(cat, "donation")) {
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
    if (!nzchar(item_id)) {
      return(invisible(NULL))
    }

    amt <- suppressWarnings(as.numeric(new_amt))
    if (is.na(amt)) {
      return(invisible(NULL))
    }

    df <- rv$cart
    if (is.null(df) || nrow(df) == 0) {
      return(invisible(NULL))
    }

    idx <- which(df$id == item_id)
    if (length(idx) != 1) {
      return(invisible(NULL))
    }

    cat <- as.character(df$category[idx] %||% "")
    if (!identical(cat, "donation")) {
      return(invisible(NULL))
    }

    if (amt <= 0) {
      cand <- df[df$id != item_id, , drop = FALSE]
    } else {
      cand <- df
      cand$quantity[idx] <- 1L
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
      desc <- as.character(r$description[1] %||% "")
      cat <- as.character(r$category[1] %||% "")
      qty <- suppressWarnings(as.integer(r$quantity[1] %||% 0L))
      price <- suppressWarnings(as.numeric(r$unit_price[1] %||% NA_real_))
      meta_json <- as.character(r$meta_json[1] %||% "")

      if (is.na(qty) || qty < 0L) qty <- 0L
      if (identical(cat, "donation")) qty <- 1L
      if (!identical(cat, "donation") && qty > max_qty) qty <- max_qty

      line_total <- if (!is.na(price)) qty * price else NA_real_

      person_details_ui <- NULL
      if (identical(cat, "season_pass") || identical(cat, "program")) {
        m <- tryCatch(jsonlite::fromJSON(meta_json, simplifyVector = TRUE), error = function(e) NULL)

        nm <- ""
        dob <- ""

        if (!is.null(m)) {
          if (identical(cat, "season_pass")) {
            nm <- as.character(m$holder_name %||% "")
            dob <- as.character(m$holder_dob %||% "")
          } else if (identical(cat, "program")) {
            nm <- as.character(m$participant_name %||% "")
            dob <- as.character(m$participant_dob %||% "")
          }
        }

        nm <- trimws(nm)
        dob <- trimws(dob)

        if (nzchar(nm) || nzchar(dob)) {
          pieces <- c()
          if (nzchar(nm)) pieces <- c(pieces, paste0("Name: ", nm))
          if (nzchar(dob)) pieces <- c(pieces, paste0("DOB: ", dob))

          person_details_ui <- tags$div(
            style = "color:#777; font-size: 0.9em;",
            paste(pieces, collapse = " \u00b7 ")
          )
        }
      }

      left_text <- if (isTRUE(show_category) && nzchar(cat)) {
        tags$div(
          tags$div(desc),
          person_details_ui,
          tags$div(style = "color:#777; font-size: 0.9em;", cat)
        )
      } else {
        tags$div(
          tags$div(desc),
          person_details_ui
        )
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
          tags$div(
            class = "qty-stepper",
            tags$button(
              type          = "button",
              class         = "btn btn-outline-secondary btn-sm qty-dec",
              `data-itemid` = as.character(r$id[1]),
              `data-max`    = as.integer(max_qty),
              "−"
            ),
            tags$span(
              class = "qty-value",
              as.character(qty)
            ),
            tags$button(
              type          = "button",
              class         = "btn btn-outline-secondary btn-sm qty-inc",
              `data-itemid` = as.character(r$id[1]),
              `data-max`    = as.integer(max_qty),
              "+"
            )
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
        // Quantity: click-only stepper (no free typing)
        $(document).off('click', '#%s .qty-dec, #%s .qty-inc');
        $(document).on('click', '#%s .qty-dec, #%s .qty-inc', function(e) {
          e.preventDefault();

          var id   = $(this).data('itemid');
          var maxq = parseInt($(this).data('max'), 10);
          if (isNaN(maxq)) maxq = %d;

          var $wrap = $(this).closest('.qty-stepper');
          var qty = parseInt($wrap.find('.qty-value').text(), 10);
          if (isNaN(qty)) qty = 0;

          var delta = $(this).hasClass('qty-inc') ? 1 : -1;
          qty = qty + delta;

          if (qty < 0) qty = 0;
          if (qty > maxq) qty = maxq;

          Shiny.setInputValue('%s', {id: id, qty: qty, nonce: Math.random()}, {priority: 'event'});
        });

        // Donation amount: keep free typing
        $(document).off('input change', '#%s .cart-amt-input');
        $(document).on('input change', '#%s .cart-amt-input', function() {
          var id = $(this).data('itemid');
          var amt = $(this).val();
          Shiny.setInputValue('%s', {id: id, amt: amt, nonce: Math.random()}, {priority: 'event'});
        });
        ",
        box_id, box_id,
        box_id, box_id,
        as.integer(max_qty),
        change_input_id,
        box_id, box_id, change_input_id
      )))
    )
  }

  # -----------------------------------------------------------------------------
  # SPECIAL EVENT CAPACITY ENFORCEMENT
  # -----------------------------------------------------------------------------

add_rows_to_cart <- function(rows_df) {
    if (is.null(rows_df) || nrow(rows_df) == 0) {
      return(invisible(FALSE))
    }

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
      if (identical(as.character(r$category[1] %||% ""), "donation")) q <- 1L

      k <- r$merge_key[1] %||% ""
      hit <- which(df$merge_key == k)

      if (length(hit) >= 1) {
        j <- hit[1]
        if (identical(as.character(df$category[j] %||% ""), "donation")) {
          df$quantity[j] <- 1L
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

    if (cat != "donation" && q <= 0) {
      return(invisible(FALSE))
    }

    if (is.na(p)) {
      showNotification("Price is N/A. Admin must set prices first.", type = "error")
      return(invisible(FALSE))
    }
    if (p < 0) {
      return(invisible(FALSE))
    }

    if (identical(cat, "donation")) q <- 1L
    q <- min(q, 20L)

    rows_df <- data.frame(
      id = UUIDgenerate(),
      category = cat,
      description = description,
      quantity = q,
      unit_price = p,
      meta_json = as.character(jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null")),
      merge_key = "",
      stringsAsFactors = FALSE
    )

    add_rows_to_cart(rows_df)
  }

  parse_event_id_from_meta <- function(meta_json) {
    s <- as.character(meta_json %||% "")
    if (!nzchar(s)) {
      return("")
    }
    obj <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(obj)) {
      return("")
    }
    as.character(obj$event_id %||% "")
  }

  cart_event_qty_in_session <- function(event_id) {
    if (!nzchar(event_id %||% "")) {
      return(0L)
    }
    df <- rv$cart
    if (is.null(df) || nrow(df) == 0) {
      return(0L)
    }

    ix <- which(df$category == "event")
    if (length(ix) == 0) {
      return(0L)
    }

    q <- 0L
    for (i in ix) {
      eid <- parse_event_id_from_meta(df$meta_json[i] %||% "")
      if (identical(eid, event_id)) q <- q + as.integer(df$quantity[i] %||% 0L)
    }
    as.integer(q)
  }

  event_sold_qty_completed <- function(event_id) {
    if (!nzchar(event_id %||% "")) {
      return(0L)
    }

    x <- db_get1(
      "SELECT COALESCE(SUM(qty), 0) AS sold
     FROM tx_items
     WHERE category = 'event'
       AND item_id  = ?event_id
       AND status IN ('COMPLETED','SANDBOX_TEST_OK')",
      event_id = as.character(event_id)
    )

    sold <- suppressWarnings(as.integer(x$sold[1]))
    if (is.na(sold)) 0L else sold
  }

  # -----------------------------------------------------------------------------
  # PERF: Batch helpers for capacity checks (1 DB round-trip for caps + sold)
  # -----------------------------------------------------------------------------

  sql_in_clause <- function(con, ids) {
    ids <- unique(as.character(ids))
    ids <- ids[nzchar(ids)]
    if (length(ids) == 0) {
      return("(NULL)")
    }
    paste0("(", paste(DBI::dbQuoteString(con, ids), collapse = ","), ")")
  }

  batch_event_caps_and_sold <- function(event_ids) {
    event_ids <- unique(as.character(event_ids))
    event_ids <- event_ids[nzchar(event_ids)]
    if (length(event_ids) == 0) {
      return(list(caps = data.frame(), sold = data.frame()))
    }

    with_db(function(con) {
      in_sql <- sql_in_clause(con, event_ids)

      caps <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT id, capacity, name, event_date
         FROM special_events
         WHERE id IN ", in_sql
        )
      )

      sold <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT item_id, COALESCE(SUM(qty), 0) AS sold
         FROM tx_items
         WHERE category = 'event'
           AND status IN ('COMPLETED','SANDBOX_TEST_OK')
           AND item_id IN ", in_sql,
          " GROUP BY item_id"
        )
      )

      list(caps = caps, sold = sold)
    })
  }

  batch_program_sold <- function(program_ids) {
    program_ids <- unique(as.character(program_ids))
    program_ids <- program_ids[nzchar(program_ids)]
    if (length(program_ids) == 0) {
      return(data.frame(item_id = character(), sold = integer()))
    }

    with_db(function(con) {
      in_sql <- sql_in_clause(con, program_ids)

      DBI::dbGetQuery(
        con,
        paste0(
          "SELECT item_id, COALESCE(SUM(qty), 0) AS sold
         FROM tx_items
         WHERE category = 'program'
           AND status IN ('COMPLETED','SANDBOX_TEST_OK')
           AND item_id IN ", in_sql,
          " GROUP BY item_id"
        )
      )
    })
  }

  validate_event_capacities_for_cart <- function(cart_df) {
    if (is.null(cart_df) || nrow(cart_df) == 0) {
      return(NULL)
    }

    ix <- which(cart_df$category == "event")
    if (length(ix) == 0) {
      return(NULL)
    }

    # requested qty per event_id
    req_by_event <- list()
    for (i in ix) {
      eid <- parse_event_id_from_meta(cart_df$meta_json[i] %||% "")
      if (!nzchar(eid)) next
      req_by_event[[eid]] <- (req_by_event[[eid]] %||% 0L) + as.integer(cart_df$quantity[i] %||% 0L)
    }
    if (length(req_by_event) == 0) {
      return(NULL)
    }

    event_ids <- names(req_by_event)

    snap <- batch_event_caps_and_sold(event_ids)
    caps <- snap$caps
    sold <- snap$sold

    # sold lookup map
    sold_map <- integer()
    if (nrow(sold) > 0) {
      sold_map <- setNames(as.integer(sold$sold), as.character(sold$item_id))
    }

    for (eid in event_ids) {
      cap_row <- caps[caps$id == eid, , drop = FALSE]
      if (nrow(cap_row) != 1) next

      cap <- suppressWarnings(as.integer(cap_row$capacity[1]))
      if (is.na(cap)) next # NA capacity = unlimited

      sold_i <- sold_map[[eid]]
      if (is.null(sold_i) || is.na(sold_i)) sold_i <- 0L

      remaining <- cap - sold_i
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
  # PROGRAM CAPACITY ENFORCEMENT
  # -----------------------------------------------------------------------------

  parse_program_id_from_meta <- function(meta_json) {
    s <- as.character(meta_json %||% "")
    if (!nzchar(s)) {
      return("")
    }
    obj <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(obj)) {
      return("")
    }
    as.character(obj$program_id %||% "")
  }

  cart_program_qty_in_session <- function(program_id) {
    if (!nzchar(program_id %||% "")) {
      return(0L)
    }
    df <- rv$cart
    if (is.null(df) || nrow(df) == 0) {
      return(0L)
    }

    ix <- which(df$category == "program")
    if (length(ix) == 0) {
      return(0L)
    }

    q <- 0L
    for (i in ix) {
      pid <- parse_program_id_from_meta(df$meta_json[i] %||% "")
      if (identical(pid, program_id)) q <- q + as.integer(df$quantity[i] %||% 0L)
    }
    as.integer(q)
  }

  program_sold_qty_completed <- function(program_id) {
    if (!nzchar(program_id %||% "")) {
      return(0L)
    }

    x <- db_get1(
      "SELECT COALESCE(SUM(qty), 0) AS sold
     FROM tx_items
     WHERE category = 'program'
       AND item_id  = ?program_id
       AND status IN ('COMPLETED','SANDBOX_TEST_OK')",
      program_id = as.character(program_id)
    )

    sold <- suppressWarnings(as.integer(x$sold[1]))
    if (is.na(sold)) 0L else sold
  }

  validate_program_capacities_for_cart <- function(cart_df) {
    if (is.null(cart_df) || nrow(cart_df) == 0) {
      return(NULL)
    }

    ix <- which(cart_df$category == "program")
    if (length(ix) == 0) {
      return(NULL)
    }

    # requested qty per program_id
    req_by_program <- list()
    for (i in ix) {
      pid <- parse_program_id_from_meta(cart_df$meta_json[i] %||% "")
      if (!nzchar(pid)) next
      req_by_program[[pid]] <- (req_by_program[[pid]] %||% 0L) + as.integer(cart_df$quantity[i] %||% 0L)
    }
    if (length(req_by_program) == 0) {
      return(NULL)
    }

    program_ids <- names(req_by_program)

    # program caps come from config (cached), so fetch once
    prog <- get_program_list()
    prog_sub <- prog[prog$id %in% program_ids, , drop = FALSE]

    # sold counts: 1 query total
    sold_df <- batch_program_sold(program_ids)

    sold_map <- integer()
    if (nrow(sold_df) > 0) {
      sold_map <- setNames(as.integer(sold_df$sold), as.character(sold_df$item_id))
    }

    for (pid in program_ids) {
      row <- prog_sub[prog_sub$id == pid, , drop = FALSE]
      if (nrow(row) != 1) next

      cap <- suppressWarnings(as.integer(row$capacity[1]))
      if (is.na(cap)) next # NA capacity = unlimited

      sold_i <- sold_map[[pid]]
      if (is.null(sold_i) || is.na(sold_i)) sold_i <- 0L

      remaining <- cap - sold_i
      requested <- as.integer(req_by_program[[pid]] %||% 0L)

      if (remaining <= 0L) {
        return(paste0("Program is full: ", row$name[1], "."))
      }
      if (requested > remaining) {
        return(paste0(
          "Not enough remaining capacity for program: ",
          row$name[1],
          ". Remaining: ", remaining,
          ", requested: ", requested, "."
        ))
      }
    }

    NULL
  }

  # -----------------------------------------------------------------------------
  # RECEIPT LOAD / DB HELPERS
  # -----------------------------------------------------------------------------

  load_receipt_token <- function(token) {
    if (is.null(token) || !nzchar(token)) {
      return(NULL)
    }
    x <- db_get1(
      "SELECT id, created_at, buyer_name, buyer_email, total_amount_cents, currency, cart_json,
              square_checkout_id, square_order_id, receipt_token, status
       FROM transactions
       WHERE receipt_token = ?token
       LIMIT 1",
      token = token
    )
    if (nrow(x) == 1) x else NULL
  }

  update_tx_status <- function(tx_id, status) {
    if (!nzchar(tx_id %||% "")) {
      return(FALSE)
    }

    with_db(function(con) {
      DBI::dbWithTransaction(con, {
        # transactions table
        db_exec(
          con,
          "UPDATE transactions SET status = ?st WHERE id = ?id",
          st = as.character(status),
          id = as.character(tx_id)
        )

        # tx_items table (keep sold-counters correct)
        db_exec(
          con,
          "UPDATE tx_items SET status = ?st WHERE tx_id = ?id",
          st = as.character(status),
          id = as.character(tx_id)
        )
      })
    })

    TRUE
  }









 






  # -----------------------------------------------------------------------------
  # RECEIPT VERIFICATION (Square)  -- with PERF timing
  # -----------------------------------------------------------------------------

  infer_status_from_square <- function(order_id) {
    # returns one of: COMPLETED, PENDING, FAILED, UNKNOWN
    if (!HAVE_SQUARE_CREDS) {
      return("UNKNOWN")
    }
    if (!nzchar(order_id %||% "")) {
      return("UNKNOWN")
    }

    payments <- timed("square_list_payments_by_order()", {
      square_list_payments_by_order(order_id)
    })

    sts <- timed("compute_payment_status()", {
      if (!is.null(payments) && length(payments) > 0) {
        toupper(vapply(payments, function(p) as.character(p$status %||% ""), character(1)))
      } else {
        character(0)
      }
    })

    if (length(sts) > 0) {
      if (any(sts %in% "COMPLETED")) {
        return("COMPLETED")
      }
      if (any(sts %in% c("CANCELED", "FAILED"))) {
        return("FAILED")
      }
      return("PENDING")
    }

    ord <- timed("square_get_order()", {
      square_get_order(order_id)
    })

    if (is.null(ord)) {
      return("UNKNOWN")
    }
    "PENDING"
  }

  verify_and_refresh_receipt <- function() {
    tx <- timed("receipt_tx()", {
      receipt_tx()
    })
    if (is.null(tx) || nrow(tx) != 1) {
      return(FALSE)
    }

    st0 <- as.character(tx$status[1] %||% "")
    if (st0 %in% c("COMPLETED", "SANDBOX_TEST_OK", "FAILED")) {
      return(TRUE)
    }

if (isTRUE(IS_FAKE_MODE)) {
  return(TRUE)
}

    new_st <- timed("infer_status_from_square()", {
      infer_status_from_square(as.character(tx$square_order_id[1] %||% ""))
    })

    if (new_st %in% c("COMPLETED", "FAILED", "PENDING", "UNKNOWN")) {
      if (new_st != "UNKNOWN") {
        timed("update_tx_status()", {
          update_tx_status(as.character(tx$id[1]), new_st)
        })
      }

      timed("receipt_tx_refresh()", {
        receipt_tx(load_receipt_token(as.character(tx$receipt_token[1] %||% "")))
      })
    }

    TRUE
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
    if (nrow(bd) == 0) {
      return(character())
    }
    x <- as.character(bd$date)
    x <- x[!is.na(x) & nzchar(x)]
    x
  })

  # -----------------------------------------------------------------------------
  # CHRISTMAS PASS: constrain dateInput (only when tab visible)
  # -----------------------------------------------------------------------------

  observe({
    if (!identical(input$main_nav, "Christmas Pass")) {
      return()
    }

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

  observeEvent(input$day_date,
    {
      picked <- suppressWarnings(as.Date(input$day_date))
      if (is.null(picked) || is.na(picked)) {
        return()
      }

      blk <- blocked_chr()
      if (length(blk) > 0 && (as.character(picked) %in% blk)) {
        showNotification("This date has been blocked from paying. Please choose another date.", type = "error")
        bump_day_date_ui()
        return()
      }
      last_valid_day_date(picked)
    },
    ignoreInit = TRUE
  )

  # -----------------------------------------------------------------------------
  # DYNAMIC NAVBAR (LAZY TAB BODIES)
  # -----------------------------------------------------------------------------

  output$main_nav_ui <- renderUI({
    config_updated_at()

    tab_on <- function(key, default = TRUE) cfg_bool(key, default)
    tabs <- list()

    if (tab_on("tab_daypass_enabled", TRUE)) {
      tabs <- c(tabs, list(tabPanel("Day Pass", value = "Day Pass", uiOutput("tab_day_ui"))))
    }
    if (tab_on("tab_christmas_enabled", TRUE)) {
      tabs <- c(tabs, list(tabPanel("Christmas Pass", value = "Christmas Pass", uiOutput("tab_xmas_ui"))))
    }
    if (tab_on("tab_season_enabled", TRUE)) {
      tabs <- c(tabs, list(tabPanel("Season Pass", value = "Season Pass", uiOutput("tab_season_ui"))))
    }
    if (tab_on("tab_programs_enabled", TRUE)) {
      tabs <- c(tabs, list(tabPanel("Programs", value = "Programs", uiOutput("tab_prog_ui"))))
    }
    if (tab_on("tab_events_enabled", TRUE)) {
      tabs <- c(tabs, list(tabPanel("Special Events", value = "Special Events", uiOutput("tab_event_ui"))))
    }
    if (tab_on("tab_donation_enabled", TRUE)) {
      tabs <- c(tabs, list(tabPanel("Donation", value = "Donation", uiOutput("tab_don_ui"))))
    }

    # Receipt + Admin always exist (Receipt nav item is hidden until we show it)
    tabs <- c(tabs, list(tabPanel("Receipt", value = "Receipt", uiOutput("tab_receipt_ui"))))
    tabs <- c(tabs, list(tabPanel("Admin", value = "Admin", uiOutput("tab_admin_ui"))))

    do.call(navbarPage, c(list(title = "BVXC", id = "main_nav"), tabs))
  })

  # Hide Receipt tab in the navbar by default (we only show it once a receipt exists)
  session$onFlushed(function() {
    session$sendCustomMessage("toggleReceiptNav", list(show = FALSE))
  }, once = TRUE)

  # Show/hide Receipt tab whenever receipt_tx changes
  observeEvent(receipt_tx(),
    {
      session$sendCustomMessage("toggleReceiptNav", list(show = !is.null(receipt_tx())))
    },
    ignoreInit = TRUE
  )

  # -----------------------------------------------------------------------------
  # TAB BODIES (LAZY): each tab UI is only created when the tab is active
  # -----------------------------------------------------------------------------

  output$tab_day_ui <- renderUI({
    req(identical(input$main_nav, "Day Pass"))
    fluidPage(
      h3("Day Passes"),
      p("Choose your ski day and passes, then add to cart. Review and pay on the right."),
      p(style = "color:#666;", "Age buckets: Child 0–8, Youth 9–18, Adult 19+."),
      fluidRow(
        column(
          4,
          uiOutput("day_date_ui"),
          numericInput("day_adult", "Adult", value = 0, min = 0, step = 1),
          numericInput("day_youth", "Youth", value = 0, min = 0, step = 1),
          numericInput("day_under9", "Under 9", value = 0, min = 0, step = 1),
          numericInput("day_family", "Family", value = 0, min = 0, step = 1),
          br(),
          actionButton("day_add_to_cart", "Add to cart")
        ),
        column(8, checkout_panel_ui("day", "Checkout"))
      )
    )
  })

  output$tab_xmas_ui <- renderUI({
    req(identical(input$main_nav, "Christmas Pass"))
    fluidPage(
      h3("Christmas Pass"),
      p("Choose a 14-day window that must include Dec 25. Add to cart, then pay on the right."),
      fluidRow(
        column(
          4,
          dateInput("xmas_start", "Start date (14-day window)", value = Sys.Date()),
          qty_stepper_input("xmas_qty", "Number of passes", value = 0, min = 0, max = 20),
          br(),
          actionButton("xmas_add_to_cart", "Add to cart")
        ),
        column(8, checkout_panel_ui("xmas", "Checkout"))
      )
    )
  })

  output$tab_season_ui <- renderUI({
    req(identical(input$main_nav, "Season Pass"))
    fluidPage(
      h3("Season Passes"),
      uiOutput("season_info"),
      tags$div(
        style = "margin:8px 0; padding:10px; border:1px solid #ddd; border-radius:6px; background:#fafafa;",
        tags$strong("Age enforcement: "),
        "Adult requires age 19+; Youth requires age 9–18 (age as-of Dec 31 of the season)."
      ),
      fluidRow(
        column(
          4,
          numericInput("season_adult", "Adult", value = 0, min = 0, step = 1),
          numericInput("season_youth", "Youth", value = 0, min = 0, step = 1),
          uiOutput("season_people_ui"),
          br(),
          actionButton("season_add_to_cart", "Add to cart")
        ),
        column(8, checkout_panel_ui("season", "Checkout"))
      )
    )
  })

  output$tab_prog_ui <- renderUI({
    req(identical(input$main_nav, "Programs"))

    # Invalidate this UI when config changes (so program list/prices/caps refresh)
    config_updated_at()

    # Compute program list only when Programs tab is opened OR config changes
    prog <- get_program_list()

    fluidPage(
      h3("Programs"),
      tags$div(
        style = "margin:8px 0; padding:10px; border:1px solid #ddd; border-radius:6px; background:#fafafa;",
        tags$strong("Age rule: "),
        "Program eligibility is checked using age as-of Dec 31 of the ski season."
      ),
      p("Select a program and number of participants, then add to cart. Review and pay on the right."),
      fluidRow(
        column(
          4,
          selectInput("program_choice", "Program", choices = setNames(prog$id, prog$name)),
          numericInput("program_qty", "Number of participants", value = 0, min = 0, step = 1),
          uiOutput("program_people_ui"),
          br(),
          actionButton("program_add_to_cart", "Add to cart")
        ),
        column(8, checkout_panel_ui("prog", "Checkout"))
      )
    )
  })

  output$tab_event_ui <- renderUI({
    req(identical(input$main_nav, "Special Events"))
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
  })

  output$tab_don_ui <- renderUI({
    req(identical(input$main_nav, "Donation"))
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
          numericInput("donation_amount", "Donation amount (CAD)", value = 0, min = 0, step = 10),
          br(),
          actionButton("donate_add_to_cart", "Add donation to cart")
        ),
        column(8, checkout_panel_ui("don", "Checkout"))
      )
    )
  })

  output$tab_receipt_ui <- renderUI({
    req(identical(input$main_nav, "Receipt"))
    fluidPage(
      h3("Payment receipt"),
      uiOutput("receipt_panel")
    )
  })

  output$tab_admin_ui <- renderUI({
    req(identical(input$main_nav, "Admin"))
    fluidPage(
      h3("Admin"),
      tags$p(APP_VERSION),
      tags$p(ENV_LABEL),
      if (db_is_postgres()) {
        tags$div(
          style = "margin:8px 0; padding:10px; border:1px solid #ddd; border-radius:6px; background:#fafafa;",
          tags$strong("Database: "), "Postgres (BVXC_DB_URL set)"
        )
      } else {
        tags$div(
          style = "margin:8px 0; padding:10px; border:1px solid #ffc107; border-radius:6px; background:#fff8e1;",
          tags$strong("Database: "), "SQLite fallback (dev only). Do not use this on Connect Cloud for persistence."
        )
      },
      hr(),
      passwordInput("admin_password", "Admin password"),
      actionButton("admin_login", "Log in"),
      uiOutput("admin_lock_msg"),
      br(),
      uiOutput("admin_content")
    )
  })

  # -----------------------------------------------------------------------------
  # Auto-load receipt from URL (?receipt=TOKEN)
  # -----------------------------------------------------------------------------

  observeEvent(session$clientData$url_search,
    {
      qs <- session$clientData$url_search %||% ""
      if (!nzchar(qs)) {
        return()
      }

      q <- tryCatch(parseQueryString(qs), error = function(e) list())
      token <- trimws(as.character(q$receipt %||% ""))
      if (!nzchar(token)) {
        return()
      }

      tx <- load_receipt_token(token)
      if (is.null(tx)) {
        return()
      }

      # If it's already loaded, don't re-run everything (prevents loops / extra work)
      cur <- receipt_tx()
      if (!is.null(cur) && nrow(cur) == 1 &&
        identical(as.character(cur$receipt_token[1] %||% ""), token)) {
        return()
      }

      receipt_tx(tx)

      # Ensure Receipt tab is visible in navbar when deep-linking
      session$sendCustomMessage("toggleReceiptNav", list(show = TRUE))

      # Clear cart ONLY if this receipt matches the checkout we just started
      if (isTRUE(rv$checkout_started) && identical(token, rv$checkout_token)) {
        clear_cart()
        rv$checkout_started <- FALSE
        rv$checkout_token <- ""
      }

      poll_count(0L)
      updateTabsetPanel(session, "main_nav", selected = "Receipt")
    },
    ignoreInit = FALSE
  )

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
        if (!(as.character(d) %in% blk)) {
          v <- d
          break
        }
      }
    }
    last_valid_day_date(v)

    dateInput("day_date", "Ski date", value = v, min = min_d, max = max_d)
  })

  # -----------------------------------------------------------------------------
  # PER-TAB CHECKOUT WIRING (cart shown on every tab)
  # -----------------------------------------------------------------------------

  checkout_prefixes <- c("day", "xmas", "season", "prog", "event", "don")

  for (p in checkout_prefixes) {
    local({
      prefix <- p
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
        if (is.null(df) || nrow(df) == 0) {
          return("Cart is empty.")
        }
        tot <- sum(df$quantity * df$unit_price, na.rm = TRUE)
        paste0("Total: $", sprintf("%.2f", tot))
      })

      output[[paste0(prefix, "_cart_pay_ui")]] <- renderUI({
        has_items <- !is.null(rv$cart) && nrow(rv$cart) > 0
        actionButton(
          inputId = paste0(prefix, "_cart_pay"),
          label   = "Pay now",
          class   = paste("btn", if (has_items) "pay-now-hot" else "")
        )
      })

      # Debounce cart edit events coming from JS-driven inputs
      cart_change <- reactive(input[[change_id]])
      cart_change_d <- debounce(cart_change, millis = 300)

      observeEvent(cart_change_d(),
        {
          x <- cart_change_d()
          if (is.null(x) || is.null(x$id)) {
            return()
          }

          if (!is.null(x$qty)) {
            set_cart_qty(x$id, x$qty)
          } else if (!is.null(x$amt)) {
            set_cart_amount(x$id, x$amt)
          }
        },
        ignoreInit = TRUE
      )

      observeEvent(input[[paste0(prefix, "_cart_clear")]],
        {
          clear_cart()
        },
        ignoreInit = TRUE
      )

      observeEvent(input[[paste0(prefix, "_cart_pay")]],
        {
          nm <- input[[paste0(prefix, "_buyer_name")]] %||% ""
          em <- input[[paste0(prefix, "_buyer_email")]] %||% ""

          if (!validate_buyer_or_notify(nm, em)) {
            return()
          }

          rv$buyer_name <- trimws(as.character(nm))
          rv$buyer_email <- trimws(as.character(em))

          do_checkout(source = prefix)
        },
        ignoreInit = TRUE
      )
    })
  }

  # -----------------------------------------------------------------------------
  # CHECKOUT (single path used by all Pay buttons)
  # -----------------------------------------------------------------------------

  do_checkout <- function(source = "tab") {
    # Prevent double-click / repeat submits
    if (isTRUE(rv$checkout_lock)) {
      showNotification("Checkout already started. Please wait.", type = "warning")
      return()
    }
    rv$checkout_lock <- TRUE
    on.exit(
      {
        rv$checkout_lock <- FALSE
      },
      add = TRUE
    )

    df <- rv$cart
    if (is.null(df) || nrow(df) == 0) {
      showNotification("Cart is empty.", type = "warning")
      return()
    }

    buyer_name <- trimws(as.character(rv$buyer_name %||% ""))
    buyer_email <- trimws(as.character(rv$buyer_email %||% ""))

    if (!validate_buyer_or_notify(buyer_name, buyer_email)) {
      return()
    }

    # Normalize/merge cart lines before checks
    df <- normalize_cart(df)

    # ---- PERF: checkout prechecks (run ONCE) ----
    msg <- timed("validate_cart_limits()", validate_cart_limits(df))
    if (!is.null(msg)) {
      showNotification(msg, type = "error")
      return()
    }

    cap_msg <- timed("validate_event_capacities()", validate_event_capacities_for_cart(df))
    if (!is.null(cap_msg)) {
      showNotification(cap_msg, type = "error")
      return()
    }

    prog_msg <- timed("validate_program_capacities()", validate_program_capacities_for_cart(df))
    if (!is.null(prog_msg)) {
      showNotification(prog_msg, type = "error")
      return()
    }

    tx_type <- infer_tx_type(df)
    total_cents <- cart_total_cents(df)

# ---------------------------------------------------------------------------
# Mode switch: Fake vs Square
# ---------------------------------------------------------------------------

# If not in Square mode, always use fake mode (dev/test path)
if (!isTRUE(IS_SQUARE_MODE)) {
  receipt_token <- UUIDgenerate()
  tx_id <- UUIDgenerate()
  created_at <- now_ts()
  status0 <- "SANDBOX_TEST_OK"

  ok <- timed("DB insert sandbox transaction", tryCatch(
    {
      db_exec1(
        "INSERT INTO transactions (
         id, created_at, buyer_name, buyer_email, total_amount_cents, currency, cart_json,
         tx_type,
         square_checkout_id, square_order_id, receipt_token, status
       ) VALUES (
         ?id, ?created_at, ?buyer_name, ?buyer_email, ?total_cents, 'CAD', ?cart_json,
         ?tx_type,
         NULL, NULL, ?receipt_token, ?status
       )",
        id            = tx_id,
        created_at    = created_at,
        buyer_name    = buyer_name,
        buyer_email   = buyer_email,
        total_cents   = total_cents,
        cart_json     = as.character(jsonlite::toJSON(df, auto_unbox = TRUE, null = "null")),
        tx_type       = tx_type,
        receipt_token = receipt_token,
        status        = status0
      )
      TRUE
    },
    error = function(e) {
      showNotification(paste("DB error saving sandbox transaction:", conditionMessage(e)), type = "error")
      FALSE
    }
  ))

  if (!ok) {
    return()
  }

  timed("tx_items insert (sandbox)", insert_tx_items_for_cart(
    tx_id      = tx_id,
    created_at = created_at,
    status     = status0,
    cart_df    = df
  ))

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

# Square mode (creds guaranteed by global enforcement)
stopifnot(isTRUE(HAVE_SQUARE_CREDS))

# ---------------------------------------------------------------------------
# Square mode
# ---------------------------------------------------------------------------

receipt_token <- UUIDgenerate()
redirect_url <- build_redirect_url(receipt_token)

res <- timed("Square payment-link POST", create_square_checkout_from_cart(
  cart_df      = df,
  buyer_email  = buyer_email,
  note         = paste("BVXC", if (SQUARE_ENV == "sandbox") "sandbox" else "production", source, "checkout"),
  redirect_url = redirect_url
))

if (is.null(res) || !nzchar(res$checkout_url %||% "")) {
  showNotification("Unable to start checkout. Please try again.", type = "error")
  return()
}

tx_id <- UUIDgenerate()
created_at <- now_ts()
status0 <- "PENDING"

ok <- timed("DB insert transaction (PENDING)", tryCatch(
  {
    db_exec1(
      "INSERT INTO transactions (
       id, created_at, buyer_name, buyer_email, total_amount_cents, currency, cart_json,
       tx_type,
       square_checkout_id, square_order_id, receipt_token, status
     ) VALUES (
       ?id, ?created_at, ?buyer_name, ?buyer_email, ?total_cents, 'CAD', ?cart_json,
       ?tx_type,
       ?checkout_id, ?order_id, ?receipt_token, ?status
     )",
      id            = tx_id,
      created_at    = created_at,
      buyer_name    = buyer_name,
      buyer_email   = buyer_email,
      total_cents   = total_cents,
      cart_json     = as.character(jsonlite::toJSON(df, auto_unbox = TRUE, null = "null")),
      tx_type       = tx_type,
      checkout_id   = res$checkout_id %||% NA_character_,
      order_id      = res$square_order %||% NA_character_,
      receipt_token = receipt_token,
      status        = status0
    )
    TRUE
  },
  error = function(e) {
    showNotification(paste("DB error saving transaction:", conditionMessage(e)), type = "error")
    FALSE
  }
))

if (!ok) {
  return()
}

timed("tx_items insert (PENDING)", insert_tx_items_for_cart(
  tx_id      = tx_id,
  created_at = created_at,
  status     = status0,
  cart_df    = df
))

rv$checkout_started <- TRUE
rv$checkout_token <- receipt_token

session$sendCustomMessage("redirect", list(url = res$checkout_url))
# DO NOT clear cart here; clear only after buyer returns with ?receipt=...

return(invisible(TRUE))
} # END do_checkout

  # -----------------------------------------------------------------------------
  # DAY PASS
  # -----------------------------------------------------------------------------

  observeEvent(input$day_add_to_cart, {
    w <- season_win()
    d <- suppressWarnings(as.Date(input$day_date))
    if (is.na(d)) {
      showNotification("Please select a valid ski date.", type = "error")
      return()
    }

    if (d < w$start || d > w$end) {
      showNotification("That date is outside the current season window.", type = "error")
      return()
    }

    if (as.character(d) %in% blocked_chr()) {
      showNotification("That date is blocked. Choose another date.", type = "error")
      bump_day_date_ui()
      return()
    }

    prices <- get_day_prices()
    pr <- setNames(prices$price, prices$type)

    qa <- qty_int(input$day_adult, "Adult")
    qy <- qty_int(input$day_youth, "Youth")
    qu <- qty_int(input$day_under9, "Under 9")
    qf <- qty_int(input$day_family, "Family")

    rows <- list()

    add_row <- function(type, qty, price) {
      if (qty <= 0) {
        return(TRUE)
      }
      p <- suppressWarnings(as.numeric(price))
      if (is.na(p)) {
        showNotification("Price is N/A. Admin must set prices first.", type = "error")
        return(FALSE)
      }
      rows[[length(rows) + 1L]] <<- data.frame(
        id = UUIDgenerate(),
        category = "day_pass",
        description = paste("Day pass –", type, "–", as.character(d)),
        quantity = as.integer(min(qty, 20L)),
        unit_price = p,
        meta_json = as.character(jsonlite::toJSON(list(type = type, date = as.character(d)), auto_unbox = TRUE, null = "null")),
        merge_key = "",
        stringsAsFactors = FALSE
      )
      TRUE
    }

    ok <- add_row("Adult", qa, pr[["Adult"]])
    if (!ok) {
      return()
    }
    ok <- add_row("Youth", qy, pr[["Youth"]])
    if (!ok) {
      return()
    }
    ok <- add_row("Under 9", qu, pr[["Under 9"]])
    if (!ok) {
      return()
    }
    ok <- add_row("Family", qf, pr[["Family"]])
    if (!ok) {
      return()
    }

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
    if (is.na(cutoff)) {
      return(p("Early-bird cutoff is not set. Admin can set it in the Admin tab."))
    }
    is_eb <- Sys.Date() <= cutoff
    p(
      if (is_eb) {
        paste("Early-bird pricing in effect until", format(cutoff, "%Y-%m-%d"))
      } else {
        paste("Regular pricing (early-bird ended", format(cutoff, "%Y-%m-%d"), ")")
      }
    )
  })

  is_valid_dob <- function(d) {
    d <- suppressWarnings(as.Date(d))
    !is.na(d) && d <= Sys.Date()
  }

  output$season_people_ui <- renderUI({
    qa <- suppressWarnings(as.integer(input$season_adult %||% 0L))
    qy <- suppressWarnings(as.integer(input$season_youth %||% 0L))
    qa <- max(0L, min(20L, qa))
    qy <- max(0L, min(20L, qy))

    if ((qa + qy) <= 0L) {
      return(NULL)
    }

    tagList(
      if (qa > 0L) {
        tagList(
          tags$hr(),
          tags$h4("Adult pass holders"),
          lapply(seq_len(qa), function(i) {
            tagList(
              textInput(paste0("season_adult_name_", i), paste0("Adult ", i, " – Name"), value = ""),
              dateInput(paste0("season_adult_dob_", i), paste0("Adult ", i, " – Date of birth"), value = NULL)
            )
          })
        )
      } else {
        NULL
      },
      if (qy > 0L) {
        tagList(
          tags$hr(),
          tags$h4("Youth pass holders"),
          lapply(seq_len(qy), function(i) {
            tagList(
              textInput(paste0("season_youth_name_", i), paste0("Youth ", i, " – Name"), value = ""),
              dateInput(paste0("season_youth_dob_", i), paste0("Youth ", i, " – Date of birth"), value = NULL)
            )
          })
        )
      } else {
        NULL
      }
    )
  })

  output$program_people_ui <- renderUI({
    qty <- suppressWarnings(as.integer(input$program_qty %||% 0L))
    qty <- max(0L, min(20L, qty))
    if (qty <= 0L) {
      return(NULL)
    }

    tagList(
      tags$hr(),
      tags$h4("Program participants"),
      lapply(seq_len(qty), function(i) {
        tagList(
          textInput(paste0("program_name_", i), paste0("Participant ", i, " – Name"), value = ""),
          dateInput(paste0("program_dob_", i), paste0("Participant ", i, " – Date of birth"), value = NULL)
        )
      })
    )
  })

  observeEvent(input$season_add_to_cart, {
    cutoff <- get_early_bird_cutoff()
    is_eb <- if (is.na(cutoff)) FALSE else (Sys.Date() <= cutoff)

    prices <- get_season_prices(is_eb)
    pr <- setNames(prices$price, prices$type)

    qa <- qty_int(input$season_adult, "Adult")
    qy <- qty_int(input$season_youth, "Youth")

    if ((qa + qy) <= 0L) {
      showNotification("Nothing to add.", type = "warning")
      return()
    }

    p_adult <- suppressWarnings(as.numeric(pr[["Adult"]]))
    p_youth <- suppressWarnings(as.numeric(pr[["Youth"]]))

    if (qa > 0L && (is.na(p_adult) || p_adult < 0)) {
      showNotification("Adult season pass price is N/A. Admin must set prices first.", type = "error")
      return()
    }
    if (qy > 0L && (is.na(p_youth) || p_youth < 0)) {
      showNotification("Youth season pass price is N/A. Admin must set prices first.", type = "error")
      return()
    }

    age_ref <- season_age_ref_date(Sys.Date())

    rows <- list()

    # Adults: one row per person (Name + DOB required, age check)
    if (qa > 0L) {
      for (i in seq_len(qa)) {
        nm <- trimws(as.character(input[[paste0("season_adult_name_", i)]] %||% ""))
        dob <- input[[paste0("season_adult_dob_", i)]]

        if (!nzchar(nm)) {
          showNotification(paste0("Adult ", i, ": name is required."), type = "error")
          return()
        }
        if (!is_valid_dob(dob)) {
          showNotification(paste0("Adult ", i, ": date of birth is required and must be valid."), type = "error")
          return()
        }

        age <- age_years_on(dob, age_ref)
        if (!identical(bvxc_age_bucket(age), "Adult")) {
          showNotification(
            paste0(
              "Adult ", i, ": Adult season pass requires age 19+ (age is ", age,
              " as of ", as.character(age_ref), ")."
            ),
            type = "error"
          )
          return()
        }

        rows[[length(rows) + 1L]] <- data.frame(
          id = UUIDgenerate(),
          category = "season_pass",
          description = "Season pass – Adult",
          quantity = 1L,
          unit_price = p_adult,
          meta_json = as.character(jsonlite::toJSON(
            list(
              type        = "Adult",
              early_bird  = is_eb,
              holder_uid  = UUIDgenerate(), # prevents merge collapse
              holder_name = nm,
              holder_dob  = as.character(as.Date(dob)),
              age_ref     = as.character(age_ref),
              age_years   = as.integer(age)
            ),
            auto_unbox = TRUE, null = "null"
          )),
          merge_key = "",
          stringsAsFactors = FALSE
        )
      }
    }

    # Youth: one row per person (Name + DOB required, age check)
    if (qy > 0L) {
      for (i in seq_len(qy)) {
        nm <- trimws(as.character(input[[paste0("season_youth_name_", i)]] %||% ""))
        dob <- input[[paste0("season_youth_dob_", i)]]

        if (!nzchar(nm)) {
          showNotification(paste0("Youth ", i, ": name is required."), type = "error")
          return()
        }
        if (!is_valid_dob(dob)) {
          showNotification(paste0("Youth ", i, ": date of birth is required and must be valid."), type = "error")
          return()
        }

        age <- age_years_on(dob, age_ref)
        if (!identical(bvxc_age_bucket(age), "Youth")) {
          showNotification(
            paste0(
              "Youth ", i, ": Youth season pass requires age 9–18 (age is ", age,
              " as of ", as.character(age_ref), ")."
            ),
            type = "error"
          )
          return()
        }

        rows[[length(rows) + 1L]] <- data.frame(
          id = UUIDgenerate(),
          category = "season_pass",
          description = "Season pass – Youth",
          quantity = 1L,
          unit_price = p_youth,
          meta_json = as.character(jsonlite::toJSON(
            list(
              type        = "Youth",
              early_bird  = is_eb,
              holder_uid  = UUIDgenerate(), # prevents merge collapse
              holder_name = nm,
              holder_dob  = as.character(as.Date(dob)),
              age_ref     = as.character(age_ref),
              age_years   = as.integer(age)
            ),
            auto_unbox = TRUE, null = "null"
          )),
          merge_key = "",
          stringsAsFactors = FALSE
        )
      }
    }

    add_rows_to_cart(do.call(rbind, rows))
  })

  # -----------------------------------------------------------------------------
  # PROGRAMS
  # -----------------------------------------------------------------------------

  observeEvent(input$program_add_to_cart, {
    programs <- get_program_list()
    id <- as.character(input$program_choice %||% "")
    qty <- qty_int(input$program_qty, "Participants")

    row <- programs[programs$id == id, , drop = FALSE]
    if (nrow(row) == 0 || qty <= 0L) {
      return()
    }

    # Price guard
    price <- suppressWarnings(as.numeric(row$price[1]))
    if (is.na(price) || price < 0) {
      showNotification("Program price is N/A. Admin must set prices first.", type = "error")
      return()
    }

    # Enforce program capacity if configured (blank/NA = unlimited)
    cap <- suppressWarnings(as.integer(row$capacity[1]))
    if (!is.na(cap)) {
      sold <- program_sold_qty_completed(id)
      in_cart <- cart_program_qty_in_session(id)
      remaining <- cap - sold - in_cart

      if (remaining <= 0L) {
        showNotification("This program is full.", type = "error")
        return()
      }
      if (qty > remaining) {
        showNotification(paste0("Only ", remaining, " spots remaining. Adjusting quantity."), type = "warning")
        qty <- remaining
      }
    }

    # Age enforcement (Dec 31 of season)
    age_ref <- season_age_ref_date(Sys.Date())
    rule <- program_age_rule(id, row$name[1])

    rows <- vector("list", qty)

    for (i in seq_len(qty)) {
      nm <- trimws(as.character(input[[paste0("program_name_", i)]] %||% ""))
      dob <- input[[paste0("program_dob_", i)]]

      if (!nzchar(nm)) {
        showNotification(paste0("Participant ", i, ": name is required."), type = "error")
        return()
      }
      if (!is_valid_dob(dob)) {
        showNotification(paste0("Participant ", i, ": date of birth is required and must be valid."), type = "error")
        return()
      }

      age <- age_years_on(dob, age_ref)

      if (!is.na(rule$min) && age < rule$min) {
        showNotification(
          paste0(
            "Participant ", i, ": wrong age for ", row$name[1], ". Requires ",
            rule$min, if (!is.na(rule$max)) paste0("–", rule$max) else "+",
            " (age is ", age, " as of ", as.character(age_ref), ")."
          ),
          type = "error"
        )
        return()
      }
      if (!is.na(rule$max) && age > rule$max) {
        showNotification(
          paste0(
            "Participant ", i, ": wrong age for ", row$name[1], ". Requires ",
            rule$min, "–", rule$max,
            " (age is ", age, " as of ", as.character(age_ref), ")."
          ),
          type = "error"
        )
        return()
      }

      rows[[i]] <- data.frame(
        id = UUIDgenerate(),
        category = "program",
        description = paste("Program –", row$name[1]),
        quantity = 1L,
        unit_price = price,
        meta_json = as.character(jsonlite::toJSON(
          list(
            program_id       = row$id[1],
            program_name     = row$name[1],
            participant_uid  = UUIDgenerate(), # prevents merge collapse
            participant_name = nm,
            participant_dob  = as.character(as.Date(dob)),
            age_ref          = as.character(age_ref),
            age_years        = as.integer(age)
          ),
          auto_unbox = TRUE, null = "null"
        )),
        merge_key = "",
        stringsAsFactors = FALSE
      )
    }

    add_rows_to_cart(do.call(rbind, rows))
  })

  # -----------------------------------------------------------------------------
  # SPECIAL EVENTS
  # -----------------------------------------------------------------------------

  output$event_picker_ui <- renderUI({
    events_nonce()
    ev <- get_special_events(enabled_only = TRUE)
    if (nrow(ev) == 0) {
      return(tags$div(style = "color:#666;", "No events available right now."))
    }
    choices <- setNames(ev$id, paste0(ev$name, " (", ev$event_date, ")"))
    selectInput("event_choice", "Event", choices = choices)
  })

  observeEvent(input$event_add_to_cart, {
    events_nonce()
    ev <- get_special_events(enabled_only = TRUE)

    id <- input$event_choice %||% ""
    qty <- qty_int(input$event_qty, "Participants")
    if (!nzchar(id) || qty <= 0 || nrow(ev) == 0) {
      return()
    }

    row <- ev[ev$id == id, , drop = FALSE]
    if (nrow(row) == 0) {
      return()
    }

    cap <- suppressWarnings(as.integer(row$capacity[1]))
    if (!is.na(cap)) {
      sold <- event_sold_qty_completed(id)
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
      meta        = list()
    )

    showNotification(paste0("Donation of $", sprintf("%.2f", amt), " added to cart."), type = "message")
  })

  # -----------------------------------------------------------------------------
  # RECEIPT PAGE (UI + STATUS POLL)
  # -----------------------------------------------------------------------------

  observeEvent(receipt_tx(),
    {
      poll_count(0L)
    },
    ignoreInit = TRUE
  )

  observe({
    tx <- receipt_tx()
    if (is.null(tx) || nrow(tx) != 1) {
      return()
    }

    st <- as.character(tx$status[1] %||% "")
    if (st %in% c("COMPLETED", "FAILED", "SANDBOX_TEST_OK")) {
      return()
    }

    n <- poll_count()
    if (n >= 12) {
      return()
    }

    invalidateLater(5000, session)
    poll_count(n + 1L)
    verify_and_refresh_receipt()
  })

  observeEvent(input$receipt_refresh,
    {
      verify_and_refresh_receipt()
    },
    ignoreInit = TRUE
  )

  observeEvent(input$receipt_clear,
    {
      receipt_tx(NULL)
      session$sendCustomMessage("toggleReceiptNav", list(show = FALSE))
      updateTabsetPanel(session, "main_nav", selected = "Day Pass")
    },
    ignoreInit = TRUE
  )

  receipt_status_class <- function(st) {
    st <- toupper(trimws(st %||% ""))
    if (st %in% c("COMPLETED", "SANDBOX_TEST_OK")) {
      return("receipt-ok")
    }
    if (st %in% c("FAILED")) {
      return("receipt-bad")
    }
    "receipt-neutral"
  }

  receipt_status_title <- function(st) {
    st <- toupper(trimws(st %||% ""))
    if (st %in% c("COMPLETED")) {
      return("COMPLETED")
    }
    if (st %in% c("SANDBOX_TEST_OK")) {
      return("SANDBOX TEST OK")
    }
    if (st %in% c("FAILED")) {
      return("FAILED")
    }
    if (st %in% c("PENDING", "PENDING_SANDBOX", "UNKNOWN", "")) {
      return("PENDING")
    }
    st
  }

  safe_money <- function(cents, currency = "CAD") {
    cents <- suppressWarnings(as.integer(cents))
    if (is.na(cents)) {
      return(paste0(currency, " ", "N/A"))
    }
    paste0("$", sprintf("%.2f", cents / 100))
  }

  receipt_url_for_token <- function(token) {
    if (!nzchar(token %||% "")) {
      return("")
    }

    if (!is.na(RETURN_BASE_URL) && nzchar(RETURN_BASE_URL)) {
      return(paste0(RETURN_BASE_URL, "/?receipt=", token))
    }

    base <- session$clientData$url_protocol %||% ""
    host <- session$clientData$url_hostname %||% ""
    port <- session$clientData$url_port %||% ""
    path <- session$clientData$url_pathname %||% "/"

    if (nzchar(port)) host <- paste0(host, ":", port)

    paste0(base, "//", host, path, "?receipt=", token)
  }

  output$receipt_qr <- renderImage(
    {
      tx <- receipt_tx()
      if (is.null(tx) || nrow(tx) != 1) {
        return(NULL)
      }

      st <- toupper(trimws(as.character(tx$status[1] %||% "")))
      if (!st %in% c("COMPLETED", "SANDBOX_TEST_OK")) {
        return(NULL)
      }

      token <- as.character(tx$receipt_token[1] %||% "")
      if (!nzchar(token)) {
        return(NULL)
      }

      # Reuse cached QR file if still valid
      if (identical(rv$receipt_qr_token, token) &&
        nzchar(rv$receipt_qr_file) &&
        file.exists(rv$receipt_qr_file)) {
        return(list(
          src = rv$receipt_qr_file,
          contentType = "image/png",
          width = 220,
          height = 220,
          alt = "Receipt QR"
        ))
      }

      url <- receipt_url_for_token(token)
      if (!nzchar(url)) {
        return(NULL)
      }

      # Clean up any previous cached file
      if (nzchar(rv$receipt_qr_file) && file.exists(rv$receipt_qr_file)) {
        try(unlink(rv$receipt_qr_file), silent = TRUE)
      }

      tf <- tempfile(fileext = ".png")
      qr <- qrcode::qr_code(url)

      grDevices::png(filename = tf, width = 360, height = 360)
      op <- par(mar = c(0, 0, 0, 0))
      on.exit(
        {
          par(op)
          grDevices::dev.off()
        },
        add = TRUE
      )
      plot(qr)

      rv$receipt_qr_token <- token
      rv$receipt_qr_file <- tf

      list(
        src = tf,
        contentType = "image/png",
        width = 220,
        height = 220,
        alt = "Receipt QR"
      )
    },
    deleteFile = FALSE
  )

  output$receipt_items <- renderUI({
    tx <- receipt_tx()
    if (is.null(tx) || nrow(tx) != 1) {
      return(NULL)
    }

    cart_df <- receipt_cart_df()
    if (is.null(cart_df) || nrow(cart_df) == 0) {
      return(tags$div("No cart details."))
    }

    need_cols <- c("category", "description", "quantity", "unit_price", "meta_json")
    if (!all(need_cols %in% names(cart_df))) {
      return(tags$div("Cart format is missing expected fields."))
    }

    cart_df$line_total <- suppressWarnings(as.numeric(cart_df$quantity) * as.numeric(cart_df$unit_price))

tagList(
  tags$h4("Items"),
  tags$table(
    class = "table table-condensed",
    tags$thead(
      tags$tr(
        tags$th("Description"),
        tags$th("Qty"),
        tags$th("Unit"),
        tags$th("Line total")
      )
    ),
    tags$tbody(
      lapply(seq_len(nrow(cart_df)), function(i) {

        desc <- as.character(cart_df$label[i] %||% cart_df$description[i] %||% "")
        qty  <- as.integer(cart_df$quantity[i] %||% 0)
        unit <- as.numeric(cart_df$unit_price[i] %||% 0)
        line <- as.numeric(cart_df$line_total[i] %||% 0)

        tags$tr(
          tags$td(desc),
          tags$td(qty),
          tags$td(sprintf("$%.2f", unit)),
          tags$td(sprintf("$%.2f", line))
        )
      })
    )
  )
)
})

  output$receipt_panel <- renderUI({
    tx <- receipt_tx()
    if (is.null(tx) || nrow(tx) != 1) {
      return(tagList(
        tags$div(
          class = "receipt-card receipt-neutral",
          tags$div(class = "receipt-title", "No receipt loaded"),
          tags$div(class = "receipt-sub", "If you have a receipt token, open: /?receipt=YOUR_TOKEN")
        ),
        br(),
        textInput("receipt_lookup_token", "Lookup receipt token", value = ""),
        actionButton("receipt_lookup_btn", "Load receipt")
      ))
    }

    st <- as.character(tx$status[1] %||% "")
    cls <- receipt_status_class(st)
    ttl <- receipt_status_title(st)

    is_final_ok <- toupper(trimws(st %||% "")) %in% c("COMPLETED", "SANDBOX_TEST_OK")

    token <- as.character(tx$receipt_token[1] %||% "")
    ord <- as.character(tx$square_order_id[1] %||% "")
    chk <- as.character(tx$square_checkout_id[1] %||% "")

    buyer_nm <- trimws(as.character(tx$buyer_name[1] %||% ""))
    thank_you_line <- if (nzchar(buyer_nm)) {
      paste0("Thank you ", buyer_nm, " for supporting the Bulkley Valley Cross Country Ski Club.")
    } else {
      "Thank you for supporting the Bulkley Valley Cross Country Ski Club."
    }

    polling_note <- if (!is_final_ok) {
      tags$span(
        tags$span(class = "receipt-spinner"),
        " Payment not confirmed yet. Do not accept this screen as proof of payment."
      )
    } else {
      NULL
    }

    tagList(
      tags$div(
        class = paste("receipt-card", cls),
        tags$div(class = "receipt-title", ttl),
        tags$div(class = "receipt-sub", thank_you_line),
        tags$div(class = "receipt-sub", polling_note),
        tags$div(
          style = "margin-top:10px;",
          tags$div(tags$strong("Date: "), as.character(tx$created_at[1] %||% "")),
          tags$div(tags$strong("Name: "), as.character(tx$buyer_name[1] %||% "")),
          tags$div(tags$strong("Email: "), as.character(tx$buyer_email[1] %||% "")),
          tags$div(tags$strong("Total: "), safe_money(
            tx$total_amount_cents[1] %||% NA_integer_,
            tx$currency[1] %||% "CAD"
          )),
          tags$div(tags$strong("Receipt token: "), token),
          if (nzchar(ord)) tags$div(tags$strong("Square order id: "), ord) else NULL,
          if (nzchar(chk)) tags$div(tags$strong("Square checkout id: "), chk) else NULL
        )
      ),
      br(),
      fluidRow(
        column(
          4,
          tags$h4("Receipt QR"),
          if (is_final_ok) {
            tagList(
              imageOutput("receipt_qr"),
              tags$div(
                style = "margin-top:8px; color:#666; font-size:0.95em;",
                "Scan to reload this receipt status."
              )
            )
          } else {
            tags$div(style = "color:#666;", "QR is available only after payment is confirmed.")
          }
        ),
        column(8, uiOutput("receipt_items"))
      ),
      br(),
      actionButton("receipt_refresh", "Refresh status"),
      actionButton("receipt_clear", "Clear receipt")
    )
  })

  observeEvent(input$receipt_lookup_btn,
    {
      token <- trimws(input$receipt_lookup_token %||% "")
      if (!nzchar(token)) {
        return()
      }

      tx <- load_receipt_token(token)
      if (is.null(tx)) {
        showNotification("Receipt token not found.", type = "error")
        return()
      }

      receipt_tx(tx)
      poll_count(0L)
      updateTabsetPanel(session, "main_nav", selected = "Receipt")
    },
    ignoreInit = TRUE
  )

  # -----------------------------------------------------------------------------
  # ADMIN: Prices / Config
  # -----------------------------------------------------------------------------

  parse_money <- function(x) {
    if (is.null(x)) {
      return(NA_real_)
    }
    v <- suppressWarnings(as.numeric(gsub(",", "", gsub("\\$", "", trimws(as.character(x))))))
    if (is.na(v) || v < 0) NA_real_ else v
  }

  parse_int_or_na <- function(x) {
    s <- trimws(as.character(x %||% ""))
    if (!nzchar(s)) {
      return(NA_integer_)
    }
    v <- suppressWarnings(as.integer(s))
    if (is.na(v) || v < 0) NA_integer_ else v
  }

  output$admin_prices_ui <- renderUI({
    admin_nonce()
    if (!isTRUE(rv$admin_logged_in)) {
      return(NULL)
    }

    prog <- get_program_list()

    panel <- function(title, ...) {
      tags$div(
        class = "panel panel-default admin-block",
        tags$div(class = "panel-heading", title),
        tags$div(class = "panel-body", ...)
      )
    }

    tagList(
      h3("Prices / Config"),
      panel(
        "Early-bird + Global limits",
        dateInput(
          "admin_cfg_early_bird_cutoff",
          "Early-bird cutoff (YYYY-MM-DD)",
          value = cfg_date("early_bird_cutoff", Sys.Date())
        ),
        fluidRow(
          column(
            6,
            numericInput(
              "admin_cfg_limit_max_total_cad",
              "Max total — blank/NA = no limit",
              value = cfg_num("limit_max_total_cad", NA_real_),
              min = 0,
              step = 10
            )
          ),
          column(
            6,
            numericInput(
              "admin_cfg_limit_max_items_total",
              "Max items — blank/NA = no limit",
              value = cfg_int("limit_max_items_total", NA_integer_),
              min = 0,
              step = 1
            )
          )
        )
      ),
      panel(
        "Day pass prices",
        fluidRow(
          column(3, numericInput("admin_price_day_adult", "Adult", value = cfg_num("price_day_adult", NA_real_), min = 0, step = 1)),
          column(3, numericInput("admin_price_day_youth", "Youth", value = cfg_num("price_day_youth", NA_real_), min = 0, step = 1)),
          column(3, numericInput("admin_price_day_under9", "Under 9", value = cfg_num("price_day_under9", NA_real_), min = 0, step = 1)),
          column(3, numericInput("admin_price_day_family", "Family", value = cfg_num("price_day_family", NA_real_), min = 0, step = 1))
        )
      ),
      panel(
        "Christmas pass",
        numericInput("admin_price_christmas_pass", "Christmas pass price",
          value = cfg_num("price_christmas_pass", NA_real_), min = 0, step = 1
        )
      ),
      panel(
        "Season pass prices",
        fluidRow(
          column(
            6, h4("Early-bird"),
            numericInput("admin_price_season_eb_adult", "Adult", value = cfg_num("price_season_eb_adult", NA_real_), min = 0, step = 1),
            numericInput("admin_price_season_eb_youth", "Youth", value = cfg_num("price_season_eb_youth", NA_real_), min = 0, step = 1)
          ),
          column(
            6, h4("Regular"),
            numericInput("admin_price_season_reg_adult", "Adult", value = cfg_num("price_season_reg_adult", NA_real_), min = 0, step = 1),
            numericInput("admin_price_season_reg_youth", "Youth", value = cfg_num("price_season_reg_youth", NA_real_), min = 0, step = 1)
          )
        )
      ),
      panel(
        "Programs (price + capacity)",
        tags$p(
          style = "color:#666;",
          "Capacity: leave blank for unlimited. Price must be set (or program will show N/A)."
        ),
        lapply(seq_len(nrow(prog)), function(i) {
          r <- prog[i, , drop = FALSE]
          fluidRow(
            column(5, tags$strong(r$name[1])),
            column(3, numericInput(paste0("admin_prog_price_", r$id[1]), "Price",
              value = cfg_num(r$price_key[1], NA_real_), min = 0, step = 1
            )),
            column(4, textInput(paste0("admin_prog_cap_", r$id[1]), "Capacity (blank = unlimited)",
              value = {
                v <- cfg_get(r$cap_key[1], "")
                if (!nzchar(v)) "" else v
              }
            ))
          )
        })
      ),
      panel(
        "Tabs enabled",
        checkboxInput("admin_tab_daypass_enabled", "Day Pass", value = cfg_bool("tab_daypass_enabled", TRUE)),
        checkboxInput("admin_tab_christmas_enabled", "Christmas Pass", value = cfg_bool("tab_christmas_enabled", TRUE)),
        checkboxInput("admin_tab_season_enabled", "Season Pass", value = cfg_bool("tab_season_enabled", TRUE)),
        checkboxInput("admin_tab_programs_enabled", "Programs", value = cfg_bool("tab_programs_enabled", TRUE)),
        checkboxInput("admin_tab_events_enabled", "Special Events", value = cfg_bool("tab_events_enabled", TRUE)),
        checkboxInput("admin_tab_donation_enabled", "Donation", value = cfg_bool("tab_donation_enabled", TRUE))
      ),
      actionButton("admin_prices_save", "Save Prices / Config")
    )
  })

  observeEvent(input$admin_prices_save,
    {
      if (!isTRUE(rv$admin_logged_in)) {
        return()
      }

      cfg_set("early_bird_cutoff", as.character(as.Date(input$admin_cfg_early_bird_cutoff)))

      # limits (numericInput returns NA if empty)
      cfg_set("limit_max_total_cad", if (is.na(input$admin_cfg_limit_max_total_cad)) "" else as.character(input$admin_cfg_limit_max_total_cad))
      cfg_set("limit_max_items_total", if (is.na(input$admin_cfg_limit_max_items_total)) "" else as.character(as.integer(input$admin_cfg_limit_max_items_total)))

      # day
      cfg_set("price_day_adult", if (is.na(input$admin_price_day_adult)) "" else as.character(input$admin_price_day_adult))
      cfg_set("price_day_youth", if (is.na(input$admin_price_day_youth)) "" else as.character(input$admin_price_day_youth))
      cfg_set("price_day_under9", if (is.na(input$admin_price_day_under9)) "" else as.character(input$admin_price_day_under9))
      cfg_set("price_day_family", if (is.na(input$admin_price_day_family)) "" else as.character(input$admin_price_day_family))

      # christmas
      cfg_set("price_christmas_pass", if (is.na(input$admin_price_christmas_pass)) "" else as.character(input$admin_price_christmas_pass))

      # season
      cfg_set("price_season_eb_adult", if (is.na(input$admin_price_season_eb_adult)) "" else as.character(input$admin_price_season_eb_adult))
      cfg_set("price_season_eb_youth", if (is.na(input$admin_price_season_eb_youth)) "" else as.character(input$admin_price_season_eb_youth))
      cfg_set("price_season_reg_adult", if (is.na(input$admin_price_season_reg_adult)) "" else as.character(input$admin_price_season_reg_adult))
      cfg_set("price_season_reg_youth", if (is.na(input$admin_price_season_reg_youth)) "" else as.character(input$admin_price_season_reg_youth))

      # programs (catalog-driven)
      cat <- program_catalog()
      for (i in seq_len(nrow(cat))) {
        pid <- cat$id[i]
        price_id <- paste0("admin_prog_price_", pid)
        cap_id <- paste0("admin_prog_cap_", pid)

        cfg_set(cat$price_key[i], if (is.na(input[[price_id]])) "" else as.character(input[[price_id]]))

        cap_val <- parse_int_or_na(input[[cap_id]])
        cfg_set(cat$cap_key[i], if (is.na(cap_val)) "" else as.character(cap_val))
      }

      # tabs
      cfg_set("tab_daypass_enabled", if (isTRUE(input$admin_tab_daypass_enabled)) "1" else "0")
      cfg_set("tab_christmas_enabled", if (isTRUE(input$admin_tab_christmas_enabled)) "1" else "0")
      cfg_set("tab_season_enabled", if (isTRUE(input$admin_tab_season_enabled)) "1" else "0")
      cfg_set("tab_programs_enabled", if (isTRUE(input$admin_tab_programs_enabled)) "1" else "0")
      cfg_set("tab_events_enabled", if (isTRUE(input$admin_tab_events_enabled)) "1" else "0")
      cfg_set("tab_donation_enabled", if (isTRUE(input$admin_tab_donation_enabled)) "1" else "0")

      showNotification("Saved Prices / Config.", type = "message")
    },
    ignoreInit = TRUE
  )

  # -----------------------------------------------------------------------------
  # ADMIN: Blocked Dates
  # -----------------------------------------------------------------------------

  output$admin_blocked_ui <- renderUI({
    admin_nonce()
    if (!isTRUE(rv$admin_logged_in)) {
      return(NULL)
    }

    blocked_nonce()
    bd <- get_blocked_dates()

    rows <- if (nrow(bd) == 0) {
      tags$tr(tags$td(colspan = 3, style = "color:#666;", "No blocked dates."))
    } else {
      lapply(seq_len(nrow(bd)), function(i) {
        d <- as.character(bd$date[i] %||% "")
        r <- as.character(bd$reason[i] %||% "")
        tags$tr(
          tags$td(d),
          tags$td(htmltools::htmlEscape(r)),
          tags$td(HTML(paste0(
            "<a href='#' class='admin-block-del' data-date='",
            htmltools::htmlEscape(d, attribute = TRUE),
            "'>Delete</a>"
          )))
        )
      })
    }

    tagList(
      h3("Blocked Dates"),
      fluidRow(
        column(4, dateInput("admin_block_date", "Date", value = Sys.Date())),
        column(8, textInput("admin_block_reason", "Reason (optional)", value = ""))
      ),
      actionButton("admin_block_add", "Add / Update"),
      br(), br(),
      tags$table(
        class = "table table-condensed",
        tags$thead(tags$tr(tags$th("Date"), tags$th("Reason"), tags$th("Action"))),
        tags$tbody(rows)
      )
    )
  })

  observeEvent(input$admin_block_add,
    {
      if (!isTRUE(rv$admin_logged_in)) {
        return()
      }
      d <- suppressWarnings(as.Date(input$admin_block_date))
      if (is.na(d)) {
        showNotification("Pick a valid date.", type = "error")
        return()
      }
      reason <- trimws(as.character(input$admin_block_reason %||% ""))

      with_db(function(con) {
        DBI::dbWithTransaction(con, {
          n <- db_exec(con, 'UPDATE blocked_dates SET reason = ?reason WHERE "date" = ?d',
            reason = if (nzchar(reason)) reason else NA_character_,
            d = as.character(d)
          )
          if (isTRUE(n == 0)) {
            tryCatch(
              db_exec(con, 'INSERT INTO blocked_dates("date", reason) VALUES (?d, ?reason)',
                d = as.character(d),
                reason = if (nzchar(reason)) reason else NA_character_
              ),
              error = function(e) {
                db_exec(con, 'UPDATE blocked_dates SET reason = ?reason WHERE "date" = ?d',
                  reason = if (nzchar(reason)) reason else NA_character_,
                  d = as.character(d)
                )
              }
            )
          }
        })
      })

      blocked_nonce(blocked_nonce() + 1L)
      showNotification("Blocked date saved.", type = "message")
    },
    ignoreInit = TRUE
  )

  observeEvent(input$admin_block_del,
    {
      if (!isTRUE(rv$admin_logged_in)) {
        return()
      }
      d <- trimws(as.character(input$admin_block_del$date %||% ""))
      if (!nzchar(d)) {
        return()
      }

      db_exec1('DELETE FROM blocked_dates WHERE "date" = ?d', d = d)
      blocked_nonce(blocked_nonce() + 1L)
      showNotification("Blocked date deleted.", type = "message")
    },
    ignoreInit = TRUE
  )

  # -----------------------------------------------------------------------------
  # ADMIN: Events
  # -----------------------------------------------------------------------------

  output$admin_events_ui <- renderUI({
    admin_nonce()
    if (!isTRUE(rv$admin_logged_in)) {
      return(NULL)
    }

    events_nonce()
    ev <- db_get1("SELECT id, name, event_date, price_cad, capacity, enabled FROM special_events ORDER BY event_date ASC")

    choices <- c("NEW" = "NEW")
    if (nrow(ev) > 0) {
      lab <- paste0(ev$name, " (", ev$event_date, ")")
      choices <- c(choices, setNames(ev$id, lab))
    }

    tagList(
      h3("Special Events"),
      selectInput("admin_event_pick", "Select event", choices = choices, selected = "NEW"),
      textInput("admin_event_name", "Event name", value = ""),
      dateInput("admin_event_date", "Event date", value = Sys.Date()),
      numericInput("admin_event_price", "Price", value = NA_real_, min = 0, step = 1),
      textInput("admin_event_capacity", "Capacity (blank = unlimited)", value = ""),
      checkboxInput("admin_event_enabled", "Enabled", value = TRUE),
      actionButton("admin_event_save", "Save / Update"),
      tags$span(" "),
      actionButton("admin_event_delete", "Delete")
    )
  })

  observeEvent(input$admin_event_pick,
    {
      if (!isTRUE(rv$admin_logged_in)) {
        return()
      }
      events_nonce()

      id <- as.character(input$admin_event_pick %||% "NEW")
      if (identical(id, "NEW")) {
        updateTextInput(session, "admin_event_name", value = "")
        updateDateInput(session, "admin_event_date", value = Sys.Date())
        updateNumericInput(session, "admin_event_price", value = NA_real_)
        updateTextInput(session, "admin_event_capacity", value = "")
        updateCheckboxInput(session, "admin_event_enabled", value = TRUE)
        return()
      }

      row <- db_get1("SELECT id, name, event_date, price_cad, capacity, enabled FROM special_events WHERE id = ?id LIMIT 1", id = id)
      if (nrow(row) != 1) {
        return()
      }

      updateTextInput(session, "admin_event_name", value = as.character(row$name[1] %||% ""))
      updateDateInput(session, "admin_event_date", value = suppressWarnings(as.Date(row$event_date[1])))
      updateNumericInput(session, "admin_event_price", value = suppressWarnings(as.numeric(row$price_cad[1])))
      cap <- row$capacity[1]
      updateTextInput(session, "admin_event_capacity", value = if (is.na(cap)) "" else as.character(cap))
      updateCheckboxInput(session, "admin_event_enabled", value = isTRUE(as.integer(row$enabled[1] %||% 0L) == 1L))
    },
    ignoreInit = TRUE
  )

  observeEvent(input$admin_event_save,
    {
      if (!isTRUE(rv$admin_logged_in)) {
        return()
      }

      id <- as.character(input$admin_event_pick %||% "NEW")
      nm <- trimws(as.character(input$admin_event_name %||% ""))
      d <- suppressWarnings(as.Date(input$admin_event_date))
      pr <- suppressWarnings(as.numeric(input$admin_event_price))
      cap <- parse_int_or_na(input$admin_event_capacity)
      en <- if (isTRUE(input$admin_event_enabled)) 1L else 0L

      if (!nzchar(nm)) {
        showNotification("Event name is required.", type = "error")
        return()
      }
      if (is.na(d)) {
        showNotification("Event date is required.", type = "error")
        return()
      }
      if (is.na(pr) || pr < 0) {
        showNotification("Event price must be set (>= 0).", type = "error")
        return()
      }

      if (identical(id, "NEW")) {
        id <- UUIDgenerate()
        db_exec1(
          "INSERT INTO special_events (id, name, event_date, price_cad, capacity, enabled, created_at)
       VALUES (?id, ?name, ?event_date, ?price_cad, ?capacity, ?enabled, ?created_at)",
          id = id, name = nm, event_date = as.character(d),
          price_cad = pr, capacity = if (is.na(cap)) NA_integer_ else cap,
          enabled = en, created_at = now_ts()
        )
      } else {
        db_exec1(
          "UPDATE special_events
       SET name = ?name, event_date = ?event_date, price_cad = ?price_cad, capacity = ?capacity, enabled = ?enabled
       WHERE id = ?id",
          id = id, name = nm, event_date = as.character(d),
          price_cad = pr, capacity = if (is.na(cap)) NA_integer_ else cap,
          enabled = en
        )
      }

      events_nonce(events_nonce() + 1L)
      showNotification("Event saved.", type = "message")
    },
    ignoreInit = TRUE
  )

  observeEvent(input$admin_event_delete,
    {
      if (!isTRUE(rv$admin_logged_in)) {
        return()
      }
      id <- as.character(input$admin_event_pick %||% "NEW")
      if (identical(id, "NEW")) {
        return()
      }

      db_exec1("DELETE FROM special_events WHERE id = ?id", id = id)
      events_nonce(events_nonce() + 1L)
      updateSelectInput(session, "admin_event_pick", selected = "NEW")
      showNotification("Event deleted.", type = "message")
    },
    ignoreInit = TRUE
  )

  # -----------------------------------------------------------------------------
  # ADMIN: Transactions (DT sortable table + download)
  # -----------------------------------------------------------------------------

  output$admin_tx_ui <- renderUI({
    admin_nonce()
    if (!isTRUE(rv$admin_logged_in)) {
      return(NULL)
    }

    tagList(
      fluidRow(
        column(3, dateInput("admin_tx_start", "From date", value = Sys.Date() - 30)),
        column(3, dateInput("admin_tx_end", "To date", value = Sys.Date())),
        column(3, textInput("admin_tx_name", "Name contains", value = "")),
        column(3, selectInput(
          "admin_tx_type", "Transaction type",
          choices = c(
            "All" = "",
            "Day pass" = "day_pass",
            "Christmas pass" = "christmas_pass",
            "Season pass" = "season_pass",
            "Program" = "program",
            "Special event" = "event",
            "Donation" = "donation",
            "Mixed" = "mixed"
          ),
          selected = ""
        ))
      ),
      fluidRow(
        column(3, selectInput(
          "admin_tx_status", "Status",
          choices = c(
            "All" = "",
            "COMPLETED" = "COMPLETED",
            "PENDING" = "PENDING",
            "FAILED" = "FAILED",
            "SANDBOX_TEST_OK" = "SANDBOX_TEST_OK",
            "UNKNOWN" = "UNKNOWN"
          ),
          selected = ""
        )),
        column(3, textInput("admin_tx_email", "Email contains", value = "")),
        column(3, selectInput(
          "admin_tx_sort_by", "Sort by",
          choices = c(
            "Date (newest first)" = "created_at",
            "Value (highest first)" = "total_amount_cents"
          ),
          selected = "created_at"
        )),
        column(3, numericInput("admin_tx_limit", "Limit", value = 50, min = 1, max = 500, step = 1))
      ),
      fluidRow(
        column(
          12,
          actionButton("admin_tx_refresh", "Refresh"),
          tags$span(" "),
          downloadButton("admin_tx_download", "Download CSV")
        )
      ),
      br(),
      if (isTRUE(HAVE_DT)) {
        DT::DTOutput("admin_tx_table")
      } else {
        tags$div(
          class = "alert alert-warning",
          "DT package is not available on this host. Transactions table is disabled."
        )
      }
    )
  })

# Only register the DT output if DT exists on this host
if (isTRUE(HAVE_DT)) {

  output$admin_tx_table <- DT::renderDT({

    admin_nonce()
    if (!isTRUE(rv$admin_logged_in)) return(NULL)

    tx_nonce()

    # Vector-safe input defaults
    lim <- suppressWarnings(as.integer(input$admin_tx_limit))
    if (is.na(lim) || lim < 1L) lim <- 50L

    sd <- input$admin_tx_start
    if (is.null(sd) || !nzchar(as.character(sd))) sd <- NULL

    ed <- input$admin_tx_end
    if (is.null(ed) || !nzchar(as.character(ed))) ed <- NULL

    nm <- trimws(as.character(input$admin_tx_name))
    if (is.na(nm)) nm <- ""

    em <- trimws(as.character(input$admin_tx_email))
    if (is.na(em)) em <- ""

    st <- trimws(as.character(input$admin_tx_status))
    if (is.na(st)) st <- ""

    tt <- trimws(as.character(input$admin_tx_type))
    if (is.na(tt)) tt <- ""

    sb <- trimws(as.character(input$admin_tx_sort_by))
    if (is.na(sb) || !nzchar(sb)) sb <- "created_at"

    df <- fetch_transactions(
      limit      = lim,
      start_date = sd,
      end_date   = ed,
      name       = nm,
      email      = em,
      status     = st,
      tx_type    = tt,
      sort_by    = sb
    )

    # Empty / NULL guard FIRST
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "No results.", stringsAsFactors = FALSE),
        rownames = FALSE,
        options  = list(dom = "t"),
        class    = "compact"
      ))
    }

    # ---- build a display table with an Action link ----
    df_out <- df

    cents <- suppressWarnings(as.numeric(df_out$total_amount_cents))
    df_out$total <- ifelse(
      is.na(cents),
      "",
      sprintf("$%.2f", cents / 100)
    )

    token <- ifelse(is.na(df_out$receipt_token), "", as.character(df_out$receipt_token))

    df_out$action <- ifelse(
      nzchar(token),
      paste0(
        "<a href='#' class='admin-tx-load' data-token='",
        htmltools::htmlEscape(token, attribute = TRUE),
        "'>View receipt</a>"
      ),
      ""
    )

    df_out <- df_out[, c(
      "created_at", "buyer_name", "buyer_email", "tx_type",
      "total", "status", "action"
    ), drop = FALSE]

    # IMPORTANT: make this the FINAL expression returned by renderDT()
    DT::datatable(
      df_out,
      rownames = FALSE,
      escape   = FALSE,  # allows <a> to be clickable
      class    = "compact stripe hover",
      options  = list(
        order      = list(list(0, "desc")),
        pageLength = 25,
        lengthMenu = list(c(25, 50, 100), c("25", "50", "100"))
      )
    )
  })
}

  observeEvent(input$admin_tx_refresh,
    {
      tx_nonce(tx_nonce() + 1L)
    },
    ignoreInit = TRUE
  )

  observeEvent(input$admin_tx_load,
    {
      if (!isTRUE(rv$admin_logged_in)) {
        return()
      }

      tok <- trimws(as.character(input$admin_tx_load$token))
      if (is.na(tok) || !nzchar(tok)) {
        return()
      }

      tx <- load_receipt_token(tok)
      if (is.null(tx)) {
        showNotification("Receipt not found.", type = "error")
        return()
      }

      receipt_tx(tx)
      poll_count(0L)
      updateTabsetPanel(session, "main_nav", selected = "Receipt")
    },
    ignoreInit = TRUE
  )

output$admin_tx_download <- downloadHandler(
  filename = function() {
    paste0("transactions_", format(Sys.Date()), ".csv")
  },
  content = function(file) {
    lim <- suppressWarnings(as.integer(input$admin_tx_limit))
    if (is.na(lim) || lim < 1L) lim <- 50L

    sd <- input$admin_tx_start
    if (is.null(sd) || !nzchar(as.character(sd))) sd <- NULL

    ed <- input$admin_tx_end
    if (is.null(ed) || !nzchar(as.character(ed))) ed <- NULL

    nm <- trimws(as.character(input$admin_tx_name))
    if (is.na(nm)) nm <- ""

    em <- trimws(as.character(input$admin_tx_email))
    if (is.na(em)) em <- ""

    st <- trimws(as.character(input$admin_tx_status))
    if (is.na(st)) st <- ""

    tt <- trimws(as.character(input$admin_tx_type))
    if (is.na(tt)) tt <- ""

    sb <- trimws(as.character(input$admin_tx_sort_by))
    if (is.na(sb) || !nzchar(sb)) sb <- "created_at"

    df <- fetch_transactions(
      limit      = lim,
      start_date = sd,
      end_date   = ed,
      name       = nm,
      email      = em,
      status     = st,
      tx_type    = tt,
      sort_by    = sb
    )

    if ("receipt_token" %in% names(df)) df$receipt_token <- NULL
    write.csv(df, file, row.names = FALSE)
  }
)

}  # end server()

shinyApp(ui = ui, server = server)
