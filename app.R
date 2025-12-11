# app.R
# Bulkley Valley Cross Country Ski Club – passes, donations, programs, special events (Square)

library(shiny)
library(httr)
library(DBI)
library(RSQLite)
library(uuid)
library(jsonlite)

# -----------------------------------------------------------------------------
# GLOBAL SETTINGS / ENV
# -----------------------------------------------------------------------------

Sys.setenv(TZ = "America/Vancouver")
APP_VERSION <- "BVXC passes v3.3 – 2025-12-10"

if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

SQUARE_ENV          <- Sys.getenv("SQUARE_ENV",           unset = NA_character_)
SQUARE_ACCESS_TOKEN <- Sys.getenv("SQUARE_ACCESS_TOKEN",  unset = NA_character_)
SQUARE_LOCATION_ID  <- Sys.getenv("SQUARE_LOCATION_ID",   unset = NA_character_)
ADMIN_PASSWORD      <- Sys.getenv("BVXC_ADMIN_PASSWORD",  unset = NA_character_)
RETURN_BASE_URL     <- Sys.getenv("BVXC_RETURN_BASE_URL", unset = NA_character_)

ALLOWED_SQUARE_ENVS <- c("sandbox", "production")

if (is.na(SQUARE_ENV) || !nzchar(SQUARE_ENV) || !(SQUARE_ENV %in% ALLOWED_SQUARE_ENVS)) {
  stop("SQUARE_ENV must be 'sandbox' or 'production'")
}

square_base_url <- if (identical(SQUARE_ENV, "production")) {
  "https://connect.squareup.com"
} else {
  "https://connect.squareupsandbox.com"
}

# --- safer env validation -----------------------------------------------------

missing_square <- c(
  if (is.na(SQUARE_ACCESS_TOKEN) || SQUARE_ACCESS_TOKEN == "") "SQUARE_ACCESS_TOKEN" else NULL,
  if (is.na(SQUARE_LOCATION_ID)  || SQUARE_LOCATION_ID  == "") "SQUARE_LOCATION_ID"  else NULL
)

if (length(missing_square) > 0) {
  stop(
    paste0(
      "Missing required environment variables: ",
      paste(missing_square, collapse = ", "),
      ". Check your .Renviron in the app directory or server configuration."
    )
  )
}

if (is.na(ADMIN_PASSWORD) || ADMIN_PASSWORD == "") {
  message("Warning: BVXC_ADMIN_PASSWORD not set. Admin tab will remain locked.")
}

CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))
CHRISTMAS_DAY_THIS_YEAR <- as.Date(sprintf("%d-12-25", CURRENT_YEAR))
CHRISTMAS_START_MIN <- CHRISTMAS_DAY_THIS_YEAR - 13
CHRISTMAS_START_MAX <- CHRISTMAS_DAY_THIS_YEAR

# -----------------------------------------------------------------------------
# CONSTANTS / HELPERS
# -----------------------------------------------------------------------------

CONST <- list(
  youth_min_age = 9L,
  youth_max_age = 18L,
  adult_min_age = 19L,

  admin_max_attempts = 5L,
  admin_window_secs  = 5 * 60,
  admin_lock_secs    = 5 * 60,

  max_ski_date_offset_days   = 183L,
  unusual_tx_threshold_count = 5L,
  unusual_tx_threshold_cents = 50000L  # $500
)

PROGRAM_CATS <- data.frame(
  key   = c("4_5",    "6_10",   "11_12",   "13_14",   "15_16",   "17_18",   "masters"),
  label = c("4–5 yrs","6–10 yrs","11–12 yrs","13–14 yrs","15–16 yrs","17–18 yrs","Masters"),
  stringsAsFactors = FALSE
)

validate <- shiny::validate
need     <- shiny::need

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

as_int0 <- function(x) {
  if (is.null(x) || length(x) == 0) return(0L)
  x <- suppressWarnings(as.integer(x))
  x[is.na(x)] <- 0L
  x
}

as_num0 <- function(x) {
  if (is.null(x) || length(x) == 0) return(0)
  x <- suppressWarnings(as.numeric(x))
  x[is.na(x)] <- 0
  x
}

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
  bad_idx    <- which(bad_start & !is.na(x))
  if (length(bad_idx) > 0) {
    x[bad_idx] <- paste0("'", x[bad_idx])
  }
  x
}

compute_age_years <- function(dob, ref_date) {
  if (length(dob) == 0) return(integer(0))
  dob      <- as.Date(dob)
  ref_date <- as.Date(ref_date)

  dob_year  <- as.integer(format(dob, "%Y"))
  dob_month <- as.integer(format(dob, "%m"))
  dob_day   <- as.integer(format(dob, "%d"))

  ref_year  <- as.integer(format(ref_date, "%Y"))
  ref_month <- as.integer(format(ref_date, "%m"))
  ref_day   <- as.integer(format(ref_date, "%d"))

  age <- ref_year - dob_year
  not_yet_birthday <- (ref_month < dob_month) |
    (ref_month == dob_month & ref_day < dob_day)
  age <- age - as.integer(not_yet_birthday)

  age[is.na(dob)] <- NA_integer_
  as.integer(age)
}

is_valid_email <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return(FALSE)
  grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", x)
}

program_group_for_age <- function(age) {
  if (is.na(age)) return(NA_character_)
  if (age >= 4  && age <= 5)  return("4_5")
  if (age >= 6  && age <= 10) return("6_10")
  if (age >= 11 && age <= 12) return("11_12")
  if (age >= 13 && age <= 14) return("13_14")
  if (age >= 15 && age <= 16) return("15_16")
  if (age >= 17 && age <= 18) return("17_18")
  if (age >= 19)              return("masters")
  NA_character_
}

collect_inputs <- function(n, prefix, suffix, input, type = "character") {
  if (n <= 0L) {
    if (identical(type, "date")) return(as.Date(character(0)))
    return(character(0))
  }

  if (identical(type, "date")) {
    vapply(
      seq_len(n),
      function(i) {
        val <- input[[paste0(prefix, "_", suffix, "_", i)]]
        as.Date(val)
      },
      FUN.VALUE = as.Date(NA)
    )
  } else {
    vapply(
      seq_len(n),
      function(i) {
        val <- input[[paste0(prefix, "_", suffix, "_", i)]] %||% ""
        sanitize_for_storage(val)
      },
      FUN.VALUE = character(1)
    )
  }
}

# -----------------------------------------------------------------------------
# LOGGING
# -----------------------------------------------------------------------------

log_event <- function(event_type, details) {
  ts <- as.character(Sys.time())
  details_json <- tryCatch(
    jsonlite::toJSON(details %||% list(), auto_unbox = TRUE, null = "null"),
    error = function(e) {
      paste0(
        '{"error":"failed_to_encode_details","message":"',
        sanitize_for_storage(e$message),
        '"}'
      )
    }
  )

  tryCatch({
    con <- get_db_connection()
    on.exit(dbDisconnect(con), add = TRUE)
    dbWriteTable(
      con,
      "event_log",
      data.frame(
        ts         = ts,
        event_type = as.character(event_type %||% ""),
        details    = as.character(details_json),
        stringsAsFactors = FALSE
      ),
      append = TRUE
    )
  }, error = function(e) {
    message("Failed to write to event_log: ", e$message)
  })

  invisible(NULL)
}

log_config_changes <- function(before, after, context = list()) {
  if (is.null(before) || is.null(after)) return(invisible(NULL))
  keys <- intersect(names(before), names(after))
  for (k in keys) {
    old_val <- before[[k]]
    new_val <- after[[k]]
    if (identical(old_val, new_val)) next
    details <- c(list(key = k, old = old_val, new = new_val), context)
    log_event("config_change", details)
  }
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# ADMIN AUTH
# -----------------------------------------------------------------------------

admin_password_matches <- function(input_pw, stored_pw) {
  if (is.na(stored_pw) || !nzchar(stored_pw)) return(FALSE)
  input_pw  <- as.character(input_pw %||% "")
  stored_pw <- as.character(stored_pw)
  identical(input_pw, stored_pw)
}

# -----------------------------------------------------------------------------
# DB
# -----------------------------------------------------------------------------

get_db_connection <- function() {
  con <- dbConnect(SQLite(), "bvxc.sqlite")
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
      checkout_id      TEXT,
      square_order_id  TEXT,
      status           TEXT,
      payment_link_id  TEXT
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

  dbExecute(con, "CREATE TABLE IF NOT EXISTS blocked_dates (date TEXT PRIMARY KEY)")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS config (
      key   TEXT PRIMARY KEY,
      value TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS event_log (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      ts         TEXT,
      event_type TEXT,
      details    TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS special_events (
      id                 INTEGER PRIMARY KEY AUTOINCREMENT,
      name               TEXT,
      event_date         TEXT,
      price_cents        INTEGER,
      max_tickets_per_tx INTEGER,
      max_amount_dollars REAL,
      is_active          INTEGER DEFAULT 1
    )
  ")

  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_transactions_created ON transactions(created)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_transactions_ski_date ON transactions(ski_date)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_transactions_product_type ON transactions(product_type)")
}

save_transaction <- function(row_df) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  dbWriteTable(con, "transactions", row_df, append = TRUE)

  email_val   <- row_df$email[1]
  created_val <- row_df$created[1]

  if (!is.na(email_val) && nzchar(email_val) &&
      !is.na(created_val) && nzchar(created_val)) {

    res <- tryCatch(
      dbGetQuery(
        con,
        "
        SELECT
          email,
          date(created) AS day,
          COUNT(*)      AS n_tx,
          SUM(total_cents) AS total_cents
        FROM transactions
        WHERE email = ? AND date(created) = date(?)
        GROUP BY email, date(created)
        ",
        params = list(email_val, created_val)
      ),
      error = function(e) NULL
    )

if (!is.null(res) && nrow(res) > 0) {
  n_tx        <- as_int0(res$n_tx[1])
  total_cents <- as_int0(res$total_cents[1])
  if (n_tx > CONST$unusual_tx_threshold_count ||
      total_cents > CONST$unusual_tx_threshold_cents) {
    log_event(
      "unusual_tx_pattern",
      list(
        email       = email_val,
        day         = res$day[1],
        n_tx        = n_tx,
        total_cents = total_cents
      )
    )
  }
}

  }
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

load_special_events <- function(active_only = FALSE) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  if (!"special_events" %in% dbListTables(con)) return(data.frame())
  df <- dbReadTable(con, "special_events")
  if (!is.data.frame(df) || nrow(df) == 0) return(df)

  if (isTRUE(active_only)) {
    df <- df[as_int0(df$is_active) == 1L, , drop = FALSE]
  }
  df
}

set_special_event_active <- function(event_id, is_active) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(
    con,
    "UPDATE special_events SET is_active = ? WHERE id = ?",
    params = list(as.integer(isTRUE(is_active)), as.integer(event_id))
  )
}

delete_special_event <- function(event_id) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "DELETE FROM special_events WHERE id = ?", params = list(as.integer(event_id)))
}

load_event_log <- function(limit = 5L) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  if (!"event_log" %in% dbListTables(con)) return(data.frame())
  df <- dbGetQuery(
    con,
    sprintf(
      "SELECT ts, event_type, details FROM event_log ORDER BY id DESC LIMIT %d",
      as.integer(limit)
    )
  )
  df
}

# -----------------------------------------------------------------------------
# CONFIG
# -----------------------------------------------------------------------------

DEFAULT_CONFIG <- list(
  # Day prices (cents)
  price_adult_day    = 1500L,
  price_youth_day    = 1000L,
  price_under9_day   = 0L,
  price_family_day   = 3000L,

  # Season prices (regular & early bird) in cents
  price_season_adult        = 25000L, # regular
  price_season_youth        = 18000L, # regular
  price_season_adult_early  = 22000L, # early-bird
  price_season_youth_early  = 16000L, # early-bird

  # Christmas price (cents)
  price_christmas_pass = 7500L,

  season_label = "2025–2026 season",

  # Early-bird cutoff (string, YYYY-MM-DD) – configurable in Admin
  early_bird_cutoff = format(Sys.Date() + 30, "%Y-%m-%d"),

  # Programs prices – regular & early bird (cents)
  price_program_4_5           = 8000L,
  price_program_4_5_early     = 7000L,
  price_program_6_10          = 9000L,
  price_program_6_10_early    = 8000L,
  price_program_11_12         = 10000L,
  price_program_11_12_early   = 9000L,
  price_program_13_14         = 11000L,
  price_program_13_14_early   = 10000L,
  price_program_15_16         = 12000L,
  price_program_15_16_early   = 11000L,
  price_program_17_18         = 13000L,
  price_program_17_18_early   = 12000L,
  price_program_masters       = 14000L,
  price_program_masters_early = 13000L,

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
  max_season_amount  = 2000,  # dollars

  # Programs limits
  max_program_participants = 20L,
  max_program_amount       = 3000, # dollars

  # Tab flags
  tab_day_enabled       = 1L,
  tab_christmas_enabled = 1L,
  tab_season_enabled    = 1L,
  tab_donation_enabled  = 1L,
  tab_programs_enabled  = 1L,
  tab_special_enabled   = 1L
)

INT_KEYS <- c(
  grep("^price_", names(DEFAULT_CONFIG), value = TRUE),
  grep("^tab_", names(DEFAULT_CONFIG), value = TRUE),
  "max_day_adult", "max_day_youth", "max_day_under9", "max_day_family",
  "max_christmas_passes",
  "max_season_adult", "max_season_youth",
  "max_program_participants"
)

NUM_KEYS <- c(
  "max_day_amount",
  "max_donation_amount",
  "max_christmas_amount",
  "max_season_amount",
  "max_program_amount"
)

load_config <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  if (!"config" %in% dbListTables(con)) return(DEFAULT_CONFIG)

  df  <- dbReadTable(con, "config")
  cfg <- DEFAULT_CONFIG

  if (nrow(df) > 0) {
    for (i in seq_len(nrow(df))) {
      k <- df$key[i]
      v <- df$value[i]
      if (!nzchar(k)) next
      if (!k %in% names(cfg)) next

      if (k %in% INT_KEYS) {
        num <- suppressWarnings(as.numeric(v))
        if (!is.na(num)) {
          if (grepl("^tab_", k)) {
            cfg[[k]] <- as.integer(ifelse(num > 0, 1, 0))
          } else {
            cfg[[k]] <- as.integer(round(num))
          }
        }
      } else if (k %in% NUM_KEYS) {
        num <- suppressWarnings(as.numeric(v))
        if (!is.na(num)) cfg[[k]] <- num
      } else {
        cfg[[k]] <- v
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
    day       = isTRUE(as.integer(cfg$tab_day_enabled)       == 1L),
    christmas = isTRUE(as.integer(cfg$tab_christmas_enabled) == 1L),
    season    = isTRUE(as.integer(cfg$tab_season_enabled)    == 1L),
    donation  = isTRUE(as.integer(cfg$tab_donation_enabled)  == 1L),
    programs  = isTRUE(as.integer(cfg$tab_programs_enabled)  == 1L),
    special   = isTRUE(as.integer(cfg$tab_special_enabled)   == 1L)
  )
}

save_tab_flags <- function(flags, cfg = NULL) {
  if (is.null(cfg)) cfg <- load_config()
  before <- cfg

  cfg$tab_day_enabled       <- as.integer(isTRUE(flags$day))
  cfg$tab_christmas_enabled <- as.integer(isTRUE(flags$christmas))
  cfg$tab_season_enabled    <- as.integer(isTRUE(flags$season))
  cfg$tab_donation_enabled  <- as.integer(isTRUE(flags$donation))
  cfg$tab_programs_enabled  <- as.integer(isTRUE(flags$programs))
  cfg$tab_special_enabled   <- as.integer(isTRUE(flags$special))

  save_config(cfg)
  log_config_changes(before, cfg, context = list(source = "save_tab_flags"))
}

# -----------------------------------------------------------------------------
# SQUARE CHECKOUT + STATUS CHECK
# -----------------------------------------------------------------------------

build_return_url <- function(session) {
  base_path <- session$clientData$url_pathname %||% "/"
  if (!startsWith(base_path, "/")) base_path <- paste0("/", base_path)

  if (!is.na(RETURN_BASE_URL) && nzchar(RETURN_BASE_URL)) {
    base <- sub("/+$", "", RETURN_BASE_URL)
    return(paste0(base, base_path, "?success=1"))
  }

  host  <- session$clientData$url_hostname
  port  <- session$clientData$url_port
  proto <- session$clientData$url_protocol

  host_port <- if (!is.null(port) && nzchar(port)) paste0(host, ":", port) else host

  paste0(proto, "//", host_port, base_path, "?success=1")
}

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
    httr::POST(
      url = paste0(square_base_url, "/v2/online-checkout/payment-links"),
      httr::add_headers(
        "Authorization" = paste("Bearer", SQUARE_ACCESS_TOKEN),
        "Content-Type"  = "application/json"
      ),
      body   = body_list,
      encode = "json"
    ),
    error = function(e) {
      log_event(
        "square_api_error",
        list(
          phase        = "http_post",
          message      = e$message,
          item_name    = item_name,
          total_cents  = total_cents,
          location_id  = SQUARE_LOCATION_ID,
          env          = SQUARE_ENV
        )
      )
      stop(sprintf("Square API call failed: %s", e$message))
    }
  )

  if (!inherits(resp, "response")) {
    log_event(
      "square_api_error",
      list(
        phase       = "no_http_response_object",
        item_name   = item_name,
        total_cents = total_cents,
        env         = SQUARE_ENV
      )
    )
    stop("Square API call did not return a valid HTTP response object.")
  }

  if (httr::http_error(resp)) {
    status <- httr::http_status(resp)
    body   <- httr::content(resp, "text", encoding = "UTF-8")

    log_event(
      "square_api_error",
      list(
        phase         = "http_error",
        status_code   = status$status_code,
        category      = status$category,
        reason        = status$reason,
        item_name     = item_name,
        total_cents   = total_cents,
        location_id   = SQUARE_LOCATION_ID,
        env           = SQUARE_ENV,
        response_body = substr(body, 1, 2000)
      )
    )

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

  pl <- content_list$payment_link
  if (is.null(pl$url) || is.null(pl$id)) {
    log_event(
      "square_api_error",
      list(
        phase       = "missing_payment_link",
        item_name   = item_name,
        total_cents = total_cents,
        env         = SQUARE_ENV,
        response    = content_list
      )
    )
    stop("Square response missing payment_link url or id.")
  }

  list(
    url      = pl$url,
    id       = pl$id,
    order_id = pl$order_id %||% NA_character_
  )
}

check_square_payment_status <- function(order_id) {
  if (is.na(order_id) || !nzchar(order_id)) return("UNKNOWN")

  url <- paste0(square_base_url, "/v2/orders/", order_id)

  resp <- tryCatch(
    httr::GET(
      url,
      httr::add_headers(
        "Authorization" = paste("Bearer", SQUARE_ACCESS_TOKEN),
        "Content-Type"  = "application/json"
      )
    ),
    error = function(e) {
      log_event(
        "square_order_check_error",
        list(message = e$message, order_id = order_id)
      )
      return("ERROR")
    }
  )

  if (!inherits(resp, "response") || httr::http_error(resp)) {
    log_event(
      "square_order_check_http_error",
      list(
        status   = if (inherits(resp, "response")) httr::http_status(resp)$status_code else NA_integer_,
        order_id = order_id
      )
    )
    return("PENDING")
  }

  content <- httr::content(resp, as = "parsed")
  state   <- content$order$state %||% "UNKNOWN"

  is_paid <- FALSE
  if (state == "COMPLETED") {
    is_paid <- TRUE
  } else if (state == "OPEN" && !is.null(content$order$tenders)) {
    is_paid <- TRUE
  }

  if (is_paid) "PAID" else "PENDING"
}

# -----------------------------------------------------------------------------
# INIT DB
# -----------------------------------------------------------------------------

init_db()

# -----------------------------------------------------------------------------
# UI HELPERS
# -----------------------------------------------------------------------------

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

ui_limits_generic <- function(items, max_amount) {
  # items: list of c(label, max_val)
  parts <- vapply(
    items,
    function(x) sprintf("up to %d %s", as.integer(x[[2]]), x[[1]]),
    character(1)
  )

  tags$p(
    sprintf(
      "Per transaction limits: %s, and a maximum of %s total.",
      paste(parts, collapse = ", "),
      fmt_cad(max_amount)
    ),
    style = "font-size:0.9em; color:#666;"
  )
}

ui_limits_day <- function(cfg) {
  ui_limits_generic(
    items = list(
      c("adult day passes",   cfg$max_day_adult),
      c("youth day passes",   cfg$max_day_youth),
      c("under-9 entries",    cfg$max_day_under9),
      c("family day passes",  cfg$max_day_family)
    ),
    max_amount = cfg$max_day_amount
  )
}

ui_price_line_season <- function(cfg, early, cutoff_date) {
  if (early) {
    tags$p(
      sprintf(
        "Early-bird until %s – Adult %s · Youth %s",
        format(cutoff_date, "%Y-%m-%d"),
        fmt_cad_cents(cfg$price_season_adult_early),
        fmt_cad_cents(cfg$price_season_youth_early)
      )
    )
  } else {
    tags$p(
      sprintf(
        "Adult %s · Youth %s",
        fmt_cad_cents(cfg$price_season_adult),
        fmt_cad_cents(cfg$price_season_youth)
      )
    )
  }
}

ui_limits_season <- function(cfg) {
  ui_limits_generic(
    items = list(
      c("adult season passes", cfg$max_season_adult),
      c("youth season passes", cfg$max_season_youth)
    ),
    max_amount = cfg$max_season_amount
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
  ui_limits_generic(
    items = list(
      c("Christmas passes", cfg$max_christmas_passes)
    ),
    max_amount = cfg$max_christmas_amount
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

render_holder_inputs <- function(n, prefix,
                                  fields = c("name", "dob"),
                                  label_prefix = "Holder") {
  if (n <= 0L) return(NULL)

  lapply(seq_len(n), function(i) {
    inputs <- list()

    if ("name" %in% fields) {
      inputs[[length(inputs) + 1]] <- textInput(
        paste0(prefix, "_name_", i),
        paste(label_prefix, i, "- full name")
      )
    }

    if ("dob" %in% fields) {
      inputs[[length(inputs) + 1]] <- dateInput(
        paste0(prefix, "_dob_", i),
        "Date of birth",
        value  = NULL,
        format = "yyyy-mm-dd"
      )
    }

    if ("notes" %in% fields) {
      inputs[[length(inputs) + 1]] <- textInput(
        paste0(prefix, "_notes_", i),
        "Notes (optional)"
      )
    }

    do.call(
      tagList,
      c(
        list(h5(paste(label_prefix, i))),
        inputs,
        list(tags$hr())
      )
    )
  })
}

ui_when_enabled <- function(output_flag, enabled_ui, disabled_title, disabled_msg) {
  tagList(
    conditionalPanel(
      condition = sprintf("output.app_loaded && output.%s", output_flag),
      enabled_ui
    ),
    conditionalPanel(
      condition = sprintf("output.app_loaded && !output.%s", output_flag),
      tagList(
        h2(disabled_title),
        p(disabled_msg, style = "color:#555; margin-top:0.75rem;")
      )
    ),
    conditionalPanel(
      condition = "!output.app_loaded",
      tagList(
        h2("Please wait while the payment app loads"),
        p(
          "If this message does not disappear after a few seconds, please refresh the page.",
          style = "color:#555; margin-top:0.75rem;"
        )
      )
    )
  )
}

sandbox_banner <- if (SQUARE_ENV == "sandbox") {
  div(
    style = "background:#fee; border:1px solid #f88; padding:6px; margin-bottom:12px; font-size:0.9em;",
    strong("SANDBOX – TEST MODE ONLY. NO REAL CARD CHARGES.")
  )
} else {
  NULL
}

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
    .datepicker.dropdown-menu {
      font-size: 14px;
      padding: 4px;
      margin-top: 40px;
    }
    .datepicker .datepicker-switch { width: 190px; }

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
    input[type='text'], input[type='number'], .form-control {
      font-size: 14px;
    }
  "))
)

# -----------------------------------------------------------------------------
# UI TABS
# -----------------------------------------------------------------------------

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
            "Pay for your day passes here, then take a ticket from the box at the trailhead and write that you paid online with your name.",
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
            "Purchase adult or youth season passes and register each pass holder by name and date of birth.",
            style = "color:#555; margin-bottom:0.75rem;"
          ),
          uiOutput("season_price_line"),
          uiOutput("season_limits_text"),
          h4("Number of season passes"),
          numericInput("n_season_adult",  "Adult season passes", value = 1, min = 0, max = 100, step = 1),
          numericInput("n_season_youth",  "Youth season passes", value = 0, min = 0, max = 100, step = 1),
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

programs_tab <- tabPanel(
  "PROGRAMS",
  fluidPage(
    div(
      style = "max-width: 750px; margin: 0 auto; padding: 1.5rem;",
      sandbox_banner,
      ui_when_enabled(
        "programs_tab_enabled",
        tagList(
          h2("Programs"),
          p(
            "Register skiers for club programs. Age categories are based on age at December 31 of the ski season.",
            style = "color:#555; margin-bottom:0.75rem;"
          ),
          uiOutput("programs_price_line"),
          uiOutput("programs_limits_text"),
          numericInput("n_program_participants",
                       "Number of participants to register",
                       value = 1, min = 0, max = 100, step = 1),
          uiOutput("programs_holder_form"),
          tags$hr(),
          uiOutput("programs_summary"),
          textInput("programs_name",  "Purchaser name (for records)"),
          textInput("programs_email", "Purchaser email (for Square receipt)"),
          tags$hr(),
          actionButton(
            "programs_pay",
            "Pay for programs",
            class = "btn btn-primary btn-lg btn-block"
          ),
          tags$br(),
          uiOutput("programs_status")
        ),
        "Programs – not available",
        "Online registration for programs is currently disabled. Please contact the club for details."
      )
    )
  )
)

special_events_tab <- tabPanel(
  "SPECIAL EVENTS",
  fluidPage(
    div(
      style = "max-width: 650px; margin: 0 auto; padding: 1.5rem;",
      sandbox_banner,
      ui_when_enabled(
        "special_tab_enabled",
        tagList(
          h2("Special events"),
          p(
            "Register and pay for special events organized by the club.",
            style = "color:#555; margin-bottom:0.75rem;"
          ),
          selectInput(
            "special_event_id",
            "Choose event",
            choices = c("No special events available" = "")
          ),
          uiOutput("special_event_details"),
          numericInput(
            "special_n_tickets",
            "Number of entries",
            value = 1, min = 0, max = 100, step = 1
          ),
          tags$hr(),
          uiOutput("special_summary"),
          textInput("special_name",  "Purchaser name (for records)"),
          textInput("special_email", "Purchaser email (for Square receipt)"),
          tags$hr(),
          actionButton(
            "special_pay",
            "Pay for special event",
            class = "btn btn-primary btn-lg btn-block"
          ),
          tags$br(),
          uiOutput("special_status")
        ),
        "Special events – not available",
        "There are currently no special events available for online registration."
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
        "View logs, adjust prices and limits, manage blocked dates, special events, and control which public tabs are live.",
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
  programs_tab,
  special_events_tab,
  admin_tab
)

app_ui <- tagList(
  base_head,
  ui_core
)

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------

admin_body_ui <- function(config_rv, tab_flags) {
  tabsetPanel(
    id = "admin_tabs",

    tabPanel(
      "Prices / limits / tabs",
      br(),

      # Early-bird cutoff ----------------------------------------------------
      h4("Early-bird cutoff"),
      p("This date controls when early-bird pricing applies for season passes and programs."),
      dateInput(
        "early_bird_cutoff_edit",
        "Early-bird cutoff date",
        value = {
          d <- suppressWarnings(as.Date(config_rv$early_bird_cutoff))
          if (is.na(d)) Sys.Date() + 30 else d
        },
        format = "yyyy-mm-dd"
      ),

      tags$hr(),

      # Day-pass prices + limits --------------------------------------------
      h4("Day-pass prices and limits (CAD)"),
      p("Adjust prices and transaction limits for online day-pass purchases."),
      fluidRow(
        column(3, numericInput("price_adult",   "Adult ($)",   value = to_dollars(config_rv$price_adult_day),  min = 0, step = 0.5)),
        column(3, numericInput("price_youth",   "Youth ($)",   value = to_dollars(config_rv$price_youth_day),  min = 0, step = 0.5)),
        column(3, numericInput("price_family",  "Family ($)",  value = to_dollars(config_rv$price_family_day), min = 0, step = 0.5)),
        column(3, numericInput("price_under9",  "Under-9 ($)", value = to_dollars(config_rv$price_under9_day), min = 0, step = 0.5))
      ),
      fluidRow(
        column(3, numericInput("max_day_adult_edit",   "Max adult passes",   value = config_rv$max_day_adult,   min = 0, step = 1)),
        column(3, numericInput("max_day_youth_edit",   "Max youth passes",   value = config_rv$max_day_youth,   min = 0, step = 1)),
        column(3, numericInput("max_day_under9_edit",  "Max under-9",        value = config_rv$max_day_under9,  min = 0, step = 1)),
        column(3, numericInput("max_day_family_edit",  "Max family passes",  value = config_rv$max_day_family,  min = 0, step = 1))
      ),
      numericInput(
        "max_day_amount_edit",
        "Max day-pass transaction amount ($)",
        value = config_rv$max_day_amount, min = 0, step = 1
      ),

      tags$hr(),

      # Christmas pass price + limits ---------------------------------------
      h4("Christmas pass price and limits (CAD)"),
      p("Adjust price and per-transaction limits for the Christmas 2-week pass."),
      fluidRow(
        column(4, numericInput(
          "season_price_christmas",
          "Christmas 2-week price ($)",
          value = to_dollars(config_rv$price_christmas_pass),
          min   = 0,
          step  = 1
        )),
        column(4, numericInput(
          "max_christmas_passes_edit",
          "Max Christmas passes / transaction",
          value = config_rv$max_christmas_passes,
          min   = 0,
          step  = 1
        )),
        column(4, numericInput(
          "max_christmas_amount_edit",
          "Max Christmas tx amount ($)",
          value = config_rv$max_christmas_amount,
          min   = 0,
          step  = 1
        ))
      ),

      tags$hr(),

      # Season pass prices + limits (table layout) ---------------------------
      h4("Season pass prices and limits (CAD)"),
      p("Early-bird and regular prices side by side for each season-pass category."),
      tags$table(
        class = "table table-condensed",
        tags$thead(
          tags$tr(
            tags$th("Category"),
            tags$th("Early-bird price ($)"),
            tags$th("Regular price ($)")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Adult"),
            tags$td(
              numericInput(
                "season_price_adult_early",
                NULL,
                value = to_dollars(config_rv$price_season_adult_early),
                min   = 0,
                step  = 1
              )
            ),
            tags$td(
              numericInput(
                "season_price_adult_reg",
                NULL,
                value = to_dollars(config_rv$price_season_adult),
                min   = 0,
                step  = 1
              )
            )
          ),
          tags$tr(
            tags$td("Youth (9–18)"),
            tags$td(
              numericInput(
                "season_price_youth_early",
                NULL,
                value = to_dollars(config_rv$price_season_youth_early),
                min   = 0,
                step  = 1
              )
            ),
            tags$td(
              numericInput(
                "season_price_youth_reg",
                NULL,
                value = to_dollars(config_rv$price_season_youth),
                min   = 0,
                step  = 1
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          6,
          textInput(
            "season_label_edit",
            "Season label",
            value = config_rv$season_label
          )
        ),
        column(
          3,
          numericInput(
            "max_season_adult_edit",
            "Max adult season passes",
            value = config_rv$max_season_adult,
            min   = 0,
            step  = 1
          )
        ),
        column(
          3,
          numericInput(
            "max_season_youth_edit",
            "Max youth season passes",
            value = config_rv$max_season_youth,
            min   = 0,
            step  = 1
          )
        )
      ),
      numericInput(
        "max_season_amount_edit",
        "Max season transaction amount ($)",
        value = config_rv$max_season_amount,
        min   = 0,
        step  = 1
      ),

      tags$hr(),

      # Donation limits ------------------------------------------------------
      h4("Donation limits (CAD)"),
      p("Maximum donation amount per online transaction."),
      numericInput(
        "max_donation_amount_edit",
        "Max donation per transaction ($)",
        value = config_rv$max_donation_amount,
        min   = 0,
        step  = 1
      ),

      tags$hr(),

      # Program prices + limits ---------------------------------------------
      h4("Program prices and limits (CAD)"),
      p("Early-bird and regular prices side by side for each category."),
      tags$table(
        class = "table table-condensed",
        tags$thead(
          tags$tr(
            tags$th("Category"),
            tags$th("Early-bird price ($)"),
            tags$th("Regular price ($)")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("4–5 yrs"),
            tags$td(
              numericInput("price_program_4_5_early", NULL,
                          value = to_dollars(config_rv$price_program_4_5_early),
                          min   = 0, step = 1)
            ),
            tags$td(
              numericInput("price_program_4_5", NULL,
                          value = to_dollars(config_rv$price_program_4_5),
                          min   = 0, step = 1)
            )
          ),
          tags$tr(
            tags$td("6–10 yrs"),
            tags$td(
              numericInput("price_program_6_10_early", NULL,
                          value = to_dollars(config_rv$price_program_6_10_early),
                          min   = 0, step = 1)
            ),
            tags$td(
              numericInput("price_program_6_10", NULL,
                          value = to_dollars(config_rv$price_program_6_10),
                          min   = 0, step = 1)
            )
          ),
          tags$tr(
            tags$td("11–12 yrs"),
            tags$td(
              numericInput("price_program_11_12_early", NULL,
                          value = to_dollars(config_rv$price_program_11_12_early),
                          min   = 0, step = 1)
            ),
            tags$td(
              numericInput("price_program_11_12", NULL,
                          value = to_dollars(config_rv$price_program_11_12),
                          min   = 0, step = 1)
            )
          ),
          tags$tr(
            tags$td("13–14 yrs"),
            tags$td(
              numericInput("price_program_13_14_early", NULL,
                          value = to_dollars(config_rv$price_program_13_14_early),
                          min   = 0, step = 1)
            ),
            tags$td(
              numericInput("price_program_13_14", NULL,
                          value = to_dollars(config_rv$price_program_13_14),
                          min   = 0, step = 1)
            )
          ),
          tags$tr(
            tags$td("15–16 yrs"),
            tags$td(
              numericInput("price_program_15_16_early", NULL,
                          value = to_dollars(config_rv$price_program_15_16_early),
                          min   = 0, step = 1)
            ),
            tags$td(
              numericInput("price_program_15_16", NULL,
                          value = to_dollars(config_rv$price_program_15_16),
                          min   = 0, step = 1)
            )
          ),
          tags$tr(
            tags$td("17–18 yrs"),
            tags$td(
              numericInput("price_program_17_18_early", NULL,
                          value = to_dollars(config_rv$price_program_17_18_early),
                          min   = 0, step = 1)
            ),
            tags$td(
              numericInput("price_program_17_18", NULL,
                          value = to_dollars(config_rv$price_program_17_18),
                          min   = 0, step = 1)
            )
          ),
          tags$tr(
            tags$td("Masters"),
            tags$td(
              numericInput("price_program_masters_early", NULL,
                          value = to_dollars(config_rv$price_program_masters_early),
                          min   = 0, step = 1)
            ),
            tags$td(
              numericInput("price_program_masters", NULL,
                          value = to_dollars(config_rv$price_program_masters),
                          min   = 0, step = 1)
            )
          )
        )
      ),
      fluidRow(
        column(6, numericInput("max_program_participants_edit", "Max participants / transaction", value = config_rv$max_program_participants, min = 0, step = 1)),
        column(6, numericInput("max_program_amount_edit",       "Max program transaction amount ($)", value = config_rv$max_program_amount, min = 0, step = 1))
      ),

      tags$hr(),

      # Special events -------------------------------------------------------
      h4("Special events"),
      p("Create special events that appear in the SPECIAL EVENTS tab. You can also set events active/inactive or delete them."),
      fluidRow(
        column(4, textInput("special_name_new", "Event name")),
        column(4, dateInput("special_date_new", "Event date", value = Sys.Date())),
        column(4, numericInput("special_price_new", "Price per entry ($)", value = 10, min = 0, step = 0.5))
      ),
      fluidRow(
        column(6, numericInput("special_max_tickets_new", "Max entries per transaction", value = 10, min = 1, step = 1)),
        column(6, numericInput("special_max_amount_new",  "Max amount per transaction ($)", value = 500, min = 0, step = 1))
      ),
      actionButton("special_add_event", "Add special event"),
      tags$br(), tags$br(),

      h5("Manage existing events"),
      fluidRow(
        column(6, selectInput("special_admin_event_id", "Select event", choices = c("No events" = ""))),
        column(3, checkboxInput("special_admin_active", "Event active", value = TRUE)),
        column(3,
               div(
                 style = "margin-top: 25px;",
                 actionButton("special_admin_update_active", "Update status"),
                 tags$br(), tags$br(),
                 actionButton("special_admin_delete", "Delete event", class = "btn btn-danger")
               ))
      ),
      tags$br(),
      tableOutput("special_events_admin_table"),

      tags$hr(),

      # Tab availability -----------------------------------------------------
      h4("Tab availability (live / disabled)"),
      fluidRow(
        column(3, checkboxInput("enable_day_tab",       "DAY PASS tab live",       value = isTRUE(tab_flags$day))),
        column(3, checkboxInput("enable_christmas_tab", "CHRISTMAS PASS tab live", value = isTRUE(tab_flags$christmas))),
        column(3, checkboxInput("enable_season_tab",    "SEASON PASS tab live",    value = isTRUE(tab_flags$season))),
        column(3, checkboxInput("enable_donation_tab",  "DONATION tab live",       value = isTRUE(tab_flags$donation)))
      ),
      fluidRow(
        column(3, checkboxInput("enable_programs_tab",  "PROGRAMS tab live",       value = isTRUE(tab_flags$programs))),
        column(3, checkboxInput("enable_special_tab",   "SPECIAL EVENTS tab live", value = isTRUE(tab_flags$special)))
      ),

      tags$hr(),

      # Blocked dates --------------------------------------------------------
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
      tableOutput("blocked_dates_table"),

      tags$br(),
      actionButton("save_all_prices_limits_tab_flags", "Save all price/limit/tab changes", class = "btn btn-primary")
    ),

    tabPanel(
      "Reports",
      br(),

      h4("System health"),
      p("Quick checks for treasurer handover and troubleshooting."),
      tags$ul(
        tags$li(strong("Database: "), textOutput("admin_db_health", inline = TRUE)),
        tags$li(strong("Environment: "), textOutput("admin_square_env_health", inline = TRUE)),
        tags$li(strong("Early-bird cutoff: "), textOutput("admin_early_bird_health", inline = TRUE))
      ),
      h5("Recent system events"),
      tableOutput("admin_recent_events"),

      tags$hr(),
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
            choices  = c("Any", "day", "season", "christmas", "donation", "programs", "special_event"),
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
      downloadButton("download_all", "Download full CSV"),

      tags$hr(),
      h4("Database backup"),
      p("Download a backup of the SQLite database for safe-keeping or handover to a new treasurer."),
      downloadButton("download_db_backup", "Download database backup")
    )
  )
}

server <- function(input, output, session) {

  output$app_loaded <- reactive({ TRUE })
  outputOptions(output, "app_loaded", suspendWhenHidden = FALSE)

  admin_session_id <- uuid::UUIDgenerate()

  config_initial <- load_config()
  config_rv <- do.call(reactiveValues, config_initial)
  get_cfg <- function() reactiveValuesToList(config_rv)

  blocked_dates <- reactiveVal(get_blocked_dates())

  flags_initial <- load_tab_flags()
  tab_flags <- reactiveValues(
    day       = isTRUE(flags_initial$day),
    christmas = isTRUE(flags_initial$christmas),
    season    = isTRUE(flags_initial$season),
    donation  = isTRUE(flags_initial$donation),
    programs  = isTRUE(flags_initial$programs),
    special   = isTRUE(flags_initial$special)
  )

  admin_ok <- reactiveVal(FALSE)

  # Special events cache
  special_events_all_rv    <- reactiveVal(load_special_events(active_only = FALSE))
  special_events_active_rv <- reactiveVal(load_special_events(active_only = TRUE))

  refresh_special_events <- function() {
    special_events_all_rv(load_special_events(active_only = FALSE))
    special_events_active_rv(load_special_events(active_only = TRUE))
  }

  # --- admin rate limiter -----------------------------------------------------

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

    ctx <- list(
      host = session$clientData$url_hostname,
      path = session$clientData$url_pathname,
      ua   = session$clientData$user_agent
    )

    if (isTRUE(admin_is_locked())) {
      remaining <- ceiling(as.numeric(difftime(admin_rl$lock_until, now, units = "secs")))
      log_event(
        "admin_login_attempt",
        list(
          status         = "blocked",
          reason         = "lockout_active",
          remaining_secs = max(0, remaining),
          admin_session  = admin_session_id,
          client         = ctx
        )
      )
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
      log_event(
        "admin_login_attempt",
        list(
          status        = "error",
          error_message = err_msg,
          admin_session = admin_session_id,
          client        = ctx
        )
      )
      return()
    }

    if (ok) {
      admin_ok(TRUE)
      admin_rl$fail_times <- as.POSIXct(character(0))
      admin_rl$lock_until <- as.POSIXct(NA)
      output$admin_auth_message <- renderText("Admin unlocked for this session.")
      log_event(
        "admin_login_attempt",
        list(
          status        = "success",
          admin_session = admin_session_id,
          client        = ctx
        )
      )
      refresh_special_events()
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
      log_event(
        "admin_login_attempt",
        list(
          status        = "failure_lockout",
          fail_count    = n_fails,
          max_attempts  = CONST$admin_max_attempts,
          admin_session = admin_session_id,
          client        = ctx
        )
      )
      return()
    }

    admin_ok(FALSE)
    output$admin_auth_message <- renderText(
      sprintf("Incorrect password. Attempt %d of %d.", n_fails, CONST$admin_max_attempts)
    )
    log_event(
      "admin_login_attempt",
      list(
        status        = "failure",
        fail_count    = n_fails,
        max_attempts  = CONST$admin_max_attempts,
        admin_session = admin_session_id,
        client        = ctx
      )
    )
  })

  # --- expose tab flags -------------------------------------------------------

  output$day_tab_enabled       <- reactive({ tab_flags$day })
  output$christmas_tab_enabled <- reactive({ tab_flags$christmas })
  output$season_tab_enabled    <- reactive({ tab_flags$season })
  output$donation_tab_enabled  <- reactive({ tab_flags$donation })
  output$programs_tab_enabled  <- reactive({ tab_flags$programs })
  output$special_tab_enabled   <- reactive({ tab_flags$special })

  outputOptions(output, "day_tab_enabled",       suspendWhenHidden = FALSE)
  outputOptions(output, "christmas_tab_enabled", suspendWhenHidden = FALSE)
  outputOptions(output, "season_tab_enabled",    suspendWhenHidden = FALSE)
  outputOptions(output, "donation_tab_enabled",  suspendWhenHidden = FALSE)
  outputOptions(output, "programs_tab_enabled",  suspendWhenHidden = FALSE)
  outputOptions(output, "special_tab_enabled",   suspendWhenHidden = FALSE)

  # --- season header ----------------------------------------------------------

output$season_header <- renderText({
  cfg <- get_cfg()
  paste("Season passes –", cfg$season_label)
})

  # --- early-bird reactive ----------------------------------------------------

  is_early_bird <- reactive({
    cfg <- get_cfg()
    cutoff_str <- cfg$early_bird_cutoff %||% ""
    cutoff_date <- suppressWarnings(as.Date(cutoff_str))
    if (is.na(cutoff_date)) return(FALSE)
    Sys.Date() <= cutoff_date
  })

  # --- central UI bindings ----------------------------------------------------

  output$price_line          <- renderUI({ ui_price_line_day(get_cfg()) })
  output$day_limits_text     <- renderUI({ ui_limits_day(get_cfg()) })
  output$season_limits_text  <- renderUI({ ui_limits_season(get_cfg()) })
  output$christmas_price_line <- renderUI({ ui_price_line_christmas(get_cfg()) })
  output$christmas_limits_text <- renderUI({ ui_limits_christmas(get_cfg()) })
  output$donation_limits_text  <- renderUI({ ui_limits_donation(get_cfg()) })

  output$programs_price_line <- renderUI({
    cfg   <- get_cfg()
    early <- is_early_bird()

    price_for <- function(row_key) {
      key <- if (early) {
        paste0("price_program_", row_key, "_early")
      } else {
        paste0("price_program_", row_key)
      }
      fmt_cad_cents(cfg[[key]])
    }

    tagList(
      h4(if (early) "Early-bird program prices" else "Regular program prices"),
      tags$table(
        class = "table table-condensed",
        tags$thead(
          tags$tr(
            tags$th("Category"),
            tags$th("Price")
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(PROGRAM_CATS)), function(i) {
            tags$tr(
              tags$td(PROGRAM_CATS$label[i]),
              tags$td(price_for(PROGRAM_CATS$key[i]))
            )
          })
        )
      )
    )
  })

  output$season_price_line <- renderUI({
    cfg <- get_cfg()
    cutoff <- suppressWarnings(as.Date(cfg$early_bird_cutoff))
    if (is.na(cutoff)) cutoff <- Sys.Date()
    ui_price_line_season(cfg, early = is_early_bird(), cutoff_date = cutoff)
  })

  output$programs_limits_text <- renderUI({
    cfg <- get_cfg()
    tags$p(
      sprintf(
        "Per transaction limits: up to %d participants and a maximum of %s total.",
        as.integer(cfg$max_program_participants),
        fmt_cad(cfg$max_program_amount)
      ),
      style = "font-size:0.9em; color:#666;"
    )
  })

  # --- querystring success → payment check -----------------------------------

  observe({
    query <- shiny::getQueryString()
    if (isTRUE(query[["success"]] == "1")) {
      con <- get_db_connection()
      on.exit(dbDisconnect(con), add = TRUE)

      pending <- dbGetQuery(
        con,
        "
        SELECT id, square_order_id
        FROM transactions
        WHERE status IS NULL OR status = 'PENDING'
        ORDER BY id DESC
        LIMIT 1
        "
      )

      if (nrow(pending) == 0) {
        showModal(
          modalDialog(
            title = "Payment return received",
            "If your payment completed, you will receive a Square email receipt. If you do not see a receipt, please contact the club.",
            easyClose = TRUE,
            footer = NULL
          )
        )
        return()
      }

      tx_id    <- pending$id[1]
      order_id <- pending$square_order_id[1]

      if (is.na(order_id) || !nzchar(order_id)) {
        log_event("payment_return_no_order_id", list(tx_id = tx_id))
        showModal(
          modalDialog(
            title = "Return from Square",
            tagList(
              p("We received your return from Square, but could not automatically confirm the order yet."),
              p("If you have a Square email receipt, your payment went through."),
              p("The treasurer can reconcile this transaction if needed.")
            ),
            easyClose = TRUE,
            footer = NULL
          )
        )
        return()
      }

      actual <- check_square_payment_status(order_id)

      if (actual == "PAID") {
        dbExecute(con, "UPDATE transactions SET status = 'PAID' WHERE id = ?", params = list(tx_id))
        log_event("payment_confirmed", list(tx_id = tx_id, order_id = order_id))
        showModal(modalDialog(
          title = "Payment confirmed",
          tagList(
            p("Thank you — your payment has been verified with Square."),
            p("You will receive an email receipt."),
            tags$hr(),
            p(strong("Next step:"), " Take a ticket from the box at the trailhead and write your name and 'PAID ONLINE' on it.")
          ),
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        showModal(
          modalDialog(
            title = "Return from Square",
            "We could not automatically confirm the payment yet. If you have a Square email receipt, your payment went through. The treasurer can reconcile PENDING transactions against Square as needed.",
            easyClose = TRUE,
            footer = NULL
          )
        )
      }
    }
  })

  # --- generic helpers --------------------------------------------------------

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

  write_tx <- function(info_id, order_id, product_type, total_cents,
                       name, email,
                       ski_date = Sys.Date(),
                       adults = 0L, youths = 0L, under9 = 0L, families = 0L,
                       christmas_passes = 0L,
                       donation_cents = 0L,
                       status = "PENDING") {

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
      checkout_id      = sanitize_for_storage(info_id),
      payment_link_id  = sanitize_for_storage(info_id),
      square_order_id  = sanitize_for_storage(order_id),
      status           = status,
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

  # ============================================================================
  # DAY PASSES
  # ============================================================================

day_total <- reactive({
  cfg      <- get_cfg()
  adults   <- as_int0(input$n_adult)
  youths   <- as_int0(input$n_youth)
  under9   <- as_int0(input$n_under9)
  families <- as_int0(input$n_family)

  passes_cents <- adults   * cfg$price_adult_day +
    youths   * cfg$price_youth_day +
    under9   * cfg$price_under9_day +
    families * cfg$price_family_day

  list(
    adults       = adults,
    youths       = youths,
    under9       = under9,
    families     = families,
    passes_cents = passes_cents,
    total_cents  = passes_cents
  )
})

  output$ski_date_warning <- renderUI({
    d  <- as.Date(input$ski_date)
    bd <- blocked_dates()
    if (is.na(d) || length(bd) == 0) return(NULL)
    if (d %in% bd) {
      tags$p(
        "This date is blocked for online day-pass sales. Please choose another date.",
        style = "color:#b00; font-weight:600; margin-top: -6px;"
      )
    } else NULL
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

  cfg <- get_cfg()

  if (t$total_cents <= 0) {
    tags$p("Select at least one paid pass.", style = "color:#b00;")
  } else if (t$total_cents > cfg$max_day_amount * 100) {
    tags$p(
      sprintf("Total exceeds the %s day-pass limit.", fmt_cad(cfg$max_day_amount)),
      style = "color:#b00; font-weight:600;"
    )
  } else NULL
})

  output$summary <- renderUI({
    t <- day_total()
    if (t$adults == 0 && t$youths == 0 && t$families == 0 && t$under9 == 0) {
      return(tags$p("No passes selected.", style = "font-weight: 600;"))
    }

    lines <- list(tags$p(
      sprintf(
        "Adult: %d, Youth: %d, Under 9: %d, Family passes: %d",
        t$adults, t$youths, t$under9, t$families
      )
    ))

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

enforce_max("n_adult",  function() get_cfg()$max_day_adult,
            "Maximum %d adult day passes per transaction.")
enforce_max("n_youth",  function() get_cfg()$max_day_youth,
            "Maximum %d youth day passes per transaction.")
enforce_max("n_under9", function() get_cfg()$max_day_under9,
            "Maximum %d under-9 entries per transaction.")
enforce_max("n_family", function() get_cfg()$max_day_family,
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

      cfg   <- get_cfg()
      calc  <- day_total()
      d     <- as.Date(input$ski_date)
      today <- Sys.Date()
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
        info_id      = info$id,
        order_id     = info$order_id,
        product_type = "day",
        total_cents  = calc$total_cents,
        name         = input$name,
        email        = input$email,
        ski_date     = d,
        adults       = calc$adults,
        youths       = calc$youths,
        under9       = calc$under9,
        families     = calc$families
      )

      redirect_square(info$url, "status", "Redirecting to Square payment page...")
    })
  })

  # ============================================================================
  # DONATIONS
  # ============================================================================

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
  cfg <- get_cfg()
  val <- as_num0(input$donation_only_amount)
  if (is.na(val) || val < 0) {
    updateNumericInput(session, "donation_only_amount", value = 0)
    return()
  }
  if (val > cfg$max_donation_amount) {
    updateNumericInput(session, "donation_only_amount", value = cfg$max_donation_amount)
    showNotification(
      sprintf("Maximum online donation is %s per transaction.", fmt_cad(cfg$max_donation_amount)),
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

      cfg  <- get_cfg()
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
        info_id        = info$id,
        order_id       = info$order_id,
        product_type   = "donation",
        total_cents    = calc$donation_cents,
        name           = input$donation_only_name,
        email          = input$donation_only_email,
        donation_cents = calc$donation_cents
      )

      redirect_square(
        info$url,
        "donation_only_status",
        "Redirecting to Square payment page for donation..."
      )
    })
  })

  # ============================================================================
  # CHRISTMAS PASSES
  # ============================================================================

christmas_total <- reactive({
  cfg <- get_cfg()
  christmas   <- as_int0(input$n_christmas)
  total_cents <- christmas * cfg$price_christmas_pass

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
      render_holder_inputs(
        n            = n,
        prefix       = "christmas_holder",
        fields       = "name",
        label_prefix = "Pass"
      )
    )
  })

enforce_max(
  "n_christmas",
  function() get_cfg()$max_christmas_passes,
  "Maximum %d Christmas passes per transaction."
)

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

      holder_names <- collect_inputs(
        n      = n_passes,
        prefix = "christmas_holder",
        suffix = "name",
        input  = input,
        type   = "character"
      )

      christmas_start <- as.Date(input$christmas_start)

      validate_christmas_purchase(
        calc             = calc,
        holder_names     = holder_names,
        start_date       = christmas_start,
        purchaser_name   = input$christmas_name,
        purchaser_email  = input$christmas_email,
        cfg              = cfg
      )

      year          <- as.integer(format(christmas_start, "%Y"))
      christmas_day <- as.Date(sprintf("%d-12-25", year))
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
        info_id          = info$id,
        order_id         = info$order_id,
        product_type     = "christmas",
        total_cents      = calc$total_cents,
        name             = input$christmas_name,
        email            = input$christmas_email,
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

  # ============================================================================
  # SEASON PASSES
  # ============================================================================

  season_total <- reactive({
    adults <- as_int0(input$n_season_adult)
    youths <- as_int0(input$n_season_youth)
    cfg    <- get_cfg()

    price_adult <- if (is_early_bird()) cfg$price_season_adult_early else cfg$price_season_adult
    price_youth <- if (is_early_bird()) cfg$price_season_youth_early else cfg$price_season_youth

    passes_cents <- adults * price_adult + youths * price_youth

    list(
      adults       = adults,
      youths       = youths,
      passes_cents = passes_cents
    )
  })

  output$season_holder_form <- renderUI({
    na <- as_int0(input$n_season_adult)
    ny <- as_int0(input$n_season_youth)
    if (na <= 0L && ny <= 0L) return(NULL)

    pieces <- list(h4("Season pass holder details"))

    if (na > 0L) {
      pieces <- c(
        pieces,
        list(h5("Adult passes")),
        render_holder_inputs(
          n            = na,
          prefix       = "season_adult",
          fields       = c("name", "dob"),
          label_prefix = "Adult pass"
        )
      )
    }

    if (ny > 0L) {
      pieces <- c(
        pieces,
        list(h5("Youth passes")),
        render_holder_inputs(
          n            = ny,
          prefix       = "season_youth",
          fields       = c("name", "dob"),
          label_prefix = "Youth pass"
        )
      )
    }

    do.call(tagList, pieces)
  })

  output$season_summary <- renderUI({
    calc <- season_total()
    if (calc$adults == 0 && calc$youths == 0) {
      return(tags$p("No season passes selected.", style = "font-weight: 600;"))
    }

    tagList(
      tags$p(
        sprintf(
          "Adult season: %d, Youth season: %d",
          calc$adults, calc$youths
        )
      ),
      tags$p(
        sprintf("Total for season passes: %s", fmt_cad_cents(calc$passes_cents)),
        style = "font-weight: 700; font-size: 1.2rem;"
      )
    )
  })

enforce_max("n_season_adult",  function() get_cfg()$max_season_adult,
            "Maximum %d adult season passes per transaction.")
enforce_max("n_season_youth",  function() get_cfg()$max_season_youth,
            "Maximum %d youth season passes per transaction.")

  validate_season_purchase <- function(calc, adult_names, youth_names,
                                       adult_dobs, youth_dobs,
                                       purchaser_name, purchaser_email, cfg) {

    ref_date   <- Sys.Date()
    adult_ages <- compute_age_years(adult_dobs, ref_date)
    youth_ages <- compute_age_years(youth_dobs, ref_date)

    invalid_youth <- length(youth_ages) > 0 &&
      any(youth_ages < CONST$youth_min_age | youth_ages > CONST$youth_max_age, na.rm = TRUE)
    invalid_adult <- length(adult_ages) > 0 &&
      any(adult_ages < CONST$adult_min_age, na.rm = TRUE)

    validate(
      need(calc$adults > 0 || calc$youths > 0, "Please select at least one season pass."),
      need(nchar(trimws(purchaser_name))  > 0, "Please enter a name."),
      need(is_valid_email(purchaser_email), "Please enter a valid email address."),
      need(calc$adults <= cfg$max_season_adult,
           sprintf("Maximum %d adult season passes per transaction.", as.integer(cfg$max_season_adult))),
      need(calc$youths <= cfg$max_season_youth,
           sprintf("Maximum %d youth season passes per transaction.", as.integer(cfg$max_season_youth))),
      need(all(nzchar(adult_names)),  "Please enter a name for each adult season pass."),
      need(all(nzchar(youth_names)),  "Please enter a name for each youth season pass."),
      need(length(adult_dobs)  == 0 || all(!is.na(adult_dobs)),
           "Please enter a date of birth for each adult season pass holder."),
      need(length(youth_dobs)  == 0 || all(!is.na(youth_dobs)),
           "Please enter a date of birth for each youth season pass holder."),
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

      adult_names <- collect_inputs(
        n      = na,
        prefix = "season_adult",
        suffix = "name",
        input  = input,
        type   = "character"
      )

      youth_names <- collect_inputs(
        n      = ny,
        prefix = "season_youth",
        suffix = "name",
        input  = input,
        type   = "character"
      )

      adult_dobs <- collect_inputs(
        n      = na,
        prefix = "season_adult",
        suffix = "dob",
        input  = input,
        type   = "date"
      )

      youth_dobs <- collect_inputs(
        n      = ny,
        prefix = "season_youth",
        suffix = "dob",
        input  = input,
        type   = "date"
      )

      validate_season_purchase(
        calc            = calc,
        adult_names     = adult_names,
        youth_names     = youth_names,
        adult_dobs      = adult_dobs,
        youth_dobs      = youth_dobs,
        purchaser_name  = input$season_name,
        purchaser_email = input$season_email,
        cfg             = cfg
      )

      info <- safe_checkout(
        total_cents = calc$passes_cents,
        item_name   = paste("Bulkley Valley XC season passes –", cfg$season_label)
      )

      write_tx(
        info_id      = info$id,
        order_id     = info$order_id,
        product_type = "season",
        total_cents  = calc$passes_cents,
        name         = input$season_name,
        email        = input$season_email,
        adults       = na,
        youths       = ny,
        families     = 0L
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

      if (length(regs_rows) > 0L) {
        append_registrations(do.call(rbind, regs_rows))
      }

      redirect_square(info$url, "season_status",
                      "Redirecting to Square payment page for season passes...")
    })
  })

  # ============================================================================
  # PROGRAMS
  # ============================================================================

program_ref_date <- reactive({
  cfg   <- get_cfg()
  label <- cfg$season_label %||% ""
  m     <- regexpr("\\d{4}", label)

  if (m[1] > 0) {
    year <- as.integer(substr(label, m[1], m[1] + 3))
  } else {
    year <- as.integer(format(Sys.Date(), "%Y"))

    log_event(
      "program_ref_date_fallback",
      list(
        season_label = label,
        used_year    = year
      )
    )
  }

  as.Date(sprintf("%d-12-31", year))
})

  output$programs_holder_form <- renderUI({
    n <- as_int0(input$n_program_participants)
    if (n <= 0L) return(NULL)

    tagList(
      h4("Program participants"),
      render_holder_inputs(
        n            = n,
        prefix       = "program",
        fields       = c("name", "dob", "notes"),
        label_prefix = "Participant"
      )
    )
  })

programs_total <- reactive({
  n   <- as_int0(input$n_program_participants)
  cfg <- get_cfg()

  if (n <= 0L) {
    return(list(
      total_cents = 0L,
      groups      = character(0),
      ages        = integer(0),
      names       = character(0),
      invalid     = FALSE
    ))
  }

  ref_date <- program_ref_date()
  early    <- is_early_bird()

  dob_vec  <- collect_inputs(n, "program", "dob",  input, type = "date")
  name_vec <- collect_inputs(n, "program", "name", input, type = "character")

  ages   <- compute_age_years(dob_vec, ref_date)
  groups <- vapply(ages, program_group_for_age, FUN.VALUE = character(1))

  invalid <- is.na(groups)

  if (any(invalid)) {
    return(list(
      total_cents = NA_integer_,
      groups      = groups,
      ages        = ages,
      names       = name_vec,
      invalid     = TRUE
    ))
  }

  get_price_for_group <- function(g) {
    key <- if (early) {
      paste0("price_program_", g, "_early")
    } else {
      paste0("price_program_", g)
    }
    cfg[[key]]
  }

  prices      <- vapply(groups, get_price_for_group, integer(1))
  total_cents <- sum(as.integer(prices), na.rm = TRUE)

  list(
    total_cents = total_cents,
    groups      = groups,
    ages        = ages,
    names       = name_vec,
    invalid     = FALSE
  )
})

  output$programs_summary <- renderUI({
    res <- programs_total()
    n <- as_int0(input$n_program_participants)
    if (n <= 0L) {
      return(tags$p("No program participants entered.", style = "font-weight: 600;"))
    }

    if (isTRUE(res$invalid) || is.na(res$total_cents)) {
      return(tags$p(
        "One or more participants are missing a DOB or are outside the supported age range (minimum 4 years).",
        style = "color:#b00; font-weight:600;"
      ))
    }

    counts <- table(res$groups)

    tagList(
      tags$p("Assigned program categories (based on age at Dec 31):"),
      tags$ul(
        lapply(names(counts), function(g) {
          lbl <- PROGRAM_CATS$label[PROGRAM_CATS$key == g]
          if (length(lbl) == 0) lbl <- g
          tags$li(sprintf("%s: %d", lbl, as.integer(counts[[g]])))
        })
      ),
      tags$p(
        sprintf("Total for programs: %s", fmt_cad_cents(res$total_cents)),
        style = "font-weight:700; font-size:1.2rem;"
      )
    )
  })

  validate_programs_purchase <- function(res, purchaser_name, purchaser_email, cfg) {
    n <- length(res$groups)
    validate(
      need(n > 0, "Please add at least one participant."),
      need(!isTRUE(res$invalid) && !is.na(res$total_cents),
           "Please check DOBs – all participants must be at least 4 years old with valid dates."),
      need(all(nzchar(res$names)), "Please enter a name for each participant."),
      need(nchar(trimws(purchaser_name)) > 0, "Please enter a purchaser name."),
      need(is_valid_email(purchaser_email), "Please enter a valid email address."),
      need(n <= cfg$max_program_participants,
           sprintf("Maximum %d program participants per transaction.", as.integer(cfg$max_program_participants))),
      need(res$total_cents <= cfg$max_program_amount * 100,
           sprintf("Program transactions are limited to %s per purchase.", fmt_cad(cfg$max_program_amount)))
    )
  }

enforce_max(
  "n_program_participants",
  function() get_cfg()$max_program_participants,
  "Maximum %d program participants per transaction."
)
 
 observeEvent(input$programs_pay, {
    with_ui_error("programs_status", {
      if (!tab_flags$programs) {
        output$programs_status <- renderUI(
          tags$p("Programs payments are currently disabled.", style = "color:#b00;")
        )
        return()
      }

      cfg <- get_cfg()
      res <- programs_total()
      validate_programs_purchase(
        res             = res,
        purchaser_name  = input$programs_name,
        purchaser_email = input$programs_email,
        cfg             = cfg
      )

      info <- safe_checkout(
        total_cents = res$total_cents,
        item_name   = "Bulkley Valley XC programs"
      )

      write_tx(
        info_id      = info$id,
        order_id     = info$order_id,
        product_type = "programs",
        total_cents  = res$total_cents,
        name         = input$programs_name,
        email        = input$programs_email
      )

      n <- length(res$groups)
      if (n > 0) {
        dob_vec <- collect_inputs(
          n      = n,
          prefix = "program",
          suffix = "dob",
          input  = input,
          type   = "date"
        )

        notes_vec <- collect_inputs(
          n      = n,
          prefix = "program",
          suffix = "notes",
          input  = input,
          type   = "character"
        )

        regs_df <- data.frame(
          checkout_id  = rep(info$id, n),
          product_type = rep("programs", n),
          holder_name  = res$names,
          holder_type  = paste0("program_", res$groups),
          holder_dob   = as.character(dob_vec),
          notes        = notes_vec,
          stringsAsFactors = FALSE
        )
        append_registrations(regs_df)
      }

      redirect_square(
        info$url,
        "programs_status",
        "Redirecting to Square payment page for programs..."
      )
    })
  })

  # ============================================================================
  # SPECIAL EVENTS
  # ============================================================================

  observeEvent(input$special_n_tickets, ignoreInit = TRUE, {
    if (as_int0(input$special_n_tickets) < 0) {
      updateNumericInput(session, "special_n_tickets", value = 0)
    }
  })

  observe({
    df <- special_events_active_rv()
    if (!is.data.frame(df) || nrow(df) == 0) {
      updateSelectInput(
        session,
        "special_event_id",
        choices = c("No special events available" = "")
      )
      return()
    }

    labels  <- paste0(df$name, " (", df$event_date, ")")
    choices <- setNames(as.character(df$id), labels)

    updateSelectInput(
      session,
      "special_event_id",
      choices = choices
    )
  })

  observe({
    df <- special_events_all_rv()
    if (!isTRUE(admin_ok())) return()

    if (!is.data.frame(df) || nrow(df) == 0) {
      updateSelectInput(session, "special_admin_event_id", choices = c("No events" = ""))
      return()
    }

    labels <- paste0(
      df$name, " (", df$event_date, ") ",
      ifelse(as_int0(df$is_active) == 1L, "[ACTIVE]", "[INACTIVE]")
    )
    choices <- setNames(as.character(df$id), labels)

    updateSelectInput(session, "special_admin_event_id", choices = choices)
  })

  special_selected <- reactive({
    df <- special_events_active_rv()
    if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
    id_raw <- input$special_event_id
    if (is.null(id_raw) || id_raw == "") return(NULL)
    id <- as_int0(id_raw)
    ev <- df[df$id == id, , drop = FALSE]
    if (nrow(ev) == 0) return(NULL)
    ev
  })

  output$special_event_details <- renderUI({
    ev <- special_selected()
    if (is.null(ev)) {
      return(tags$p("No special event selected.", style = "color:#555; font-style:italic;"))
    }
    tagList(
      tags$p(strong(ev$name)),
      tags$p(sprintf("Event date: %s", ev$event_date)),
      tags$p(sprintf("Price per entry: %s", fmt_cad_cents(ev$price_cents))),
      tags$p(
        sprintf(
          "Per transaction: up to %d entries and a maximum of %s.",
          as_int0(ev$max_tickets_per_tx),
          fmt_cad(as_num0(ev$max_amount_dollars))
        ),
        style = "font-size:0.9em; color:#666;"
      )
    )
  })

  special_total <- reactive({
    ev <- special_selected()
    n  <- as_int0(input$special_n_tickets)
    if (is.null(ev) || n <= 0) {
      return(list(valid = FALSE, total_cents = 0L, n = n, ev = ev))
    }
    total_cents <- n * as_int0(ev$price_cents[1])
    list(valid = TRUE, total_cents = total_cents, n = n, ev = ev)
  })

  output$special_summary <- renderUI({
    res <- special_total()
    if (!res$valid || is.null(res$ev)) {
      return(tags$p("No entries selected.", style = "font-weight: 600;"))
    }
    ev <- res$ev
    tagList(
      tags$p(sprintf("Event: %s", ev$name)),
      tags$p(sprintf("Entries: %d", res$n)),
      tags$p(
        sprintf("Total to pay: %s", fmt_cad_cents(res$total_cents)),
        style = "font-weight: 700; font-size: 1.2rem;"
      )
    )
  })

  validate_special_purchase <- function(res, purchaser_name, purchaser_email) {
    ev <- res$ev
    validate(
      need(res$valid && !is.null(ev), "Please select an event and at least one entry."),
      need(nchar(trimws(purchaser_name)) > 0, "Please enter a purchaser name."),
      need(is_valid_email(purchaser_email), "Please enter a valid email address.")
    )

    max_t <- as_int0(ev$max_tickets_per_tx[1])
    max_a <- as_num0(ev$max_amount_dollars[1])

    if (res$n > max_t) {
      stop(sprintf("Maximum %d entries per transaction for this event.", max_t))
    }
    if (res$total_cents > max_a * 100) {
      stop(sprintf("This event is limited to %s per transaction.", fmt_cad(max_a)))
    }
  }

  observeEvent(input$special_pay, {
    with_ui_error("special_status", {
      if (!tab_flags$special) {
        output$special_status <- renderUI(
          tags$p("Special events payments are currently disabled.", style = "color:#b00;")
        )
        return()
      }

      res <- special_total()
      validate_special_purchase(
        res             = res,
        purchaser_name  = input$special_name,
        purchaser_email = input$special_email
      )

      ev <- res$ev
      item_name <- paste("Bulkley Valley XC special event –", ev$name[1])

      info <- safe_checkout(
        total_cents = res$total_cents,
        item_name   = item_name
      )

      event_date <- as.Date(ev$event_date[1])

      write_tx(
        info_id      = info$id,
        order_id     = info$order_id,
        product_type = "special_event",
        total_cents  = res$total_cents,
        name         = input$special_name,
        email        = input$special_email,
        ski_date     = event_date
      )

      redirect_square(
        info$url,
        "special_status",
        "Redirecting to Square payment page for special event..."
      )
    })
  })

  # ============================================================================
  # ADMIN: REPORTS + SETTINGS
  # ============================================================================

  filtered_log <- reactive({
    if (!isTRUE(admin_ok())) return(data.frame())

    df <- load_transactions()
    if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())

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
    if (!isTRUE(admin_ok())) return(NULL)

    df <- filtered_log()
    if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

    df$total_dollars    <- to_dollars(df$total_cents)
    df$donation_dollars <- to_dollars(df$donation_cents)

    df$created  <- as.character(df$created)
    df$ski_date <- as.character(df$ski_date)

    df[, c(
      "id", "created", "ski_date",
      "product_type",
      "adults", "youths", "under9", "families", "christmas_passes",
      "total_dollars", "donation_dollars",
      "status",
      "name", "email",
      "payment_link_id", "checkout_id",
      "square_order_id"
    )]
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

  output$download_db_backup <- downloadHandler(
    filename = function() {
      paste0("bvxc_db_backup_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(admin_ok())

      base_db <- "bvxc.sqlite"
      wal_db  <- "bvxc.sqlite-wal"
      shm_db  <- "bvxc.sqlite-shm"

      if (!file.exists(base_db)) {
        stop("Database file bvxc.sqlite not found in app directory.")
      }

      tryCatch({
        con <- get_db_connection()
        on.exit(dbDisconnect(con), add = TRUE)
        dbExecute(con, "PRAGMA wal_checkpoint(FULL);")
      }, error = function(e) {
        log_event("db_backup_warning", list(message = e$message))
      })

      db_files <- c(base_db)
      if (file.exists(wal_db)) db_files <- c(db_files, wal_db)
      if (file.exists(shm_db)) db_files <- c(db_files, shm_db)

      tmpdir <- tempfile("bvxc_db_backup_")
      dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)

      file.copy(db_files, file.path(tmpdir, basename(db_files)), overwrite = TRUE)

      oldwd <- getwd()
      on.exit(setwd(oldwd), add = TRUE)
      setwd(tmpdir)

      utils::zip(zipfile = file, files = basename(db_files))
    }
  )

  # --- Admin health panel outputs --------------------------------------------

  output$admin_db_health <- renderText({
    req(admin_ok())
    if (file.exists("bvxc.sqlite")) "OK – bvxc.sqlite present" else "WARNING – bvxc.sqlite missing"
  })

  output$admin_square_env_health <- renderText({
    req(admin_ok())
    paste0("Square environment: ", SQUARE_ENV)
  })

  output$admin_early_bird_health <- renderText({
    req(admin_ok())
    cfg <- get_cfg()
    paste0("Early-bird cutoff (config): ", cfg$early_bird_cutoff)
  })

  output$admin_recent_events <- renderTable({
    req(admin_ok())
    df <- load_event_log(5L)
    if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
    df
  })

  # --- blocked dates ----------------------------------------------------------

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

  # --- Admin: special events table + add + enable/disable/delete -------------

  output$special_events_admin_table <- renderTable({
    req(admin_ok())
    df <- special_events_all_rv()
    if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
    df[order(as.Date(df$event_date), decreasing = FALSE), ]
  })

  observeEvent(input$special_add_event, {
    req(admin_ok())

    name  <- trimws(input$special_name_new %||% "")
    date  <- as.Date(input$special_date_new)
    price <- as_num0(input$special_price_new)
    max_t <- as_int0(input$special_max_tickets_new)
    max_a <- as_num0(input$special_max_amount_new)

    validate(
      need(nzchar(name), "Please enter an event name."),
      need(!is.na(date), "Please choose an event date."),
      need(!is.na(price) && price >= 0, "Price cannot be negative."),
      need(max_t > 0, "Max entries per transaction must be at least 1."),
      need(!is.na(max_a) && max_a >= 0, "Max amount per transaction cannot be negative.")
    )

    con <- get_db_connection()
    on.exit(dbDisconnect(con), add = TRUE)
    dbWriteTable(
      con,
      "special_events",
      data.frame(
        name               = sanitize_for_storage(name),
        event_date         = as.character(date),
        price_cents        = to_cents(price),
        max_tickets_per_tx = as.integer(max_t),
        max_amount_dollars = max_a,
        is_active          = 1L,
        stringsAsFactors   = FALSE
      ),
      append = TRUE
    )

    refresh_special_events()

    showModal(
      modalDialog(
        title = "Special event added",
        "The event has been added and is available in the SPECIAL EVENTS tab (if marked active).",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  observeEvent(input$special_admin_event_id, {
    req(admin_ok())
    df <- special_events_all_rv()
    id_raw <- input$special_admin_event_id
    if (is.null(id_raw) || id_raw == "") {
      updateCheckboxInput(session, "special_admin_active", value = TRUE)
      return()
    }
    id <- as_int0(id_raw)
    ev <- df[df$id == id, , drop = FALSE]
    if (nrow(ev) == 0) return()
    updateCheckboxInput(session, "special_admin_active", value = as_int0(ev$is_active[1]) == 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$special_admin_update_active, {
    req(admin_ok())
    id_raw <- input$special_admin_event_id
    validate(need(!is.null(id_raw) && nzchar(id_raw), "Please select a special event."))
    id <- as_int0(id_raw)

    set_special_event_active(id, isTRUE(input$special_admin_active))
    refresh_special_events()

    showModal(
      modalDialog(
        title = "Special event updated",
        if (isTRUE(input$special_admin_active)) {
          "Event marked ACTIVE. It will be visible in the public SPECIAL EVENTS tab."
        } else {
          "Event marked INACTIVE. It will be hidden from the public SPECIAL EVENTS tab."
        },
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  observeEvent(input$special_admin_delete, {
    req(admin_ok())
    id_raw <- input$special_admin_event_id
    validate(need(!is.null(id_raw) && nzchar(id_raw), "Please select a special event to delete."))

    id <- as_int0(id_raw)

    delete_special_event(id)
    refresh_special_events()

    showModal(
      modalDialog(
        title = "Special event deleted",
        "The selected special event has been permanently deleted. Existing transactions remain in the log.",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  # --- admin body -------------------------------------------------------------

  output$admin_body <- renderUI({
    if (!admin_ok()) return(NULL)
    admin_body_ui(config_rv, tab_flags)
  })

  # --- save EVERYTHING from prices/limits/tabs panel -------------------------

  observeEvent(input$save_all_prices_limits_tab_flags, {
    req(admin_ok())

    cfg_before <- load_config()

    # Early-bird cutoff
    eb_date <- as.Date(input$early_bird_cutoff_edit)
    if (!is.na(eb_date)) {
      config_rv$early_bird_cutoff <- format(eb_date, "%Y-%m-%d")
    }

    # Day prices
    pa <- as_num0(input$price_adult)
    py <- as_num0(input$price_youth)
    pf <- as_num0(input$price_family)
    pu <- as_num0(input$price_under9)

    if (!is.na(pa) && pa >= 0) config_rv$price_adult_day  <- to_cents(pa)
    if (!is.na(py) && py >= 0) config_rv$price_youth_day  <- to_cents(py)
    if (!is.na(pf) && pf >= 0) config_rv$price_family_day <- to_cents(pf)
    if (!is.na(pu) && pu >= 0) config_rv$price_under9_day <- to_cents(pu)

    # Day limits
    mda     <- as_num0(input$max_day_adult_edit)
    mdy     <- as_num0(input$max_day_youth_edit)
    mdu     <- as_num0(input$max_day_under9_edit)
    mdf     <- as_num0(input$max_day_family_edit)
    mda_amt <- as_num0(input$max_day_amount_edit)

    if (!is.na(mda)     && mda >= 0)     config_rv$max_day_adult   <- mda
    if (!is.na(mdy)     && mdy >= 0)     config_rv$max_day_youth   <- mdy
    if (!is.na(mdu)     && mdu >= 0)     config_rv$max_day_under9  <- mdu
    if (!is.na(mdf)     && mdf >= 0)     config_rv$max_day_family  <- mdf
    if (!is.na(mda_amt) && mda_amt >= 0) config_rv$max_day_amount  <- mda_amt

    # Donation limits
    mdon <- as_num0(input$max_donation_amount_edit)
    if (!is.na(mdon) && mdon >= 0) config_rv$max_donation_amount <- mdon

    # Season/Christmas prices
    sa_e <- as_num0(input$season_price_adult_early)
    sy_e <- as_num0(input$season_price_youth_early)
    sa_r <- as_num0(input$season_price_adult_reg)
    sy_r <- as_num0(input$season_price_youth_reg)
    sc   <- as_num0(input$season_price_christmas)
    label <- input$season_label_edit %||% config_rv$season_label

    if (!is.na(sa_e) && sa_e >= 0) config_rv$price_season_adult_early  <- to_cents(sa_e)
    if (!is.na(sy_e) && sy_e >= 0) config_rv$price_season_youth_early  <- to_cents(sy_e)
    if (!is.na(sa_r) && sa_r >= 0) config_rv$price_season_adult        <- to_cents(sa_r)
    if (!is.na(sy_r) && sy_r >= 0) config_rv$price_season_youth        <- to_cents(sy_r)
    if (!is.na(sc)   && sc   >= 0) config_rv$price_christmas_pass      <- to_cents(sc)
    config_rv$season_label <- label

    # Season limits
    msa     <- as_num0(input$max_season_adult_edit)
    msy     <- as_num0(input$max_season_youth_edit)
    msa_amt <- as_num0(input$max_season_amount_edit)

    if (!is.na(msa)     && msa >= 0)     config_rv$max_season_adult  <- msa
    if (!is.na(msy)     && msy >= 0)     config_rv$max_season_youth  <- msy
    if (!is.na(msa_amt) && msa_amt >= 0) config_rv$max_season_amount <- msa_amt

    # Christmas limits
    mcp <- as_num0(input$max_christmas_passes_edit)
    mca <- as_num0(input$max_christmas_amount_edit)
    if (!is.na(mcp) && mcp >= 0) config_rv$max_christmas_passes <- mcp
    if (!is.na(mca) && mca >= 0) config_rv$max_christmas_amount <- mca

    # Program prices – early
    e_4_5  <- as_num0(input$price_program_4_5_early)
    e_6_10 <- as_num0(input$price_program_6_10_early)
    e_11_12<- as_num0(input$price_program_11_12_early)
    e_13_14<- as_num0(input$price_program_13_14_early)
    e_15_16<- as_num0(input$price_program_15_16_early)
    e_17_18<- as_num0(input$price_program_17_18_early)
    e_m    <- as_num0(input$price_program_masters_early)

    if (!is.na(e_4_5)   && e_4_5   >= 0) config_rv$price_program_4_5_early       <- to_cents(e_4_5)
    if (!is.na(e_6_10)  && e_6_10  >= 0) config_rv$price_program_6_10_early      <- to_cents(e_6_10)
    if (!is.na(e_11_12) && e_11_12 >= 0) config_rv$price_program_11_12_early     <- to_cents(e_11_12)
    if (!is.na(e_13_14) && e_13_14 >= 0) config_rv$price_program_13_14_early     <- to_cents(e_13_14)
    if (!is.na(e_15_16) && e_15_16 >= 0) config_rv$price_program_15_16_early     <- to_cents(e_15_16)
    if (!is.na(e_17_18) && e_17_18 >= 0) config_rv$price_program_17_18_early     <- to_cents(e_17_18)
    if (!is.na(e_m)     && e_m     >= 0) config_rv$price_program_masters_early   <- to_cents(e_m)

    # Program prices – regular
    r_4_5  <- as_num0(input$price_program_4_5)
    r_6_10 <- as_num0(input$price_program_6_10)
    r_11_12<- as_num0(input$price_program_11_12)
    r_13_14<- as_num0(input$price_program_13_14)
    r_15_16<- as_num0(input$price_program_15_16)
    r_17_18<- as_num0(input$price_program_17_18)
    r_m    <- as_num0(input$price_program_masters)

    if (!is.na(r_4_5)   && r_4_5   >= 0) config_rv$price_program_4_5             <- to_cents(r_4_5)
    if (!is.na(r_6_10)  && r_6_10  >= 0) config_rv$price_program_6_10            <- to_cents(r_6_10)
    if (!is.na(r_11_12) && r_11_12 >= 0) config_rv$price_program_11_12           <- to_cents(r_11_12)
    if (!is.na(r_13_14) && r_13_14 >= 0) config_rv$price_program_13_14           <- to_cents(r_13_14)
    if (!is.na(r_15_16) && r_15_16 >= 0) config_rv$price_program_15_16           <- to_cents(r_15_16)
    if (!is.na(r_17_18) && r_17_18 >= 0) config_rv$price_program_17_18           <- to_cents(r_17_18)
    if (!is.na(r_m)     && r_m     >= 0) config_rv$price_program_masters         <- to_cents(r_m)

    # Program limits
    mp <- as_num0(input$max_program_participants_edit)
    ma <- as_num0(input$max_program_amount_edit)
    if (!is.na(mp) && mp >= 0) config_rv$max_program_participants <- mp
    if (!is.na(ma) && ma >= 0) config_rv$max_program_amount       <- ma

    # Tab flags
    flags <- list(
      day       = isTRUE(input$enable_day_tab),
      christmas = isTRUE(input$enable_christmas_tab),
      season    = isTRUE(input$enable_season_tab),
      donation  = isTRUE(input$enable_donation_tab),
      programs  = isTRUE(input$enable_programs_tab),
      special   = isTRUE(input$enable_special_tab)
    )

    tab_flags$day       <- flags$day
    tab_flags$christmas <- flags$christmas
    tab_flags$season    <- flags$season
    tab_flags$donation  <- flags$donation
    tab_flags$programs  <- flags$programs
    tab_flags$special   <- flags$special

    cfg <- cfg_before

    # Copy everything from config_rv to cfg for persistence
    rv_list <- reactiveValuesToList(config_rv)
    for (k in names(rv_list)) {
      cfg[[k]] <- rv_list[[k]]
    }

    save_config(cfg)

    log_config_changes(
      cfg_before,
      cfg,
      context = list(source = "save_all_prices_limits_tab_flags", admin_session = admin_session_id)
    )

    save_tab_flags(flags, cfg = cfg)

    showModal(
      modalDialog(
        title = "Settings saved",
        "All price, limit, early-bird, and tab-availability changes have been saved and apply to new transactions.",
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
}

shinyApp(ui = app_ui, server = server)
