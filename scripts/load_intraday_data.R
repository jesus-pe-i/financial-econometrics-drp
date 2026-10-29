# ================================================================
# Historical intraday data in R: SPY and Bitcoin
# Sources:
#   SPY: Alpaca consolidated SIP 1-minute bars, from 2016
#   BTCUSDT: Binance official 1-minute candles, from Aug 2017
#   BTCUSD: optional Bitstamp community 1-minute candles, from 2012
# ================================================================

packages <- c("data.table", "httr2", "R.utils")
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages)) install.packages(missing_packages)

library(data.table)
library(httr2)
options(timeout = max(600, getOption("timeout")))

ROOT <- "intraday_data"
dir.create(ROOT, showWarnings = FALSE, recursive = TRUE)

# ---------- 1. SPY: Alpaca, consolidated SIP ----------
# Create free Alpaca market-data keys, then set in the R session:
# Sys.setenv(ALPACA_API_KEY = "your_key", ALPACA_SECRET_KEY = "your_secret")
# Dates are inclusive, and the downloaded bars are UTC-stamped.

download_spy <- function(start_date = as.Date("2016-01-01"),
                         end_date = Sys.Date() - 1L) {
  key <- Sys.getenv("ALPACA_API_KEY")
  secret <- Sys.getenv("ALPACA_SECRET_KEY")
  if (!nzchar(key) || !nzchar(secret)) {
    stop("Set ALPACA_API_KEY and ALPACA_SECRET_KEY first.")
  }
  if (start_date > end_date) stop("start_date is after end_date")
  
  out_dir <- file.path(ROOT, "SPY_1m_alpaca_SIP")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  first_month <- as.Date(format(start_date, "%Y-%m-01"))
  last_month <- as.Date(format(end_date, "%Y-%m-01"))
  month_starts <- seq(first_month, last_month, by = "month")
  
  for (m_number in month_starts) {
    m <- as.Date(m_number, origin = "1970-01-01")
    file <- file.path(out_dir, sprintf("SPY_%s.csv", format(m, "%Y-%m")))
    if (file.exists(file)) {
      message("Already saved: ", basename(file))
      next
    }
    
    next_month <- seq(m, by = "month", length.out = 2L)[2L]
    period_start <- max(m, start_date)
    # The API's end is inclusive; duplicates at boundaries will be removed.
    period_end <- min(next_month, end_date + 1L)
    
    base_request <- request("https://data.alpaca.markets/v2/stocks/SPY/bars") |>
      req_headers(
        `APCA-API-KEY-ID` = key,
        `APCA-API-SECRET-KEY` = secret
      ) |>
      req_url_query(
        timeframe = "1Min",
        start = sprintf("%sT00:00:00Z", period_start),
        end = sprintf("%sT00:00:00Z", period_end),
        feed = "sip",
        adjustment = "raw",
        limit = 10000
      ) |>
      req_retry(max_tries = 5)
    
    token <- NULL
    pages <- list()
    repeat {
      req <- if (is.null(token)) base_request else
        req_url_query(base_request, page_token = token)
      
      result <- resp_body_json(req_perform(req), simplifyVector = TRUE)
      if (is.data.frame(result$bars) && nrow(result$bars)) {
        pages[[length(pages) + 1L]] <- as.data.table(result$bars)
      }
      token <- result$next_page_token
      if (is.null(token) || length(token) == 0L || !nzchar(token)) break
      Sys.sleep(0.4)  # Be considerate of API request limits.
    }
    
    if (!length(pages)) {
      warning("No SPY bars returned for ", format(m, "%Y-%m"))
      next
    }
    
    d <- rbindlist(pages, use.names = TRUE, fill = TRUE)
    d[, timestamp := as.POSIXct(sub("Z$", "", t),
                                format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")]
    setnames(d, c("o", "h", "l", "c", "v"),
             c("open", "high", "low", "close", "volume"))
    d <- d[, .(timestamp, open, high, low, close, volume)]
    setorder(d, timestamp)
    d <- unique(d, by = "timestamp")
    fwrite(d, file)
    message("Saved SPY ", format(m, "%Y-%m"), " (", nrow(d), " bars)")
    Sys.sleep(0.4)
  }
  invisible(out_dir)
}

# ---------- 2. Bitcoin: official Binance spot archive ----------
# Saves one parsed CSV per calendar month/day so downloads are resumable.
# Monthly archive = all completed months; daily archive = current month
# through yesterday. BTCUSDT quotes the price in USDT, not USD.

download_binance_file <- function(day, kind = c("monthly", "daily"),
                                  symbol = "BTCUSDT", interval = "1m") {
  kind <- match.arg(kind)
  label <- format(day, if (kind == "monthly") "%Y-%m" else "%Y-%m-%d")
  out_dir <- file.path(ROOT, paste0(symbol, "_", interval, "_binance"))
  zip_dir <- file.path(out_dir, "zip")
  dir.create(zip_dir, showWarnings = FALSE, recursive = TRUE)
  
  name <- sprintf("%s-%s-%s", symbol, interval, label)
  output <- file.path(out_dir, paste0(name, ".csv"))
  if (file.exists(output)) return(invisible(TRUE))
  
  url <- sprintf("https://data.binance.vision/data/spot/%s/klines/%s/%s/%s.zip",
                 kind, symbol, interval, name)
  zip_path <- file.path(zip_dir, paste0(name, ".zip"))
  if (!file.exists(zip_path)) {
    status <- tryCatch(
      suppressWarnings(download.file(url, zip_path, mode = "wb", quiet = TRUE,
                                     method = "libcurl")),
      error = function(e) 1L
    )
    if (!identical(as.integer(status), 0L)) {
      if (file.exists(zip_path)) unlink(zip_path)
      warning("Download not available: ", url)
      return(invisible(FALSE))
    }
  }
  
  members <- tryCatch(utils::unzip(zip_path, list = TRUE)$Name,
                      error = function(e) character())
  csv_file <- members[grepl("\\.csv$", members, ignore.case = TRUE)][1L]
  if (is.na(csv_file)) {
    warning("Invalid ZIP file: ", zip_path)
    unlink(zip_path)
    return(invisible(FALSE))
  }
  
  tmp_dir <- tempfile("binance_extract_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  utils::unzip(zip_path, files = csv_file, exdir = tmp_dir)
  d <- fread(file.path(tmp_dir, csv_file), header = FALSE,
             select = c(1L, 2L, 3L, 4L, 5L, 6L, 9L))
  setnames(d, c("ts", "open", "high", "low", "close", "volume", "trades"))
  if (!nrow(d)) return(invisible(FALSE))
  
  raw_time <- as.numeric(d$ts)
  # Spot files use milliseconds before 2025 and microseconds from 2025.
  divisor <- if (raw_time[1L] > 1e14) 1e6 else 1e3
  d[, timestamp := as.POSIXct(raw_time / divisor,
                              origin = "1970-01-01", tz = "UTC")]
  d[, ts := NULL]
  setcolorder(d, c("timestamp", "open", "high", "low", "close",
                   "volume", "trades"))
  setorder(d, timestamp)
  d <- unique(d, by = "timestamp")
  fwrite(d, output)
  message("Saved Binance ", label, " (", nrow(d), " bars)")
  invisible(TRUE)
}

download_binance <- function(start_month = as.Date("2017-08-01"),
                             through_yesterday = TRUE) {
  today <- Sys.Date()
  this_month <- as.Date(format(today, "%Y-%m-01"))
  last_month <- as.Date(format(this_month - 1L, "%Y-%m-01"))
  
  for (m_number in seq(start_month, last_month, by = "month")) {
    m <- as.Date(m_number, origin = "1970-01-01")
    download_binance_file(m, "monthly")
  }
  if (through_yesterday && this_month <= today - 1L) {
    for (d_number in seq(this_month, today - 1L, by = "day")) {
      d <- as.Date(d_number, origin = "1970-01-01")
      download_binance_file(d, "daily")
    }
  }
  invisible(file.path(ROOT, "BTCUSDT_1m_binance"))
}

# ---------- 3. Optional: longer Bitstamp BTC/USD history ----------
# Community mirror: 2012 through Jan 2025 bulk file, plus updates.
# Important: this dataset fills some missing minutes with flat zero-volume
# candles. Check the repository's provenance notes before modelling them.

download_bitstamp <- function() {
  out_dir <- file.path(ROOT, "BTCUSD_1m_bitstamp_community")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  root_url <- paste0(
    "https://raw.githubusercontent.com/ff137/",
    "bitstamp-btcusd-minute-data/main/data/"
  )
  sources <- c(
    historical = paste0(root_url,
                        "historical/btcusd_bitstamp_1min_2012-2025.csv.gz"),
    updates = paste0(root_url,
                     "updates/btcusd_bitstamp_1min_latest.csv")
  )
  local_files <- c(
    historical = file.path(out_dir, "bitstamp_bulk_2012_2025.csv.gz"),
    updates = file.path(out_dir, "bitstamp_updates.csv")
  )
  
  for (part in names(sources)) {
    if (file.exists(local_files[[part]])) next
    message("Downloading Bitstamp ", part, " ...")
    tryCatch(
      download.file(sources[[part]], local_files[[part]],
                    mode = "wb", method = "libcurl"),
      error = function(e) warning(conditionMessage(e))
    )
  }
  
  present <- local_files[file.exists(local_files)]
  if (!length(present)) stop("No Bitstamp files downloaded")
  d <- rbindlist(lapply(present, function(f) fread(f)),
                 use.names = TRUE, fill = TRUE)
  setnames(d, names(d), tolower(names(d)))
  d[, timestamp := as.POSIXct(as.numeric(timestamp),
                              origin = "1970-01-01", tz = "UTC")]
  setorder(d, timestamp)
  d <- unique(d, by = "timestamp")
  output <- file.path(out_dir, "BTCUSD_1m_all.csv")
  fwrite(d, output)
  message("Saved Bitstamp: ", nrow(d), " minutes")
  invisible(output)
}

# ---------- 4. Load saved data and construct daily realised variance ----------

load_saved <- function(folder, pattern = "\\.csv$") {
  files <- list.files(file.path(ROOT, folder), pattern = pattern,
                      full.names = TRUE)
  if (!length(files)) stop("No CSV files found under ", folder)
  d <- rbindlist(lapply(files, fread), use.names = TRUE, fill = TRUE)
  if (!inherits(d$timestamp, "POSIXct")) {
    d[, timestamp := as.POSIXct(sub("Z$", "", timestamp),
                                format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")]
  }
  setorder(d, timestamp)
  unique(d, by = "timestamp")
}

make_daily_rv <- function(d, market = c("SPY", "BTC")) {
  market <- match.arg(market)
  d <- copy(d)
  if (market == "SPY") {
    # Regular New York session only; handles US daylight saving time.
    clock <- format(d$timestamp, "%H:%M:%S", tz = "America/New_York")
    d <- d[clock >= "09:30:00" & clock < "16:00:00"]
    d[, day := as.Date(timestamp, tz = "America/New_York")]
  } else {
    d[, day := as.Date(timestamp, tz = "UTC")]
  }
  setorder(d, timestamp)
  # First return of each day is excluded: no SPY overnight move; one
  # boundary observation omitted for BTC too. Check missing bars before use.
  d[, ret := c(NA_real_, diff(log(close))), by = day]
  d[, .(rv = sum(ret^2, na.rm = TRUE), n_bars = .N), by = day]
}

# ---------- RUN: uncomment the downloads you want ----------
# download_binance()  # BTCUSDT, Aug 2017 to yesterday
# download_bitstamp() # BTCUSD, 2012 to current community updates
# download_spy()      # SPY, 2016 to yesterday (requires Alpaca keys)

# Example after downloading:
# spy <- load_saved("SPY_1m_alpaca_SIP", "^SPY_.*\\.csv$")
# btc <- load_saved("BTCUSDT_1m_binance", "^BTCUSDT-1m-.*\\.csv$")
# rv_spy <- make_daily_rv(spy, "SPY")
# rv_btc <- make_daily_rv(btc, "BTC")
# plot(rv_spy$day, rv_spy$rv, type = "l", main = "SPY daily realised variance")
# plot(rv_btc$day, rv_btc$rv, type = "l", main = "BTC daily realised variance")


Sys.setenv(
  ALPACA_API_KEY = "PKHULYSIFFKDZUYGOD33HMACNT",
  ALPACA_SECRET_KEY = "83BuwJMoojZUqwZURkig9DkWY6dQKAw9L1xSLSZ1pEMb"
)

files <- list.files(
  "intraday_data",
  recursive = TRUE,
  full.names = TRUE
)

info <- file.info(files)
info <- info[!info$isdir, ]

cat("Files:", nrow(info), "\n")
cat("Total MB:", round(sum(info$size) / 1024^2, 2), "\n")
cat("Total GB:", round(sum(info$size) / 1024^3, 3), "\n")
