# ======================================================================
# SPY INTRADAY -> DAILY VOLATILITY DATASET (Alpaca SIP, 2016-2026)
# ======================================================================
# INPUT:  intraday_data/SPY_1m_alpaca_SIP/SPY_YYYY-MM.csv
# OUTPUT: intraday_data/SPY_rebuilt/{daily_all, daily_model,
#         quality_review, calendar, minutes_clean, diagnostic_plots}
#
# No downloads/API key needed. Run from folder containing intraday_data.
# Uses exchange date and local time in America/New_York (DST-aware).
# Session end is exclusive: 16:00 normally; 13:00 on early-close days.
# Prices are unadjusted Alpaca minute prices; last in-window close is
# not necessarily the official 16:00 or 13:00 auction closing price.
# RV = first-minute open->close squared + successive observed closes.
# Returns across verified circuit breaker halts are kept (irregular span).
# 5-minute RV uses last AVAILABLE close in each clock-aligned 5m bucket;
# empty buckets are skipped, jumps across gaps remain in the measure.
# ======================================================================

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}
library(data.table)

ROOT <- "intraday_data"
INPUT_DIR <- file.path(ROOT, "SPY_1m_alpaca_SIP")
OUT <- file.path(ROOT, "SPY_rebuilt")
SAVE_MINUTES <- TRUE   # Set FALSE if you don't need another large CSV.
SAVE_PLOTS <- TRUE

stopifnot(dir.exists(INPUT_DIR))
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
files <- sort(list.files(INPUT_DIR,
                         pattern = "^SPY_[0-9]{4}-[0-9]{2}\\.csv$", full.names = TRUE))
if (!length(files)) stop("No SPY_YYYY-MM.csv files in ", INPUT_DIR)

cat("\n================== 1. INPUT ==================\n")
cat("Files:", length(files), " | First:", basename(files[1L]),
    " | Last:", basename(tail(files, 1L)), "\n")
cat("Size (MB):", round(sum(file.info(files)$size) / 1024^2, 1), "\n")

d <- rbindlist(lapply(files, function(f) {
  x <- fread(f)
  needed <- c("timestamp", "open", "high", "low", "close", "volume")
  if (!all(needed %in% names(x))) {
    stop("Missing required columns in ", f, ": ",
         paste(setdiff(needed, names(x)), collapse = ", "))
  }
  x[, ..needed]
}), use.names = TRUE)

# UTC parsing works regardless of whether fread inferred POSIXct or text.
if (inherits(d$timestamp, "POSIXct")) {
  d[, timestamp := as.POSIXct(as.numeric(timestamp),
                              origin = "1970-01-01", tz = "UTC")]
} else {
  s <- gsub("T", " ", sub("Z$", "", as.character(d$timestamp)), fixed = TRUE)
  parsed <- as.POSIXct(s, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  if (anyNA(parsed)) stop("Could not parse all UTC timestamps")
  d[, timestamp := parsed]
}
if (anyNA(d$timestamp)) stop("Some UTC timestamps are missing")
for (col in c("open", "high", "low", "close", "volume")) {
  set(d, j = col, value = as.numeric(d[[col]]))
}

n_raw <- nrow(d)
setorder(d, timestamp)
duplicates <- n_raw - uniqueN(d$timestamp)
d <- unique(d, by = "timestamp")

# Alpaca minute timestamps mark the BEGINNING of each one-minute bucket.
d[, date := as.IDate(format(timestamp, "%Y-%m-%d",
                            tz = "America/New_York"))]
clock <- format(d$timestamp, "%H:%M", tz = "America/New_York")
d[, slot := as.integer(substr(clock, 1, 2)) * 60L +
    as.integer(substr(clock, 4, 5)) - 570L]
rm(clock)
d[, valid_price := is.finite(open) & is.finite(high) &
    is.finite(low) & is.finite(close) &
    open > 0 & high > 0 & low > 0 & close > 0]

cat("Raw rows:", format(n_raw, big.mark = ","),
    " | Duplicate UTC timestamps:", duplicates, "\n")
cat("Distinct rows:", format(nrow(d), big.mark = ","),
    " | Invalid OHLC rows:", sum(!d$valid_price), "\n")
cat("Coverage:", as.character(min(d$date)), "to",
    as.character(max(d$date)), "\n")

# ======================================================================
# 2. NYSE EQUITY SESSION CALENDAR (specific to 2016-2026)
# ======================================================================
# Standard holiday rules, plus historical special closures and an explicit
# list of early-close dates. Holidays are not guessed from observed bars.
# Calendar source/check:
# https://www.nyse.com/markets/hours-calendars
# https://www.kibot.com/quality/market-holidays.html
# Before extending this script beyond 2026, validate the new calendar.

nth_weekday <- function(year, month, weekday, n) {
  first <- as.Date(sprintf("%04d-%02d-01", year, month))
  w <- as.POSIXlt(first)$wday  # Sun=0, Mon=1,...,Sat=6
  first + (weekday - w) %% 7 + 7 * (n - 1)
}
last_weekday <- function(year, month, weekday) {
  first_next <- if (month == 12L)
    as.Date(sprintf("%04d-01-01", year + 1L)) else
      as.Date(sprintf("%04d-%02d-01", year, month + 1L))
  last <- first_next - 1L
  last - (as.POSIXlt(last)$wday - weekday) %% 7
}
observed <- function(day, saturday_to_friday = TRUE) {
  w <- as.POSIXlt(day)$wday
  if (w == 0L) return(day + 1L)
  if (w == 6L && saturday_to_friday) return(day - 1L)
  day
}
good_friday <- function(y) {
  # Gregorian Easter, Meeus/Jones/Butcher, valid for these years.
  a <- y %% 19; b <- y %/% 100; c <- y %% 100
  dd <- b %/% 4; e <- b %% 4
  f <- (b + 8) %/% 25; g <- (b - f + 1) %/% 3
  h <- (19*a + b - dd - g + 15) %% 30
  i <- c %/% 4; k <- c %% 4
  l <- (32 + 2*e + 2*i - h - k) %% 7
  m <- (a + 11*h + 22*l) %/% 451
  p <- h + l - 7*m + 114
  as.Date(sprintf("%04d-%02d-%02d", y, p %/% 31, 1 + p %% 31)) - 2L
}
annual_closures <- function(y) {
  dates <- c(
    observed(as.Date(sprintf("%04d-01-01", y)),
             saturday_to_friday = FALSE),  # Saturday New Year NOT Friday
    nth_weekday(y, 1L, 1L, 3L),            # MLK
    nth_weekday(y, 2L, 1L, 3L),            # Presidents Day
    good_friday(y),
    last_weekday(y, 5L, 1L),               # Memorial Day
    observed(as.Date(sprintf("%04d-07-04", y))),
    nth_weekday(y, 9L, 1L, 1L),            # Labor Day
    nth_weekday(y, 11L, 4L, 4L),           # Thanksgiving
    observed(as.Date(sprintf("%04d-12-25", y)))
  )
  if (y >= 2022L) dates <- c(dates,
                             observed(as.Date(sprintf("%04d-06-19", y))))
  as.Date(dates, origin = "1970-01-01")
}

# NYSE early closes at 13:00 ET, verified for 2016-2026.
EARLY_CLOSE <- as.IDate(c(
  "2016-11-25",
  "2017-07-03", "2017-11-24",
  "2018-07-03", "2018-11-23", "2018-12-24",
  "2019-07-03", "2019-11-29", "2019-12-24",
  "2020-11-27", "2020-12-24",
  "2021-11-26",
  "2022-11-25",
  "2023-07-03", "2023-11-24",
  "2024-07-03", "2024-11-29", "2024-12-24",
  "2025-07-03", "2025-11-28", "2025-12-24",
  "2026-11-27", "2026-12-24"
))
SPECIAL_CLOSED <- as.IDate(c("2018-12-05", "2025-01-09"))
MWCB_DATES <- as.IDate(c("2020-03-09", "2020-03-12",
                         "2020-03-16", "2020-03-18"))


session_dates <- d[
  slot >= 0L & slot < 390L,
  date
]

cal <- data.table(
  date = seq(
    min(session_dates),
    max(session_dates),
    by = "day"
  )
)

cal[, year := as.integer(format(date, "%Y"))]

if (any(cal$year < 2016L | cal$year > 2026L)) {
  stop("Calendar is vetted for 2016-2026 only; update it for other years")
}
holidays <- as.IDate(c(
  as.Date(unlist(lapply(unique(cal$year), annual_closures)),
          origin = "1970-01-01"), as.Date(SPECIAL_CLOSED)
))
cal[, weekend := as.POSIXlt(as.Date(date))$wday %in% c(0L, 6L)]
cal[, holiday := date %in% holidays]
cal[, is_trading := !weekend & !holiday]
cal[, early_close := date %in% EARLY_CLOSE & is_trading]
cal[, expected_bars := fifelse(!is_trading, 0L,
                               fifelse(early_close, 210L, 390L))]
cal[, session_type := fifelse(!is_trading, "closed",
                              fifelse(early_close, "early_close",
                                      fifelse(date %in% MWCB_DATES,
                                              "market_halt", "regular")))]
cal[, expected_close_et := fifelse(!is_trading, NA_character_,
                                   fifelse(early_close, "13:00", "16:00"))]

# Find date gaps across downloaded months without assuming holidays trade.
cat("\n================== 2. CALENDAR ==================\n")
cat("Expected trading sessions (inclusive observed span):",
    sum(cal$is_trading), "\n")
cat("Expected early closes in covered period:",
    sum(cal$early_close), "\n")

# ======================================================================
# 3. FILTER TO THE ACTUAL EXCHANGE SESSION AND AUDIT EXCLUSIONS
# ======================================================================
# Extended hours are not included. Crucial: on an early close the bars
# at 13:00..15:59 are NOT regular-session observations.

joined <- merge(d, cal[, .(date, is_trading, expected_bars,
                           session_type, early_close)],
                by = "date", all.x = TRUE, sort = FALSE)

# Diagnostic: bars that old 09:30-16:00 filter would falsely include.
late_early <- joined[early_close == TRUE & slot >= 210L & slot < 390L,
                     .(bars_after_1300 = .N), by = date]

in_window <- joined[is_trading == TRUE & slot >= 0L &
                      slot < expected_bars]
invalid_in_window <- in_window[valid_price == FALSE,
                               .(bad_ohlc_bars = .N), by = date]
rth <- in_window[valid_price == TRUE,
                 .(date, timestamp, slot, open, high, low, close, volume)]
rm(d, joined, in_window)
gc(verbose = FALSE)
setorder(rth, date, slot)
if (!nrow(rth)) stop("No valid SPY bars inside exchange sessions")

# First return measures within the session, not overnight movement.
# When bars are missing, jumps between observed closes are retained,
# *but* those jumps no longer represent one-minute intervals.
rth[, r_1m := c(log(close[1L] / open[1L]), diff(log(close))), by = date]
rth[, gap_minutes := c(NA_integer_, diff(slot)), by = date]
rth[, bin5 := slot %/% 5L]

# Calendar-clock 5-minute aggregation (09:30-09:34, 09:35-09:39, ...).
# Include first block open->its last close, and adjacent available
# blocks' last-close returns. On halt days, jumps span empty bins.
b5 <- rth[, .(bin_close = close[.N], bin_open = open[1L],
              n_minutes = .N, end_slot = slot[.N]), by = .(date, bin5)]
setorder(b5, date, bin5)
b5[, r_5m := c(log(bin_close[1L] / bin_open[1L]),
               diff(log(bin_close))), by = date]
rv5 <- b5[, .(rv_5m_raw = sum(r_5m^2),
              n_bins_5m = .N,
              incomplete_5m_bins = sum(n_minutes < 5L)), by = date]

# Session summaries of the observed, price-valid bars.
raw_day <- rth[, .(
  n_bars = .N,
  first_slot = slot[1L], last_slot = slot[.N],
  max_gap_min = if (.N > 1L) max(diff(slot)) else NA_integer_,
  n_internal_gaps = sum(gap_minutes > 1L, na.rm = TRUE),
  day_open = open[1L], day_close = close[.N],
  rv_1m_raw = sum(r_1m^2),
  r_oc_raw = log(close[.N] / open[1L]),
  sum_r_1m = sum(r_1m),
  max_abs_r_1m = max(abs(r_1m))
), by = date]

# Include all scheduled trading days, even if zero valid bars arrived.
daily <- merge(cal[is_trading == TRUE,
                   .(date, year, session_type, expected_bars,
                     expected_close_et)],
               raw_day, by = "date", all.x = TRUE, sort = TRUE)
daily <- merge(daily, rv5, by = "date", all.x = TRUE, sort = TRUE)
daily <- merge(daily, invalid_in_window, by = "date", all.x = TRUE,
               sort = TRUE)
daily <- merge(daily, late_early, by = "date", all.x = TRUE,
               sort = TRUE)
for (col in c("bad_ohlc_bars", "bars_after_1300")) {
  set(daily, j = col, value = fifelse(is.na(daily[[col]]),
                                      0L, as.integer(daily[[col]])))
}
daily[, missing_bars := expected_bars - fifelse(is.na(n_bars),
                                                0L, n_bars)]
daily[, start_ok := !is.na(first_slot) & first_slot == 0L]
daily[, end_ok := !is.na(last_slot) &
        last_slot == expected_bars - 1L]
daily[, exact_session := missing_bars == 0L &
        start_ok & end_ok & bad_ohlc_bars == 0L]

# Precisely the documented 2020 market-wide halt signature in this feed:
# 376 observed bars, first and last minute present, 15-minute max gap.
# An anomalous 15-minute gap on another day is NOT assumed legitimate.
daily[, verified_halt := session_type == "market_halt" &
        missing_bars == 14L & start_ok & end_ok &
        !is.na(max_gap_min) & max_gap_min == 15L &
        n_internal_gaps == 1L & bad_ohlc_bars == 0L]
daily[, unexplained_missing := fifelse(verified_halt,
                                       0L, missing_bars)]
daily[, quality_flag := fcase(
  is.na(n_bars), "NO_DATA",
  verified_halt, "HALT_VERIFIED",
  session_type == "market_halt", "HALT_REVIEW",
  session_type == "early_close" & exact_session, "EARLY_CLEAN",
  session_type == "early_close", "EARLY_REVIEW",
  exact_session, "REGULAR_CLEAN",
  default = "REVIEW_GAPS"
)]

# Main comparable 390-min window sample: fully observed regular sessions
# plus historically verified market-halt days (gaps are real halts).
# Excludes early closes from main loss comparisons, but retains in 'all'.
daily[, use_main_rv := quality_flag %in%
        c("REGULAR_CLEAN", "HALT_VERIFIED")]
daily[, rv_1m := fifelse(use_main_rv, rv_1m_raw, NA_real_)]
daily[, rv_5m := fifelse(use_main_rv, rv_5m_raw, NA_real_)]

# Endpoint-based daily returns: NOT conditional on all minutes existing.
# We require current and previous expected trading sessions to have
# valid endpoint prices; no bridging of a completely missing session.
daily[, open_ok := start_ok & is.finite(day_open) & day_open > 0]
daily[, close_ok := end_ok & is.finite(day_close) & day_close > 0]
daily[, prev_close := shift(day_close)]
daily[, prev_close_ok := shift(close_ok, fill = FALSE)]
daily[, cc_valid := close_ok & prev_close_ok]
daily[, r_oc := fifelse(open_ok & close_ok,
                        log(day_close / day_open), NA_real_)]
daily[, sq_oc := r_oc^2]
daily[, r_cc := fifelse(cc_valid,
                        log(day_close / prev_close), NA_real_)]
daily[, sq_cc := r_cc^2]
daily[, r_overnight := fifelse(open_ok & prev_close_ok,
                               log(day_open / prev_close), NA_real_)]

# Algebraic identity: intraday observed returns telescope to OC return.
daily[, reconciliation_abs := abs(sum_r_1m - r_oc_raw)]

# ======================================================================
# 4. PRINT QUALITY AND VOLATILITY DIAGNOSTICS
# ======================================================================
cat("\n================== 3. QUALITY ==================\n")
cat("Expected sessions:", nrow(daily), "\n")
cat("Sessions with data:", sum(!is.na(daily$n_bars)), "\n")
cat("Sessions entirely missing:", sum(is.na(daily$n_bars)), "\n")
cat("Strictly complete (regular or early):", sum(daily$exact_session), "\n")
cat("Verified 2020 market halts:", sum(daily$verified_halt), "/ 4\n")
cat("Main RV eligible sessions:", sum(daily$use_main_rv), "\n")
cat("Available squared close-close returns:",
    sum(is.finite(daily$sq_cc)), "\n")
cat("Observations excluded by the early-close 13:00 cutoff:",
    sum(daily$bars_after_1300), "\n")
cat("Max return reconciliation error:",
    format(max(daily$reconciliation_abs, na.rm = TRUE),
           scientific = TRUE), "\n")

cat("\nQuality categories:\n")
print(daily[, .N, by = quality_flag][order(-N)])
cat("\nYear-by-year breakdown:\n")
print(daily[, .(sessions = .N,
                observed = sum(!is.na(n_bars)),
                full_regular = sum(quality_flag == "REGULAR_CLEAN"),
                early_close = sum(session_type == "early_close"),
                market_halt = sum(verified_halt),
                main_rv = sum(use_main_rv)), by = year])

cat("\nAll non-clean sessions (review separately):\n")
review <- daily[!quality_flag %in%
                  c("REGULAR_CLEAN", "EARLY_CLEAN", "HALT_VERIFIED"),
                .(date, session_type, quality_flag, expected_bars, n_bars,
                  missing_bars, first_slot, last_slot, max_gap_min,
                  n_internal_gaps, bad_ohlc_bars, rv_1m_raw)]
print(review, digits = 6)

cat("\nFour 2020 circuit-breaker sessions:\n")
print(daily[date %in% MWCB_DATES,
            .(date, n_bars, max_gap_min, missing_bars, verified_halt,
              rv_1m_raw, rv_5m_raw, sq_cc)], digits = 7)

cat("\nEarly closes (13:00 cutoff, bars formerly treated as regular):\n")
print(daily[session_type == "early_close",
            .(date, n_bars, expected_bars, missing_bars,
              bars_after_1300, quality_flag)], digits = 5)

stat <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(list(n = 0L, mean = NA_real_,
                              median = NA_real_, p95 = NA_real_,
                              max = NA_real_))
  list(n = length(x), mean = mean(x), median = median(x),
       p95 = as.numeric(quantile(x, 0.95)), max = max(x))
}
vol_stats <- rbindlist(list(
  c(list(series = "RV 1m, MAIN"), stat(daily$rv_1m)),
  c(list(series = "RV 5m, MAIN"), stat(daily$rv_5m)),
  c(list(series = "RV 1m, ALL observed"), stat(daily$rv_1m_raw)),
  c(list(series = "Open-close squared"), stat(daily$sq_oc)),
  c(list(series = "Close-close squared"), stat(daily$sq_cc))
), fill = TRUE)
cat("\n================== 4. VARIANCE SUMMARY ==================\n")
print(vol_stats, digits = 7)
cat("\nCorrelation RV1m vs RV5m (main): ",
    cor(daily$rv_1m, daily$rv_5m, use = "complete.obs"), "\n")
cat("Correlation RV1m vs sq CC (main):",
    cor(daily$rv_1m, daily$sq_cc, use = "complete.obs"), "\n")
cat("\nTop 12 MAIN realised variance dates:\n")
print(head(daily[use_main_rv == TRUE][order(-rv_1m),
                                      .(date, session_type, rv_1m, rv_5m, sq_oc, sq_cc)], 12L),
      digits = 7)
cat("\nTop 12 absolute observed minute returns:\n")
print(head(rth[order(-abs(r_1m)),
               .(date, timestamp, slot, r_1m,
                 abs_pct = 100 * abs(r_1m), gap_minutes)], 12L), digits = 7)

# ======================================================================
# 5. SAVE BOTH FULL DATA AND STRICT MODEL DATA; MAKE PLOTS
# ======================================================================
# All calendar dates are retained in 'daily_all'; 'daily_model' contains
# regular-clean and verified-halt dates only. Keep source/full set to
# construct lagged features without accidentally skipping bad days.

fwrite(cal, file.path(OUT, "SPY_session_calendar.csv"))
fwrite(daily, file.path(OUT, "SPY_daily_all.csv"))
fwrite(daily[use_main_rv == TRUE],
       file.path(OUT, "SPY_daily_model.csv"))
fwrite(review, file.path(OUT, "SPY_quality_review.csv"))
fwrite(vol_stats, file.path(OUT, "SPY_volatility_summary.csv"))
if (SAVE_MINUTES) {
  fwrite(rth, file.path(OUT, "SPY_minutes_clean.csv"))
}

# Base R plots (no new dependencies).
plot_diagnostics <- function() {
  op <- par(mfrow = c(4, 1), mar = c(3.1, 4.7, 2.1, 1.1))
  on.exit(par(op))
  plot(daily$date, daily$day_close, type = "l", xlab = "",
       ylab = "USD", main = "SPY: end-of-session minute close (unadjusted)")
  plot(daily$date, daily$rv_1m_raw, type = "l", xlab = "",
       ylab = "Variance", main = "SPY: 1-minute RV (all observed sessions)")
  points(daily$date[!daily$use_main_rv],
         daily$rv_1m_raw[!daily$use_main_rv], pch = 4, col = 2, cex = 0.6)
  plot(daily$date, daily$rv_5m, type = "l", xlab = "",
       ylab = "Variance", main = "SPY: 5-minute RV (main sample)")
  plot(daily$date, daily$sq_cc, type = "l", xlab = "Date",
       ylab = "Squared return", main = "SPY: squared close-to-close returns")
}
if (SAVE_PLOTS) {
  pdf(file.path(OUT, "SPY_volatility_plots.pdf"),
      width = 11, height = 11)
  plot_diagnostics()
  dev.off()
  if (interactive()) plot_diagnostics()
}

# Convenient objects in the environment after source():
spy_daily <- daily
spy_minutes <- rth

cat("\n================== DONE ==================\n")
cat("Outputs in:", normalizePath(OUT, winslash = "/"), "\n")
cat("Important: 1m/5m RV measure the regular/early exchange session only;\n")
cat("they do NOT include SPY overnight variance or the closing auction.\n")
cat("All daily close prices are unadjusted SPY price proxies.\n")
cat("Send the QUALITY, VARIANCE SUMMARY and TOP RV outputs for review.\n")
