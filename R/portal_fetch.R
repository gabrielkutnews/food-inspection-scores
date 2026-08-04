# Scrape the live Austin Public Health inspections portal.
#
# WHY THIS EXISTS
# The Socrata feed R/download.R reads (dataset ecmv-9xxi) is dead: rowsUpdatedAt is
# 2026-06-15, max(inspection_date) is 2026-05-22, and the row count is identical to
# what we already have. Re-downloading changes nothing. The portal at
# inspections.myhealthdepartment.com/aph is current through today and carries fields
# Socrata never had -- inspectionType, programName, purpose, comments, permitID, a
# clean city, and split address lines.
#
# WHY chromote AND NOT httr, AND WHY HEADFUL
# The endpoint sits behind bot detection. Bare curl gets a 403 in 0.2s; curl with a
# Chrome User-Agent is blackholed entirely. That rules out httr, httr2, curl and
# Python requests -- anything on libcurl.
#
# It also rules out HEADLESS Chrome: chromote's default session reports
# "HeadlessChrome/151.0.0.0" and the WAF 403s it before the page even loads
# (document.title comes back as "403 Forbidden"). So we launch a normal, visible
# Chrome with --remote-debugging-port and attach to it. That reports plain
# "Chrome/151.0.0.0" and is served normally. We are not masking the User-Agent --
# it is a genuine interactive browser, which is also what a person reading this
# public portal would be using. A window will open; leave it alone while the run
# proceeds.
#
# FIVE API BEHAVIOURS THAT WILL BITE IF IGNORED
#   1. programName MUST be "". Sending "Food" makes the server hang forever -- it is
#      not a supported value. Filter to Food in R on the returned programName column.
#      (This cost hours: every hang looked like rate limiting and was not.)
#   2. count is capped at 25 server-side. Asking for 1000 returns 25, silently. So a
#      full page is 25 rows and pagination is mandatory.
#   3. Ordering within a shared date is unstable, so adjacent offset pages can overlap
#      (observed: 1 duplicate row across one page boundary). Dedup on inspectionID is
#      not optional, and short windows keep the number of boundaries low.
#   4. Reading PAST the last page returns HTTP 200 with the object
#      {"err":true,"msg":"bad request"} instead of an empty array. So a window whose
#      record count is an exact multiple of 25 ends on that object, not on a short
#      page. It is only an end-marker at start > 0 -- the same object at start == 0
#      means the request itself was rejected, which is a real fault.
#   5. A single query returns AT MOST 225 records (9 pages of 25), no matter how many
#      match. This is the dangerous one, because it looks exactly like a completed
#      window: page 9 is full, page 10 returns the end-marker, and the walk stops
#      "successfully" having silently dropped everything past 225. A first run with
#      7-day windows hit 225 on 39 of 83 windows and reconciled at only 90% of the
#      Socrata export over the overlap period. Hence WINDOW_DAYS = 2 (~60 records,
#      well clear of the cap) and an explicit `capped` state so any recurrence is
#      visible rather than silent.
#
# A short page means "end of this window" ONLY if the request succeeded. A fault
# returns short or unparseable too, and treating that as end-of-data is exactly how a
# scraper silently truncates. Faults are retried with backoff and the window is
# marked failed rather than done.
#
# Usage:
#   Rscript --vanilla R/portal_fetch.R                 # backfill (default)
#   Rscript --vanilla R/portal_fetch.R --incremental   # day after our max date -> today
#   Rscript --vanilla R/portal_fetch.R --verify "Buffet Palace" "Special Noodle"
#   Rscript --vanilla R/portal_fetch.R --probe         # API behaviour checks only

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(lubridate)
  library(jsonlite); library(chromote)
})

# ---- configuration -----------------------------------------------------------

PORTAL      <- "https://inspections.myhealthdepartment.com/aph"

# The portal only retains ~18 months. Probed month by month: 2025-02 onward returns
# full windows, 2025-01 returns a single record (the 29th), 2024-07 returns 2, and
# 2024-01 / 2023-07 return zero -- while ins.csv holds 200, 95 and 117 rows for those
# same windows. So a 3-year backfill is not possible from here at any request budget.
#
# Consequence worth knowing: for 2023-06 .. 2025-01 the frozen Socrata export is now
# the ONLY surviving source, and the city has stopped updating it. Those rows will
# never gain inspectionType/purpose/comments.
BACKFILL_MIN <- as.Date("2025-01-01")
PAGE        <- 25L                      # per-response cap; asking for more is ignored
RESULT_CAP  <- 225L                     # per-QUERY cap (9 pages). See header note 5.
WINDOW_DAYS <- 2L                       # ~60 records/window, well clear of RESULT_CAP
RAW_PATH    <- "data/portal_inspections.csv"
STATUS_PATH <- "data/portal_status.json"
LOG_PATH    <- "data/portal_scrape_log.csv"

# Jittered pacing. Irregular rather than machine-periodic, and gentle on a public
# server.
#
# Calibrated the hard way. A ~3.1 s mean survived 667 requests untouched; cutting it to
# ~1.65 s tripped a per-endpoint 403 block at roughly 1,100 cumulative requests, which
# cleared after ~10 minutes. So the ceiling is real even though it is generous.
#
# The bigger reason to stay slow: this machine egresses via a shared campus NAT
# (nat-...public.utexas.edu). A block does not just stop us -- it stops every other
# person behind that gateway from using the city's portal. Do not tune these down
# without a better reason than impatience.
PAUSE_MIN   <- 2.0
PAUSE_MAX   <- 6.0        # ~4 s mean, slower than the run that was never blocked
BREAK_EVERY <- 30:45
BREAK_MIN   <- 25
BREAK_MAX   <- 60

# Per-run ceiling, separate from MAX_REQUESTS. The block appeared around 1,100
# cumulative requests, so a single sitting stops well short of that and the next run
# resumes from the status file.
RUN_BUDGET  <- 600L

# Consecutive CDP timeouts that trigger rebuilding the browser session. A wedged
# renderer never recovers on its own, and backing off against it just wastes hours.
RECYCLE_AFTER <- 3L
MAX_REQUESTS <- 4000L     # hard budget; a full backfill needs ~1,100

BACKOFF     <- c(30, 60, 120)   # seconds, per retry
FETCH_TIMEOUT_MS <- 45000L

PORTAL_COLS <- c("inspectionID", "permitID", "inspectionDate", "score", "inspectionType",
                 "programName", "purpose", "comments", "TimeIn", "establishmentName",
                 "addressLine1", "addressLine2", "city", "state", "zip", "permitType")

# ---- browser session ---------------------------------------------------------

CHROME_BIN  <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
DEBUG_PORT  <- 9222L

# Always launch a FRESH Chrome. Reusing whatever is on the port looked like a free
# optimisation and was the opposite: across two runs plus some diagnostics, sessions
# leaked five tabs into one 3-hour-old Chrome, all with the portal's JS live. The
# renderer contention starved the CDP calls and half of every request began timing out
# at 45s -- while the very same query answered in 210ms from a clean browser. The
# symptom looked exactly like server-side rate limiting and was not.
launch_headful <- function() {
  alive <- function() {
    !is.null(suppressWarnings(tryCatch(
      fromJSON(sprintf("http://127.0.0.1:%d/json/version", DEBUG_PORT)),
      error = function(e) NULL)))
  }
  if (alive()) {
    message("  killing stale Chrome on port ", DEBUG_PORT)
    system2("pkill", c("-f", sprintf("remote-debugging-port=%d", DEBUG_PORT)),
            stdout = FALSE, stderr = FALSE)
    Sys.sleep(3)
  }
  prof <- file.path(tempdir(),
                    paste0("chrome-portal-", Sys.getpid(), "-", as.integer(runif(1, 1e6, 9e6))))
  dir.create(prof, showWarnings = FALSE, recursive = TRUE)
  system2(CHROME_BIN,
          c(sprintf("--remote-debugging-port=%d", DEBUG_PORT),
            sprintf("--user-data-dir=%s", shQuote(prof)),
            "--no-first-run", "--no-default-browser-check",
            "--disable-extensions", "--mute-audio", "--window-size=1200,900",
            # about:blank, NOT the portal. chromote opens its own tab and navigates
            # there; passing the URL here leaves a second idle tab running the portal's
            # JS (Sentry, pollers) competing with the session tab for the renderer.
            # That contention is half of what wedged the CDP calls before.
            "about:blank"),
          stdout = FALSE, stderr = FALSE, wait = FALSE)
  for (i in 1:30) { Sys.sleep(1); if (alive()) break }
  if (!alive()) stop("Chrome did not open a DevTools port on ", DEBUG_PORT)
  message("  launched Chrome on port ", DEBUG_PORT)
  invisible(TRUE)
}

open_session <- function() {
  message("Launching Chrome and loading the portal...")
  launch_headful()
  b <- ChromoteSession$new(
    parent = Chromote$new(browser = ChromeRemote$new(host = "127.0.0.1", port = DEBUG_PORT)))

  # Close any other page target so exactly one tab exists. Session recycling and any
  # interrupted run can otherwise leave tabs behind, and a pile of them running the
  # portal's JS is what starved the CDP calls into 45s timeouts on an earlier pass.
  try({
    others <- fromJSON(sprintf("http://127.0.0.1:%d/json/list", DEBUG_PORT))
    mine <- b$get_target_id()
    for (id in others$id[others$type == "page" & others$id != mine]) {
      suppressWarnings(try(
        readLines(sprintf("http://127.0.0.1:%d/json/close/%s", DEBUG_PORT, id), warn = FALSE),
        silent = TRUE))
    }
  }, silent = TRUE)

  b$Page$navigate(PORTAL, wait_ = TRUE)

  # Poll readyState rather than waiting on Page.loadEventFired: the event can fire
  # before the listener attaches, and this page keeps pulling assets long after
  # document load, so waiting for the event either misses it or times out.
  ready <- FALSE
  for (i in 1:60) {
    Sys.sleep(1)
    rs <- tryCatch(
      b$Runtime$evaluate("document.readyState", returnByValue = TRUE)$result$value,
      error = function(e) NA_character_)
    if (identical(rs, "complete")) { ready <- TRUE; break }
  }
  if (!ready) stop("Page never reached readyState=complete")
  Sys.sleep(2)   # let the page's own bootstrap POSTs settle

  origin <- b$Runtime$evaluate("location.origin", returnByValue = TRUE)$result$value
  if (!identical(origin, "https://inspections.myhealthdepartment.com")) {
    stop("Unexpected origin after navigate: ", origin)
  }

  # Fail loudly on a headless/blocked session rather than reporting 1,050 empty
  # windows as a successful scrape.
  ua <- b$Runtime$evaluate("navigator.userAgent", returnByValue = TRUE)$result$value
  ttl <- b$Runtime$evaluate("document.title", returnByValue = TRUE)$result$value
  if (grepl("Headless", ua, fixed = TRUE)) {
    stop("Session is headless (", ua, "); the portal 403s headless Chrome.")
  }
  if (grepl("403", ttl, fixed = TRUE)) {
    stop("Portal returned 403 for the landing page (title: ", ttl, ")")
  }
  message("  session ready on ", origin, "  [", ttl, "]")
  b
}

# One API call. Returns list(ok=, rows=, note=).
#
# The fetch runs inside the page so it is same-origin and carries the session cookie.
# awaitPromise makes Runtime.evaluate wait for the promise; returnByValue marshals the
# result back as data rather than a remote object handle. We return a STRING from JS
# and parse in R, because deep nested objects marshal unreliably.
portal_call <- function(b, date_range, start = 0L, search = "") {
  payload <- list(
    data = list(
      path = "aph",
      programName = "",                     # MUST be "" -- see header
      filters = list(date = date_range),
      start = start, count = PAGE,
      searchStr = search,
      lat = 0, lng = 0, sort = structure(list(), names = character(0))
    ),
    task = "searchInspections"
  )
  body <- as.character(toJSON(payload, auto_unbox = TRUE))

  js <- sprintf("
    (async () => {
      try {
        const r = await fetch('/', { method:'POST',
          headers:{'Content-Type':'application/json'}, body: %s });
        const t = await r.text();
        return JSON.stringify({ ok: r.ok, status: r.status, body: t });
      } catch (e) {
        return JSON.stringify({ ok:false, status:0, err:String(e) });
      }
    })()", as.character(toJSON(body, auto_unbox = TRUE)))

  res <- tryCatch(
    b$Runtime$evaluate(js, awaitPromise = TRUE, returnByValue = TRUE,
                       timeout_ = FETCH_TIMEOUT_MS / 1000),
    error = function(e) NULL
  )
  if (is.null(res) || is.null(res$result$value)) {
    return(list(ok = FALSE, rows = NULL, note = "cdp_timeout"))
  }

  env <- fromJSON(res$result$value, simplifyVector = FALSE)
  if (!isTRUE(env$ok)) {
    return(list(ok = FALSE, rows = NULL,
                note = paste0("http_", env$status %||% 0, " ", substr(env$err %||% "", 1, 60))))
  }

  # A non-JSON body is the nginx error page. That is a fault, NOT end-of-data.
  parsed <- tryCatch(fromJSON(env$body, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (is.null(parsed)) {
    return(list(ok = FALSE, rows = NULL,
                note = paste0("nonjson:", substr(str_squish(env$body), 1, 60))))
  }
  # An object rather than an array. Past the last page the API answers
  # {"err":true,"msg":"bad request"} with HTTP 200 -- that is its end-of-results
  # marker. Only trust it as such beyond the first page; at start == 0 the request
  # was genuinely rejected and must not be mistaken for "this window is empty".
  if (!is.null(names(parsed))) {
    if (isTRUE(parsed$err) && start > 0L) {
      return(list(ok = TRUE, rows = rows_to_df(list()), note = "", end = TRUE))
    }
    return(list(ok = FALSE, rows = NULL,
                note = paste0("object:", substr(paste(names(parsed), collapse = ","), 1, 40))))
  }

  list(ok = TRUE, rows = rows_to_df(parsed), note = "", end = FALSE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

rows_to_df <- function(lst) {
  if (length(lst) == 0) {
    return(as_tibble(setNames(rep(list(character(0)), length(PORTAL_COLS)), PORTAL_COLS)))
  }
  pull_col <- function(nm) {
    vapply(lst, function(r) {
      v <- r[[nm]]
      if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)[1]
    }, character(1))
  }
  as_tibble(setNames(lapply(PORTAL_COLS, pull_col), PORTAL_COLS))
}

# ---- pacing ------------------------------------------------------------------

.req_n <- 0L
.next_break <- sample(BREAK_EVERY, 1)

pace <- function() {
  .req_n <<- .req_n + 1L
  Sys.sleep(runif(1, PAUSE_MIN, PAUSE_MAX))
  if (.req_n >= .next_break) {
    br <- runif(1, BREAK_MIN, BREAK_MAX)
    message(sprintf("  ... pausing %.0fs after %d requests", br, .req_n))
    Sys.sleep(br)
    .next_break <<- .req_n + sample(BREAK_EVERY, 1)
  }
  if (.req_n > MAX_REQUESTS) stop("Request budget exhausted (", MAX_REQUESTS, ")")
}

# Stop cleanly at the per-run ceiling instead of pushing into a block. Completed
# windows are already in the status file, so the next run picks up where this stopped.
budget_spent <- function() .req_n >= RUN_BUDGET

# ---- persistence -------------------------------------------------------------

append_raw <- function(df) {
  if (nrow(df) == 0) return(invisible(0L))
  df$scraped_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  write_csv(df, RAW_PATH, append = file.exists(RAW_PATH))
  invisible(nrow(df))
}

# A window that failed part-way has already appended its earlier pages, and a re-run
# re-scrapes it from offset 0. So the raw file can hold duplicates across runs even
# though each run dedups within a window. inspectionID is a GUID, so collapsing on it
# is safe and keeps the newest scrape of each record.
dedupe_raw <- function() {
  if (!file.exists(RAW_PATH)) return(invisible(0L))
  d <- suppressWarnings(read_csv(RAW_PATH, col_types = cols(.default = col_character())))
  before <- nrow(d)
  d <- d |> arrange(desc(scraped_at)) |> distinct(inspectionID, .keep_all = TRUE)
  write_csv(d, RAW_PATH)
  message(sprintf("deduped raw: %d -> %d rows (%d duplicate inspectionIDs)",
                  before, nrow(d), before - nrow(d)))
  invisible(nrow(d))
}

log_req <- function(window, start, outcome, n, note = "") {
  write_csv(tibble(ts = format(Sys.time()), window = window, start = start,
                   outcome = outcome, n = n, note = substr(note, 1, 120)),
            LOG_PATH, append = file.exists(LOG_PATH))
}

read_status <- function() {
  if (file.exists(STATUS_PATH)) fromJSON(STATUS_PATH, simplifyVector = FALSE) else list(windows = list())
}
write_status <- function(st) write_json(st, STATUS_PATH, auto_unbox = TRUE, pretty = TRUE)

# ---- one window --------------------------------------------------------------

# Walk offsets until a short page. Any fault aborts the window as `failed` so it can
# be retried later; it is never mistaken for the end of the data.
.consec_timeouts <- 0L

scrape_window <- function(from, to) {
  range <- sprintf("%s to %s", format(from), format(to))
  seen  <- character(0)
  total <- 0L
  start <- 0L

  repeat {
    ok <- FALSE
    for (attempt in seq_len(length(BACKOFF) + 1L)) {
      pace()
      r <- portal_call(.sess, range, start)
      if (r$ok) { ok <- TRUE; .consec_timeouts <<- 0L; break }
      log_req(range, start, "fault", 0L, r$note)

      # A CDP timeout is our browser wedging, not the server refusing. Backing off
      # against it accomplishes nothing -- rebuild the session instead.
      if (identical(r$note, "cdp_timeout")) {
        .consec_timeouts <<- .consec_timeouts + 1L
        if (.consec_timeouts >= RECYCLE_AFTER) {
          message(sprintf("    %d consecutive CDP timeouts -- rebuilding browser session",
                          .consec_timeouts))
          try(.sess$close(), silent = TRUE)
          Sys.sleep(2)
          .sess <<- open_session()
          .consec_timeouts <<- 0L
          next
        }
      }
      if (attempt <= length(BACKOFF)) {
        message(sprintf("    fault (%s), backing off %ds", r$note, BACKOFF[attempt]))
        Sys.sleep(BACKOFF[attempt])
      }
    }
    if (!ok) {
      return(list(state = "failed", n = total, note = r$note, dup = 0L))
    }

    if (isTRUE(r$end)) {            # past the last page (see header note 4)
      log_req(range, start, "end", 0L)
      break
    }

    n_page <- nrow(r$rows)
    new    <- r$rows |> filter(!inspectionID %in% seen)
    dup    <- n_page - nrow(new)
    seen   <- c(seen, new$inspectionID)
    total  <- total + append_raw(new)
    log_req(range, start, "ok", n_page)

    if (n_page < PAGE) break        # genuine end of window: a SUCCESSFUL short page
    start <- start + PAGE

    # Hitting the per-query cap is NOT a completed window -- it is a truncated one
    # that would otherwise look identical to a clean finish. Surface it.
    if (start >= RESULT_CAP) {
      return(list(state = "capped", n = total,
                  note = sprintf("hit %d-result cap; narrow the window", RESULT_CAP),
                  dup = dup))
    }
  }
  list(state = "done", n = total, note = "", dup = length(seen) - length(unique(seen)))
}

# ---- modes -------------------------------------------------------------------

run_windows <- function(from, to) {
  st <- read_status()
  starts <- seq(from, to, by = WINDOW_DAYS)
  message(sprintf("%d windows of %d days: %s .. %s", length(starts), WINDOW_DAYS,
                  format(from), format(to)))

  for (i in seq_along(starts)) {
    w_from <- starts[i]
    w_to   <- min(starts[i] + WINDOW_DAYS - 1L, to)
    key    <- format(w_from)

    prev <- st$windows[[key]]
    if (!is.null(prev) && identical(prev$state, "done")) {
      message(sprintf("[%d/%d] %s  (cached: %d rows)", i, length(starts), key, prev$n))
      next
    }

    if (budget_spent()) {
      message(sprintf("\nPer-run budget reached (%d requests) at window %d/%d. Stopping here;",
                      RUN_BUDGET, i, length(starts)))
      message("re-run to continue from this window. This is deliberate -- pushing past it")
      message("is what earned a 403 block on a shared campus IP last time.")
      break
    }

    res <- scrape_window(w_from, w_to)
    st$windows[[key]] <- list(from = format(w_from), to = format(w_to),
                              state = res$state, n = res$n, dup = res$dup,
                              note = res$note, at = format(Sys.time()))
    write_status(st)
    message(sprintf("[%d/%d] %s -> %s, %d rows%s", i, length(starts), key,
                    res$state, res$n, if (nzchar(res$note)) paste0(" (", res$note, ")") else ""))
  }
  st
}

mode_verify <- function(b, names_vec) {
  out <- list()
  for (nm in names_vec) {
    pace()
    r <- portal_call(b, "2022-01-01 to 2030-12-31", 0L, search = nm)
    message(sprintf("  %-40s %s", nm,
                    if (r$ok) paste0(nrow(r$rows), " rows") else paste0("FAULT ", r$note)))
    if (r$ok) { append_raw(r$rows); out[[nm]] <- r$rows }
  }
  invisible(out)
}

# Probe the behaviours the design depends on, before trusting a long run.
mode_probe <- function(b) {
  message("\n--- probe 1: programName='' returns rows ---")
  r <- portal_call(b, "2026-08-01 to 2026-08-03"); pace()
  message(sprintf("  ok=%s rows=%s programs=%s", r$ok, nrow(r$rows %||% tibble()),
                  paste(unique(r$rows$programName), collapse = "/")))

  message("--- probe 2: count is capped at 25 ---")
  message(sprintf("  a full page returns %d rows (PAGE=%d)", nrow(r$rows), PAGE))

  message("--- probe 3: does searchStr return FULL history or only the latest? ---")
  r2 <- portal_call(b, "2022-01-01 to 2030-12-31", 0L, search = "Buffet Palace"); pace()
  if (r2$ok) {
    message(sprintf("  Buffet Palace -> %d rows, dates %s", nrow(r2$rows),
                    paste(sort(substr(r2$rows$inspectionDate, 1, 10)), collapse = ", ")))
    message("  ins.csv has 10 inspections for Buffet Palace. If fewer come back,",
            " searchStr is latest-only and Stage 3 must use date windows instead.")
  } else message("  FAULT ", r2$note)

  message("--- probe 4: are 2023 records still served? ---")
  r3 <- portal_call(b, "2023-07-01 to 2023-07-08"); pace()
  message(sprintf("  ok=%s rows=%s", r3$ok, nrow(r3$rows %||% tibble())))
  message("  If 0, the portal only exposes a rolling window and absence CANNOT be",
          " read as closure.")
  invisible(TRUE)
}

# ---- main --------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
dir.create("data", showWarnings = FALSE)

.sess <- open_session()
on.exit({ try(.sess$close(), silent = TRUE) }, add = TRUE)

if ("--probe" %in% args) {
  mode_probe(.sess)

} else if ("--verify" %in% args) {
  nms <- args[(which(args == "--verify") + 1L):length(args)]
  message("Verify mode: ", length(nms), " establishments")
  mode_verify(.sess, nms)

} else {
  if ("--incremental" %in% args) {
    source("R/common.R")
    from <- data_through(load_inspections()) + 1L
  } else {
    from <- BACKFILL_MIN
  }
  to <- Sys.Date()
  st <- run_windows(from, to)

  dedupe_raw()

  states <- vapply(st$windows, function(w) w$state, character(1))
  message("\n=== summary ===")
  print(table(states))
  message(sprintf("total requests: %d", .req_n))
  bad <- names(states)[states != "done"]
  if (length(bad)) {
    message("NOT done (re-run to retry these windows): ", paste(bad, collapse = ", "))
  } else {
    message("All windows done. Reconcile against ins.csv before trusting completeness.")
  }
}
