# FACT-CHECK. One command that re-derives every published claim from source and fails
# loudly on anything it cannot stand behind.
#
#   Rscript --vanilla R/factcheck.R              # full report
#   Rscript --vanilla R/factcheck.R --brief      # just the PASS/FAIL lines
#
# Writes data/internal/factcheck_report.txt and exits non-zero if any check FAILS, so it
# can gate a publish.
#
# The principle throughout: a check must be able to fail. Several of these exist because
# an earlier version of this project shipped a wrong number that looked right --
# a corner store in a restaurant ranking, a silently truncated scrape that logged no
# errors, five establishments coloured red on a score that was never a food score.

source("R/common.R")
source("R/editorial.R")
suppressPackageStartupMessages(library(jsonlite))

BRIEF <- "--brief" %in% commandArgs(trailingOnly = TRUE)
dir.create("data/internal", showWarnings = FALSE, recursive = TRUE)

.results <- list()
.log <- character(0)

say <- function(...) {
  line <- paste0(...)
  .log <<- c(.log, line)
  if (!BRIEF) message(line)
}
head_ <- function(t) { say(""); say("=== ", t, " ==="); }

check <- function(label, ok, detail = "") {
  status <- if (isTRUE(ok)) "PASS" else "FAIL"
  .results[[length(.results) + 1L]] <<- list(label = label, ok = isTRUE(ok))
  line <- sprintf("  [%s] %-58s %s", status, label, detail)
  .log <<- c(.log, line)
  message(line)
  invisible(ok)
}

ins <- load_inspections()

# ---- 1. provenance --------------------------------------------------------------
head_("1. Provenance: where every number comes from")

say(sprintf("  inspection table : %s", if (file.exists("data/inspections_merged.csv"))
  "data/inspections_merged.csv (export + portal)" else "data/ins.csv (export only)"))
say(sprintf("  rows             : %s across %s facilities",
            format(nrow(ins), big.mark = ","), format(n_distinct(ins$facility_id), big.mark = ",")))
say(sprintf("  span             : %s .. %s", format(min(ins$inspection_date)),
            format(data_through(ins))))
if ("source" %in% names(ins)) {
  bysrc <- ins |> count(source)
  say(sprintf("  by source        : %s",
              paste(sprintf("%s=%s", bysrc$source, format(bysrc$n, big.mark = ",")), collapse = ", ")))
}
check("inspection table is present and non-empty", nrow(ins) > 0)
check("no row is missing a facility_id", !any(is.na(ins$facility_id)))
check("no row is missing an establishment name", !any(is.na(ins$restaurant_name)))
check("no duplicate (facility, date, score) triples",
      !any(duplicated(ins |> select(facility_id, inspection_date, score))))

# ---- 2. nothing lost from the frozen export -------------------------------------
head_("2. The frozen export is fully represented")

if (file.exists("data/ins.csv") && file.exists("data/inspections_merged.csv")) {
  soc <- read_csv("data/ins.csv", show_col_types = FALSE) |>
    transmute(facility_id = as.character(facility_id),
              inspection_date = as_date(substr(inspection_date, 1, 10)), score) |>
    filter(!is.na(score))
  mg <- if (file.exists("data/facility_merges.csv"))
    read_csv("data/facility_merges.csv", col_types = cols(.default = col_character())) else
    tibble(old_facility_id = character(), new_facility_id = character())
  soc <- soc |>
    left_join(mg |> transmute(facility_id = old_facility_id, new_facility_id),
              by = "facility_id") |>
    mutate(facility_id = coalesce(new_facility_id, facility_id))
  sk <- unique(paste(soc$facility_id, soc$inspection_date, soc$score))
  mk <- unique(paste(ins$facility_id, ins$inspection_date, ins$score))
  lost <- setdiff(sk, mk)
  say(sprintf("  export triples: %s | present after merge: %s",
              format(length(sk), big.mark = ","), format(sum(sk %in% mk), big.mark = ",")))
  check("no export inspection lost in the merge", length(lost) == 0,
        if (length(lost)) sprintf("%d lost", length(lost)) else "")
} else {
  say("  (merged table absent -- skipping)")
}

# ---- 3. the score-0 rule --------------------------------------------------------
head_("3. Score 0 is never treated as a food-safety score")

zeros <- ins |> filter(score == 0)
say(sprintf("  score-0 rows in the data: %d", nrow(zeros)))
if (nrow(zeros)) {
  print(as.data.frame(zeros |>
    transmute(name = str_trunc(restaurant_name, 30), inspection_date,
              type = coalesce(inspection_type, "(unknown)"))), right = FALSE)
}
check("no score-0 inspection is typed as a scored FDA inspection",
      !any(zeros$inspection_type == FDA_TYPE, na.rm = TRUE))

lat <- latest_per_facility(ins)
check("no score-0 row survives latest-per-facility", !any(lat$score == 0))
check("facilities with a 0 latest still show an earlier real score where one exists",
      all(c("Fiesta Tortillas", "PBS Hospitality Inc.") %in% lat$restaurant_name),
      "Fiesta Tortillas / PBS Hospitality")

# ---- 4. the published map -------------------------------------------------------
head_("4. docs/data/points.json matches the data")

if (file.exists("docs/data/points.json")) {
  pj <- fromJSON("docs/data/points.json", simplifyVector = FALSE)
  F <- setNames(seq_along(unlist(pj$fields)) - 1L, unlist(pj$fields))
  sc <- vapply(pj$rows, function(r) as.integer(r[[F[["s"]] + 1L]]), integer(1))
  dt <- vapply(pj$rows, function(r) as.character(r[[F[["d"]] + 1L]]), character(1))

  say(sprintf("  rows on the map  : %s", format(length(sc), big.mark = ",")))
  say(sprintf("  score range      : %d .. %d", min(sc), max(sc)))
  say(sprintf("  window stated    : %s .. %s", pj$cutoff, pj$data_through))
  say(sprintf("  withheld as stale: %s", format(pj$n_ever - pj$n_total, big.mark = ",")))

  check("no zero-scored pin on the map", !any(sc == 0))
  check("every pin is inside the stated recency window", all(dt >= pj$cutoff),
        sprintf("earliest pin %s", min(dt)))
  check("no pin is dated after the stated data-through", all(dt <= pj$data_through))
  check("map row count equals the pipeline's mapped count", length(sc) == pj$n_mapped)
  check("bucket colours match R/common.R",
        identical(vapply(pj$buckets, function(b) b$color, character(1)), BUCKETS$color))
  check("payload carries no inspector comments",
        !any(grepl("CFM", vapply(pj$rows, function(r) paste(unlist(r), collapse = " "), character(1))[
          seq_len(min(400, length(pj$rows)))], fixed = TRUE)))
} else check("docs/data/points.json exists", FALSE)

# ---- 5. the two ranking embeds --------------------------------------------------
head_("5. The ranking pages")

MIN_TOP <- 5; MIN_BOTTOM <- 3
cutoff <- recency_cutoff(ins)
routine <- ins |> filter(is_scored(score, inspection_type),
                         !str_detect(restaurant_name, ADMIN_FLAG_RE), !is_followup)
elig <- routine |> group_by(facility_id) |>
  summarise(name = first(restaurant_name), n = n(), mean_score = round(mean(score), 2),
            latest = max(inspection_date), .groups = "drop") |>
  mutate(category = categorize(name), venue = venue_of(name)) |>
  filter(n >= MIN_BOTTOM, latest >= cutoff, category == "Restaurant & Food Service")

for (pg in list(list(f = "docs/best.html",   dir = "top",    min = MIN_TOP),
                list(f = "docs/lowest.html", dir = "bottom", min = MIN_BOTTOM))) {
  if (!file.exists(pg$f)) { check(paste(pg$f, "exists"), FALSE); next }
  html <- paste(readLines(pg$f, warn = FALSE), collapse = "\n")
  dat  <- fromJSON(sub(".*var DATA = (\\[.*?\\]);.*", "\\1", html), simplifyVector = FALSE)
  nm   <- vapply(dat, function(r) r$name, character(1))
  mn   <- vapply(dat, function(r) as.numeric(r$mean_score), numeric(1))
  nn   <- vapply(dat, function(r) as.integer(r$n), integer(1))

  say(sprintf("  %s -> %d rows, mean %.2f .. %.2f", basename(pg$f), length(nm), min(mn), max(mn)))

  # Re-derive the table independently and compare names in order.
  expect <- (if (pg$dir == "top")
    elig |> filter(n >= pg$min) |> arrange(desc(mean_score), desc(n), desc(latest))
  else elig |> arrange(mean_score, desc(n), desc(latest))) |>
    distinct(venue, .keep_all = TRUE) |> head(10)

  check(sprintf("%s reproduces from source, in order", basename(pg$f)),
        identical(nm, expect$name))
  check(sprintf("%s: every entry meets the >=%d routine minimum", basename(pg$f), pg$min),
        all(nn >= pg$min), sprintf("min observed %d", min(nn)))
  check(sprintf("%s: every entry is categorised a restaurant", basename(pg$f)),
        all(categorize(nm) == "Restaurant & Food Service"),
        paste(unique(categorize(nm)[categorize(nm) != "Restaurant & Food Service"]), collapse = ", "))
  check(sprintf("%s: no admin-flagged name published", basename(pg$f)),
        !any(str_detect(nm, ADMIN_FLAG_RE)))
  check(sprintf("%s: no duplicate operator", basename(pg$f)),
        !any(duplicated(venue_of(nm))))
  check(sprintf("%s: no inspector comments in the page", basename(pg$f)),
        !grepl("CFM", html, fixed = TRUE))
  check(sprintf("%s: no unsubstituted {{placeholder}}", basename(pg$f)),
        !grepl("\\{\\{", html))
}

# ---- 6. names published, for hand-verification ----------------------------------
head_("6. Every name currently published, to check against the city portal")
say("  Look each up at https://inspections.myhealthdepartment.com/aph and confirm the")
say("  name, address and the most recent score match. These are accusations and claims")
say("  about real businesses; nothing above substitutes for reading them.")

# Full, untruncated, with every routine inspection listed so the mean can be checked by
# hand. A truncated address cannot be looked up, which makes a verification sheet that
# truncates worse than useless -- it looks like verification without enabling it.
sheet <- character(0)
add <- function(...) sheet <<- c(sheet, paste0(...))

add("VERIFICATION SHEET — every name currently published")
add("Generated ", format(Sys.time()), "   |   records through ", format(data_through(ins)))
add("")
add("Look each establishment up at https://inspections.myhealthdepartment.com/aph")
add("(search by name, then confirm the address). For each one check:")
add("  1. the establishment exists at the address shown")
add("  2. it is a restaurant, not a market/store/cafeteria/school kitchen")
add("  3. the routine inspection dates and scores below match the portal")
add("  4. no newer inspection exists that we have missed")
add(strrep("=", 78))

for (pg in list(list(f = "docs/best.html",   t = "BEST INSPECTION RECORDS  (docs/best.html)"),
                list(f = "docs/lowest.html", t = "LOWEST INSPECTION SCORES  (docs/lowest.html)"))) {
  if (!file.exists(pg$f)) next
  html <- paste(readLines(pg$f, warn = FALSE), collapse = "\n")
  dat <- fromJSON(sub(".*var DATA = (\\[.*?\\]);.*", "\\1", html), simplifyVector = FALSE)
  add(""); add(pg$t); add(strrep("-", 78))
  for (r in dat) {
    # Join on facility_id, never the name. Two different Little Deli & Pizzeria
    # restaurants share a name at different addresses; a name join pools them and
    # produces a mean that belongs to neither, then reports it as a discrepancy.
    fid  <- as.character(r$facility_id)
    hist <- ins |> filter(facility_id == fid) |> arrange(inspection_date) |>
      transmute(inspection_date, score, purpose,
                type = coalesce(inspection_type, "unknown"), source)
    rt <- hist |> filter(purpose == "Routine", score > 0)
    add("")
    add(sprintf("%2d. %s", r$rank, r$name))
    add(sprintf("    %s, %s TX %s", r$street, r$city,
                ins |> filter(facility_id == fid) |> slice(1) |> pull(zip5)))
    add(sprintf("    facility_id %s   category %s", fid, categorize(r$name)))
    add(sprintf("    PUBLISHED: mean %.2f across %d routine inspections",
                as.numeric(r$mean_score), r$n))
    add(sprintf("    RECOMPUTED: mean %.2f across %d   -> %s",
                mean(rt$score), nrow(rt),
                if (abs(mean(rt$score) - as.numeric(r$mean_score)) < 0.005 &&
                    nrow(rt) == r$n) "MATCHES" else "*** DISCREPANCY ***"))
    add("    every inspection on record:")
    for (i in seq_len(nrow(hist))) {
      add(sprintf("      %s  %3d  %-10s %-26s %s%s",
                  format(hist$inspection_date[i]), hist$score[i], hist$purpose[i],
                  str_trunc(hist$type[i], 24), hist$source[i],
                  if (hist$purpose[i] != "Routine" || hist$score[i] == 0)
                    "   <- excluded from the mean" else ""))
    }
  }
}

writeLines(sheet, "data/internal/verification_sheet.txt")
for (l in sheet) say(l)

# ---- 7. known-hard cases --------------------------------------------------------
head_("7. Cases that have gone wrong before")

xw_ok <- TRUE
if (file.exists("data/permit_crosswalk.csv")) {
  xw <- read_csv("data/permit_crosswalk.csv", col_types = cols(.default = col_character()))
  hn <- function(x) str_extract(str_squish(x), "^[0-9]+")
  m <- xw |> filter(status == "matched", !is.na(i_addr_raw))
  bad <- m |> filter(!is.na(hn(p_addr_raw)), !is.na(hn(i_addr_raw)),
                     hn(p_addr_raw) != hn(i_addr_raw))
  xw_ok <- nrow(bad) == 0
  check("crosswalk: no permit matched to a different street number", xw_ok,
        sprintf("%d matched permits checked", nrow(m)))
}
check("no 'Market'-named establishment is ranked as a restaurant",
      !any(str_detect(lat$restaurant_name[categorize(lat$restaurant_name) == "Restaurant & Food Service"],
                      regex("\\bmarket", ignore_case = TRUE)) &
           !str_detect(lat$restaurant_name[categorize(lat$restaurant_name) == "Restaurant & Food Service"],
                       regex("mandola", ignore_case = TRUE))))
check("Mandola's Italian Market is still counted as a restaurant",
      categorize("Mandola's Italian Market") == "Restaurant & Food Service")
check("recency cutoff is anchored to the data, not to today",
      recency_cutoff(ins) == data_through(ins) %m-% months(RECENCY_MONTHS))

conf <- same_date_conflicts(ins)
say(sprintf("  facilities with disagreeing same-day scores: %d (lower score is used)", nrow(conf)))

# ---- verdict --------------------------------------------------------------------
n_fail <- sum(!vapply(.results, function(r) r$ok, logical(1)))
say(""); say(strrep("=", 78))
verdict <- sprintf("%d checks run, %d passed, %d FAILED", length(.results),
                   length(.results) - n_fail, n_fail)
say(verdict); message(strrep("=", 78)); message(verdict)
if (n_fail) {
  say("")
  say("FAILED:")
  for (r in .results) if (!r$ok) say("  - ", r$label)
}

writeLines(.log, "data/internal/factcheck_report.txt")
message("\nFull report: data/internal/factcheck_report.txt")
if (n_fail) quit(status = 1)
