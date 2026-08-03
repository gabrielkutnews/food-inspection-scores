# Verify the portal scrape, then fact-check the withdrawn claims against it.
#
# Two jobs, in order:
#
#   1. COMPLETENESS. The scrape overlaps data/ins.csv from 2025-01-29 to 2026-05-22.
#      Over that span both sources should describe the same inspections, so a per-week
#      count comparison is the real proof the scrape is complete. The absence of errors
#      in the log proves only that nothing crashed -- a silently truncated offset walk
#      logs no errors at all. Compliance visits are excluded from the comparison
#      because the Socrata export never carried that purpose.
#
#   2. FACT-CHECK. Recompute the withdrawn lowest-scoring list on routine inspections
#      only, using portal data where it exists, and report the inspectionType behind
#      every score of 0 so that decision rests on evidence rather than inference.
#
# Run after R/portal_fetch.R.

source("R/common.R")

RAW_PATH <- "data/portal_inspections.csv"
stopifnot("run R/portal_fetch.R first" = file.exists(RAW_PATH))

portal <- read_csv(RAW_PATH, col_types = cols(.default = col_character())) |>
  mutate(
    date  = as_date(substr(inspectionDate, 1, 10)),
    score = suppressWarnings(as.integer(score))
  ) |>
  distinct(inspectionID, .keep_all = TRUE)

ins <- load_inspections()

message(sprintf("Portal: %d inspections, %s .. %s",
                nrow(portal), format(min(portal$date)), format(max(portal$date))))
message(sprintf("  programs: %s",
                paste(sprintf("%s=%d", names(table(portal$programName)),
                              table(portal$programName)), collapse = ", ")))
message(sprintf("  purposes: %s",
                paste(sprintf("%s=%d", names(table(portal$purpose)),
                              table(portal$purpose)), collapse = ", ")))

food <- portal |> filter(programName == "Food")
message(sprintf("  Food-program only: %d", nrow(food)))

# ---- 1. completeness -----------------------------------------------------------

OVERLAP_FROM <- max(min(food$date), as.Date("2025-02-01"))
OVERLAP_TO   <- data_through(ins)

cmp <- inner_join(
  ins |>
    filter(inspection_date >= OVERLAP_FROM, inspection_date <= OVERLAP_TO) |>
    mutate(wk = floor_date(inspection_date, "week")) |>
    count(wk, name = "socrata"),
  food |>
    # Socrata has only Routine and Follow-Up, so compare like with like.
    filter(purpose %in% c("Routine", "Follow-Up"),
           date >= OVERLAP_FROM, date <= OVERLAP_TO) |>
    mutate(wk = floor_date(date, "week")) |>
    count(wk, name = "portal"),
  by = "wk"
) |>
  mutate(diff = portal - socrata, pct = round(100 * portal / socrata, 1))

message("")
message(sprintf("=== weekly reconciliation, %s .. %s (%d weeks) ===",
                format(OVERLAP_FROM), format(OVERLAP_TO), nrow(cmp)))
message(sprintf("  socrata total %d | portal total %d | portal/socrata %.1f%%",
                sum(cmp$socrata), sum(cmp$portal),
                100 * sum(cmp$portal) / sum(cmp$socrata)))
message(sprintf("  weeks where portal < 90%% of socrata: %d", sum(cmp$pct < 90)))
message(sprintf("  weeks where portal > 110%% of socrata: %d", sum(cmp$pct > 110)))

worst <- cmp |> arrange(pct) |> head(8)
if (nrow(worst)) {
  message("  lowest-coverage weeks (investigate before trusting completeness):")
  print(as.data.frame(worst), right = FALSE)
}

# A uniform shortfall across every week is the signature of a systematic loss (an
# end-exclusive date filter, say), which matters far more than one bad week.
if (nrow(cmp) > 10 && median(cmp$pct) < 95) {
  message(sprintf("  WARNING: median weekly coverage is %.1f%% -- looks systematic, not incidental.",
                  median(cmp$pct)))
}

# ---- 2. the score-zero question ------------------------------------------------

message("")
message("=== every portal inspection recorded at 0 ===")
z <- portal |> filter(score == 0)
if (nrow(z) == 0) {
  message("  none in the scraped range.")
} else {
  print(as.data.frame(
    z |> count(programName, inspectionType, name = "n") |> arrange(desc(n))
  ), right = FALSE)
  message("")
  message("  the five facilities our export shows at 0:")
  print(as.data.frame(
    portal |>
      filter(str_detect(establishmentName,
                        regex("Fiesta Tortillas|PBS Hospitality|Texas Meat|Maher Business|5 Rivers Tea",
                              ignore_case = TRUE))) |>
      arrange(establishmentName, date) |>
      transmute(name = str_trunc(establishmentName, 28), date, score,
                type = str_trunc(inspectionType, 22), purpose,
                note = str_trunc(str_squish(comments), 40))
  ), right = FALSE)
}

# ---- 3. fact-check the withdrawn list -------------------------------------------

WITHDRAWN <- c("Special Noodle", "Hunan Bistro", "Biryani & Co.", "Gang Nam Korean BBQ",
               "India Gate", "Fruttilandia", "Buffet Palace", "LW - Lakeway Market",
               "Pho Phi Vietnamese Noodles and Grill", "Wing Daddy's Sauce House")

message("")
message("=== the ten withdrawn restaurants: does the portal change the picture? ===")

ours <- ins |>
  filter(restaurant_name %in% WITHDRAWN, !is.na(score), score > 0) |>
  group_by(restaurant_name) |>
  summarise(our_routine = sum(!is_followup),
            our_mean_routine = round(mean(score[!is_followup]), 2),
            our_latest = max(inspection_date), .groups = "drop")

theirs <- food |>
  filter(purpose == "Routine") |>
  mutate(key = str_squish(establishmentName)) |>
  filter(key %in% WITHDRAWN) |>
  group_by(key) |>
  summarise(portal_routine = n(),
            portal_mean = round(mean(score, na.rm = TRUE), 2),
            portal_latest = max(date), .groups = "drop")

chk <- ours |>
  left_join(theirs, by = c("restaurant_name" = "key")) |>
  mutate(newer = !is.na(portal_latest) & portal_latest > our_latest) |>
  arrange(our_mean_routine)
print(as.data.frame(chk |> mutate(restaurant_name = str_trunc(restaurant_name, 30))),
      right = FALSE)
message("")
message(sprintf("  %d of %d have an inspection on the portal newer than our export.",
                sum(chk$newer, na.rm = TRUE), nrow(chk)))
message("  Portal counts cover only the scraped window, so portal_routine is NOT a")
message("  full history -- combine with ins.csv before republishing anything.")

# Positive control. If this does not come back, the scrape is not trustworthy and no
# conclusion may be drawn from any establishment's absence.
ctl <- portal |> filter(str_detect(establishmentName, "Shoal Creek Saloon"))
message("")
message(sprintf("=== positive control: OOB - Shoal Creek Saloon -> %d portal records %s",
                nrow(ctl), if (nrow(ctl) > 0) "(OK)" else "(SCRAPE SUSPECT)"))
if (nrow(ctl)) {
  print(as.data.frame(ctl |> transmute(name = str_trunc(establishmentName, 30), date, score,
                                       type = str_trunc(inspectionType, 22), purpose)),
        right = FALSE)
}
