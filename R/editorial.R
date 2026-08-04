# EDITORIAL COPY — everything a reader sees.
#
# This is the only file you need to open to change wording. Titles, captions, the
# standfirst under each headline, the methodology notes and the cross-links between the
# map and the two ranking pages all live here. No numbers are computed in this file, and
# nothing here affects which establishments appear — that is decided by the thresholds in
# R/common.R and R/rankings.R.
#
# Placeholders in {{braces}} are filled in at build time. Available everywhere:
#
#   {{through}}    newest inspection in the data, e.g. "August 4, 2026"
#   {{cutoff}}     start of the recency window,   e.g. "February 4, 2026"
#   {{generated}}  the day the page was built
#   {{min_top}}    routine-inspection minimum for the best-records table
#   {{min_bottom}} routine-inspection minimum for the lowest-scores table
#   {{n_shown}}    establishments on the map
#   {{n_excluded}} establishments withheld as too stale
#
# Leave a link's `url` as "" to hide that link entirely.

EDITORIAL <- list(

  # ---- the map (docs/index.html) ----------------------------------------------
  map = list(
    browser_title = "Austin-area food establishment inspection scores",
    credit_long   = paste(
      "Most recent inspection per establishment. Includes schools, stores and care",
      "facilities as well as restaurants, and covers surrounding jurisdictions."),
    window_note   = paste(
      "Shows only establishments inspected between <b>{{cutoff}}</b> and",
      "<b>{{through}}</b>. {{n_excluded}} others on record were last inspected before",
      "that and are not shown."),
    source_line   = "Source: City of Austin."
  ),

  # ---- best records (docs/best.html) -----------------------------------------
  best = list(
    browser_title = "Austin restaurants with the best inspection records",
    headline      = "Austin-area restaurants with the best inspection records",
    standfirst    = paste(
      "Average score across <b>routine</b> inspections &mdash; not a snapshot of a single",
      "visit. Restaurants, cafes and coffee shops only. Inspection records run through",
      "<b>{{through}}</b>."),
    table_caption = paste(
      "Highest average, among restaurants with at least <b>{{min_top}}</b> routine",
      "inspections. A spotless record over three visits is common enough to be",
      "unremarkable, so the bar is higher here.")
  ),

  # ---- lowest scores (docs/lowest.html) --------------------------------------
  lowest = list(
    browser_title = "Austin restaurants with the lowest inspection scores",
    headline      = "Austin-area restaurants with the lowest inspection scores",
    standfirst    = paste(
      "Average score across <b>routine</b> inspections &mdash; not a snapshot of a single",
      "visit. Restaurants, cafes and coffee shops only. Inspection records run through",
      "<b>{{through}}</b>."),
    table_caption = paste(
      "Lowest average, among restaurants with at least <b>{{min_bottom}}</b> routine",
      "inspections. A low average across three visits is a pattern rather than one bad",
      "day, which is why the threshold is lower here than for the best-records list.")
  ),

  # ---- shared methodology note, appended to both ranking pages ---------------
  # Edit freely, but the substantive claims here are load-bearing: they are what makes
  # the tables defensible. See METHODOLOGY.md for why each sentence is present.
  method_note = paste(
    "<b>How this was built.</b> One row per licensed restaurant, averaged across its",
    "<b>routine</b> inspections. Follow-up re-checks and permit-related visits are real",
    "inspections and appear on the map, but they are excluded from this average and from",
    "the minimum-inspection count, because both happen in response to a prior result and",
    "so are not independent observations.",
    "Also excluded: names carrying an out-of-business or ineligible-for-renewal flag; and",
    "where one operator licenses several counters at a single address, only its",
    "best-scoring counter appears.",
    "Chains and coffee shops count as restaurants; schools, groceries, markets,",
    "convenience stores, care facilities, stadium concessions, staff-only canteens and",
    "retailers that merely hold a food permit are excluded. The city publishes no",
    "facility-type field, so type is inferred from the establishment name and a few will",
    "be misclassified &mdash; each list is checked by hand before publication.",
    "A score of 0 is not a low score on the 100-point scale: the city records it for",
    "inspections that scale does not apply to, such as pool, wholesale and pre-opening",
    "checks. Those are excluded rather than counted as failures.",
    "Source: City of Austin. Inspection records through {{through}}; page built",
    "{{generated}}."),

  # ---- cross-links -------------------------------------------------------------
  # Set `url` to "" to hide a link. These are relative so they work on GitHub Pages and
  # anywhere else the docs/ folder is served.
  links = list(
    map    = list(label = "View the full map",       url = "index.html"),
    best   = list(label = "Best inspection records", url = "best.html"),
    lowest = list(label = "Lowest inspection scores", url = "lowest.html")
  )
)

# Fill {{placeholders}}. Errors on anything left unsubstituted, so a typo in a token name
# fails the build instead of shipping "{{throuhg}}" to a reader.
fill <- function(txt, vals) {
  for (k in names(vals)) txt <- gsub(paste0("{{", k, "}}"), vals[[k]], txt, fixed = TRUE)
  leftover <- unlist(regmatches(txt, gregexpr("\\{\\{[a-z_]+\\}\\}", txt)))
  if (length(leftover)) {
    stop("unknown placeholder(s) in editorial copy: ", paste(unique(leftover), collapse = ", "))
  }
  txt
}
