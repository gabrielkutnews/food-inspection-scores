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
#   {{cutoff}}     start of the recency window,   e.g. "February 4, 2025"
#   {{generated}}  the day the page was built
#   {{min_top}}    routine-inspection minimum for the best-records table
#   {{min_bottom}} routine-inspection minimum for the lowest-scores table
#   {{n_shown}}    establishments on the map
#   {{n_excluded}} establishments withheld as too stale
#   {{n_unmapped}} establishments that could not be geocoded
#   {{source}}     both source links, defined in SOURCE_LINKS just below
#
# It is all plain text. Nothing here is JavaScript, so a stray apostrophe or quote cannot
# break a page. That matters: the map's wording used to be inlined in a <script> block, and
# a single hand-edit left an unterminated string which took the whole script down and
# published a blank map.
#
# Leave a link's `url` as "" to hide that link entirely.

# The two source links, defined once and available everywhere as {{source}}. The map note and
# both ranking pages use it, so a changed URL or label is edited here and nowhere else.
#
# Two sources are credited because they cover different periods. The open-data export is
# frozen -- its newest inspection is 2026-05-22 -- and the Austin Public Health portal
# supplies everything after that, so neither one alone accounts for the data.
SOURCE_LINKS <- paste0(
  "<a href=\"https://inspections.myhealthdepartment.com/aph/\"",
  " target=\"_blank\" rel=\"noopener\">Austin Public Health</a> and",
  " <a href=\"https://data.austintexas.gov/Health-and-Community-Services",
  "/Food-Establishment-Inspection-Scores/ecmv-9xxi/about_data\"",
  " target=\"_blank\" rel=\"noopener\">City of Austin Open Data Portal</a>")

EDITORIAL <- list(

  # ---- the map (docs/index.html) ----------------------------------------------
  map = list(
    browser_title = "Austin-area food establishment inspection scores",
    credit_long   = paste(
      "Most recent inspection per establishment. Includes schools, stores and care",
      "facilities as well as restaurants, and covers surrounding jurisdictions."),
    # The note under the map is `source_line` then a line break then `window_note`.
    # Both are rendered as HTML, so <a> and <b> tags below are live. Keep the dates as
    # {{cutoff}} / {{through}} rather than typing them: they are computed from the data,
    # so a hand-typed date silently becomes a lie the next time the data is refreshed.
    #
    source_line   = "Source: {{source}}.",
    window_note   = paste(
      "The map only shows establishments with inspections between <b>{{cutoff}}</b> and",
      "<b>{{through}}</b>, the latest available data."),
    unmapped_note = "{{n_unmapped}} locations could not be placed on the map."
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
      "Highest average scores among restaurants with at least <b>{{min_top}}</b> routine",
      "inspections.")
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
      "Lowest average scores among restaurants with at least <b>{{min_bottom}}</b> routine",
      "inspections.")
  ),

  # ---- shared footnote, appended to both ranking pages -----------------------
  # Shortened at editorial request. The longer version that used to sit here also stated the
  # out-of-business exclusion, the one-counter-per-operator rule, how establishment type is
  # inferred (and that a few are therefore misclassified), and that a score of 0 is not a
  # failing score. All of that still governs the tables and is recorded in METHODOLOGY.md --
  # it is simply no longer stated to the reader. Re-add a sentence here if a piece leans on it.
  method_note = paste(
    "Follow-up, re-checks and permit-related visits are excluded from the",
    "minimum-inspection count. Source: {{source}}.")
)

# Cross-links between the three pages were removed at editorial request, so each embed stands
# alone in a CMS. To bring them back, restore the `links` list here and the `.xlinks` block in
# R/rankings.R:
#   links = list(
#     map    = list(label = "View the full map",        url = "index.html"),
#     best   = list(label = "Best inspection records",  url = "best.html"),
#     lowest = list(label = "Lowest inspection scores", url = "lowest.html"))

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
