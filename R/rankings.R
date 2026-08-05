# Two separate ranking embeds: docs/best.html and docs/lowest.html
#
# Split rather than one page with two tables, so each can be embedded independently and
# a newsroom can run one without the other.
#
# ROUTINE INSPECTIONS ONLY, for both the threshold and the average. A follow-up re-check
# is triggered BY a poor routine inspection, so it is not an independent observation --
# counting it would let a bad score manufacture the eligibility to be ranked on it. Four
# restaurants once reached a lowest-scoring table on two routine inspections plus the
# follow-up their own bad routine produced. See METHODOLOGY.md.
#
# Follow-up and permit-related visits stay in the data and on the map; they just do not
# count toward the ranking metric.
#
# ALL READER-FACING WORDING LIVES IN R/editorial.R. Nothing in this file needs editing to
# change a headline, a caption, a note or a cross-link.

source("R/common.R")
source("R/editorial.R")
suppressPackageStartupMessages(library(jsonlite))

MIN_TOP    <- 5   # routine inspections needed for a "best record" claim
MIN_BOTTOM <- 3   # routine inspections needed to appear in the lowest-scores table
TOP_N      <- 10

dir.create("docs", showWarnings = FALSE)
dir.create("data/internal", showWarnings = FALSE, recursive = TRUE)

ins    <- load_inspections()
cutoff <- recency_cutoff(ins)   # anchored to the data, not to today

routine <- ins |>
  filter(is_scored(score, inspection_type),
         !str_detect(restaurant_name, ADMIN_FLAG_RE),
         !is_followup)

eligible <- routine |>
  group_by(facility_id) |>
  summarise(
    name = first(restaurant_name), street = display_street(first(street)),
    city = first(city), n = n(), mean_score = round(mean(score), 2),
    min_score = min(score), max_score = max(score),
    latest = max(inspection_date),
    latest_score = score[which.max(inspection_date)][1], .groups = "drop"
  ) |>
  mutate(category = categorize(name), venue = venue_of(name)) |>
  filter(n >= MIN_BOTTOM, latest >= cutoff)

restaurants <- eligible |> filter(category == "Restaurant & Food Service")

message(sprintf("Eligible: %d facilities inspected since %s, of which %d restaurants.",
                nrow(eligible), format(cutoff), nrow(restaurants)))
message(sprintf("  at >=%d routine inspections: %d restaurants",
                MIN_TOP, sum(restaurants$n >= MIN_TOP)))

# Ties on the mean break toward more inspections (more evidence), then recency.
rank_table <- function(df, direction) {
  df <- if (direction == "top") {
    df |> filter(n >= MIN_TOP) |> arrange(desc(mean_score), desc(n), desc(latest))
  } else {
    df |> arrange(mean_score, desc(n), desc(latest))
  }
  df |>
    distinct(venue, .keep_all = TRUE) |>   # one slot per parent operator
    head(TOP_N) |>
    mutate(rank = row_number()) |>
    # facility_id travels with the row so any audit can join on it rather than on the
    # name. Two different Little Deli & Pizzeria locations share a name; matching on the
    # name pools them and reports a mean that belongs to neither.
    select(rank, facility_id, name, category, street, city, mean_score, n,
           min_score, max_score, latest_score, latest)
}

tables <- list(
  all_top            = rank_table(eligible, "top"),
  all_bottom         = rank_table(eligible, "bottom"),
  restaurants_top    = rank_table(restaurants, "top"),
  restaurants_bottom = rank_table(restaurants, "bottom")
)

# Internal only and gitignored. rankings_all_eligible.csv is one ascending sort from
# reconstituting a lowest-scores table, so it must not sit in the served tree.
for (nm in names(tables)) {
  write_csv(tables[[nm]], file.path("data/internal", paste0("rankings_", nm, ".csv")))
}
write_csv(eligible |> arrange(desc(mean_score)), "data/internal/rankings_all_eligible.csv")

# ---- console ------------------------------------------------------------------

show <- function(title, df) {
  message(""); message(title)
  print(as.data.frame(
    df |> transmute(rank, name = str_trunc(name, 40), mean = mean_score, n,
                    range = paste0(min_score, "-", max_score),
                    latest = paste0(latest_score, " on ", format(latest)))
  ), right = FALSE)
}
show(sprintf("BEST %d RESTAURANTS -- highest routine average, >=%d inspections", TOP_N, MIN_TOP),
     tables$restaurants_top)
show(sprintf("LOWEST %d RESTAURANTS -- lowest routine average, >=%d inspections", TOP_N, MIN_BOTTOM),
     tables$restaurants_bottom)

audit <- function(title, df, direction) {
  message(""); message(title)
  d <- if (direction == "top") df |> filter(n >= MIN_TOP) |> arrange(desc(mean_score), desc(n))
       else df |> arrange(mean_score, desc(n))
  print(as.data.frame(d |> distinct(venue, .keep_all = TRUE) |> head(20) |>
    transmute(name = str_trunc(name, 40), street = str_trunc(street, 26), n,
              mean = mean_score, latest)), right = FALSE)
}
audit("AUDIT -- top 20 candidates for the BEST list. Anything here that is not a\n         restaurant needs a pattern or an override in R/common.R.",
      restaurants, "top")
audit("AUDIT -- top 20 candidates for the LOWEST list. THIS IS THE PUBLISHED-ACCUSATION\n         list: check every name is a restaurant a reader could walk into.",
      restaurants, "bottom")

# ---- page builder ---------------------------------------------------------------

VALS <- list(
  through    = str_squish(format(data_through(ins), "%B %e, %Y")),
  cutoff     = str_squish(format(cutoff, "%B %e, %Y")),
  generated  = str_squish(format(Sys.Date(), "%B %e, %Y")),
  through_short = fmt_date_ap(data_through(ins)),
  cutoff_short  = fmt_date_ap(cutoff),
  min_top    = MIN_TOP,
  min_bottom = MIN_BOTTOM,
  source     = SOURCE_LINKS,
  n_shown    = "", n_excluded = "", n_unmapped = ""
)

CSS <- '
  :root { --ink:#1a1a1a; --muted:#6b6b6b; --line:#e6e6e6;
          --font:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif; }
  * { box-sizing:border-box; }
  body { margin:0; padding:14px; font-family:var(--font); color:var(--ink); background:#fff;
         -webkit-text-size-adjust:100%; }
  h1 { font-size:18px; margin:0 0 4px; line-height:1.25; }
  .sub { font-size:13px; color:var(--muted); line-height:1.5; margin-bottom:14px; }
  .cap { font-size:12.5px; color:var(--muted); line-height:1.5; margin:0 0 10px; }
  table { border-collapse:collapse; width:100%; font-size:14px; }
  th,td { text-align:left; padding:8px 9px; border-bottom:1px solid var(--line); vertical-align:top; }
  th { font-size:11px; text-transform:uppercase; letter-spacing:.04em; color:var(--muted);
       font-weight:700; border-bottom:1.5px solid #d0d0d0; white-space:nowrap; }
  td.num, th.num { text-align:right; font-variant-numeric:tabular-nums; white-space:nowrap; }
  td.rank { color:var(--muted); font-variant-numeric:tabular-nums; width:1%; }
  .nm { font-weight:600; }
  .addr { font-size:12px; color:var(--muted); }
  .chip { display:inline-grid; place-items:center; min-width:36px; height:24px; padding:0 6px;
          border-radius:4px; color:#fff; font-weight:700; font-size:13px;
          text-shadow:0 1px 1px rgba(0,0,0,.28); }
  .note { font-size:12px; color:var(--muted); line-height:1.6; margin-top:18px;
          border-top:1px solid var(--line); padding-top:10px; }
  .note a { color:#1976d2; }
  .sm-only { display:none; }
  /* Mobile. Four columns do not fit a 320px phone, so the Inspections column collapses and
     its value moves into the name cell (.sm-only above). Type stays >=15px: below that iOS
     zooms the whole embed when a reader taps, and an embed cannot be zoomed back out.
     Long establishment names wrap rather than force a horizontal scrollbar. */
  @media (max-width:560px) {
    body { padding:11px; }
    h1 { font-size:17px; }
    .sub { font-size:14px; }
    .cap { font-size:13.5px; }
    table { font-size:15px; }
    th,td { padding:12px 6px; }
    th:first-child, td.rank { padding-right:2px; }
    .hide-sm { display:none; }
    .sm-only { display:inline; }
    .addr { font-size:13px; }
    .nm { overflow-wrap:anywhere; }
    .note { font-size:13px; }
  }
  /* Very narrow phones: the address is the first thing to give, not the score. */
  @media (max-width:360px) {
    th,td { padding:11px 4px; }
    table { font-size:14.5px; }
  }'

JS_TABLE <- '
function bucket(s){ return s<=69?"red":s<=79?"orange":s<=89?"yellow":"green"; }
function esc(s){ return String(s).replace(/[&<>"]/g,function(c){
  return {"&":"&amp;","<":"&lt;",">":"&gt;","\\"":"&quot;"}[c]; }); }
function fmtDate(d){
  if(!d) return "";
  var p=String(d).slice(0,10).split("-");
  var m=["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
  return (m[+p[1]-1]||"?")+" "+(+p[2])+", "+p[0];
}
function table(rows){
  if(!rows || !rows.length) return "<p class=\'cap\'>No restaurants meet the threshold.</p>";
  return "<table><thead><tr><th></th><th>Restaurant</th>" +
    "<th class=\'num\'>Average</th><th class=\'num hide-sm\'>Inspections</th>" +
    "<th class=\'num\'>Latest</th></tr></thead><tbody>" +
    rows.map(function(r){
      // The inspection count is repeated inside the name cell and shown only on narrow
      // screens, where its own column is hidden. Dropping the column outright would take
      // away the evidence for the "at least N routine inspections" claim in the caption.
      return "<tr><td class=\'rank\'>"+r.rank+"</td>" +
        "<td><div class=\'nm\'>"+esc(r.name)+"</div>" +
        "<div class=\'addr\'>"+esc(r.street)+", "+esc(r.city) +
          "<span class=\'sm-only\'> &middot; "+r.n+(r.n==1?" inspection":" inspections")+"</span>" +
        "</div></td>" +
        "<td class=\'num\'><span class=\'chip\' style=\'background:"+COLORS[bucket(r.mean_score)]+"\'>" +
          r.mean_score.toFixed(1)+"</span></td>" +
        "<td class=\'num hide-sm\'>"+r.n+"</td>" +
        "<td class=\'num\'>"+r.latest_score+"<div class=\'addr\'>"+fmtDate(r.latest)+"</div></td></tr>";
    }).join("") + "</tbody></table>";
}
document.getElementById("tbl").innerHTML = table(DATA);

// Tell the embedding page how tall this content actually is.
//
// A fixed iframe height cannot fit both layouts: at 1280px the rows sit on one line each,
// at 375px every name and address wraps, so the same table is several hundred pixels taller
// on a phone. A height chosen to fit the phone leaves dead space on desktop, which is the
// gap under the embed. This lets the parent resize instead of guessing.
//
// Harmless if the CMS ignores it -- then the fixed height in the iframe tag still applies.
function postHeight(){
  if (window.parent === window) return;   // not embedded; nothing to tell
  var d = document.documentElement;
  window.parent.postMessage({
    embed: "food-inspection-scores",
    slug: (location.pathname.split("/").pop() || "").replace(".html", ""),
    height: Math.ceil(Math.max(d.scrollHeight, d.getBoundingClientRect().height))
  }, "*");
}
postHeight();
addEventListener("load", postHeight);
addEventListener("resize", postHeight);
// Fonts landing late change the wrap points and therefore the height.
if (document.fonts && document.fonts.ready) document.fonts.ready.then(postHeight);
if (window.ResizeObserver) new ResizeObserver(postHeight).observe(document.body);'

build_page <- function(copy, rows) {
  # Strip the jurisdiction city code for display only, and only here -- at the last step
  # before the HTML is written. Everything upstream (grouping, categorising, venue dedup,
  # the internal CSVs, the verification sheet) keeps the raw name, so the published name can
  # always be traced back to the record it came from.
  rows$name <- display_name(rows$name)

  tpl <- paste0('<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>', copy$browser_title, '</title>
<style>', CSS, '
</style>
</head>
<body>
<h1>', copy$headline, '</h1>
<p class="cap">', copy$table_caption, '</p>
<div id="tbl"></div>
<div class="note">', EDITORIAL$method_note, '</div>
<script>
var DATA = ', as.character(toJSON(rows, dataframe = "rows", auto_unbox = TRUE, Date = "ISO8601")), ';
var COLORS = { red:"', BUCKETS$color[1], '", orange:"', BUCKETS$color[2],
              '", yellow:"', BUCKETS$color[3], '", green:"', BUCKETS$color[4], '" };',
JS_TABLE, '
</script>
</body>
</html>
')
  fill(tpl, VALS)
}

writeLines(build_page(EDITORIAL$best,   tables$restaurants_top),    "docs/best.html")
writeLines(build_page(EDITORIAL$lowest, tables$restaurants_bottom), "docs/lowest.html")

# The old combined page is replaced by the two above. Leave a redirect rather than a 404,
# because the URL may already be embedded somewhere.
writeLines(paste0('<!doctype html><html lang="en"><head><meta charset="utf-8">',
  '<meta http-equiv="refresh" content="0; url=best.html">',
  '<title>Moved</title></head><body style="font-family:sans-serif;padding:16px">',
  'This page is now split in two: <a href="best.html">best inspection records</a> and ',
  '<a href="lowest.html">lowest inspection scores</a>.</body></html>'),
  "docs/rankings.html")

message("")
message("Wrote docs/best.html, docs/lowest.html (docs/rankings.html now redirects)")
message("      and data/internal/rankings_*.csv")
