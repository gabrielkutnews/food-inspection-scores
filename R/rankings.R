# Top 10 / bottom 10 restaurants by inspection consistency.
#
# Why not a straight top 10 on the latest score: 590 establishments scored a
# perfect 100 on their most recent inspection, so "the top 10" would be a 1.7%
# arbitrary slice of a 590-way tie. Ranking by mean score across every inspection
# on record turns that tie into an actual finding, and on the bottom end it
# surfaces repeat offenders instead of one bad day.
#
# The two thresholds are deliberately different, because the tables answer
# different questions. A perfect record over three visits is unremarkable --
# hundreds have one -- so calling a record "best" needs volume: MIN_TOP = 5. A
# 67.7 average over three visits is already a pattern worth flagging, and a
# symmetric cut would hide the six worst restaurants in Austin (Special Noodle,
# Hunan Bistro, Biryani & Co., Gang Nam, India Gate and Fruttilandia all have
# 3-4 visits), so MIN_BOTTOM = 3.
#
# Exclusions, all deliberate:
#   score == 0   Five facilities. Not real scores -- Fiesta Tortillas went
#                96 -> 98 -> 0 and PBS Hospitality went 99 -> 0. A 0 means no
#                score was issued (closed, refused entry), not a filthy kitchen.
#   ^OOB         282 names carry this prefix; it reads as out-of-business.
#   INELIGIBLE   One name is literally "INELIGIBLE FOR RENEWAL - ...".
#   stale        Latest inspection older than RECENCY_MONTHS.
#   non-restaurant  See categorize() in R/common.R. The published tables are
#                restaurants only; the all-establishments equivalents are still
#                written to data/rankings_all_*.csv for fact-checking.

source("R/common.R")
suppressPackageStartupMessages(library(jsonlite))

MIN_TOP    <- 5   # a "best record" claim needs a track record
MIN_BOTTOM <- 3   # a sustained low average is already newsworthy
TOP_N      <- 10

dir.create("docs", showWarnings = FALSE)

cutoff <- as_date(Sys.Date()) %m-% months(RECENCY_MONTHS)
ins    <- load_inspections()

eligible <- ins |>
  filter(!is.na(score), score > 0, !str_detect(restaurant_name, ADMIN_FLAG_RE)) |>
  group_by(facility_id) |>
  summarise(
    name       = first(restaurant_name),
    street     = display_street(first(street)),
    city       = first(city),
    n          = n(),
    mean_score = round(mean(score), 2),
    min_score  = min(score),
    max_score  = max(score),
    latest     = max(inspection_date),
    latest_score = score[which.max(inspection_date)][1],
    .groups = "drop"
  ) |>
  mutate(category = categorize(name), venue = venue_of(name)) |>
  filter(n >= MIN_BOTTOM, latest >= cutoff)

restaurants <- eligible |> filter(category == "Restaurant & Food Service")

message(sprintf("Eligible: %d facilities inspected since %s, of which %d restaurants.",
                nrow(eligible), format(cutoff), nrow(restaurants)))
message(sprintf("  at >=%d inspections: %d restaurants, %d with a perfect record.",
                MIN_TOP, sum(restaurants$n >= MIN_TOP),
                sum(restaurants$mean_score == 100 & restaurants$n >= MIN_TOP)))

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
    select(rank, name, category, street, city, mean_score, n,
           min_score, max_score, latest_score, latest)
}

tables <- list(
  all_top            = rank_table(eligible, "top"),
  all_bottom         = rank_table(eligible, "bottom"),
  restaurants_top    = rank_table(restaurants, "top"),
  restaurants_bottom = rank_table(restaurants, "bottom")
)

for (nm in names(tables)) {
  write_csv(tables[[nm]], file.path("data", paste0("rankings_", nm, ".csv")))
}
write_csv(eligible |> arrange(desc(mean_score)), "data/rankings_all_eligible.csv")

# ---- console ------------------------------------------------------------------

show <- function(title, df) {
  message("")
  message(title)
  print(as.data.frame(
    df |> transmute(rank, name = str_trunc(name, 42), category = str_trunc(category, 18),
                    mean = mean_score, n, range = paste0(min_score, "-", max_score),
                    latest = paste0(latest_score, " on ", format(latest)))
  ), right = FALSE)
}

perfect_rest <- sum(restaurants$mean_score == 100 & restaurants$n >= MIN_TOP)

show(sprintf("TOP %d RESTAURANTS -- highest average, >=%d inspections", TOP_N, MIN_TOP),
     tables$restaurants_top)
show(sprintf("BOTTOM %d RESTAURANTS -- lowest average, >=%d inspections", TOP_N, MIN_BOTTOM),
     tables$restaurants_bottom)

if (perfect_rest > TOP_N) {
  message("")
  message(sprintf(
    "NOTE: %d restaurants hold a perfect record, so the top table is a sample of a tie\n      rather than a ranking. Say so if you publish it.", perfect_rest))
}

# Audit. "Restaurant & Food Service" is the residual category, so anything the
# patterns in R/common.R do not recognise lands in these tables by default. Eyeball
# this after every data refresh -- half of this pool were not restaurants before
# the patterns were tightened.
message("")
message(sprintf("AUDIT -- top %d restaurant candidates. Anything here that is not a\n         restaurant needs a pattern or an override in R/common.R.", 20))
print(as.data.frame(
  restaurants |>
    filter(n >= MIN_TOP) |>
    arrange(desc(mean_score), desc(n)) |>
    distinct(venue, .keep_all = TRUE) |>
    head(20) |>
    transmute(name = str_trunc(name, 40), street = str_trunc(street, 26), n, mean = mean_score)
), right = FALSE)

# ---- rankings.html -------------------------------------------------------------
# Restaurants only. The all-establishments tables stay in data/rankings_all_*.csv
# for fact-checking but are not published: schools dominate them, because they are
# inspected roughly twice as often as restaurants.

payload <- list(
  top    = tables$restaurants_top,
  bottom = tables$restaurants_bottom
)

html <- sprintf('<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Austin-area food inspection rankings</title>
<style>
  :root { --ink:#1a1a1a; --muted:#6b6b6b; --line:#e6e6e6;
          --font:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif; }
  * { box-sizing:border-box; }
  body { margin:0; padding:14px; font-family:var(--font); color:var(--ink); background:#fff; }
  h1 { font-size:17px; margin:0 0 3px; }
  .sub { font-size:12.5px; color:var(--muted); line-height:1.5; margin-bottom:12px; }
  h2 { font-size:13px; text-transform:uppercase; letter-spacing:.05em; color:var(--muted);
       margin:22px 0 7px; }
  table { border-collapse:collapse; width:100%%; font-size:13.5px; }
  th,td { text-align:left; padding:7px 9px; border-bottom:1px solid var(--line); vertical-align:top; }
  th { font-size:11px; text-transform:uppercase; letter-spacing:.04em; color:var(--muted);
       font-weight:700; border-bottom:1.5px solid #d0d0d0; white-space:nowrap; }
  td.num, th.num { text-align:right; font-variant-numeric:tabular-nums; white-space:nowrap; }
  td.rank { color:var(--muted); font-variant-numeric:tabular-nums; width:1%%; }
  .nm { font-weight:600; }
  .addr { font-size:11.5px; color:var(--muted); }
  .chip { display:inline-grid; place-items:center; min-width:34px; height:23px; padding:0 5px;
          border-radius:4px; color:#fff; font-weight:700; font-size:12.5px;
          text-shadow:0 1px 1px rgba(0,0,0,.28); }
  .note { font-size:11.5px; color:var(--muted); line-height:1.55; margin-top:16px;
          border-top:1px solid var(--line); padding-top:9px; }
  .cap { font-size:12px; color:var(--muted); line-height:1.5; margin:0 0 8px; }
  @media (max-width:560px) { .hide-sm { display:none; } body { padding:10px; } }
</style>
</head>
<body>
<h1>Austin-area restaurants, ranked by inspection consistency</h1>
<div class="sub">
  Average score across every inspection on record &mdash; not a snapshot of a single
  visit. Restaurants, cafes and coffee shops only, each with an inspection in the last
  %d months.
</div>

<h2>Best records</h2>
<p class="cap">
  Highest average, among restaurants inspected at least <b>%d</b> times. A spotless
  record over three visits is common enough to be unremarkable, so the bar is higher
  here than for the list below.
</p>
<div id="top"></div>

<h2>Lowest average scores</h2>
<p class="cap">
  Lowest average, among restaurants inspected at least <b>%d</b> times. A low average
  across three visits is already a pattern rather than one bad day, which is why the
  threshold is lower &mdash; requiring five would hide the worst-scoring restaurants
  in the city.
</p>
<div id="bottom"></div>

<div class="note">
  <b>How this was built.</b> One row per licensed restaurant, averaged across every
  inspection on record. Excludes five facilities scored 0 &mdash; a 0 means no score was
  issued, not a failing kitchen &mdash; plus names carrying an out-of-business or
  ineligible-for-renewal flag. Where one operator licenses several counters at a single
  address, only its best-scoring counter appears.
  Chains and coffee shops count as restaurants; schools, groceries, convenience stores,
  care facilities, stadium concessions, staff-only canteens and retailers that merely
  hold a food permit are excluded. The city publishes no facility-type field, so type is
  inferred from the establishment name and a few will be misclassified &mdash; each list
  was checked by hand before publication.
  Source: City of Austin food establishment inspection scores. Generated %s.
</div>

<script>
var DATA = %s;
var COLORS = { red:"%s", orange:"%s", yellow:"%s", green:"%s" };
function bucket(s){ return s<=69?"red":s<=79?"orange":s<=89?"yellow":"green"; }
function esc(s){ return String(s).replace(/[&<>"]/g,function(c){
  return {"&":"&amp;","<":"&lt;",">":"&gt;","\\"":"&quot;"}[c]; }); }
function fmtDate(d){
  var p=String(d).slice(0,10).split("-");
  var m=["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
  return m[+p[1]-1]+" "+(+p[2])+", "+p[0];
}
// No Type column: every row is a restaurant now, so it would repeat itself ten times.
function table(rows){
  if(!rows.length) return "<p class=\'sub\'>No restaurants meet the threshold.</p>";
  return "<table><thead><tr><th></th><th>Restaurant</th>" +
    "<th class=\'num\'>Average</th>" +
    "<th class=\'num hide-sm\'>Inspections</th><th class=\'num hide-sm\'>Range</th>" +
    "<th class=\'num\'>Latest</th></tr></thead><tbody>" +
    rows.map(function(r){
      return "<tr><td class=\'rank\'>"+r.rank+"</td>" +
        "<td><div class=\'nm\'>"+esc(r.name)+"</div>" +
        "<div class=\'addr\'>"+esc(r.street)+", "+esc(r.city)+"</div></td>" +
        "<td class=\'num\'><span class=\'chip\' style=\'background:"+COLORS[bucket(r.mean_score)]+"\'>" +
          r.mean_score.toFixed(1)+"</span></td>" +
        "<td class=\'num hide-sm\'>"+r.n+"</td>" +
        "<td class=\'num hide-sm\'>"+r.min_score+"&ndash;"+r.max_score+"</td>" +
        "<td class=\'num\'>"+r.latest_score+"<div class=\'addr\'>"+fmtDate(r.latest)+"</div></td></tr>";
    }).join("") + "</tbody></table>";
}
document.getElementById("top").innerHTML    = table(DATA.top);
document.getElementById("bottom").innerHTML = table(DATA.bottom);
</script>
</body>
</html>
',
  RECENCY_MONTHS, MIN_TOP, MIN_BOTTOM,
  str_squish(format(Sys.Date(), "%B %e, %Y")),
  toJSON(payload, dataframe = "rows", auto_unbox = TRUE, Date = "ISO8601"),
  BUCKETS$color[1], BUCKETS$color[2], BUCKETS$color[3], BUCKETS$color[4]
)

writeLines(html, "docs/rankings.html")
message("")
message("Wrote docs/rankings.html and data/rankings_*.csv")
