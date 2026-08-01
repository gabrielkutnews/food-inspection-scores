# Top 10 / bottom 10 by inspection consistency.
#
# Why not a straight top 10 on the latest score: 590 establishments scored a
# perfect 100 on their most recent inspection, so "the top 10" would be a 1.7%
# arbitrary slice of a 590-way tie. Ranking by mean score across at least three
# inspections turns that tie into an actual finding, and on the bottom end it
# surfaces repeat offenders instead of one bad day.
#
# Exclusions, all deliberate:
#   score == 0   Five facilities. Not real scores -- Fiesta Tortillas went
#                96 -> 98 -> 0 and PBS Hospitality went 99 -> 0. A 0 means no
#                score was issued (closed, refused entry), not a filthy kitchen.
#   ^OOB         282 names carry this prefix; it reads as out-of-business.
#   INELIGIBLE   One name is literally "INELIGIBLE FOR RENEWAL - ...".
#   stale        Latest inspection older than RECENCY_MONTHS.
#   n < 3        Not enough inspections for a mean to mean anything.

source("R/common.R")
suppressPackageStartupMessages(library(jsonlite))

MIN_INSPECTIONS <- 3
TOP_N           <- 10

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
  filter(n >= MIN_INSPECTIONS, latest >= cutoff)

message(sprintf("Eligible: %d facilities (>=%d inspections, inspected since %s).",
                nrow(eligible), MIN_INSPECTIONS, format(cutoff)))
message(sprintf("  of those, %d hold a perfect mean of 100.", sum(eligible$mean_score == 100)))

# Ties on the mean break toward more inspections (more evidence), then recency.
rank_table <- function(df, direction) {
  df <- if (direction == "top") {
    arrange(df, desc(mean_score), desc(n), desc(latest))
  } else {
    arrange(df, mean_score, desc(n), desc(latest))
  }
  df |>
    distinct(venue, .keep_all = TRUE) |>   # one slot per parent operator
    head(TOP_N) |>
    mutate(rank = row_number()) |>
    select(rank, name, category, street, city, mean_score, n,
           min_score, max_score, latest_score, latest)
}

restaurants <- eligible |> filter(category == "Restaurant & Food Service")

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

perfect_all  <- sum(eligible$mean_score == 100)
perfect_rest <- sum(restaurants$mean_score == 100)

show(sprintf("TOP 10 -- all establishments (%d of %d with a perfect record, most-inspected first)",
             TOP_N, perfect_all), tables$all_top)
show("BOTTOM 10 -- all establishments",    tables$all_bottom)
show(sprintf("TOP 10 -- restaurants only (%d of %d with a perfect record, most-inspected first)",
             TOP_N, perfect_rest), tables$restaurants_top)
show("BOTTOM 10 -- restaurants only",      tables$restaurants_bottom)

message("")
message(sprintf(
  "NOTE: %d establishments hold a perfect record, so the top table is a sample of a\n      %d-way tie, not a ranking -- rank 1 does not beat rank 10. The bottom table IS\n      a ranking; those means are genuinely distinct.",
  perfect_all, perfect_all))

n_school_top <- sum(tables$all_top$category == "School & Childcare")
if (n_school_top >= TOP_N / 2) {
  message(sprintf(
    "      %d of the top %d are schools/childcare: they are inspected ~6 times per cycle\n      vs 3-4 for restaurants, so ordering by inspection count favours them.",
    n_school_top, TOP_N))
}

# ---- rankings.html -------------------------------------------------------------

payload <- list(
  tables = tables,
  meta = list(
    all         = list(perfect = perfect_all,  eligible = nrow(eligible)),
    restaurants = list(perfect = perfect_rest, eligible = nrow(restaurants))
  )
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
  .scope { display:inline-flex; border:1px solid #cfcfcf; border-radius:6px; overflow:hidden; margin-bottom:14px; }
  .scope button { font:inherit; font-size:13px; padding:6px 13px; border:0; background:#fff;
                  cursor:pointer; color:var(--ink); }
  .scope button[aria-pressed="true"] { background:#1a1a1a; color:#fff; }
  h2 { font-size:13px; text-transform:uppercase; letter-spacing:.05em; color:var(--muted);
       margin:20px 0 7px; }
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
<h1>Austin-area food establishments, ranked by inspection consistency</h1>
<div class="sub">
  Average score across every inspection on record, for establishments with at least
  %d inspections and one within the last %d months. Not a snapshot of a single visit.
</div>

<div class="scope" role="group" aria-label="Scope">
  <button type="button" data-scope="all" aria-pressed="true">All establishments</button>
  <button type="button" data-scope="restaurants" aria-pressed="false">Restaurants only</button>
</div>

<h2>Perfect records</h2>
<p class="cap" id="top-cap"></p>
<div id="top"></div>
<h2>Lowest average scores</h2>
<p class="cap" id="bottom-cap"></p>
<div id="bottom"></div>

<div class="note">
  <b>How this was built.</b> One row per licensed establishment. Excludes five facilities
  scored 0 &mdash; a 0 means no score was issued, not a failing kitchen &mdash; plus names
  carrying an out-of-business or ineligible-for-renewal flag. Where one operator licenses
  many counters at a single venue, only its best-scoring counter appears. Schools are
  inspected roughly twice as often as restaurants, which lifts them wherever inspection
  count breaks a tie &mdash; use the restaurants-only view to compare like with like.
  Establishment type is inferred from the name, since the city publishes no type field,
  so a few will be miscategorised. Source: City of Austin food establishment inspection
  scores. Generated %s.
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
function table(rows){
  if(!rows.length) return "<p class=\'sub\'>No establishments meet the threshold in this view.</p>";
  return "<table><thead><tr><th></th><th>Establishment</th>" +
    "<th class=\'hide-sm\'>Type</th><th class=\'num\'>Average</th>" +
    "<th class=\'num hide-sm\'>Inspections</th><th class=\'num hide-sm\'>Range</th>" +
    "<th class=\'num\'>Latest</th></tr></thead><tbody>" +
    rows.map(function(r){
      return "<tr><td class=\'rank\'>"+r.rank+"</td>" +
        "<td><div class=\'nm\'>"+esc(r.name)+"</div>" +
        "<div class=\'addr\'>"+esc(r.street)+", "+esc(r.city)+"</div></td>" +
        "<td class=\'hide-sm addr\'>"+esc(r.category)+"</td>" +
        "<td class=\'num\'><span class=\'chip\' style=\'background:"+COLORS[bucket(r.mean_score)]+"\'>" +
          r.mean_score.toFixed(1)+"</span></td>" +
        "<td class=\'num hide-sm\'>"+r.n+"</td>" +
        "<td class=\'num hide-sm\'>"+r.min_score+"&ndash;"+r.max_score+"</td>" +
        "<td class=\'num\'>"+r.latest_score+"<div class=\'addr\'>"+fmtDate(r.latest)+"</div></td></tr>";
    }).join("") + "</tbody></table>";
}
function draw(scope){
  var m = DATA.meta[scope], T = DATA.tables;
  document.getElementById("top").innerHTML    = table(T[scope+"_top"]);
  document.getElementById("bottom").innerHTML = table(T[scope+"_bottom"]);

  // The top table is a sample of a large tie, not a ranking. Say so plainly:
  // ordering it 1..10 would imply a difference that is not in the data.
  document.getElementById("top-cap").innerHTML = m.perfect > 10
    ? "<b>"+m.perfect.toLocaleString()+"</b> of "+m.eligible.toLocaleString()+
      " establishments have never scored below 100. No ten of them are \\u201cthe best\\u201d \\u2014 " +
      "these are the ten with the most inspections behind that record."
    : "Establishments with the highest average score.";
  document.getElementById("bottom-cap").innerHTML =
    "Lowest average across all inspections on record. These averages are genuinely " +
    "distinct, so this one is a ranking.";

  document.querySelectorAll("[data-scope]").forEach(function(b){
    b.setAttribute("aria-pressed", String(b.dataset.scope===scope));
  });
}
document.querySelectorAll("[data-scope]").forEach(function(b){
  b.addEventListener("click", function(){ draw(b.dataset.scope); });
});
draw("all");
</script>
</body>
</html>
',
  MIN_INSPECTIONS, RECENCY_MONTHS,
  str_squish(format(Sys.Date(), "%B %e, %Y")),
  toJSON(payload, dataframe = "rows", auto_unbox = TRUE, Date = "ISO8601"),
  BUCKETS$color[1], BUCKETS$color[2], BUCKETS$color[3], BUCKETS$color[4]
)

writeLines(html, "docs/rankings.html")
message("")
message("Wrote docs/rankings.html and data/rankings_*.csv")
