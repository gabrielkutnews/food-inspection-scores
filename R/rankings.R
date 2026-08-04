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

ins    <- load_inspections()
cutoff <- recency_cutoff(ins)   # anchored to the data, not to today

# ROUTINE INSPECTIONS ONLY, for both the threshold and the mean.
#
# This corrects a real defect in work that was published. A follow-up re-check is
# triggered BY a poor routine inspection, so it is not an independent observation of
# the same establishment -- and counting it let a bad score manufacture its own
# eligibility. Four of the ten restaurants published as Austin's lowest-scoring had
# only TWO routine inspections; they reached MIN_BOTTOM = 3 solely because their own
# bad routine produced a follow-up. Three of those four were published as #1, #2 and
# #3 worst.
#
# Note the direction: routine-only averages are LOWER in 8 of the 10 cases (Special
# Noodle 67.67 -> 64.50), because follow-ups score well above the visit that
# triggered them. So this tightens the finding. What it removes is manufactured
# eligibility, not severity.
#
# Follow-up and compliance rows are kept in the data and on the map -- they are real
# inspections -- but they do not count toward the ranking metric.
routine <- ins |>
  filter(!is.na(score), score > 0,
         !str_detect(restaurant_name, ADMIN_FLAG_RE),
         !is_followup)

eligible <- routine |>
  group_by(facility_id) |>
  summarise(
    name       = first(restaurant_name),
    street     = display_street(first(street)),
    city       = first(city),
    n          = n(),                       # routine inspections only
    mean_score = round(mean(score), 2),     # routine inspections only
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

# Internal only, and gitignored. These previously lived in tracked data/ and were
# served from the GitHub Pages site, which meant "withdrawing" the bottom table by
# deleting a <div> withdrew nothing: rankings_all_eligible.csv is a single ascending
# sort away from reconstituting the whole list.
dir.create("data/internal", showWarnings = FALSE, recursive = TRUE)
for (nm in names(tables)) {
  write_csv(tables[[nm]], file.path("data/internal", paste0("rankings_", nm, ".csv")))
}
write_csv(eligible |> arrange(desc(mean_score)), "data/internal/rankings_all_eligible.csv")

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

# The BOTTOM pool matters more than the top: this is where a misclassified establishment
# becomes a published accusation. A corner store called "LW - Lakeway Market" reached the
# lowest-scoring table before bare "market" was added to the grocery patterns, and
# "Star Market" was on course to follow it.
message("")
message(sprintf("AUDIT -- bottom %d restaurant candidates. THIS IS THE PUBLISHED-ACCUSATION\n         list. Check every name is a restaurant a reader could walk into.", 20))
print(as.data.frame(
  restaurants |>
    arrange(mean_score, desc(n)) |>
    distinct(venue, .keep_all = TRUE) |>
    head(20) |>
    transmute(name = str_trunc(name, 40), street = str_trunc(street, 26), n,
              mean = mean_score, latest)
), right = FALSE)

# ---- rankings.html -------------------------------------------------------------
# Restaurants only. The all-establishments tables stay in data/internal/ for
# fact-checking but are not published: schools dominate them, because they are
# inspected roughly twice as often as restaurants.
#
# The BOTTOM TABLE IS WITHDRAWN. It is deliberately absent from `payload`, not merely
# hidden in the HTML, so the ten names are not in the shipped page source or in any
# archive snapshot of it. It returns once the portal scrape has fact-checked the
# survivors against inspections the frozen export never carried. See CORRECTIONS.md.

payload <- list(
  top = tables$restaurants_top
)

# Token substitution rather than a positional sprintf. The template is ~120 lines with
# CSS percent signs in it, and every copy edit risked either doubling a %% or silently
# permuting the argument list. render() also errors if any token is left unfilled.
render <- function(tpl, vals) {
  for (k in names(vals)) tpl <- gsub(paste0("{{", k, "}}"), vals[[k]], tpl, fixed = TRUE)
  if (grepl("\\{\\{", tpl)) {
    stop("unsubstituted token(s): ",
         paste(unique(unlist(str_extract_all(tpl, "\\{\\{[a-z_]+\\}\\}"))), collapse = ", "))
  }
  tpl
}

html <- render('<!doctype html>
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
  .withdrawn { font-size:12.5px; line-height:1.6; background:#fdf6e3; border:1px solid #e6d9a8;
               border-left:4px solid #d9a300; border-radius:5px; padding:11px 13px;
               margin:16px 0 4px; color:#4a3d18; }
  @media (max-width:560px) { .hide-sm { display:none; } body { padding:10px; } }
</style>
</head>
<body>
<h1>Austin-area restaurants, ranked by inspection consistency</h1>
<div class="sub">
  Average score across <b>routine</b> inspections &mdash; not a snapshot of a single
  visit. Restaurants, cafes and coffee shops only. Inspection records run through
  <b>{{through}}</b>; this page was built {{generated}}.
</div>

<div class="withdrawn" id="corrections">
  <b>Correction &mdash; the lowest-scoring list has been withdrawn.</b>
  It counted follow-up re-checks as independent inspections. A follow-up happens
  <i>because</i> a routine inspection went badly, so counting it toward the
  three-inspection minimum let a poor score qualify a restaurant on its own. Four of
  the ten restaurants named had only two routine inspections and should never have
  appeared; three of those four were listed as the first, second and third worst in
  Austin. Pooling the re-checks also raised those averages rather than lowering them,
  because follow-ups score well above the visit that triggered them.
  The list will return, computed from routine inspections only, once the city&rsquo;s
  live portal has been checked for inspections the frozen open-data export never
  carried. Named in full in CORRECTIONS.md.
</div>

<h2>Best records</h2>
<p class="cap">
  Highest average, among restaurants with at least <b>{{min_top}}</b> routine
  inspections. A spotless record over three visits is common enough to be unremarkable,
  so the bar is higher here.
</p>
<div id="top"></div>

<div class="note">
  <b>How this was built.</b> One row per licensed restaurant, averaged across its
  <b>routine</b> inspections. Follow-up re-checks and compliance visits are real
  inspections and appear on the map, but they are excluded from this average and from
  the minimum-inspection count, because both happen in response to a prior result and
  so are not independent observations.
  Also excluded: names carrying an out-of-business or ineligible-for-renewal flag, and
  where one operator licenses several counters at a single address, only its
  best-scoring counter appears.
  Chains and coffee shops count as restaurants; schools, groceries, convenience stores,
  care facilities, stadium concessions, staff-only canteens and retailers that merely
  hold a food permit are excluded. The city publishes no facility-type field, so type is
  inferred from the establishment name and a few will be misclassified &mdash; each list
  was checked by hand before publication.
  A handful of establishments are recorded with a score of 0. A 0 is not a low score on
  the 100-point scale and we have not established for every case which permit type
  produced it, so those are held back pending verification against the city&rsquo;s
  portal.
  Source: City of Austin. Inspection records through {{through}}; page built
  {{generated}}.
</div>

<script>
var DATA = {{data}};
var COLORS = { red:"{{c_red}}", orange:"{{c_orange}}", yellow:"{{c_yellow}}", green:"{{c_green}}" };
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
document.getElementById("top").innerHTML = table(DATA.top);
// No bottom table: DATA carries no `bottom` key while the list is withdrawn.
</script>
</body>
</html>
',
  list(
    min_top   = MIN_TOP,
    through   = str_squish(format(data_through(ins), "%B %e, %Y")),
    generated = str_squish(format(Sys.Date(), "%B %e, %Y")),
    data      = as.character(toJSON(payload, dataframe = "rows",
                                    auto_unbox = TRUE, Date = "ISO8601")),
    c_red     = BUCKETS$color[1],
    c_orange  = BUCKETS$color[2],
    c_yellow  = BUCKETS$color[3],
    c_green   = BUCKETS$color[4]
  ))

writeLines(html, "docs/rankings.html")

# Guard: the withdrawn names must not be reachable from anything we publish.
withdrawn <- c("Special Noodle", "Hunan Bistro", "Biryani & Co.", "Fruttilandia")
leaked <- withdrawn[vapply(withdrawn, function(n)
  any(grepl(n, readLines("docs/rankings.html", warn = FALSE), fixed = TRUE)), logical(1))]
if (length(leaked)) stop("withdrawn name still present in docs/rankings.html: ",
                         paste(leaked, collapse = ", "))

message("")
message("Wrote docs/rankings.html (bottom table withdrawn) and data/internal/rankings_*.csv")
