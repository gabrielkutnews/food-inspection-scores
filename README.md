# Austin-area food establishment inspection scores

An interactive map and consistency rankings built from the City of Austin's food
establishment inspection export, designed to be embedded in Grove via an iframe.

> **The open-data source has stopped updating.** Socrata dataset `ecmv-9xxi` is frozen at
> `rowsUpdatedAt = 2026-06-15` with no inspection after 2026-05-22 — verified against the
> Socrata metadata API. Re-running `R/download.R` changes nothing. Fresh scores now come
> from `R/portal_fetch.R`, which reads the city's live portal. That portal only retains
> ~18 months, so for 2023-06 → 2025-01 the frozen export is the **only** surviving source.

## Run order

```bash
Rscript R/download.R      # frozen source; kept for provenance
Rscript R/portal_fetch.R  # scrape the live portal (opens a Chrome window, ~1 h backfill)
Rscript R/portal_check.R  # reconcile the scrape against ins.csv -- the completeness proof
Rscript R/crosswalk.R     # build data/permit_crosswalk.csv (permitID <-> facility_id)
Rscript R/merge.R         # union both sources -> data/inspections_merged.csv
Rscript R/geocode.R    # network; only geocodes addresses missing from the cache
Rscript R/prep.R          # writes docs/data/points.json (the map)
Rscript R/rankings.R      # writes docs/best.html and docs/lowest.html
Rscript R/factcheck.R     # verifies every published claim; non-zero exit = do not publish
```

| File | Role |
|---|---|
| `R/download.R` | Pulls the city export to `data/ins.csv`. **The source is dead** — frozen 2026-06-15, see below |
| `R/portal_fetch.R` | Scrapes the city's live inspections portal via headful Chrome + chromote |
| `R/portal_check.R` | Week-by-week reconciliation of the scrape against `ins.csv`, plus score-0 audit |
| `R/crosswalk.R` | Matches portal `permitID` to `facility_id`; writes the crosswalk + a review file |
| `R/merge.R` | Unions export + portal into `data/inspections_merged.csv`, the pipeline's input |
| `R/common.R` | Shared: city/category derivation, address cleaning, score buckets, dedupe-to-latest |
| `R/geocode.R` | Address normalisation + Census/ArcGIS geocoding, cached in `data/geocoded_cache.csv` |
| `R/prep.R` | Builds the map payload |
| `R/rankings.R` | Builds the two ranking embeds |
| **`R/editorial.R`** | **All reader-facing wording: titles, captions, notes, cross-links** |
| `R/factcheck.R` | Re-derives every published claim from source; exits non-zero on any failure |
| `docs/` | The published site (GitHub Pages root) |
| `hmm_food.R` | The original exploratory script — kept as-is, not part of the pipeline |

`hmm_food.R` is the first-pass analysis this project grew out of: it builds a
3-bucket leaflet map and writes `restaurant_inspection_map.html` via `saveWidget`.
It is preserved for reference. Line 57 reads `data/geo_coded`, which is not a real
filename, so it stops there; the corrected versions of that logic live in `R/`.

Its `write_csv(ins, "data/ins.csv")` on line 18 is **commented out**, and should stay
that way. It does not error when `ins` is in scope — it silently overwrites the source
data with whatever that object currently holds, which is how `data/ins.csv` once
acquired a 12th column and reformatted dates.

## Editorial control: what to edit, and where

| To change… | Edit | Then run |
|---|---|---|
| Headlines, standfirsts, table captions, the methodology note | `R/editorial.R` | `Rscript R/rankings.R` |
| Links between the map and the two ranking pages | `R/editorial.R` → `links` | `Rscript R/rankings.R` |
| The map's credit line and window note | `R/editorial.R` → `map` | `Rscript R/prep.R` |
| Score band colours | `BUCKETS` in `R/common.R` | `prep.R` **and** `rankings.R` |
| Recency window length | `RECENCY_MONTHS` in `R/common.R` | `prep.R` and `rankings.R` |
| How many routine inspections a ranking needs | `MIN_TOP` / `MIN_BOTTOM` in `R/rankings.R` | `rankings.R` |
| How many rows each table shows | `TOP_N` in `R/rankings.R` | `rankings.R` |

`R/editorial.R` is the only file you need for wording. It computes nothing. Placeholders in
`{{braces}}` — `{{through}}`, `{{cutoff}}`, `{{generated}}`, `{{min_top}}`, `{{min_bottom}}`,
`{{n_excluded}}` — are filled at build time, and a typo in a placeholder name **fails the
build** rather than shipping `{{throuhg}}` to a reader.

To hide a cross-link, set its `url` to `""`. To retitle a page, change `headline` (the page
heading) and `browser_title` (the tab).

## Fact-checking before you publish

```bash
Rscript R/factcheck.R           # full report
Rscript R/factcheck.R --brief   # just the PASS/FAIL lines
```

32 checks, in seven groups: provenance; that nothing was lost merging the two sources; that
a score of 0 is never treated as a food score; that the map payload matches the data; that
each ranking page **re-derives from source in the same order**; the full list of every name
published, for hand-checking against the city portal; and the specific cases that have gone
wrong before (address conflation in the crosswalk, "Market"-named corner stores reaching a
restaurant ranking, the recency window drifting off the clock).

It exits non-zero on any failure, so it can gate a publish. The report is written to
`data/internal/factcheck_report.txt`.

Group 6 is the part software cannot do for you: it prints every published name, address and
score so you can look each up at
`inspections.myhealthdepartment.com/aph` and confirm it. These are claims about real
businesses.

## Methodology, and what to check before publishing

**One row per establishment**, showing its most recent inspection. Where a facility
has two inspections on the same date with different scores, the lower one wins —
row order is not a defensible tiebreak for a food-safety map. `R/prep.R` prints
those conflicts (currently 2) on every run.

**Scores of 0 are real values, but never a failing food score.**

Now settled from the scrape. Across 16,879 portal inspections, **not one of the ~5,800
zeros is a `2017 FDA Food Inspection`** — the type that produces the 0–100 scores on the
map. Every zero belongs to a type the scale does not apply to: mobile-vendor permit
checks (3,002), pool and spa inspections (2,624), wholesale (101), central preparation
facilities (49), pre-opening walkthroughs, farmers' markets, vending machines.

All five of the export's zeros are explained:

| facility | inspection type | inspector's note |
|---|---|---|
| 5 Rivers Tea Company | **Preopening** | "Passed CO inspection after permit to…" |
| Maher Business LLC | **Preopening** | — |
| Fiesta Tortillas | **Wholesale** | — |
| Texas Meat & Produce | **Wholesale** | "No open TCS foods." |
| PBS Hospitality | Wholesale, then **95** on 2026-05-27 | standard food inspection |

Two are certificate-of-occupancy checks scored before the business served anyone. So
`SCORED ⟺ inspectionType == "2017 FDA Food Inspection"` is the rule, and it is exact:
that set is precisely the set with a nonzero score. Historical Socrata rows carry no
inspection type, so for 2023-06 → 2025-01 the rule cannot be applied and those rows
must be treated as unknown rather than assumed scored.

**`OOB` does NOT reliably mean out of business** — now settled. `OOB - Shoal Creek
Saloon` shows portal routine inspections in April 2025 (80), December 2025 (88) and May
2026 (63), *plus a mobile-vendor permit renewal in December 2025*. A closed restaurant
does not renew permits. So the 282 `OOB`-prefixed names cannot be excluded on the
assumption that they are closed, and absence from the portal cannot be read as closure
either — the portal only retains ~18 months, so anything quiet before 2025 is missing
for that reason alone. The prefix is carried as an internal flag and stripped from
display; exclusion decisions need `permitType` and positive evidence.

**The map is not a restaurant dataset; the rankings are.** The map covers 460
schools and childcare centres, 487 convenience stores and pharmacies, 291
groceries, 196 institutional and custodial kitchens, 102 stadium and airport
concessions, 80 healthcare kitchens and 16 specialty retailers alongside 4,879
restaurants — hence "food establishments" in its title, and a type filter so a
reader can narrow it. `docs/rankings.html` publishes **restaurants only**.

`category` is inferred from the establishment name because the city publishes no
type field, and `"Restaurant & Food Service"` is the **residual** bucket — anything
matching no pattern lands there. That makes the patterns in `R/common.R`
load-bearing for the rankings: before they were tightened, roughly half the top 40
candidates were not restaurants (a daycare, a candy shop, a coffee wholesaler,
Apple's staff canteens, sorority kitchens, a school food pod, a hospital gift shop).
`R/rankings.R` prints the **top 20 candidates with their category on every run** —
check that list after any data refresh, before publishing. Chains and coffee shops
count as restaurants; wholesalers, staff-only dining and retailers holding a food
permit do not.

**Only routine inspections count**, toward both the threshold and the average. A
follow-up re-check happens *because* a routine inspection went badly, so it is not an
independent observation — counting it let a bad score supply the third inspection that
qualified a restaurant to be ranked on it. That defect put four restaurants in a
early draft table who should never have been eligible; see `METHODOLOGY.md`. Follow-up and compliance rows stay in the data and on the map.

**The two ranking thresholds differ on purpose.** The top requires ≥5 routine
inspections (`MIN_TOP`), the bottom ≥3 (`MIN_BOTTOM`). A spotless record over three
visits is unremarkable, so a "best" claim needs volume; a low average over three visits
is already a pattern, and a symmetric ≥5 cut would hide the worst-scoring restaurants
in the city.

**The lowest-scoring table is currently withdrawn** pending the portal fact-check. It is
absent from the page payload, not merely hidden, and the ranking CSVs are no longer
tracked or served — `rankings_all_eligible.csv` was one sort away from reconstituting
the list.

**It is not only Austin.** Roughly 650 facilities are in Pflugerville, Manor, Bee
Cave, Lakeway, West Lake Hills, Del Valle, Sunset Valley and other jurisdictions.

**The best-records table is a real ranking.** At ≥5 inspections no restaurant holds a
perfect record, so the top is ordered by genuinely distinct averages rather than
being a sample of a tie. That was not true at ≥3, where 8 restaurants tied at exactly
100.00 — if you lower `MIN_TOP`, the script warns when the tie exceeds ten and you
should label the table accordingly. Ties on the mean break toward more inspections,
then recency, and only one counter per operator per address can appear.

**Recency is anchored to the data, not the clock.** The window is 18 months back from
the **newest inspection in the dataset**, via `recency_cutoff()` in `R/common.R`. It used
to be 18 months back from `Sys.Date()`, which slid forward daily against a frozen export
and had silently dropped 132 facilities before it was caught. The map hides older
inspections by default but lets the reader show them, and its label states the actual
window rather than "last 18 months".

**Coverage.** 6,507 of 6,511 facilities are mapped (99.9%). The four that aren't are
in Austin and failed both geocoders.

**Co-located pins.** 3,511 facilities share an exact address with another (42 at the
airport alone). Each is nudged onto a golden-angle spiral of up to ~45 m so every
one is clickable; that is smaller than the geocoder's own interpolation error, and
the popup says when a pin has been moved.

Cross-check any establishment you name against the city's inspection portal before
publishing.

## The permit crosswalk

The portal keys establishments by `permitID` (a GUID); the export keys them by
`facility_id`. **There is no shared key**, so `R/crosswalk.R` matches on name and
address and writes `data/permit_crosswalk.csv`.

Only one inspection type produces a score on the 100-point scale —
`2017 FDA Food Inspection` — and that set is *exactly* the set with a nonzero score.
Those 5,730 permits are the crosswalk universe; the portal's other ~4,300 permits are
mobile vendors, pools and wholesale, which the export never covered.

| tier | matched | note |
|---|---|---|
| `name+street+zip` | 5,310 | 6 of them disambiguated by shared inspection events |
| `name+num+zip` | 26 | house number only, for suite-formatting differences |
| `name+zip` | 5 | |
| `street+zip` + corroborated name | 182 | 149 by name containment, 33 by shared events |
| **total** | **5,523 / 5,730 (96.4%)** | |

Of the 207 unmatched, **140 first appear after the export froze** — genuinely new
establishments, given synthetic ids of the form `P:<permitID>`. The other 67 go to
`data/crosswalk_review.csv` and are never force-matched.

Three rules that matter if you change this:

- **Ambiguity is judged per permit, not per key.** One permit pointing at several
  facilities would misattribute an inspection; several permits pointing at one facility
  is normal, because Austin re-issues permits under new numbers when a business renews
  or changes hands.
- **The event fingerprint disambiguates, it never accepts.** 16 facilities at the Q2
  Stadium address share a byte-identical five-inspection fingerprint, so a name-blind
  fingerprint join would coin-flip between them. Used *within* a name key it is
  excellent: it correctly separated the two `Elroy & Ross Market` facility_ids whose
  scores systematically disagree.
- **Never make the jurisdiction-prefix hyphen optional** to catch `ABIA Amy's Ice Cream`
  vs `ABIA - Amy's Ice Cream`. It would turn `PF Chang's` into `Chang's` and merge it
  with any Pflugerville namesake. Tier 4 handles those cases with corroboration instead.

## The merge

`R/merge.R` unions the frozen export with the scraped portal into
`data/inspections_merged.csv`: **22,150 inspections across 6,720 facilities,
2023-06-16 → 2026-08-04**. Net **+1,186 inspections** and **74 days** recovered versus
the export, plus 206 facilities that opened after it froze.

**The dedup key is `(facility_id, date, score)`, not `(facility_id, date)`.** Letting the
portal win per calendar day looks obviously right — it is the live system — and silently
destroys data. Nine facilities have two inspections on one day and the sources capture
different subsets:

| | export | portal |
|---|---|---|
| Satellite…Eat.Drink.Orbit, 2026-03-03 | 100 **and 87** | 100 only |
| Taco Bell #030157, 2025-03-03 | 100 and 95 | 95 only |
| Man Pasand Supermarket, 2025-06-18 | 91 | **83** and 91 |
| St. Louis Catholic Church, 2026-05-21 | 98 | **91** and 98 |

Per-day precedence would have dropped Satellite's 87 — the lower of the two, and the one
that matters on a food-safety map. Keying on the score keeps every distinct observation
from either source; where a triple appears in both, the portal row wins for its richer
metadata. Verified: **all 20,907 export triples survive the merge**.

**Facility name and address come from the export where available.** The geocode cache is
keyed on `"street_norm, city, TX, zip5"`; the portal's cleaner address strings would
change that key for thousands of facilities and force a needless re-geocode. Verified:
**6,514 of 6,514 established facilities keep a cached key**, and only the 206 genuinely
new ones need geocoding.

**Non-FDA portal rows are excluded from the union but used to annotate it.** They are not
scored on the 100-point scale, so they must not enter an average — but they still say
what a row *was*. That is how all five of the export's score-0 rows finally got a type:
three `Wholesale`, two `Preopening`. Without it those zeros stayed unexplainable from the
merged table alone.

`comments` is deliberately absent from the merged file. It stays in the gitignored raw
scrape, reachable for reporting, and out of anything feeding the published payload.

11,108 rows still have no inspection type — the pre-2025 span the portal never retained.
Those are **unknown**, not assumed scored.
