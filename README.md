# Austin-area food establishment inspection scores

An interactive map and consistency rankings built from the City of Austin's food
establishment inspection export, designed to be embedded in Grove via an iframe.

## Run order

```bash
Rscript R/download.R   # only needed to refresh data/ins.csv from the city's API
Rscript R/geocode.R    # network; only geocodes addresses missing from the cache
Rscript R/prep.R       # writes docs/data/points.json
Rscript R/rankings.R   # writes docs/rankings.html + data/rankings_*.csv
```

| File | Role |
|---|---|
| `R/download.R` | Pulls the city export to `data/ins.csv` (download block commented out by default) |
| `R/common.R` | Shared: city/category derivation, address cleaning, score buckets, dedupe-to-latest |
| `R/geocode.R` | Address normalisation + Census/ArcGIS geocoding, cached in `data/geocoded_cache.csv` |
| `R/prep.R` | Builds the map payload |
| `R/rankings.R` | Builds the ranking tables |
| `docs/` | The published site (GitHub Pages root) |
| `hmm_food.R` | The original exploratory script — kept as-is, not part of the pipeline |

`hmm_food.R` is the first-pass analysis this project grew out of: it builds a
3-bucket leaflet map and writes `restaurant_inspection_map.html` via `saveWidget`.
It is preserved unchanged for reference and does **not** run as written — line 18
writes `ins` before it exists and line 57 reads `data/geo_coded`, which is not a
real filename. The corrected versions of that logic live in `R/`.

Editing thresholds: `RECENCY_MONTHS` and `BUCKETS` live at the top of `R/common.R`;
`MIN_TOP`, `MIN_BOTTOM` and `TOP_N` at the top of `R/rankings.R`.

## Embedding in Grove

Publish `docs/` to GitHub Pages (Settings → Pages → `main` / `/docs`), then paste:

```html
<iframe src="https://USERNAME.github.io/food-inspection-scores/"
        title="Austin-area food establishment inspection scores"
        width="100%" height="620" style="border:0;width:100%;max-width:100%"
        loading="lazy" scrolling="no"></iframe>
```

The rankings embed separately from `.../rankings.html` (height ~1100).

The page carries `<meta name="viewport">` and sizes itself to the iframe, so it is
responsive at any width down to 320px. If Grove allows `<script>` tags, pym.js will
auto-fit the height and remove the inner scrollbar; the fixed height above is the
safe fallback.

## Methodology, and what to check before publishing

**One row per establishment**, showing its most recent inspection. Where a facility
has two inspections on the same date with different scores, the lower one wins —
row order is not a defensible tiebreak for a food-safety map. `R/prep.R` prints
those conflicts (currently 2) on every run.

**Scores of 0 are not real scores.** Five facilities have one; Fiesta Tortillas went
96 → 98 → 0 and PBS Hospitality went 99 → 0. A 0 means no score was issued — closed,
refused entry, out of business. They are excluded from the rankings. Never publish
them as failing kitchens.

**`OOB` and `INELIGIBLE` name prefixes** are administrative flags (282 and 1
facilities). Excluded from the rankings. `OOB` reads as out-of-business but is
applied inconsistently — Shoal Creek Saloon carries it while still being inspected
through May 2026 — so verify against the city portal before relying on it.

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

**The two ranking thresholds differ on purpose.** The top requires ≥5 inspections
(`MIN_TOP`), the bottom ≥3 (`MIN_BOTTOM`). A spotless record over three visits is
unremarkable, so a "best" claim needs volume; a low average over three visits is
already a pattern, and a symmetric ≥5 cut would hide the six worst-scoring
restaurants in the city. Both captions on the page state their threshold.

**It is not only Austin.** Roughly 650 facilities are in Pflugerville, Manor, Bee
Cave, Lakeway, West Lake Hills, Del Valle, Sunset Valley and other jurisdictions.

**Both ranking tables are real rankings.** At ≥5 inspections no restaurant holds a
perfect record, so the top is ordered by genuinely distinct averages rather than
being a sample of a tie. That was not true at ≥3, where 8 restaurants tied at exactly
100.00 — if you lower `MIN_TOP`, the script warns when the tie exceeds ten and you
should label the table accordingly. Ties on the mean break toward more inspections,
then recency, and only one counter per operator per address can appear.

**Recency.** Rankings require an inspection within 18 months. The map hides older
inspections by default but lets the reader show them, rather than silently dropping
~950 pins.

**Coverage.** 6,507 of 6,511 facilities are mapped (99.9%). The four that aren't are
in Austin and failed both geocoders.

**Co-located pins.** 3,511 facilities share an exact address with another (42 at the
airport alone). Each is nudged onto a golden-angle spiral of up to ~45 m so every
one is clickable; that is smaller than the geocoder's own interpolation error, and
the popup says when a pin has been moved.

Cross-check any establishment you name against the city's inspection portal before
publishing.
