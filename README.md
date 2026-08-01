# Austin-area food establishment inspection scores

An interactive map and consistency rankings built from the City of Austin's food
establishment inspection export, designed to be embedded in Grove via an iframe.

## Run order

```bash
Rscript R/geocode.R    # network; only geocodes addresses missing from the cache
Rscript R/prep.R       # writes docs/data/points.json
Rscript R/rankings.R   # writes docs/rankings.html + data/rankings_*.csv
```

`hmm_food.R` refreshes `data/ins.csv` from the city's API — uncomment the download
block when you want new data, then re-run all three.

| File | Role |
|---|---|
| `R/common.R` | Shared: city/category derivation, address cleaning, score buckets, dedupe-to-latest |
| `R/geocode.R` | Address normalisation + Census/ArcGIS geocoding, cached in `data/geocoded_cache.csv` |
| `R/prep.R` | Builds the map payload |
| `R/rankings.R` | Builds the ranking tables |
| `docs/` | The published site (GitHub Pages root) |

Editing thresholds: `RECENCY_MONTHS` and `BUCKETS` live at the top of `R/common.R`;
`MIN_INSPECTIONS` and `TOP_N` at the top of `R/rankings.R`.

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

**This is not a restaurant dataset.** It covers 425 schools and childcare centres,
337 convenience stores and pharmacies, 277 groceries, 63 healthcare kitchens and
stadium/airport concessions alongside restaurants. Hence "food establishments" in
the title. `category` is inferred from the establishment name — the city publishes
no type field — so expect some miscategorisation.

**It is not only Austin.** Roughly 650 facilities are in Pflugerville, Manor, Bee
Cave, Lakeway, West Lake Hills, Del Valle, Sunset Valley and other jurisdictions.

**The top 10 is a tie, not a ranking.** 68 establishments have never scored below
100. The table shows ten of them ordered by inspection count, and says so. Schools
are inspected about twice as often as restaurants, which is why they dominate that
ordering — use the restaurants-only view for a restaurant story. The bottom 10 *is*
a real ranking; those averages are genuinely distinct.

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
