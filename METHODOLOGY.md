# Methodology

Decisions recorded **before** the refreshed data was in hand, so that the rules were not
chosen after seeing which restaurants they would name. Dated 2026-08-03.

## What counts as an inspection

Austin records three inspection purposes: **Routine**, **Follow-Up** and **Compliance**.

Only **Routine** inspections count toward the rankings — both the minimum-inspection
threshold and the average.

Follow-Up and Compliance visits are real inspections and are kept in the data and shown
on the map, but they are conditional on a prior result: the city performs them *because*
something was wrong. Including them means:

- a bad routine score can supply the extra observation that makes a restaurant eligible
  to be ranked on that same bad score, and
- the average mixes a routine visit with its own re-check, and re-checks score well above
  the visit that triggered them.

**Why this rule exists.** An early draft of the lowest-scoring table counted follow-ups
toward the three-inspection minimum. Four of its ten entries turned out to have only
**two** routine inspections each — Special Noodle, Hunan Bistro, Biryani & Co. and
Fruttilandia — and had qualified only because their own poor routine triggered the
re-check that became their third "inspection." Three of the four sat at ranks 1–3.

Pooling the re-checks also *raised* their averages rather than lowering them, because a
follow-up scores well above the visit that triggered it. So the defect was manufactured
eligibility, not overstated severity.

That draft lived on a personal GitHub Pages site and was never published by KUT, so no
reader-facing correction was warranted. The rule above is the fix, recorded here so the
reasoning survives even though the draft does not.

## Thresholds

| | |
|---|---|
| Best-records table | at least **5** routine inspections |
| Lowest-scores table | at least **3** routine inspections |

The two differ deliberately. A spotless record over three visits is unremarkable —
hundreds of establishments have one — so a "best" claim needs volume. A low average over
three visits is already a pattern rather than one bad day, and requiring five there would
hide the worst-scoring restaurants in the city.

## Scores of 0

A 0 is a real recorded value, but it is not a low score on the 100-point scale — the city
records it for inspections that scale does not apply to (pool and spa inspections, and at
least one wholesale facility handling no time/temperature-controlled food). The
discriminator is the inspection type, which the open-data export does not carry and the
live portal does.

Settled from the scrape: across 16,879 portal inspections, **not one** of the ~5,800 zeros
is a `2017 FDA Food Inspection`. All five zeros in the export are typed — three
`Wholesale`, two `Preopening` (certificate-of-occupancy checks carried out before the
business served anyone). Score-0 inspections are excluded from the map and the rankings; a
facility whose *latest* visit was one still shows its most recent genuinely scored
inspection rather than disappearing.

## What is and is not a restaurant

The city publishes no facility-type field, so type is inferred from the establishment
name, and `Restaurant & Food Service` is the **residual** category — anything matching no
other pattern lands there. That makes the patterns load-bearing, so `R/rankings.R` prints
its top 20 candidates with their inferred category on every run, and that list is checked
by hand before anything is published.

Chains and coffee shops count as restaurants. Schools and childcare, groceries,
convenience stores and pharmacies, care facilities, stadium and airport concessions,
staff-only canteens, wholesalers and retailers that merely hold a food permit do not.

Where one operator licenses several counters at a single address, only its best-scoring
counter can occupy a slot, so a single venue cannot fill the table.

## The recency window is a hard filter

An establishment must have been inspected within **12 months of the newest inspection in
the dataset** (`RECENCY_MONTHS` in `R/common.R`) — not within 12 months of today. Anchoring to the data keeps the window
stable and the build reproducible; anchoring to the clock silently drops establishments as
a dataset ages, which is what happened to 132 of them before it was caught.

It is a **hard filter**, not a checkbox. Establishments outside the window do not appear at
all — on the map or in either ranking. It used to be a toggle a reader could switch off,
which put the burden of the caveat on them: an establishment last inspected in 2023 says
nothing about its kitchen today. Both the map and the ranking pages state the window and
how many establishments are withheld because of it.

Pages state three dates separately: what the records run through, when they were retrieved,
and when the page was built. Collapsing them into "generated" overstates freshness.

## Data sources, and a caveat about history

- **2023-06-16 → 2025-01**: City of Austin open-data export (Socrata `ecmv-9xxi`). This
  export was frozen on 2026-06-15 and the city has stopped updating it. It is now the
  **only** surviving source for this period, and it carries no inspection type, purpose
  or inspector comments.
- **2025-01-29 → present**: scraped from the city's live inspections portal, which
  retains roughly 18 months. Probed month by month: 2024 and 2023 return essentially
  nothing.

Inspector comments are collected for reporting but never published: they name individual
certified food managers along with their certificate numbers and expiry dates.
