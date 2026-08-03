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

This rule was adopted after finding that four of ten restaurants in a published
lowest-scoring table had only two routine inspections each. See `CORRECTIONS.md`.

## Thresholds

| | |
|---|---|
| Best-records table | at least **5** routine inspections |
| Lowest-scores table | at least **3** routine inspections |

The two differ deliberately. A spotless record over three visits is unremarkable —
hundreds of establishments have one — so a "best" claim needs volume. A low average over
three visits is already a pattern rather than one bad day, and requiring five there would
hide the worst-scoring restaurants in the city.

## The recency window

An establishment must have been inspected within **18 months of the newest inspection in
the dataset** — not within 18 months of today. Anchoring to the data keeps the window
stable and the build reproducible; anchoring to the clock silently drops establishments
as a dataset ages, which is what happened to 132 of them before this was caught.

Pages state three dates separately: what the records run through, when they were
retrieved, and when the page was built. They are not the same thing and collapsing them
into "generated" overstates freshness.

## Scores of 0

A 0 is a real recorded value, but it is not a low score on the 100-point scale — the city
records it for inspections that scale does not apply to (pool and spa inspections, and at
least one wholesale facility handling no time/temperature-controlled food). The
discriminator is the inspection type, which the open-data export does not carry and the
live portal does.

Establishments recorded at 0 are held out of the score bands until their inspection type
is verified, and the reason is stated per establishment rather than asserted generally.

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
