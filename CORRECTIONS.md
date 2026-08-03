# Corrections

## 2026-08-03 — The lowest-scoring restaurants list has been withdrawn

**What was published.** A table titled "Lowest average scores" naming ten Austin-area
restaurants, ranked 1–10 by their average food-inspection score, on
`gabrielkutnews.github.io/food-inspection-scores/rankings.html`.

**What was wrong.** The table required a restaurant to have at least three inspections
before it could be ranked. That minimum counted **follow-up re-checks** as inspections.
A follow-up re-check is not an independent inspection: the city performs one *because* a
routine inspection went badly. Counting it meant a poor routine score could supply the
very third observation that qualified the restaurant to be ranked on it.

**Four restaurants should never have appeared in the table at all.** Each had only two
routine inspections on record:

| Restaurant | Published rank | Routine inspections | Follow-ups |
|---|---|---|---|
| Special Noodle | 1st worst | 2 | 1 |
| Hunan Bistro | 2nd worst | 2 | 1 |
| Biryani & Co. | 3rd worst | 2 | 1 |
| Fruttilandia | 6th worst | 2 | 1 |

Three of the four were presented as the first, second and third worst restaurants in
Austin. They are named here because a correction that quietly deletes an accusation
cannot be checked by the people it was made about.

**A second point, in fairness to all ten.** Pooling the re-checks *raised* the published
averages rather than lowering them, because a follow-up typically scores well above the
visit that triggered it. So the error was not that the restaurants were made to look
worse than the arithmetic allowed — it was that the metric mixed two different kinds of
visit, and in four cases manufactured the eligibility to be ranked.

**Third: the data was 73 days old.** Every score in the table came from a City of Austin
open-data export frozen on 2026-06-15 containing no inspection after 2026-05-22. The
city has stopped updating that export. The live city portal already showed at least one
of the ten had been inspected again since.

**What happens now.** The list is withdrawn from the page, from the page's data payload,
and from the CSV files that were served alongside it. It will be republished only after:

1. the metric is recomputed from **routine inspections only**, for both the
   minimum-inspection threshold and the average; and
2. every remaining entry is checked against the city's live inspections portal for
   inspections the frozen export never carried.

The "Best records" table remains published. It was unaffected by the eligibility defect
— every entry in it has five or more routine inspections — but it is now computed on the
routine-only basis as well.

## 2026-08-03 — Correction to a claim about scores of 0

**What was published.** The methodology note stated that a score of 0 "means no score was
issued, not a failing kitchen," and five establishments recorded at 0 were excluded on
that basis.

**What is actually known.** A 0 is a real recorded value, so the original wording was
wrong to describe it as an absence. But it is also not a low score on the 100-point FDA
scale: the city records 0 for inspections that scale does not apply to. Confirmed on the
portal, a pool inspection is scored 0 with the findings written into a comments field
instead, and one of the five food establishments — Fiesta Tortillas — carries the
inspection type "Wholesale" with the note "No open or handling of TCS foods."

For the other four we have **not** established which permit type produced the 0, and
saying otherwise would repeat the original error of asserting a reason we had not
checked. A further complication: one of the five, PBS Hospitality Inc., appears on the
live portal with a later score in the nineties that the frozen export never carried, so
its 0 was not its current standing.

Those establishments are therefore held back from the score bands pending verification
against the portal, and the "no score was issued" wording has been removed.
