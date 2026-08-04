# Build data/permit_crosswalk.csv: portal permitID <-> ins.csv facility_id.
#
# The two sources share no key. The portal identifies establishments by permitID (a
# GUID) and the Socrata export by facility_id (a number), so a merge needs a crosswalk
# matched on name and address.
#
# THE UNIVERSE. The portal holds 10,056 permits but most are not what ins.csv covers:
# 2,165 mobile-vendor permits, 2,005 pool/spa permits, and a tail of wholesale and
# preopening permits. Exactly one inspection type produces a score on the 100-point
# scale -- "2017 FDA Food Inspection" -- and that set (5,730 permits) is precisely the
# set with a nonzero score. Those are the only permits worth crosswalking, and they sit
# against the 5,555 ins.csv facilities that were still being inspected once the
# portal's window opened. The other 963 facilities stopped being inspected before the
# portal's history begins, so their absence carries no information.
#
# MATCHING ORDER. Name-and-address keys first, tightest to loosest. A match is accepted
# only when a permit points at exactly ONE facility -- that is the direction that
# corrupts data if wrong. Several permits pointing at one facility is fine and expected,
# because Austin re-issues permits under new numbers.
#
# Measured on this data, before per-permit acceptance and tier 4:
#
#   name+street+zip   92.8% of permits, 5,289 clean, 15 ambiguous keys
#   name+num+zip      93.3%,            5,295 clean, 22 ambiguous
#   name+zip          93.4%   <-- DELETED: all 5 of its unique matches were wrong
#   name alone        94.0%,            4,424 clean, 317 ambiguous  <-- never used
#
# Any key that does not compare the address merges different sites of the same business.
# name+zip looked like a cheap extra point of coverage and was in fact 5 conflations
# (two Kerbey Lane Cafes, two Jersey Mike's, two Master Donuts...). Every surviving tier
# compares street or house number, and a global assertion demotes any match whose house
# number disagrees with its facility's -- see ADDRESS GUARD below.
#
# THE EVENT FINGERPRINT IS A DISAMBIGUATOR, NOT AN ACCEPTOR. Both sources cover
# 2025-01-29 .. 2026-05-22, so a permit and a facility that are the same establishment
# should share (inspection_date, score) pairs. That is powerful for choosing between
# same-named candidates -- notably the 23 retired-permit handoffs in ins.csv, where one
# business has two facility_ids and the portal has one permit. It is NOT used to accept
# a match on its own: 16 facilities at the Q2 Stadium address share a byte-identical
# five-event fingerprint, so a name-blind fingerprint join would coin-flip between them.
#
# Nothing is force-matched. Unresolved permits land in data/crosswalk_review.csv.

source("R/common.R")

RAW_PATH    <- "data/portal_inspections.csv"
XW_PATH     <- "data/permit_crosswalk.csv"
REVIEW_PATH <- "data/crosswalk_review.csv"

stopifnot("run R/portal_fetch.R first" = file.exists(RAW_PATH))

# ---- normalisation -------------------------------------------------------------
# The portal gives a clean city and split address lines; ins.csv has one string with
# the city glued on and artifacts like "Bunit". Both sides come to the same shape.
# Jurisdiction prefixes (PF-, LW-, OOB- ...) are stripped because the portal does not
# always carry them.

PREFIX_RE <- "^(oob|pf|lw|bc|mn|wl|sv|rw|abia|cota|tc|rr|heb)\\s*-\\s*"

norm_name <- function(x) {
  x |> str_to_lower() |> str_remove(PREFIX_RE) |> str_remove_all("[^a-z0-9]+")
}
norm_street <- function(x) {
  x |> str_to_lower() |>
    str_replace_all("\\b(suite|ste|unit|bunit|bldg|building|apt|rm|room|fl|floor)\\b", " ") |>
    str_remove_all("[^a-z0-9]+")
}
house_num <- function(x) str_extract(str_squish(x), "^\\d+")

# ---- load ----------------------------------------------------------------------

portal_all <- read_csv(RAW_PATH, col_types = cols(.default = col_character())) |>
  mutate(date  = as_date(substr(inspectionDate, 1, 10)),
         score = suppressWarnings(as.integer(score))) |>
  distinct(inspectionID, .keep_all = TRUE)

portal <- portal_all |> filter(inspectionType == "2017 FDA Food Inspection")
ins    <- load_inspections()
pmin   <- min(portal$date)

P <- portal |>
  group_by(permitID) |>
  summarise(p_name_raw = first(establishmentName),
            p_addr_raw = first(addressLine1),
            p_city     = first(city),
            p_zip      = substr(first(zip), 1, 5),
            p_n        = n(),
            p_first    = min(date), p_last = max(date), .groups = "drop") |>
  mutate(k_name = norm_name(p_name_raw), k_street = norm_street(p_addr_raw),
         k_num  = house_num(p_addr_raw))

I <- ins |>
  # facility_id is an identifier, not a quantity, and synthetic ids for new permits
  # take the form "P:<guid>" -- so it is character everywhere downstream.
  mutate(facility_id = as.character(facility_id)) |>
  group_by(facility_id) |>
  summarise(i_name_raw = first(restaurant_name),
            i_addr_raw = first(street),
            i_city     = first(city),
            i_zip      = first(zip5),
            i_n        = n(),
            i_first    = min(inspection_date), i_last = max(inspection_date),
            .groups = "drop") |>
  mutate(k_name = norm_name(i_name_raw), k_street = norm_street(i_addr_raw),
         k_num  = house_num(i_addr_raw))

I_active <- I |> filter(i_last >= pmin)

message(sprintf("Universe: %d FDA permits vs %d ins facilities active since %s",
                nrow(P), nrow(I_active), format(pmin)))
message(sprintf("  (%d further ins facilities went quiet before the portal window and are not expected)",
                nrow(I) - nrow(I_active)))

# ---- event fingerprint, for disambiguation only ---------------------------------

p_events <- portal |> transmute(permitID, ev = paste0(date, ":", score)) |> distinct()
i_events <- ins |> filter(!is.na(score)) |>
  transmute(facility_id = as.character(facility_id),
            ev = paste0(inspection_date, ":", score)) |> distinct()

shared_events <- function(pids, fids) {
  inner_join(p_events |> filter(permitID %in% pids),
             i_events |> filter(facility_id %in% fids),
             by = "ev", relationship = "many-to-many") |>
    count(permitID, facility_id, name = "n_shared")
}

# ---- tiered matching -----------------------------------------------------------

# name+zip is DELETED, not merely guarded. It produced exactly 5 matches and all 5 were
# address conflations between different sites of the same business: two Kerbey Lane Cafes
# (300 vs 3003 S Lamar), two Jersey Mike's (3005 S Lamar vs 600 E Ben White), two Master
# Donuts on E Riverside, and two more. A tier with a 100% error rate has no threshold
# worth tuning. Every remaining tier compares the street or the house number.
TIERS <- list(
  list(name = "name+street+zip", cols = c("k_name", "k_street", "zip")),
  list(name = "name+num+zip",    cols = c("k_name", "k_num", "zip"))
)

matched  <- tibble(permitID = character(), facility_id = character(),
                   match_method = character(), confidence = character(),
                   n_shared_events = integer())
ambiguous <- tibble()

p_left <- P
i_pool  <- I_active

for (t in TIERS) {
  if (nrow(p_left) == 0) break
  keyp <- p_left |> mutate(zip = p_zip) |>
    transmute(permitID, k = do.call(paste, c(lapply(t$cols, function(c) get(c)), sep = "|")))
  keyi <- i_pool |> mutate(zip = i_zip) |>
    transmute(facility_id, k = do.call(paste, c(lapply(t$cols, function(c) get(c)), sep = "|")))

  j <- inner_join(keyp, keyi, by = "k", relationship = "many-to-many")
  if (nrow(j) == 0) next

  # Ambiguity is judged PER PERMIT, not per key. The direction that corrupts data is
  # one permit pointing at several facilities -- that would attribute an inspection to
  # the wrong business. Several permits pointing at ONE facility is normal and correct:
  # Austin re-issues a permit under a new number when a business changes hands or
  # renews, so "Donut4U" and "OOB - Donut 4U" at the same address are one establishment
  # with two permits, and both belong on the same facility_id. Requiring a key to be
  # 1:1 in both directions rejected 10 of those needlessly.
  clean <- j |> group_by(permitID) |>
    filter(n_distinct(facility_id) == 1) |> ungroup()

  # Where a permit still points at several facilities, let shared inspection events
  # choose -- among candidates that already share a name key, never name-blind.
  amb <- j |> anti_join(clean, by = "permitID")
  resolved <- tibble()
  if (nrow(amb) > 0) {
    sh <- shared_events(unique(amb$permitID), unique(amb$facility_id))
    resolved <- amb |>
      left_join(sh, by = c("permitID", "facility_id")) |>
      mutate(n_shared = coalesce(n_shared, 0L)) |>
      group_by(permitID) |>
      # A winner needs >=2 shared events AND strictly more than any rival.
      filter(n_shared >= 2, n_shared == max(n_shared), sum(n_shared == max(n_shared)) == 1) |>
      ungroup() |>
      distinct(permitID, .keep_all = TRUE)
    ambiguous <- bind_rows(ambiguous,
                           amb |> anti_join(resolved, by = "permitID") |> mutate(tier = t$name))
  }

  add <- bind_rows(
    clean |> transmute(permitID, facility_id, match_method = t$name,
                       confidence = "exact", n_shared_events = NA_integer_),
    resolved |> transmute(permitID, facility_id, match_method = paste0(t$name, "+events"),
                          confidence = "resolved_by_events", n_shared_events = n_shared)
  )
  matched <- bind_rows(matched, add)

  message(sprintf("  %-16s +%d matched (%d exact, %d by events)",
                  t$name, nrow(add), nrow(clean), nrow(resolved)))

  # Only consume PERMITS between tiers. Removing matched facilities from the pool
  # would stop a second permit for the same business from ever matching it.
  p_left <- p_left |> filter(!permitID %in% matched$permitID)
}

# ---- tier 4: exact address, corroborated name ------------------------------------
# 229 of the 244 remaining permits sit at an address that DOES exist in ins.csv -- the
# business is there, the name string differs. Two systematic causes:
#
#   "ABIA Amy's Ice Cream"        vs "ABIA - Amy's Ice Cream"      (prefix without a hyphen)
#   "7-Eleven - Vaishnavi Food"   vs "7-Eleven - #23621 Vaishnavi" (store number)
#
# The tempting fix -- making the hyphen optional in PREFIX_RE -- would turn "PF Chang's"
# into "Chang's" and merge it with any Pflugerville namesake. So instead: take exact
# street+zip candidates and require the names to corroborate, either by containment
# after prefix-stripping or by shared inspection events. Address alone is never enough
# (616 addresses hold more than one facility; the airport terminal holds dozens).

loose_name <- function(x) {
  x |> str_to_lower() |>
    str_remove("^(oob|pf|lw|bc|mn|wl|sv|rw|abia|cota|tc|rr|heb)\\s*-?\\s*") |>
    str_remove_all("#\\s*\\d+") |>          # store numbers
    str_remove_all("[^a-z0-9]+")
}

if (nrow(p_left) > 0) {
  cand <- inner_join(
    p_left |> transmute(permitID, k = paste(k_street, p_zip), pl = loose_name(p_name_raw)),
    I_active |> transmute(facility_id, k = paste(k_street, i_zip), il = loose_name(i_name_raw)),
    by = "k", relationship = "many-to-many"
  ) |>
    # Guard the empty case BEFORE str_detect runs. A few names are nothing but a prefix
    # ("ABIA", "OOB -"), so loose_name() returns "" -- and an empty pattern matches
    # everything, which would merge unrelated tenants at one address. R's `&` is
    # vectorised rather than short-circuiting, so a guard in the same expression still
    # evaluates str_detect; substitute an unmatchable sentinel instead.
    mutate(pl = if_else(nzchar(pl), pl, "none"),
           il = if_else(nzchar(il), il, "none"),
           contains = str_detect(pl, fixed(il)) | str_detect(il, fixed(pl)))

  sh4 <- shared_events(unique(cand$permitID), unique(cand$facility_id))
  ok4 <- cand |>
    left_join(sh4, by = c("permitID", "facility_id")) |>
    mutate(n_shared = coalesce(n_shared, 0L)) |>
    filter(contains | n_shared >= 2) |>
    group_by(permitID) |>
    filter(n_distinct(facility_id) == 1) |>       # still must be unambiguous per permit
    ungroup() |>
    distinct(permitID, .keep_all = TRUE)

  if (nrow(ok4) > 0) {
    matched <- bind_rows(matched, ok4 |>
      transmute(permitID, facility_id,
                match_method = "street+zip+name_corroborated",
                confidence = if_else(n_shared >= 2, "address+events", "address+name_containment"),
                n_shared_events = n_shared))
    message(sprintf("  %-16s +%d matched (%d by name containment, %d by shared events)",
                    "street+zip+name", nrow(ok4), sum(ok4$contains), sum(!ok4$contains)))
    p_left <- p_left |> filter(!permitID %in% matched$permitID)
  }
}


# ---- tier 5: retired-permit handoffs --------------------------------------------
# The remaining ambiguity is almost entirely one pattern. Austin issues a NEW
# facility_id when a business re-permits, so ins.csv holds two ids for one restaurant,
# and the portal's single permit matches both. Measured on the 7 businesses left:
# 4 are clean handoffs (Epic Poke, Pour Choices, Smiling Donuts, St. Michael's) with
# 175-364 day gaps and no overlap; 3 genuinely overlap in time and are left alone.
#
# For a handoff the right answer is not "pick one" -- both ids are the same business, so
# the permit attaches to whichever id is currently active and the older id is recorded in
# data/facility_merges.csv so the merge stage can combine the two histories. Suppressing
# the old id instead would discard real inspections.
#
# Guard: same normalised name, spans strictly disjoint, and a gap of at least 150 days.
# Without the gap requirement two concurrent counters that happen not to overlap in a
# small sample would be merged.
HANDOFF_MIN_GAP <- 150

merges <- tibble(old_facility_id = character(), new_facility_id = character(),
                 business = character(), gap_days = integer())

if (nrow(ambiguous) > 0) {
  spans <- ins |> mutate(facility_id = as.character(facility_id)) |>
    group_by(facility_id) |>
    summarise(f_first = min(inspection_date), f_last = max(inspection_date),
              f_n = n(), .groups = "drop")

  # ADDRESS GUARD. A handoff is one business re-permitted at the SAME address. Without
  # this check the rule crossed two campuses: St. Michael's Catholic Preparatory School
  # has separate facilities at 2500 Wimberly Ln and 3000 Barton Creek Blvd, both in
  # 78735 and both inspected on 2026-05-01, and the permit for Barton Creek was merged
  # into the Wimberly facility. The name+zip tier never compares streets, and temporal
  # disjointness cannot distinguish "re-permitted" from "a second site".
  cand5 <- ambiguous |> distinct(permitID, facility_id) |>
    left_join(spans, by = "facility_id") |>
    left_join(P |> select(permitID, p_name_raw, k_num_p = k_num, k_street_p = k_street),
              by = "permitID") |>
    left_join(I |> select(facility_id, k_num_i = k_num, k_street_i = k_street),
              by = "facility_id") |>
    filter(!is.na(k_num_p), !is.na(k_num_i), k_num_p == k_num_i)

  resolved5 <- list(); merged5 <- list()
  for (pid in unique(cand5$permitID)) {
    g <- cand5 |> filter(permitID == pid) |> arrange(f_first)
    if (nrow(g) < 2) next
    disjoint <- all(g$f_last[-nrow(g)] < g$f_first[-1])
    gap <- as.integer(min(g$f_first[-1] - g$f_last[-nrow(g)]))
    if (!disjoint || is.na(gap) || gap < HANDOFF_MIN_GAP) next   # a real overlap: leave it
    live <- g$facility_id[which.max(g$f_last)]
    resolved5[[pid]] <- tibble(permitID = pid, facility_id = live,
                               match_method = "handoff", confidence = "retired_permit_handoff",
                               n_shared_events = NA_integer_)
    merged5[[pid]] <- tibble(old_facility_id = setdiff(g$facility_id, live),
                             new_facility_id = live,
                             business = g$p_name_raw[1], gap_days = gap)
  }
  if (length(resolved5)) {
    add5 <- bind_rows(resolved5)
    matched <- bind_rows(matched, add5)
    merges  <- bind_rows(merges, bind_rows(merged5)) |> distinct(old_facility_id, .keep_all = TRUE)
    ambiguous <- ambiguous |> filter(!permitID %in% add5$permitID)
    p_left <- p_left |> filter(!permitID %in% add5$permitID)
    message(sprintf("  %-16s +%d matched (%d facility histories to merge)",
                    "handoff", nrow(add5), nrow(merges)))
  }
}
write_csv(merges, "data/facility_merges.csv")

# GLOBAL ADDRESS ASSERTION. Two tiers do not compare streets by construction --
# name+zip, and the handoff resolver built on top of the ambiguous pool. Rather than
# trusting each to guard itself, demote any match whose house number disagrees with its
# facility's to the review file. This is the check that would have caught the St.
# Michael's Barton Creek / Wimberly Lane conflation on its own.
addr_check <- matched |>
  left_join(P |> select(permitID, k_num_p = k_num), by = "permitID") |>
  left_join(I |> select(facility_id, k_num_i = k_num), by = "facility_id") |>
  mutate(crosses = !is.na(k_num_p) & !is.na(k_num_i) & k_num_p != k_num_i)

crossed <- addr_check |> filter(crosses)
if (nrow(crossed) > 0) {
  message(sprintf("\nDEMOTED %d match(es) whose house number disagrees with the facility:",
                  nrow(crossed)))
  print(as.data.frame(crossed |>
    left_join(P |> select(permitID, p_name_raw, p_addr_raw), by = "permitID") |>
    left_join(I |> select(facility_id, i_addr_raw), by = "facility_id") |>
    transmute(name = str_trunc(p_name_raw, 30), portal = str_trunc(p_addr_raw, 24),
              ins = str_trunc(i_addr_raw, 24), method = match_method)), right = FALSE)
  matched <- matched |> filter(!permitID %in% crossed$permitID)
  merges  <- merges |> filter(new_facility_id %in% matched$facility_id)
  ambiguous <- bind_rows(ambiguous,
                         crossed |> transmute(permitID, facility_id, tier = "address_conflict"))
  p_left <- bind_rows(p_left, P |> filter(permitID %in% crossed$permitID)) |>
    distinct(permitID, .keep_all = TRUE)
}

# ---- new establishments ---------------------------------------------------------
# A permit with no match is either genuinely new (opened after the export froze) or a
# failed match. Its first-seen date is the discriminator: a permit whose inspections
# all postdate the export cannot be in it.
FREEZE <- data_through(ins)

new_permits <- p_left |>
  mutate(status = if_else(p_first > FREEZE, "new_after_freeze", "unmatched"))

message(sprintf("\nUnmatched permits: %d  (%d first seen after %s -- genuinely new; %d need review)",
                nrow(new_permits), sum(new_permits$status == "new_after_freeze"),
                format(FREEZE), sum(new_permits$status == "unmatched")))

# ---- assemble -------------------------------------------------------------------

xw <- bind_rows(
  matched |>
    left_join(P |> select(permitID, p_name_raw, p_addr_raw, p_city, p_zip), by = "permitID") |>
    left_join(I |> select(facility_id, i_name_raw, i_addr_raw), by = "facility_id") |>
    mutate(status = "matched"),
  new_permits |>
    transmute(permitID, facility_id = paste0("P:", permitID),
              match_method = NA_character_, confidence = NA_character_,
              n_shared_events = NA_integer_,
              p_name_raw, p_addr_raw, p_city, p_zip,
              i_name_raw = NA_character_, i_addr_raw = NA_character_, status)
) |>
  arrange(status, p_name_raw)

stopifnot("permitID must be unique in the crosswalk" = !any(duplicated(xw$permitID)))
dup_fac <- xw |> filter(status == "matched") |> count(facility_id) |> filter(n > 1)
if (nrow(dup_fac) > 0) {
  message(sprintf("NOTE: %d facility_ids matched by more than one permit (separate permit types at one site)",
                  nrow(dup_fac)))
}

write_csv(xw, XW_PATH)

# Review file, deduplicated per permit (a permit retried across tiers appeared once per
# tier before, inflating 67 real cases into 123 rows) and self-triaging: the only reason
# an unresolved permit matters editorially is if it could reach a published table. That
# needs >= MIN_BOTTOM routine inspections AND a mean low enough to place. Anything else
# is bookkeeping.
p_stats <- portal |>
  filter(purpose == "Routine") |>
  group_by(permitID) |>
  summarise(n_routine = n(), mean_score = round(mean(score, na.rm = TRUE), 1),
            .groups = "drop")

review <- bind_rows(
  ambiguous |> distinct(permitID, facility_id) |> mutate(reason = "ambiguous_time_overlap"),
  new_permits |> filter(status == "unmatched") |>
    transmute(permitID, facility_id = NA_character_, reason = "no_key_match")
) |>
  distinct(permitID, facility_id, .keep_all = TRUE) |>
  left_join(P |> select(permitID, p_name_raw, p_addr_raw, p_zip, p_first, p_last), by = "permitID") |>
  left_join(p_stats, by = "permitID") |>
  mutate(n_routine = coalesce(n_routine, 0L),
         category = categorize(p_name_raw),
         # Could this permit plausibly land in a published ranking if left unresolved?
         could_affect_rankings =
           n_routine >= 3 & !is.na(mean_score) & mean_score < 85 &
           category == "Restaurant & Food Service") |>
  arrange(desc(could_affect_rankings), reason, p_name_raw)
write_csv(review, REVIEW_PATH)

# ---- report ---------------------------------------------------------------------

message("")
message("=== crosswalk ===")
print(as.data.frame(xw |> count(status, match_method, confidence, name = "n") |> arrange(desc(n))),
      right = FALSE)
message(sprintf("\nCoverage: %d of %d FDA permits matched to an existing facility (%.1f%%)",
                sum(xw$status == "matched"), nrow(P), 100 * sum(xw$status == "matched") / nrow(P)))
message(sprintf("Wrote %s (%d rows) and %s (%d rows for manual review)",
                XW_PATH, nrow(xw), REVIEW_PATH, nrow(review)))

if (nrow(review) > 0) {
  message("\nSample needing review:")
  print(as.data.frame(review |> head(10) |>
    transmute(reason, name = str_trunc(p_name_raw, 30), addr = str_trunc(p_addr_raw, 24),
              zip = p_zip, first = p_first)), right = FALSE)
}
