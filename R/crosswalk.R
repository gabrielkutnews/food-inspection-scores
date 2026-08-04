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
#   name+zip          93.4%,            5,220 clean, 60 ambiguous
#   name alone        94.0%,            4,424 clean, 317 ambiguous  <-- never used
#
# Name alone buys one point of coverage for a 20-fold rise in ambiguity, so the name
# tiers stop at name+zip. Tier 4 then recovers the systematic name-string differences
# (see below), taking the total to 96.4%. Everything left goes to review.
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

TIERS <- list(
  list(name = "name+street+zip", cols = c("k_name", "k_street", "zip")),
  list(name = "name+num+zip",    cols = c("k_name", "k_num", "zip")),
  list(name = "name+zip",        cols = c("k_name", "zip"))
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

review <- bind_rows(
  ambiguous |> transmute(permitID, facility_id, reason = paste0("ambiguous:", tier)),
  new_permits |> filter(status == "unmatched") |>
    transmute(permitID, facility_id = NA_character_, reason = "no_key_match")
) |>
  left_join(P |> select(permitID, p_name_raw, p_addr_raw, p_zip, p_first, p_last), by = "permitID") |>
  arrange(reason, p_name_raw)
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
