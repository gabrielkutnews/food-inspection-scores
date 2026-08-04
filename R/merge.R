# Merge the frozen Socrata export with the scraped portal into one inspection table.
#
# Writes data/inspections_merged.csv, which becomes the input to prep.R and rankings.R.
#
# COVERAGE OF EACH SOURCE
#   Socrata  2023-06-16 .. 2026-05-22   frozen; the ONLY source before 2025-01-29
#   Portal   2025-01-29 .. today        richer: inspectionType, purpose, permitID
# They overlap 2025-01-29 .. 2026-05-22, where each holds records the other lacks
# (9,835 shared triples, 95 Socrata-only, 82 portal-only).
#
# THE DEDUP KEY IS (facility_id, date, score), NOT (facility_id, date).
# It is tempting to let the portal win per calendar day, since it is the live system.
# That silently destroys data. Nine facilities have two FDA inspections on one day, and
# the two sources capture different subsets of them:
#
#   Satellite...Eat.Drink.Orbit  2026-03-03   socrata 100 and 87   portal 100 only
#   Taco Bell #030157            2025-03-03   socrata 100 and 95   portal 95 only
#   Man Pasand Supermarket       2025-06-18   socrata 91           portal 83 and 91
#   St. Louis Catholic Church    2026-05-21   socrata 98           portal 91 and 98
#
# Portal-wins-per-day would have dropped Satellite's 87 -- the lower of the two, and the
# one that matters on a food-safety map. Keying on the score keeps every distinct
# observation from either source and loses nothing; where a triple exists in both, the
# portal row is preferred for its richer metadata.
#
# FACILITY ATTRIBUTES COME FROM SOCRATA WHERE AVAILABLE.
# The geocode cache is keyed on "street_norm, city, TX, zip5" at 99.9% coverage. The
# portal's cleaner address strings would change that key for thousands of facilities and
# trigger a needless re-geocode, so an established facility keeps its Socrata name and
# address; only genuinely new facilities take the portal's.
#
# COMMENTS ARE DELIBERATELY NOT CARRIED HERE. Inspector free text names individual
# certified food managers with certificate numbers and expiry dates. It stays in the
# gitignored raw scrape, reachable for reporting, and is kept out of every file that
# feeds the published payload.

source("R/common.R")

RAW_PATH    <- "data/portal_inspections.csv"
XW_PATH     <- "data/permit_crosswalk.csv"
MERGES_PATH <- "data/facility_merges.csv"
OUT_PATH    <- "data/inspections_merged.csv"

stopifnot("run R/portal_fetch.R first" = file.exists(RAW_PATH),
          "run R/crosswalk.R first"    = file.exists(XW_PATH))

xw     <- read_csv(XW_PATH, col_types = cols(.default = col_character()))
merges <- if (file.exists(MERGES_PATH)) {
  read_csv(MERGES_PATH, col_types = cols(.default = col_character()))
} else tibble(old_facility_id = character(), new_facility_id = character())

# ---- Socrata side ---------------------------------------------------------------

soc <- load_inspections() |>
  mutate(facility_id = as.character(facility_id))

# Re-parent retired permits so one business is one facility. Suppressing the old id
# instead would discard real inspections -- Burnet Chevron's 2024 score and its 2026
# score belong to the same business.
if (nrow(merges) > 0) {
  soc <- soc |>
    left_join(merges |> select(old_facility_id, new_facility_id),
              by = c("facility_id" = "old_facility_id")) |>
    mutate(facility_id = coalesce(new_facility_id, facility_id)) |>
    select(-new_facility_id)
  message(sprintf("Re-parented %d retired facility ids into their live counterparts.",
                  nrow(merges)))
}

soc_rows <- soc |>
  transmute(facility_id, inspection_date, score,
            purpose        = if_else(is_followup, "Follow-Up", "Routine"),
            inspection_type = NA_character_,   # Socrata never carried it
            program_name    = NA_character_,
            permit_id       = NA_character_,
            inspection_id   = NA_character_,
            source          = "socrata")

# ---- portal side ----------------------------------------------------------------

portal <- read_csv(RAW_PATH, col_types = cols(.default = col_character())) |>
  distinct(inspectionID, .keep_all = TRUE) |>
  mutate(date = as_date(substr(inspectionDate, 1, 10)),
         score = suppressWarnings(as.integer(score)))

# Only FDA food inspections are comparable to the export. The rest -- mobile-vendor
# permits, pools, wholesale, pre-opening -- are not scored on the 100-point scale and
# would pollute every average. See README.
portal_fda <- portal |>
  filter(inspectionType == "2017 FDA Food Inspection") |>
  inner_join(xw |> select(permitID, facility_id), by = "permitID")

por_rows <- portal_fda |>
  transmute(facility_id, inspection_date = date, score,
            purpose         = purpose,
            inspection_type = inspectionType,
            program_name    = programName,
            permit_id       = permitID,
            inspection_id   = inspectionID,
            source          = "portal")

message(sprintf("Socrata rows: %d | portal FDA rows: %d (of %d scraped)",
                nrow(soc_rows), nrow(por_rows), nrow(portal)))

# ---- union ----------------------------------------------------------------------
# Portal first so distinct() keeps its row when a triple appears in both sources.

merged <- bind_rows(por_rows, soc_rows) |>
  filter(!is.na(facility_id)) |>
  distinct(facility_id, inspection_date, score, .keep_all = TRUE) |>
  arrange(facility_id, inspection_date, score)

# ---- facility attributes --------------------------------------------------------

soc_attr <- soc |>
  group_by(facility_id) |>
  summarise(restaurant_name = first(restaurant_name), address = first(address),
            street = first(street), city = first(city), zip5 = first(zip5),
            .groups = "drop")

por_attr <- portal_fda |>
  group_by(facility_id) |>
  summarise(p_name = first(establishmentName),
            p_street = str_squish(first(addressLine1)),
            p_city = first(city), p_zip = substr(first(zip), 1, 5),
            .groups = "drop")

attrs <- full_join(soc_attr, por_attr, by = "facility_id") |>
  transmute(facility_id,
            # Socrata wins where present, to keep the geocode cache keys stable.
            restaurant_name = coalesce(restaurant_name, p_name),
            address = coalesce(address, p_street),
            street  = coalesce(street,  p_street),
            city    = coalesce(city,    p_city),
            zip5    = coalesce(zip5,    p_zip),
            attr_source = if_else(is.na(p_name) | !is.na(soc_attr$facility_id[match(facility_id, soc_attr$facility_id)]),
                                  "socrata", "portal"))

out <- merged |>
  left_join(attrs |> select(-attr_source), by = "facility_id") |>
  mutate(is_followup = purpose == "Follow-Up") |>   # kept for backward compatibility
  select(facility_id, restaurant_name, address, street, city, zip5,
         inspection_date, score, purpose, is_followup,
         inspection_type, program_name, permit_id, inspection_id, source)

# ---- annotate Socrata rows with the portal's inspection type ---------------------
# Non-FDA portal rows are excluded from the union above -- correctly, since they are not
# scored on the 100-point scale -- but they still TELL us what a Socrata row was. All
# five of the export's score-0 rows are like this: the portal records them as Wholesale
# or Preopening, so dropping those rows left the zeros typeless and unexplainable from
# this table alone. Annotating on (normalised name, date, score) fills the type in
# without adding or removing a single row.
type_lookup <- portal |>
  transmute(nk = str_remove_all(str_to_lower(establishmentName), "[^a-z0-9]+"),
            inspection_date = date, score,
            t_type = inspectionType, t_prog = programName) |>
  filter(!is.na(score)) |>
  distinct(nk, inspection_date, score, .keep_all = TRUE)

before_na <- sum(is.na(out$inspection_type))
out <- out |>
  mutate(nk = str_remove_all(str_to_lower(restaurant_name), "[^a-z0-9]+")) |>
  left_join(type_lookup, by = c("nk", "inspection_date", "score")) |>
  mutate(inspection_type = coalesce(inspection_type, t_type),
         program_name    = coalesce(program_name, t_prog)) |>
  select(-nk, -t_type, -t_prog)

message(sprintf("Annotated %d Socrata rows with an inspection type from the portal (%d still unknown).",
                before_na - sum(is.na(out$inspection_type)), sum(is.na(out$inspection_type))))

stopifnot("annotation must not change the row count" = nrow(out) == nrow(merged))

stopifnot("every row needs a facility_id" = !any(is.na(out$facility_id)),
          "every row needs a name"        = !any(is.na(out$restaurant_name)))

write_csv(out, OUT_PATH)

# ---- report ---------------------------------------------------------------------

message("")
message(sprintf("=== merged: %d inspections across %d facilities, %s .. %s ===",
                nrow(out), n_distinct(out$facility_id),
                format(min(out$inspection_date)), format(max(out$inspection_date))))
message("by source:")
print(as.data.frame(out |> count(source, name = "rows")), right = FALSE)
message("by purpose:")
print(as.data.frame(out |> count(purpose, name = "rows") |> arrange(desc(rows))), right = FALSE)

gained <- nrow(out) - nrow(soc_rows)
message(sprintf("\nNet new inspections vs the frozen export: %+d", gained))
message(sprintf("Newest inspection: %s (export stopped at %s -- %d days recovered)",
                format(max(out$inspection_date)), format(max(soc$inspection_date)),
                as.integer(max(out$inspection_date) - max(soc$inspection_date))))

new_fac <- setdiff(out$facility_id, soc$facility_id)
message(sprintf("Facilities new since the export: %d", length(new_fac)))

# The geocode cache must not be invalidated: an established facility's key has to be
# byte-identical to what it was, or 99.9% coverage silently becomes a re-geocode run.
cache <- read_csv("data/geocoded_cache.csv",
                  col_types = cols(zip5 = col_character(), .default = col_guess()))
keys <- out |> distinct(facility_id, street, city, zip5) |>
  mutate(q = str_squish(str_c(normalize_street(street), city, "TX", zip5, sep = ", ")))
old_fac_keys <- keys |> filter(!facility_id %in% new_fac)
message(sprintf("Geocode keys for established facilities already cached: %d of %d (%.1f%%)",
                sum(old_fac_keys$q %in% cache$query), nrow(old_fac_keys),
                100 * mean(old_fac_keys$q %in% cache$query)))
message(sprintf("  new facilities needing a geocode: %d", length(new_fac)))
message(sprintf("\nWrote %s", OUT_PATH))
