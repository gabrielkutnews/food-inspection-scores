# Geocoding: recover the addresses the first pass missed.
#
# The original run hardcoded "Austin" as the city for every address, so a Lakeway
# address was submitted as "2000 Medical Dr Lakeway, Austin, TX, 78734" and the
# Census matcher saw a contradiction. That plus unit numbers, ALL-CAPS entries
# and highway notation left 1,207 of 6,518 facilities (18.5%) off the map, and
# the failures were systematic: 87% of SVRD addresses, 70% of ALL-CAPS, 57% of
# highway addresses, 33% of those carrying a unit number.
#
# This script rebuilds the query with the real city and a normalised street, then
# geocodes ONLY what is still missing. Results are cached to
# data/geocoded_cache.csv and appended as each pass finishes, so a failed run
# never loses work and reruns are nearly free.
#
# Network required. Free, no API key.

source("R/common.R")
suppressPackageStartupMessages(library(tidygeocoder))

CACHE_PATH  <- "data/geocoded_cache.csv"
LEGACY_PATH <- "data/geocoded_addresses.csv"

# ArcGIS is keyless and matches many of the highway/service-road addresses the
# Census geocoder rejects, but it is one-at-a-time and slow (~1/sec). Set FALSE
# to skip the second pass.
USE_ARCGIS_FALLBACK <- TRUE

cache_cols <- c("query", "street_norm", "city", "zip5", "latitude", "longitude", "source")

# zip5 must stay character -- read_csv otherwise guesses double and the cache
# fails to bind against freshly built rows.
CACHE_TYPES <- cols(
  query = col_character(), street_norm = col_character(), city = col_character(),
  zip5 = col_character(), latitude = col_double(), longitude = col_double(),
  source = col_character()
)

message("Loading inspections...")
ins <- load_inspections()

# Key built by geocode_query() in R/common.R, never inlined here -- the cache is joined on
# this string, so a second copy of the expression that drifts makes every lookup miss.
targets <- ins |>
  mutate(street_norm = normalize_street(street),
         query       = geocode_query(street, city, zip5, address)) |>
  filter(!is.na(street_norm), street_norm != "") |>
  distinct(query, street_norm, city, zip5)

message(sprintf("%d distinct normalised addresses to resolve.", nrow(targets)))

# ---- Cache -------------------------------------------------------------------

cache <- if (file.exists(CACHE_PATH)) {
  # Dedupe on read: `query` is the cache key and rows_update below requires it to
  # be unique, but successive runs can otherwise append the same address twice.
  read_csv(CACHE_PATH, col_types = CACHE_TYPES) |>
    arrange(is.na(latitude)) |>          # keep a resolved row over an unresolved one
    distinct(query, .keep_all = TRUE)
} else {
  tibble(query = character(), street_norm = character(), city = character(),
         zip5 = character(), latitude = double(), longitude = double(),
         source = character())
}

# On first run, seed from the previous geocoding pass. Its key was built as
# str_c(address, "Austin", "TX", zip_5) using the full (city-suffixed) address,
# so rebuild that exact string to join on. ~4,400 addresses come back for free.
if (nrow(cache) == 0 && file.exists(LEGACY_PATH)) {
  message("Seeding cache from the previous geocoding pass...")
  legacy <- read_csv(LEGACY_PATH, show_col_types = FALSE) |>
    select(full_address, latitude, longitude) |>
    filter(!is.na(latitude), !is.na(longitude)) |>
    distinct(full_address, .keep_all = TRUE)

  seeded <- ins |>
    mutate(street_norm  = normalize_street(street),
           query        = geocode_query(street, city, zip5, address),
           legacy_key   = str_squish(str_c(address, "Austin", "TX", zip5, sep = ", "))) |>
    distinct(query, street_norm, city, zip5, legacy_key) |>
    inner_join(legacy, by = c("legacy_key" = "full_address")) |>
    mutate(source = "prior") |>
    distinct(query, .keep_all = TRUE) |>
    select(all_of(cache_cols))

  cache <- seeded
  write_csv(cache, CACHE_PATH)
  message(sprintf("  seeded %d addresses.", nrow(cache)))
}

resolved <- function(cache) cache |> filter(!is.na(latitude)) |> pull(query)

report <- function(label) {
  hit <- sum(targets$query %in% resolved(cache))
  message(sprintf("%-22s %d / %d addresses resolved (%.1f%%)",
                  label, hit, nrow(targets), 100 * hit / nrow(targets)))
}
report("after seeding:")

# ---- Pass 1: Census batch ----------------------------------------------------

todo <- targets |> filter(!query %in% cache$query)

if (nrow(todo) > 0) {
  message(sprintf("Census batch geocoding %d addresses...", nrow(todo)))
  got <- geo(
    street     = todo$street_norm,
    city       = todo$city,
    state      = rep("TX", nrow(todo)),   # tidygeocoder requires equal-length components
    postalcode = todo$zip5,
    method     = "census",
    mode       = "batch",
    quiet      = TRUE
  )

  new_rows <- todo |>
    mutate(latitude = got$lat, longitude = got$long,
           source = if_else(is.na(got$lat), NA_character_, "census")) |>
    select(all_of(cache_cols))

  cache <- bind_rows(cache, new_rows) |> distinct(query, .keep_all = TRUE)
  write_csv(cache, CACHE_PATH)
  report("after census:")
} else {
  message("Nothing new for the Census pass.")
}

# ---- Pass 2: ArcGIS for whatever Census could not match ----------------------

if (USE_ARCGIS_FALLBACK) {
  retry <- cache |> filter(is.na(latitude)) |> distinct(query, .keep_all = TRUE)
  if (nrow(retry) > 0) {
    message(sprintf("ArcGIS fallback on %d unmatched addresses (~%d min)...",
                    nrow(retry), ceiling(nrow(retry) / 60)))
    got <- geo(address = retry$query, method = "arcgis", quiet = TRUE)

    cache <- cache |>
      rows_update(
        tibble(query = retry$query, latitude = got$lat, longitude = got$long,
               source = if_else(is.na(got$lat), NA_character_, "arcgis")),
        by = "query", unmatched = "ignore"
      )
    write_csv(cache, CACHE_PATH)
    report("after arcgis:")
  }
}

# ---- Facility-level coverage -------------------------------------------------

coverage <- ins |>
  mutate(street_norm = normalize_street(street),
         query = geocode_query(street, city, zip5, address)) |>
  latest_per_facility() |>
  mutate(mapped = query %in% resolved(cache))

message("")
message(sprintf("Facilities mappable: %d of %d (%.1f%%) -- was 5,304 of 6,511 (81.5%%)",
                sum(coverage$mapped), nrow(coverage), 100 * mean(coverage$mapped)))

still_missing <- coverage |> filter(!mapped) |> count(city, sort = TRUE)
if (nrow(still_missing) > 0) {
  message("Remaining gaps by city:")
  print(as.data.frame(head(still_missing, 10)))
}
