# Build the map payload: docs/data/points.json
#
# One row per facility, carrying its most recent inspection. Emitted as an
# array-of-arrays with a `fields` header rather than an array of objects, so the
# key names are not repeated 5,000 times, and with coordinates rounded to five
# decimals (~1.1 m). The result is a fraction of the 1.3 MB self-contained
# widget saveWidget used to produce.
#
# Run after R/geocode.R.

source("R/common.R")
source("R/editorial.R")
suppressPackageStartupMessages(library(jsonlite))

CACHE_PATH <- "data/geocoded_cache.csv"
OUT_PATH   <- "docs/data/points.json"

stopifnot("run R/geocode.R first" = file.exists(CACHE_PATH))
dir.create("docs/data", recursive = TRUE, showWarnings = FALSE)

ins <- load_inspections()

# Inspections on record per facility -- context for the popup, and the basis for
# the consistency rankings.
counts <- ins |>
  filter(!is.na(score)) |>
  count(facility_id, name = "n_inspections")

coords <- read_csv(CACHE_PATH, show_col_types = FALSE) |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  distinct(query, .keep_all = TRUE) |>
  select(query, latitude, longitude)

cutoff <- recency_cutoff(ins)   # anchored to the data, not to today

all_pts <- ins |>
  latest_per_facility() |>
  left_join(counts, by = "facility_id") |>
  mutate(query = str_squish(str_c(normalize_street(street), city, "TX", zip5, sep = ", "))) |>
  left_join(coords, by = "query")

# The recency window is now a HARD FILTER, not a checkbox the reader can switch off.
# An establishment last inspected in 2023 tells a reader nothing about its kitchen today,
# and offering it behind a toggle put the burden of that caveat on them. The map is now
# simply "establishments inspected in the last RECENCY_MONTHS months", stated on the page.
n_ever   <- nrow(all_pts)
pts      <- all_pts |> filter(inspection_date >= cutoff)
n_window <- nrow(pts)

n_all <- n_window
pts   <- pts |> filter(!is.na(latitude), !is.na(longitude))

message(sprintf("Facilities with a scored inspection ever: %d", n_ever))
message(sprintf("  inspected since %s (the published set): %d  -- %d excluded as stale",
                format(cutoff), n_window, n_ever - n_window))
message(sprintf("  of those, mappable: %d (%.1f%%), %d dropped for missing coordinates.",
                nrow(pts), 100 * nrow(pts) / n_window, n_window - nrow(pts)))

# ---- Un-stack co-located facilities ------------------------------------------
# ~2,000 establishments share an exact coordinate with another (42 sit on one pin
# at the airport), which makes them unclickable. Spread each cluster over a
# golden-angle spiral of up to ~45 m -- smaller than the geocoder's own
# interpolation error, since most matches are Non_Exact street interpolations --
# so every facility is reachable and marker counts still match the filter counts.
GOLDEN_ANGLE <- pi * (3 - sqrt(5))

pts <- pts |>
  group_by(latitude, longitude) |>
  mutate(
    stack_n = n(),
    i       = row_number() - 1,
    r_m     = 7 * sqrt(i),
    lat_out = latitude  + (r_m * cos(i * GOLDEN_ANGLE)) / 111320,
    lon_out = longitude + (r_m * sin(i * GOLDEN_ANGLE)) /
                          (111320 * cos(latitude * pi / 180))
  ) |>
  ungroup()

message(sprintf("Co-located: %d facilities share a coordinate with another; largest stack is %d.",
                sum(pts$stack_n > 1), max(pts$stack_n)))

# ---- Assemble ----------------------------------------------------------------

city_levels <- sort(unique(pts$city))

out <- pts |>
  mutate(
    category = categorize(restaurant_name),
    c_idx    = match(city, city_levels) - 1L,
    k_idx    = match(category, CATEGORIES) - 1L,
    bucket   = score_bucket(score)
  ) |>
  arrange(desc(score)) |>   # greens drawn first, so low scores land on top
  transmute(
    n = restaurant_name,
    a = display_street(street),
    c = c_idx,
    z = zip5,
    y = round(lat_out, 5),
    x = round(lon_out, 5),
    s = as.integer(score),
    d = format(inspection_date, "%Y-%m-%d"),
    f = as.integer(is_followup),
    k = k_idx,
    q = as.integer(n_inspections),
    u = as.integer(stack_n - 1)
  )

# Reader-facing copy, filled here and rendered verbatim by docs/index.html. Keeping it in
# R/editorial.R means a wording change never requires editing JavaScript.
COPY_VALS <- list(
  through    = str_squish(format(data_through(ins), "%B %e, %Y")),
  cutoff     = str_squish(format(cutoff, "%B %e, %Y")),
  generated  = str_squish(format(Sys.Date(), "%B %e, %Y")),
  min_top    = "", min_bottom = "",
  n_shown    = format(nrow(out), big.mark = ","),
  n_excluded = format(n_ever - n_window, big.mark = ","),
  n_unmapped = format(n_window - nrow(out), big.mark = ",")
)

payload <- list(
  copy_window_note   = fill(EDITORIAL$map$window_note, COPY_VALS),
  copy_unmapped_note = if (n_window > nrow(out)) fill(EDITORIAL$map$unmapped_note, COPY_VALS) else "",
  copy_credit_long   = fill(EDITORIAL$map$credit_long, COPY_VALS),
  copy_source_line   = fill(EDITORIAL$map$source_line, COPY_VALS),
  # Three distinct dates, never collapsed into one "generated". The page previously
  # said "Data generated <today>" while describing inspections that stopped 73 days
  # earlier, which read as a freshness claim it could not support.
  generated    = format(Sys.Date(), "%Y-%m-%d"),   # when this file was built
  data_through = format(data_through(ins), "%Y-%m-%d"),  # newest inspection in it
  cutoff       = format(cutoff, "%Y-%m-%d"),       # start of the recency window
  recency_months = RECENCY_MONTHS,
  n_ever     = n_ever,      # facilities with any scored inspection on record
  n_total    = n_all,       # of those, inspected inside the window (the published set)
  n_mapped   = nrow(out),   # of those, successfully geocoded
  cities     = city_levels,
  categories = CATEGORIES,
  # As a list of named lists, not a tibble: write_json(dataframe = "values")
  # applies globally and would flatten these into positional arrays.
  buckets    = lapply(seq_len(nrow(BUCKETS)), function(i) as.list(BUCKETS[i, ])),
  fields     = names(out),
  rows       = out
)

write_json(payload, OUT_PATH, dataframe = "values", auto_unbox = TRUE, digits = NA)

message(sprintf("Wrote %s (%.0f KB raw).", OUT_PATH, file.size(OUT_PATH) / 1024))

message("")
message("Bucket counts (all mapped facilities):")
print(table(score_bucket(pts$score))[BUCKETS$key])
message(sprintf("Within the last %d months (map default):", RECENCY_MONTHS))
print(table(score_bucket(pts$score[pts$inspection_date >= cutoff]))[BUCKETS$key])

conf <- same_date_conflicts(ins)
if (nrow(conf) > 0) {
  message("")
  message("Same-date score conflicts -- resolved to the lower score, review before publishing:")
  print(as.data.frame(conf))
}
