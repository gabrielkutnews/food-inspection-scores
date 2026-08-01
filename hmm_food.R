# Austin-area food inspection scores -- raw data acquisition.
#
# This script does one job: pull the City of Austin inspection export and cache
# it to data/ins.csv. Everything downstream lives in R/:
#
#   R/common.R    shared helpers (city, category, buckets, dedupe)
#   R/geocode.R   address normalisation + Census geocoding  (slow, network)
#   R/prep.R      builds docs/data/points.json for the map
#   R/rankings.R  builds docs/rankings.html and the ranking CSVs
#
# Run order:  hmm_food.R -> R/geocode.R -> R/prep.R -> R/rankings.R

library(tidyverse)
library(janitor)

# Downloading Data =====
# Uncomment to refresh. ~21k rows, takes a minute. The write belongs inside this
# block -- it used to sit outside it and ran before `ins` existed, which only
# ever worked because a stale .RData was holding the object in memory.

# ins <- read_csv(
#   "https://data.austintexas.gov/api/v3/views/ecmv-9xxi/query.csv?$limit=3000000"
# ) %>%
#   clean_names()
#
# write_csv(ins, "data/ins.csv")

ins <- read_csv("data/ins.csv", show_col_types = FALSE)

glimpse(ins)

# Note on the columns: `created_at` and `updated_at` are a single constant value
# across all rows -- they are bulk-export stamps, not per-record timestamps, so
# they cannot be used to break ties between inspections. R/common.R breaks
# same-date ties on the lower score instead.
