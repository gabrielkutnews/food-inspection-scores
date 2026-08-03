# Raw data acquisition -- pulls the City of Austin inspection export to
# data/ins.csv. Uncomment the block below to refresh; ~21k rows, takes a minute.
#
# Run order:  R/download.R -> R/geocode.R -> R/prep.R -> R/rankings.R
#
# hmm_food.R is the original exploratory script and is not part of this pipeline.

library(tidyverse)
library(janitor)

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
