# Shared helpers for the Austin-area food inspection pipeline.
# Sourced by R/geocode.R, R/prep.R and R/rankings.R.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(lubridate)
})

# Analysis window. Inspections older than this are dropped from the rankings and
# hidden by default on the map: 301 facilities were last inspected in 2023 and
# 590 in 2024, and presenting those as current would be wrong.
RECENCY_MONTHS <- 18

# Score buckets. Reds are only ~0.2% of the map, so the UI has to show counts.
BUCKETS <- tibble::tibble(
  key   = c("red", "orange", "yellow", "green"),
  label = c("69 & below", "70–79", "80–89", "90–100"),
  color = c("#d62828", "#f77f00", "#ecc30b", "#2a9d3f"),
  lo    = c(0L, 70L, 80L, 90L),
  hi    = c(69L, 79L, 89L, 100L)
)

# Cities that appear at the end of the `address` field, longest first so that
# "Bee Caves" wins over "Bee Cave" and "West Lake Hills" is not read as "Hills".
CITIES <- c(
  "West Lake Hills", "Dripping Springs", "Travis County", "Marble Falls",
  "Sunset Valley", "Pflugerville", "Rollingwood", "Cedar Park", "Round Rock",
  "Briarcliff", "Creedmoor", "Bee Caves", "Spicewood", "Manchaca", "Del Valle",
  "Bee Cave", "Volente", "Lakeway", "Elgin", "Hutto", "Manor", "Austin"
)
CITY_RE <- paste0("(", paste(CITIES, collapse = "|"), ")")

# Prefixes the city stamps onto `restaurant_name`. Used only when the address
# tail has no city. OOB is an administrative flag, not a jurisdiction.
NAME_PREFIX_CITY <- c(
  PF = "Pflugerville", LW = "Lakeway", BC = "Bee Cave", MN = "Manor",
  WL = "West Lake Hills", SV = "Sunset Valley", RW = "Rollingwood",
  ABIA = "Austin", COTA = "Austin"
)

# Names carrying an administrative flag rather than a normal establishment name.
ADMIN_FLAG_RE <- regex("^OOB\\b|INELIGIBLE", ignore_case = TRUE)

score_bucket <- function(score) {
  cut(score,
      breaks = c(-Inf, 69, 79, 89, Inf),
      labels = BUCKETS$key,
      right  = TRUE) |> as.character()
}

derive_city <- function(address, restaurant_name) {
  addr <- str_squish(address)
  city <- str_to_title(str_extract(addr, regex(paste0(CITY_RE, "$"), ignore_case = TRUE)))
  city[!is.na(city) & city == "Bee Caves"] <- "Bee Cave"

  pfx <- str_extract(restaurant_name, "^[A-Z]{2,4}(?= - )")
  coalesce(city, unname(NAME_PREFIX_CITY[pfx]), "Austin")
}

# The street portion, with the trailing city token removed (it is carried in its
# own column and re-attached at geocode time).
street_only <- function(address) {
  a <- str_squish(address)
  # A few rows arrive fully comma-formatted, sometimes with the bare city token
  # appended on top of it: "106 CANYON RIDGE, AUSTIN, TX, 78753 Austin".
  a <- str_remove(a, regex(",\\s*[A-Za-z .]+,\\s*TX,?\\s*\\d{5}(-\\d{4})?(\\s+[A-Za-z .]+)?$",
                           ignore_case = TRUE))
  # Only one city-strip pass -- a second would turn "1234 Oak Manor" into "1234 Oak".
  str_squish(str_remove(a, regex(paste0("[,\\s]+", CITY_RE, "$"), ignore_case = TRUE)))
}

# Tidy a street address for display. Unlike normalize_street() this KEEPS suite
# and unit numbers -- a reader needs "Ste 900" to find the place -- and only
# removes the "Bunit" token, which is an export artifact rather than part of any
# real address, plus the orphaned "#" it leaves behind.
# Abbreviations that must stay upper-case when de-shouting an address.
KEEP_UPPER <- c("NB", "SB", "EB", "WB", "NE", "NW", "SE", "SW",
                "IH", "FM", "SH", "US", "RR", "RM", "CR", "TX")

display_street <- function(street) {
  a <- str_squish(street)

  # "Bunit 300" is how a unit number arrives in this export. Keep the number,
  # drop the stray B; a trailing "Bunit" with no number is pure noise.
  a <- str_replace_all(a, regex("#?\\s*\\bBunit\\b", ignore_case = TRUE), " Unit")
  a <- str_replace_all(a, regex("\\bUnit\\s*#\\s*", ignore_case = TRUE), "Unit ")
  a <- str_remove(a, regex("\\s*#?\\s*\\bUnit\\s*$", ignore_case = TRUE))
  a <- str_replace_all(a, "#\\s*#", "#")
  a <- str_remove(a, "\\s*#\\s*$")

  # De-shout each run of capitals rather than testing the whole string: the feed
  # mixes cases within one address ("11150 RESEARCH SB BLVD Unit 210A"), so an
  # all-or-nothing ALL-CAPS test leaves those untouched. Matching letter runs
  # (not whitespace tokens) means adjacent punctuation does not defeat it.
  a <- str_replace_all(a, "[A-Z]{2,}", function(m) {
    ifelse(m %in% KEEP_UPPER, m, str_to_title(m))
  })

  a <- str_replace_all(a, regex("\\bSvrd\\b", ignore_case = TRUE), "Service Rd")

  # 90 addresses arrive already mixed-case from the city ("3811 N Ih 35",
  # "5204 Fm 2222", "11920 e Us 290 Hwy Wb"), which the de-shouting above leaves
  # alone. Upper-case road designators only when a number follows: "Rm" and "Cr"
  # are deliberately excluded because one address is genuinely "Ste A Rm 1154.2".
  a <- str_replace_all(a, regex("\\b(IH|FM|SH|US|RR)(?=\\s*\\d)", ignore_case = TRUE), toupper)
  a <- str_replace_all(a, regex("\\b(NB|SB|EB|WB)\\b", ignore_case = TRUE), toupper)
  a <- str_replace_all(a, "\\b([nsew])\\b", toupper)

  str_squish(a)
}

# Rewrite a street address into something the Census geocoder can match. The
# 1,207 unmapped facilities are not random: 87% of SVRD addresses, 70% of
# ALL-CAPS ones, 57% of highway addresses and 33% of those containing a unit
# number failed. Each of those is addressed below.
normalize_street <- function(street) {
  a <- str_squish(street)

  # ALL-CAPS entries fail at 70%; title-case them.
  caps <- !is.na(a) & !str_detect(a, "[a-z]")
  a[caps] <- str_to_title(a[caps])

  # Secondary address lines. "Bunit" is how a "B unit" suffix arrives in this
  # export; strip it and the conventional forms alike.
  a <- str_remove(a, regex("\\s+B?unit\\b.*$", ignore_case = TRUE))
  a <- str_remove(a, regex("\\s+(Ste|Suite|Unit|Bldg|Building|Apt|Rm|Room|Fl|Floor|Lot|Space|Spc)\\.?\\s*[A-Za-z0-9&/-]*\\.?$",
                           ignore_case = TRUE))
  a <- str_remove(a, "\\s+#\\s*[A-Za-z0-9&/-]+$")

  # Highway notation.
  a <- str_replace_all(a, regex("\\bSVRD\\b", ignore_case = TRUE), "Service Rd")
  a <- str_remove_all(a, regex("\\s+\\b(NB|SB|EB|WB)\\b", ignore_case = TRUE))
  a <- str_replace_all(a, regex("\\bIH\\s*-?\\s*(\\d+)", ignore_case = TRUE), "I-\\1")
  a <- str_replace_all(a, regex("\\bHwy\\b", ignore_case = TRUE), "Highway")
  a <- str_replace_all(a, regex("\\bFM\\s*-?\\s*(\\d+)", ignore_case = TRUE), "FM \\1")

  str_squish(a)
}

# Coarse facility type from the establishment name.
#
# The city publishes no type field, so this is inferred, and "Restaurant & Food
# Service" is the RESIDUAL bucket -- anything matching no pattern below lands
# there. That makes the patterns load-bearing for the rankings: an audit of the
# top 40 candidates found roughly half were not restaurants (a daycare, a candy
# shop, a coffee wholesaler, Apple's staff canteens, sorority kitchens, a
# high-school food pod, a hospital gift shop). R/rankings.R prints the top 20
# with their category so new leakers surface before they reach print.
#
# Chains and coffee shops DO count as restaurants; wholesalers, retailers that
# happen to hold a food permit, and staff-only dining do not.

# Checked BEFORE the school branch. "Dell Children's Medical Center" and "Texas
# Children's Hospital" are hospitals, so the children-related school patterns
# below would otherwise claim them. Reordering the whole Healthcare branch up here
# is not an option either -- its "care cent" would swallow "Child Care Center".
RE_MEDICAL_STRONG <- paste(
  "hospital", "medical cent", "\\bclinic\\b", "healthcare", "\\bseton\\b",
  "dell children", sep = "|")

RE_SCHOOL <- paste(
  "school", "elementary", "middle sch", "high sch", "\\bh s$", "\\bisd\\b",
  "academy", "montessori", "childcare", "child care", "early childhood",
  "kinder", "daycare", "day care", "learning cent", "learning experience",
  "early learning", "preschool", "pre-school", "prep school", "college prep",
  "head start", "nursery", "goddard", "primrose", "child development",
  "children'?s cent", "children'?s courtyard", "\\bcdc\\b",
  # charter networks whose names never contain the word "school"
  "\\bkipp\\b", "^idea ", "idea public", "harmony science", "\\bvalor\\b",
  "\\bbasis\\b",
  "university", "college", "dining hall", sep = "|")

RE_HEALTH <- paste(
  "nursing", "rehab", "assisted living", "senior living", "memory care",
  "memory support", "hospital", "clinic", "medical cent", "health cent",
  "hospice", "care cent", "\\btreatment\\b", "mother teresa", "marbridge",
  sep = "|")

# Staff-only dining, contract feeders, and production/wholesale sites. "Caffe
# Mac(s)" and "Parmer Lane N ..." are Apple's campus canteens; a sorority
# kitchen and a school food pod are not places a reader can eat either.
RE_INSTITUTIONAL <- paste(
  "aramark", "sodexo", "compass group", "canteen", "cafeteria",
  "employee dining", "corporate dining", "break room", "micro market",
  "commissary", "catering", "caffe mac", "^parmer lane \\d", "@ samsung",
  "sorority", "fraternity", "delta gamma", "\\bpod$", "prep kitchen",
  "importing", "wholesale", "distribut", "roasting",
  # residential, custodial and social-service kitchens. Note the patterns are
  # specific: a bare "county" would catch County Line, a real BBQ restaurant.
  "\\bshelter\\b", "settlement home", "home for children",
  "\\bjail\\b", "correctional", "gardner betts", sep = "|")

RE_CONVENIENCE <- paste(
  "7-eleven", "7 eleven", "foods? mart", "\\bmart\\b", "xpress", "\\bcvs\\b",
  "walgreen", "\\bpharmacy\\b", "dollar gen", "dollar tree", "family dollar",
  "circle k", "\\bexxon\\b", "\\bshell\\b", "\\bvalero\\b", "chevron",
  "conoco", "texaco", "\\bquik", "corner store", "convenience",
  "gas station", sep = "|")

# Note the optional apostrophe on randall's -- the plain "randalls" spelling
# never matched the data, so two supermarket Starbucks kiosks were escaping
# into the restaurant pool.
RE_GROCERY <- paste(
  "h-e-b", "\\bheb\\b", "randall'?s", "wal-?mart", "\\btarget\\b", "costco",
  "sam'?s club", "sprouts", "whole foods", "trader joe", "fiesta mart",
  "supermercado", "supermarket", "grocery", "food market", "central market",
  "world market", sep = "|")

# Stadium, arena and airport concession counters. Kept out of "Restaurant"
# because a single venue can run dozens of separately-licensed stands that are
# all inspected on the same day with the same score -- nine Q2 Stadium counters
# would otherwise take nine of ten slots in a restaurant ranking.
RE_VENUE <- paste(
  "^levy ", "levy at", "q2 stadium", "^cota", "^abia", "circuit of the am",
  "moody cent", "erwin cent", "\\bstadium\\b", "\\barena\\b", "amphitheat",
  "concession", "suite pantry", "club level", "hawking", "grand stand",
  "campsite", "waterpark", "water park", "quarries", sep = "|")

# Shops that hold a food permit but are not somewhere you eat.
RE_RETAIL <- paste(
  "candy", "\\bgift", "general store", "oils? & vinegar", "total wine",
  "wine & spirits", "\\bliquor\\b", "serasana", "\\bfeed\\b", "florist",
  sep = "|")

# Names no pattern can reach, mapped to the bucket they belong in.
# "Growing Imaginations L.C." is a learning centre, but "L.C." cannot be matched
# generically without also catching every "L.L.C."; "Westgate II" is a building
# name with no food signal at all.
# Audited 2026-08-03 -- re-check the audit print after any data refresh.
NAME_OVERRIDES <- c(
  "Growing Imaginations L.C." = "School & Childcare",
  "Westgate II"               = "Retail & Specialty"
)

categorize <- function(nm) {
  n <- str_to_lower(nm)
  case_when(
    nm %in% names(NAME_OVERRIDES)       ~ unname(NAME_OVERRIDES[nm]),
    str_detect(n, RE_MEDICAL_STRONG)    ~ "Healthcare",
    str_detect(n, RE_SCHOOL)            ~ "School & Childcare",
    str_detect(n, RE_HEALTH)            ~ "Healthcare",
    str_detect(n, RE_INSTITUTIONAL)     ~ "Institutional & Catering",
    str_detect(n, RE_CONVENIENCE)       ~ "Convenience & Fuel",
    str_detect(n, RE_GROCERY)           ~ "Grocery",
    str_detect(n, RE_VENUE)             ~ "Venue & Concessions",
    str_detect(n, RE_RETAIL)            ~ "Retail & Specialty",
    TRUE                                ~ "Restaurant & Food Service"
  )
}

CATEGORIES <- c("Restaurant & Food Service", "School & Childcare",
                "Convenience & Fuel", "Grocery", "Healthcare",
                "Venue & Concessions", "Institutional & Catering",
                "Retail & Specialty")

# Multi-unit operators license each counter separately ("Levy at Q2
# Stadium/South Main 103"). Collapse to the parent so one operator cannot
# occupy most of a ten-row table.
venue_of <- function(nm) {
  v <- str_squish(str_remove(nm, "/.*$"))
  v <- str_remove(v, regex("\\s+(#|No\\.?|Unit|Stand|Store)\\s*\\d+\\s*$", ignore_case = TRUE))
  str_remove(v, "\\s+\\d+\\s*$")
}

# Load the raw inspection export with consistent typing. `created_at` and
# `updated_at` are dropped: both are a single constant across all 20,964 rows
# (bulk-export stamps), so the original desc(updated_at) tiebreak did nothing.
load_inspections <- function(path = "data/ins.csv") {
  read_csv(path, show_col_types = FALSE) |>
    transmute(
      facility_id,
      restaurant_name = str_squish(restaurant_name),
      address         = str_squish(address),
      zip5            = str_extract(as.character(zip_code), "^\\d{5}"),
      inspection_date = as_date(inspection_date),
      score,
      is_followup     = process_description == "Follow-Up Inspection"
    ) |>
    mutate(
      city   = derive_city(address, restaurant_name),
      street = street_only(address)
    )
}

# One row per facility: its most recent inspection. Same-date ties resolve to the
# LOWEST score -- six facilities have multiple rows on their latest date and four
# of those disagree, including one holding both a 100 and an 87 on 2026-03-03.
# Row order is not a defensible tiebreak for a food-safety map.
latest_per_facility <- function(ins) {
  ins |>
    filter(!is.na(score)) |>
    arrange(facility_id, desc(inspection_date), score) |>
    group_by(facility_id) |>
    slice(1) |>
    ungroup()
}

same_date_conflicts <- function(ins) {
  ins |>
    filter(!is.na(score)) |>
    group_by(facility_id) |>
    filter(inspection_date == max(inspection_date)) |>
    filter(n() > 1) |>
    summarise(restaurant_name = first(restaurant_name),
              inspection_date = first(inspection_date),
              scores = paste(sort(score), collapse = " / "),
              disagree = n_distinct(score) > 1,
              .groups = "drop")
}
