library(tidyverse)
library(janitor)
library(lubridate)
# install.packages("tidygeocoder")
library(tidygeocoder)
library(leaflet)
# install.packages("leaflet.extras")
library(leaflet.extras)
library(htmlwidgets)

# Downloading Data =====

# ins <- read_csv(
#   "https://data.austintexas.gov/api/v3/views/ecmv-9xxi/query.csv?$limit=3000000"
# ) %>%
#   clean_names()

write_csv(ins, "data/ins.csv")

ins <- read_csv("data/ins.csv")

glimpse(ins)

ins <- ins |>
  mutate(
    inspection_date = as_date(inspection_date),
    score_group = case_when(
      score <= 69 ~ "Red",
      score >= 70 & score <= 85 ~ "Yellow",
      score >= 86 & score <= 100 ~ "Green",
      TRUE ~ NA_character_
    ))

glimpse(ins)

# Geocoding ====
# Geocodio FTW? NOPE TINYGEOCODER!!!

geo <- ins |>
  mutate(
    zip_5 = str_extract(as.character(zip_code), "^\\d{5}"),
    full_address = str_squish(str_c(address, "Austin", "TX", zip_5, sep = ", "))
  ) |>
  distinct(full_address, .keep_all = TRUE)

# geo_coded <- geo |>
#   geocode(
#     address = full_address,
#     method = "census",
#     lat = latitude,
#     long = longitude,
#     full_results = TRUE
#   )

# write_csv(geo_coded, "data/geocoded_addresses.csv")

geo_coded <- read_csv("data/geo_coded")

ins_geo <- ins |>
  mutate(
    zip_5 = str_extract(as.character(zip_code), "^\\d{5}"),
    full_address = str_squish(str_c(address, "Austin", "TX", zip_5, sep = ", "))
  ) |>
  left_join(
    geo_coded |>
      select(full_address, latitude, longitude),
    by = "full_address"
  )

# Mapping====

map_data <- ins_geo |>
  mutate(
    inspection_date = as_date(inspection_date),
    updated_at = ymd_hms(updated_at, quiet = TRUE),
    score_group = case_when(
      score <= 69 ~ "Red: 69 and below",
      score >= 70 & score <= 85 ~ "Yellow: 70 to 85",
      score >= 86 & score <= 100 ~ "Green: 86 to 100",
      TRUE ~ "Missing score"
    ),
    score_color = case_when(
      score <= 69 ~ "red",
      score >= 70 & score <= 85 ~ "gold",
      score >= 86 & score <= 100 ~ "green",
      TRUE ~ "gray"
    ),
    popup_text = paste0(
      "<strong>", restaurant_name, "</strong><br>",
      address, "<br>",
      "ZIP: ", zip_code, "<br>",
      "Inspection date: ", inspection_date, "<br>",
      "Score: ", score, "<br>",
      "Category: ", score_group
    )
  ) |>
  arrange(facility_id, desc(inspection_date), desc(updated_at)) |>
  group_by(facility_id) |>
  slice(1) |>
  ungroup() |>
  filter(!is.na(latitude), !is.na(longitude))

restaurant_map <- leaflet(map_data) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 6,
    color = ~score_color,
    fillColor = ~score_color,
    fillOpacity = 0.8,
    stroke = FALSE,
    popup = ~popup_text,
    label = ~restaurant_name,
    group = ~score_group
  ) |>
  addSearchFeatures(
    targetGroups = c(
      "Red: 69 and below",
      "Yellow: 70 to 85",
      "Green: 86 to 100"
    ),
    options = searchFeaturesOptions(
      propertyName = "label",
      textPlaceholder = "Search restaurant...",
      zoom = 17,
      openPopup = TRUE,
      hideMarkerOnCollapse = TRUE
    )
  ) |>
  addLayersControl(
    overlayGroups = c(
      "Red: 69 and below",
      "Yellow: 70 to 85",
      "Green: 86 to 100"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  leaflet::addLegend(
    position = "bottomright",
    colors = c("red", "gold", "green"),
    labels = c("69 and below", "70 to 85", "86 to 100"),
    title = "Inspection Score"
  )

restaurant_map

saveWidget(
  restaurant_map,
  "restaurant_inspection_map.html",
  selfcontained = TRUE
)

