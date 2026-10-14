library(tidyverse)
library(psrcelmer)
library(sf)

un <- Sys.getenv("USERNAME")
process_dir <- file.path("C:/Users",
                         str_to_lower(un),
                         "OneDrive - PSRC/coding/centers-monitoring")

source(file.path(process_dir, "pre-process", "0_general_inputs.R"))

# Place Shapefiles -------------------------------------------------------
place_shape <- readRDS(file.path(process_dir, 
                                 "data", 
                                 "place_shape.rds")) |> 
  st_transform(spn)

all_names <- c(unique(place_shape$place_id))

rgc_names <- place_shape |>
  st_drop_geometry() |>
  filter(place_type == rgc_title) |>
  select("place_id") |>
  pull() |>
  unique()

mic_names <- place_shape |>
  st_drop_geometry() |>
  filter(place_type == mic_title) |>
  select("place_id") |>
  pull() |>
  unique()

# Levels for Factors ------------------------------------------------------
county_order <- c("Region-Region", "King-County", 
                  "Kitsap-County", "Pierce-County", 
                  "Snohomish-County")

place_order <- c(county_order, 
                 paste0("AllRGCs-", 
                        str_replace_all(rgc_title, " ", "")),
                 rgc_names,
                 paste0("AllMICs-", 
                        str_replace_all(mic_title, " ", "")),
                 mic_names)

grouping_order <- c("Population", "Households", "Housing Units")

# Parcelized Population data from Elmer -----------------------------------
population_data <- NULL
for (y in ofm_years) {
  
  # Parcel population
  print(str_glue(
    "Loading {y} OFM based parcelized estimates of total population")
    )
  
  if (y >= 2020) {
    ofm_vintage <- ofm_postcensal_year
  } else {
    ofm_vintage <- ofm_intercensal_year
  }
  
  q <- paste0("SELECT parcel_dim_id, estimate_year, total_pop, housing_units, occupied_housing_units from ofm.parcelized_saep_facts WHERE ofm_vintage = ", ofm_vintage, " AND estimate_year = ", y, "")
  p <- get_query(sql = q)
  
  # Parcel Dimensions
  if (y >= 2020) {
    parcel_yr <- parcel_postcensal_year
  } else {
    parcel_yr <- parcel_intercensal_year
  }
  
  print(str_glue("Loading {parcel_yr} parcel dimensions from Elmer"))
  q <- paste0("SELECT parcel_dim_id, parcel_id, county_name, x_coord_state_plane, y_coord_state_plane from small_areas.parcel_dim WHERE base_year = ", parcel_yr, " ")
  d <- get_query(sql = q) 
  
  # Add Dimensions to Parcels
  p <- left_join(p, d, by="parcel_dim_id")
  
  if (is.null(population_data)) 
  {population_data <- p} 
  else 
      {population_data <- bind_rows(population_data, p)}
  
  rm(q, p, d)
  
}

# Population data by County --------------------------------------------------
county_data <- population_data |>
  select(year = "estimate_year", place_name = "county_name", 
         population = "total_pop", "housing_units", 
         households = "occupied_housing_units") |>
  mutate(place_category = "County", place_type = "County") |>
  mutate(place_id = paste(str_replace_all(place_name, " ", ""), 
                          str_replace_all(place_category, " ", ""), 
                          sep = "-")
         ) |>
  group_by(year, place_id, place_name, place_category, place_type) |>
  summarise(Population = round(sum(population),0), 
            `Housing Units` = round(sum(housing_units),0), 
            Households = round(sum(households),0)) |>
  as_tibble() |>
  pivot_longer(!c(year, place_id, place_name, place_category, place_type), 
               names_to = "grouping", 
               values_to = "estimate") |>
  mutate(concept = "People, Housing and Households", share = 0) |>
  as_tibble() 

region_data <- county_data |>
  group_by(year, grouping, concept) |>
  summarise(estimate = sum(estimate), share = sum(share)) |>
  as_tibble() |>
  mutate(place_type = "Region", 
         place_name = "Region", 
         place_category = "Region") |>
  mutate(place_id = paste(str_replace_all(place_name, " ", ""), 
                          str_replace_all(place_category, " ", ""), 
                          sep = "-")
         ) 

county_data <- bind_rows(county_data, region_data) |>
  mutate(grouping = factor(grouping, levels = grouping_order)) |>
  mutate(place_id = factor(place_id, levels = county_order)) |>
  arrange(year, place_id, grouping)

rm(region_data)

# Population data by Place -----------------------------------------------
print(str_glue("Creating a parcel layer to intersection with layer boundries"))
p_lyr <- population_data |> 
  select(year = "estimate_year", "parcel_id", 
         x = "x_coord_state_plane", y = "y_coord_state_plane", 
         population = "total_pop", "housing_units", 
         households = "occupied_housing_units")

p_lyr <- st_as_sf(p_lyr, coords = c("x", "y"), crs = spn)

place_data <- NULL
for (place in all_names) {
  print(str_glue("Sumarzing parcel population and housing data for {place}."))
  
  # Parcels that intersect place
  s_lyr <- place_shape |> 
    filter(place_id == place)
  
  m_lyr <- suppressWarnings(st_intersection(p_lyr, s_lyr)) |> 
    st_drop_geometry() |>
    group_by(year, place_id, place_name, place_category, place_type) |>
    summarise(Population = round(sum(population),0), 
              `Housing Units` = round(sum(housing_units),0), 
              `Households` = round(sum(households),0)) |>
    as_tibble() |>
    pivot_longer(!c(year, place_id, place_name, place_category, place_type), 
                 names_to = "grouping", 
                 values_to = "estimate") |>
    mutate(concept = "People, Housing and Households", share = 0) |>
    as_tibble() 
  
  
  if (is.null(place_data)) 
  {place_data <- m_lyr} else 
      {place_data <- bind_rows(place_data, m_lyr)}
  rm(m_lyr, s_lyr)
}

# Final Cleanup -----------------------------------------------------------
a <- place_data |>
  group_by(year, place_type, grouping, concept) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(place_name = case_when(
    place_type == rgc_title ~ "All RGCs",
    place_type == mic_title ~ "All MICs"),
    place_id = paste(str_replace_all(place_name, " ", ""), 
                     str_replace_all(place_type, " ", ""), 
                     sep = "-"),
    place_category = case_when(
      place_type == rgc_title ~ "All RGCs",
      place_type == mic_title ~ "All MICs")
  ) |>
  mutate(share = 1)

combined_data <- bind_rows(county_data, place_data, a)

combined_data <- combined_data |> 
  mutate(year = as.character(year)) |>
  mutate(share = 1) |>
  mutate(place_id = factor(place_id, levels = place_order)) |> 
  mutate(grouping = factor(grouping, levels = grouping_order)) |>
  arrange(year, place_id, grouping)

# Manual Overrides --------------------------------------------------------
manual_data_overrides <- read_csv(
  "data/manual_centers_population_housing_data.csv", 
  show_col_types = FALSE) |>
  mutate(year = as.character(year)) |>
  select("place_id", "year", `Population` = "manual_population", `Housing Units` = "manual_housing_units") |>
  pivot_longer(!c(place_id, year), names_to = "grouping", values_to = "manual_estimate") |>
  mutate(manual_override = 1)

print(str_glue("Combining Data and overwriting data with manual updates"))
final_data <- left_join(combined_data, 
               manual_data_overrides, 
               by=c("place_id","year", "grouping")) |>
  mutate(estimate = case_when(
    !(is.na(manual_override)) ~ manual_estimate,
    (is.na(manual_override)) ~ estimate)) |>
  filter(grouping != "Households") |>
  select(-"manual_estimate", -"manual_override")

saveRDS(final_data, file.path(process_dir, "data", "population-housing.rds"))
