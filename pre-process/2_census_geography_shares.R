# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrcelmer)
library(sf)

un <- Sys.getenv("USERNAME")
process_dir <- file.path("C:/Users",
                         str_to_lower(un),
                         "OneDrive - PSRC/coding/centers-monitoring")

source(file.path(process_dir, "pre-process", "0_general_inputs.R"))

# Spatial Layers ------------------------------------------------------------
print(str_glue("Reading in layers for Places, blockgroups & tracts"))
places <- readRDS(file.path(process_dir, 
                            "data", 
                            "place_shape.rds")) |> 
  st_transform(spn)

blockgrp10 <- st_read_elmergeo("blockgrp2010") |> 
  st_transform(spn) |> 
  select(geoid = "geoid10") |> 
  mutate(census_year = 2010)

blockgrp20 <- st_read_elmergeo("blockgrp2020") |> 
  st_transform(spn) |> select(geoid = "geoid20") |> 
  mutate(census_year = 2020)

tract10 <- st_read_elmergeo("tract2010") |> 
  st_transform(spn) |> 
  select(geoid = "geoid10") |> 
  mutate(census_year = 2010)

tract20 <- st_read_elmergeo("tract2020") |> 
  st_transform(spn) |>
  select(geoid = "geoid20") |> 
  mutate(census_year = 2020)

# Census Geographies with Places ------------------------------------------
print(str_glue("Intersect Places with blockgroups"))
t10 <- suppressWarnings(st_intersection(blockgrp10, places)) |> 
  st_drop_geometry() |> 
  select("census_year", "geoid", 
         "place_id", "place_name", 
         "place_category", "place_type") |> 
  distinct()

t20 <- suppressWarnings(st_intersection(blockgrp20, places)) |> 
  st_drop_geometry() |> 
  select("census_year", "geoid", 
         "place_id", "place_name", 
         "place_category", "place_type") |> 
  distinct()

blockgrp_places <- bind_rows(t10, t20) |> 
  as_tibble()

print(str_glue("Intersect Places with tracts"))
t10 <- suppressWarnings(st_intersection(tract10, places)) |> 
  st_drop_geometry() |> 
  select("census_year", "geoid", 
         "place_id", "place_name", 
         "place_category", "place_type") |> 
  distinct()

t20 <- suppressWarnings(st_intersection(tract20, places)) |> 
  st_drop_geometry() |> 
  select("census_year", "geoid", 
         "place_id", "place_name", 
         "place_category", "place_type") |>
  distinct()

tract_places <- bind_rows(t10, t20) |> as_tibble()

rm(t10, t20)

# Parcel Data ---------------------------------------------------------------
parcel_data <- NULL
for (y in census_years) {
  t <- get_parcel_data(yrs = y)
  if (is.null(parcel_data)) {parcel_data <- t} 
  else {parcel_data <- bind_rows(parcel_data, t)}
  rm(t)
}

# Splits for Places --------------------------
place_bg_splits <- generate_census_splits(census_geog = "block_group", 
                                          place_shape = places, 
                                          census_df = blockgrp_places)

place_tr_splits <- generate_census_splits(census_geog = "tract", 
                                          place_shape = places, 
                                          census_df = tract_places)

# Final Output ------------------------------------------------------------
bg_splits <- place_bg_splits |>
  filter(percent_of_total_pop>0 | 
           percent_of_household_pop>0 | 
           percent_of_housing_units>0 | 
           percent_of_occupied_housing_units>0)

tr_splits <- place_tr_splits |>
  filter(percent_of_total_pop>0 | 
           percent_of_household_pop>0 | 
           percent_of_housing_units>0 | 
           percent_of_occupied_housing_units>0)

saveRDS(bg_splits, file.path(process_dir, 
                             "data", 
                             "blockgroup_splits.rds"))

saveRDS(tr_splits, file.path(process_dir, 
                             "data", 
                             "tract_splits.rds"))
