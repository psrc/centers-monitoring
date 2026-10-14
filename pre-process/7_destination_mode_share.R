# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrcelmer)
library(sf)

un <- Sys.getenv("USERNAME")
process_dir <- file.path("C:/Users",
                         str_to_lower(un),
                         "OneDrive - PSRC/coding/centers-monitoring")

source(file.path(process_dir, 
                 "pre-process", 
                 "0_general_inputs.R"))

base_model_yr <- 2023
centers_dest_mod_file <- "tour_centers_dest.csv"

# Place Shapefiles -------------------------------------------------------
place_shape <- readRDS(file.path(process_dir, 
                                 "data", 
                                 "place_shape.rds")) |> 
  st_transform(spn)

all_names <- c(unique(place_shape$place_id))

rgc_name <- unique(place_shape |> 
                     st_drop_geometry() |> 
                     filter(place_type == rgc_title) |> 
                     select("place_id") |> 
                     pull()
                   )

mic_name <- unique(place_shape |> 
                     st_drop_geometry() |> 
                     filter(place_type == mic_title) |> 
                     select("place_id") |> pull()
                   )

county_order <- c("Region-Region", "King-County", 
                  "Kitsap-County", "Pierce-County", 
                  "Snohomish-County")

place_order <- c(county_order, "AllRGCs-AllRGCs", 
                 rgc_name, "AllMICs-AllMICs", mic_name)

# Levels for Factors ------------------------------------------------------
dest_mode_order <- c("SOV", "non-SOV")

# Data --------------------------------------------------------------------
place_modes <- read_csv(file.path(process_dir, 
                                  "data", 
                                  centers_dest_mod_file)
                        ) |>
  drop_na() |>
  mutate(concept = case_when(
    pdpurp == "Work" ~ "Work",
    TRUE ~ "Non-Work")) |>
  filter(tmodetp != "School Bus") |>
  mutate(grouping = case_when(
    tmodetp == "SOV" ~ "SOV",
    tmodetp %in% c("HOV2", "HOV3+") ~ "non-SOV",
    tmodetp == "Bike" ~ "non-SOV",
    tmodetp %in% c("Transit") ~ "non-SOV",
    tmodetp == "TNC" ~ "non-SOV",
    tmodetp == "Walk" ~ "non-SOV",
    tmodetp == "Park" ~ "non-SOV")) |>
  mutate(year = as.character(base_model_yr)) |> 
  select(place_name="tour_d_place", 
         "year", "grouping", 
         estimate="toexpfac", 
         "concept") |>
  group_by(place_name, year, grouping, concept) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble()

t <- place_shape |> st_drop_geometry()
place_modes <- left_join(place_modes, t, by="place_name")
rm(t)

# RGC and MIC's
centers <- place_modes |> filter(place_name != "Not in RGC")
totals <- centers |> 
  group_by(place_name, year, concept, place_type) |> 
  summarise(total = sum(estimate)) |> 
  as_tibble()

centers <- left_join(centers, 
                     totals,  
                     by=c("place_name", "year", "concept", "place_type")
                     ) |>
  mutate(share = estimate / total) |>
  select(-"total")

rm(totals)

# Region
region <- place_modes |> 
  group_by(year, grouping, concept) |> 
  summarise(estimate = sum(estimate)) |>
  as_tibble() |> 
  mutate(place_name = "Region", 
         place_type = "Region", 
         place_id = "Region-Region", 
         place_category = "Region")

totals <- region |>  
  group_by(year, concept) |> 
  summarise(total = sum(estimate)) |> 
  as_tibble()

region <- left_join(region, totals,  by=c("year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total")

rm(totals)

# All RGCs
all_rgcs <- place_modes |> 
  filter(place_type == rgc_title & place_name != "Not in RGC") |> 
  group_by(year, grouping, concept) |> 
  summarise(estimate = sum(estimate), acres = sum(acres)) |> 
  as_tibble() |> 
  mutate(place_name = "All RGCs", 
         place_type = rgc_title, 
         place_id = "AllRGCs-AllRGCs", 
         place_category = rgc_title)

totals <- all_rgcs |> 
  group_by(year, concept) |> 
  summarise(total = sum(estimate)) |> 
  as_tibble()

all_rgcs <- left_join(all_rgcs, totals,  by=c("year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total")

rm(totals)

# All MICs
all_mics <- place_modes |> 
  filter(place_type == mic_title & place_name != "Not in RGC") |> 
  group_by(year, grouping, concept) |> 
  summarise(estimate = sum(estimate), acres = sum(acres)) |> 
  as_tibble() |> 
  mutate(place_name = "All MICs", 
         place_type = mic_title, 
         place_id = "AllMICs-AllMICs", 
         place_category = mic_title)

totals <- all_mics |>  
  group_by(year, concept) |> 
  summarise(total = sum(estimate)) |> 
  as_tibble()

all_mics <- left_join(all_mics, totals,  by=c("year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total")

rm(totals)

destination_mode_share <- bind_rows(centers, region, all_rgcs, all_mics)

destination_mode_share <- destination_mode_share |> 
  mutate(place_id = factor(place_id, levels = place_order)) |> 
  arrange(year, place_id, grouping)

rm(dest_mode_order, centers, 
   region, all_rgcs, all_mics, 
   place_modes, place_shape)

saveRDS(destination_mode_share |> 
          filter(concept == "Work"), 
        "data/destination_mode_share.rds")
