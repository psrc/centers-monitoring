# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrcelmer)
library(sf)

un <- Sys.getenv("USERNAME")
process_dir <- file.path("C:/Users",str_to_lower(un),
                         "OneDrive - PSRC/coding/centers-monitoring")

source(file.path(process_dir, "pre-process", "0_general_inputs.R"))

# Place Shapefiles -------------------------------------------------------
place_shape <- readRDS(file.path(process_dir, "data", "place_shape.rds")) |> 
  st_transform(spn)

all_names <- c(unique(place_shape$place_id))

rgc_names <- unique(place_shape |> 
                      st_drop_geometry() |> 
                      filter(place_type == rgc_title) |> 
                      select("place_id") |> 
                      pull()
                    )

mic_names <- unique(place_shape |> 
                      st_drop_geometry() |> 
                      filter(place_type == mic_title) |> 
                      select("place_id") |> 
                      pull()
                    )

county_order <- c("Region-Region", "King-County", 
                  "Kitsap-County", "Pierce-County", 
                  "Snohomish-County")

place_order <- c(county_order, "AllRGCs-AllRGCs", 
                 rgc_names, "AllMICs-AllMICs", mic_names)

land_order <- c("Core Industrial", "Industrial-Commercial", 
                "Aviation Operations", "Military", 
                "Industrial Waterways", "Total Industrial", 
                "Limited Industrial", "Non-Industrial", 
                "Total Gross Acreage")

# Industrial Jobs ---------------------------------------------------------
industrial_jobs <- read_csv("data/industrial_employment.csv", 
                            show_col_types = FALSE) |>
  mutate(year = as.character(year)) |>
  mutate(place_id = factor(place_id, levels = place_order)) |>
  arrange(place_id, grouping, year)

saveRDS(industrial_jobs , 
        file.path(process_dir, "data", "industrial_jobs.rds"))

# Industrial Land ---------------------------------------------------------
industrial_land <- read_csv("data/industrial_land.csv", 
                            show_col_types = FALSE) |>
  mutate(year = as.character(year)) |>
  mutate(place_id = factor(place_id, levels = place_order)) |>
  mutate(grouping = factor(grouping, levels = land_order)) |>
  arrange(place_id, grouping, year) |>
  mutate(estimate = round(estimate, 0))

# Data for IL Pie Chart  
industrial_land_shares <- industrial_land |>
  filter(grouping %in% c("Total Industrial", 
                         "Limited Industrial", 
                         "Non-Industrial*")
         )

saveRDS(industrial_land , 
        file.path(process_dir, "data", "industrial_land.rds"))

saveRDS(industrial_land_shares , 
        file.path(process_dir, "data", "industrial_land_shares.rds"))
