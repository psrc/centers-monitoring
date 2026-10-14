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

# Intersection Density ---------------------------------------------------------
intersections <- read_csv("data/center_intersection_density.csv", 
                            show_col_types = FALSE) |>
  mutate(place_id = factor(place_id, levels = place_order)) |>
  arrange(place_id)

saveRDS(intersections , 
        file.path(process_dir, 
                  "data", 
                  "intersection_density.rds"))
