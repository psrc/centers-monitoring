# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrcelmer)
library(sf)

un <- Sys.getenv("USERNAME")
process_dir <- file.path("C:/Users",
                         str_to_lower(un),
                         "OneDrive - PSRC/coding/centers-monitoring")

source(file.path(process_dir, "pre-process", "0_general_inputs.R"))

# Regional Growth Center -----------------------------------------------------
rgc_shape <- st_read_elmergeo(layer_name = "urban_centers") |>
  mutate(place_id = paste(str_replace_all(name, " ", ""), 
                          str_replace_all(rgc_title, " ", ""), 
                          sep = "-")
         ) |>
  mutate(place_type = rgc_title) |>
  select("place_id", place_name = "name", 
         "place_category" = category, 
         "place_type", "acres", 
         geometry="Shape") |>
  arrange(place_id)

rgc_shape <- rgc_shape |> 
  st_make_valid() |> 
  st_transform(wgs84)

saveRDS(rgc_shape, file.path(process_dir, 
                             "data", 
                             "rgc_shape.rds"))

# Manufacturing & Industrial Centers --------------------------------------
mic_shape <- st_read_elmergeo(layer_name = "micen") |>
  mutate(mic = str_replace_all(mic,
                               "Paine Field / Boeing Everett", 
                               "Paine Field/Boeing Everett")
         ) |>
  mutate(mic = str_replace_all(mic,
                               "Puget Sound Industrial Center- Bremerton", 
                               "Puget Sound Industrial Center - Bremerton")
         ) |>
  mutate(place_id = paste(str_replace_all(mic, " ", ""), 
                          str_replace_all(mic_title, " ", ""), 
                          sep = "-")
         ) |>
  mutate(place_type = mic_title) |>
  select("place_id", place_name = "mic", 
         "place_category" = category, "place_type", 
         "acres", geometry="Shape") |>
  arrange(place_id)

mic_shape <- mic_shape |> 
  st_make_valid() |> 
  st_transform(wgs84)

saveRDS(mic_shape, 
        file.path(process_dir, 
                  "data", 
                  "mic_shape.rds"))

# All Centers --------------------------------------
place_shape <- bind_rows(rgc_shape, mic_shape)

place_shape <- place_shape |> 
  st_make_valid() |> 
  st_transform(wgs84)

saveRDS(place_shape, file.path(process_dir, 
                               "data", 
                               "place_shape.rds"))
