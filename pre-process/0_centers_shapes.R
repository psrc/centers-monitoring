# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrcelmer)
library(sf)

# Inputs ------------------------------------------------------------------
wgs84 <- 4326
spn <- 2285

# Regional Growth Center -------------------------------------------------------
rgc_shape <- st_read_elmergeo(layer_name = "urban_centers") |>
  st_transform(crs = spn) |>
  mutate(name = str_replace_all(name, "Greater Downtown Kirkland", "Kirkland Greater Downtown")) |>
  mutate(name = str_replace_all(name, "Redmond-Overlake", "Redmond Overlake")) |>
  mutate(name = str_replace_all(name, "Bellevue", "Bellevue Downtown")) |>
  select("name", "category", "acres") |>
  arrange(name)

rgc_shape <- rgc_shape |> 
  st_make_valid() |> 
  st_transform(wgs84) |> 
  rename(geometry="Shape")

saveRDS(rgc_shape, "data/rgc_shape.rds")

# Manufacturing & Industrial Centers --------------------------------------
mic_shape <- st_read_elmergeo(layer_name = "micen") |>
  mutate(mic = str_replace_all(mic,"Cascade", "Cascade Industrial Center - Arlington/Marysville")) |>
  mutate(mic = str_replace_all(mic,"Paine Field / Boeing Everett", "Paine Field/Boeing Everett")) |>
  mutate(mic = str_replace_all(mic,"Sumner Pacific", "Sumner-Pacific")) |>
  mutate(mic = str_replace_all(mic,"Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton")) |>
  select(name="mic", "category", "acres")

mic_shape <- mic_shape |> 
  st_make_valid() |> 
  st_transform(wgs84) |> 
  rename(geometry="Shape")

saveRDS(mic_shape, "data/mic_shape.rds")
