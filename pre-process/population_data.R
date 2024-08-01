# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrcelmer)
library(sf)

source("pre-process/functions.R")

# Basic Inputs ------------------------------------------------------------

# Silence the dplyr summarize message
options(dplyr.summarise.inform = FALSE)

# Population Years from OFM Parcelization
ofm_years <- c(2010, 2011, 2016, 2020, 2021, 2022, 2023)

# Make sure these match the boundary definition you want the data to be based on
rgc_title <- "regional_growth_center_2024_04_23"
mic_title <- "manufacturing_industrial_center_2024_04_23"

rgc_dashboard_title <- "Regional Growth Center (12/12/2023)"
mic_dashboard_title <- "MIC (1/5/2024)"

# Coordinate Reference Systems
wgs84 <- 4326
spn <- 2285

# Centers Names & Types ---------------------------------------------------
rgc_names <- st_read_elmergeo(layer_name = "urban_centers") |> 
  select("name") |> 
  st_drop_geometry() |> 
  mutate(name = gsub("Redmond-Overlake", "Redmond Overlake", name)) |>
  mutate(name = gsub("Bellevue", "Bellevue Downtown", name)) |>
  mutate(name = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", name)) |>
  pull() |>
  unique()

mic_names <- st_read_elmergeo(layer_name = "micen") |> 
  select("mic") |>
  st_drop_geometry() |>
  mutate(mic = gsub("Kent MIC", "Kent", mic)) |>
  mutate(mic = gsub("Paine Field / Boeing Everett", "Paine Field/Boeing Everett", mic)) |>
  mutate(mic = gsub("Sumner Pacific", "Sumner-Pacific", mic)) |>
  mutate(mic = gsub("Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton", mic)) |>
  mutate(mic = gsub("Cascade", "Cascade Industrial Center - Arlington/Marysville", mic)) |>
  pull() |>
  unique()

# Population and Housing Data for Centers ---------------------------------------------
population_housing <- process_ofm_data_for_centers(yrs =ofm_years, dec = -1)
saveRDS(population_housing, "data/pop_hsg_data.rds")
