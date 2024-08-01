# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrcelmer)
library(sf)

rgc_title <- "Regional Growth Center (12/12/2023)"
mic_title <- "MIC (1/5/2024)"

base_model_yr <- 2018
rgc_dest_mod_file <- "data/tour_rgc_dest.csv"

dest_mode_order <- c("SOV", "non-SOV")
rgcs <- st_read_elmergeo(layer_name = "urban_centers") |> select("name") |> st_drop_geometry() |> pull() |> unique()
mics <-  st_read_elmergeo(layer_name = "micen") |> select("mic") |> st_drop_geometry() |> pull() |> unique()

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

rgc_modes <- read_csv(rgc_dest_mod_file) |>
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
  select(geography="tour_d_rgc", "year", "grouping", estimate="toexpfac", "concept") |>
  group_by(geography, year, grouping, concept) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(geography_type = case_when(
    geography %in% mics ~ mic_title,
    geography %in% rgcs ~ rgc_title,
    TRUE ~ "Not in a Center")) |>
  mutate(geography = gsub("Redmond-Overlake", "Redmond Overlake", geography)) |>
  mutate(geography = gsub("Bellevue", "Bellevue Downtown", geography)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  mutate(geography = gsub("Kent MIC", "Kent", geography)) |>
  mutate(geography = gsub("Paine Field / Boeing Everett", "Paine Field/Boeing Everett", geography)) |>
  mutate(geography = gsub("Sumner Pacific", "Sumner-Pacific", geography)) |>
  mutate(geography = gsub("Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton", geography)) |>
  mutate(geography = gsub("Cascade", "Cascade Industrial Center - Arlington/Marysville", geography)) |>
  
  mutate(grouping = factor(grouping, levels = dest_mode_order)) |>
  arrange(geography, grouping, year)

# RGC and MIC's
centers <- rgc_modes |> filter(geography != "Not in RGC")
totals <- centers |> group_by(geography, year, concept, geography_type) |> summarise(total = sum(estimate)) |> as_tibble()

centers <- left_join(centers, totals,  by=c("geography", "year", "concept", "geography_type")) |>
  mutate(share = estimate / total) |>
  select(-"total")

rm(totals)

# Region
region <- rgc_modes |> group_by(year, grouping, concept) |> summarise(estimate = sum(estimate)) |> as_tibble() |> mutate(geography = "Region", geography_type = "Region")
totals <- region |>  group_by(year, concept) |> summarise(total = sum(estimate)) |> as_tibble()

region <- left_join(region, totals,  by=c("year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total")

rm(totals)

# All RGCs
all_rgcs <- rgc_modes |> filter(geography_type == rgc_title & geography != "Not in RGC") |> group_by(year, grouping, concept) |> summarise(estimate = sum(estimate)) |> as_tibble() |> mutate(geography = "All RGCs", geography_type = rgc_title)
totals <- all_rgcs |> group_by(year, concept) |> summarise(total = sum(estimate)) |> as_tibble()

all_rgcs <- left_join(all_rgcs, totals,  by=c("year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total")

rm(totals)

# All MICs
all_mics <- rgc_modes |> filter(geography_type == mic_title & geography != "Not in RGC") |> group_by(year, grouping, concept) |> summarise(estimate = sum(estimate)) |> as_tibble() |> mutate(geography = "All MICs", geography_type = mic_title)
totals <- all_mics |>  group_by(year, concept) |> summarise(total = sum(estimate)) |> as_tibble()

all_mics <- left_join(all_mics, totals,  by=c("year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total")

rm(totals)

destination_mode_share <- bind_rows(centers, region, all_rgcs, all_mics)
ord <- unique(c("Region", "All RGCs", rgc_names, "All MICs", mic_names))
destination_mode_share <- destination_mode_share |> mutate(geography = factor(geography, levels = ord)) |> arrange(geography, grouping, year)
rm(dest_mode_order, centers, region, all_rgcs, all_mics, rgc_modes)
saveRDS(destination_mode_share, "data/destination_mode_share.rds")
