# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrcelmer)
library(sf)

rgc_title <- "Regional Growth Center (4/23/2024)"
mic_title <- "MIC (1/5/2024)"

base_model_yr <- 2018
rgc_dest_mod_file <- "data/tour_rgc_dest.csv"

year_ord <- c("2024","2023","2022","2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010")

dest_mode_order <- c("SOV", "non-SOV")
rgcs <- st_read_elmergeo(layer_name = "urban_centers") |> select("name") |> st_drop_geometry() |> pull() |> unique()
mics <-  st_read_elmergeo(layer_name = "micen") |> select("mic") |> st_drop_geometry() |> pull() |> unique()

rgc_names <- st_read_elmergeo(layer_name = "urban_centers") |> 
  select(rgc="name") |> 
  st_drop_geometry() |> 
  mutate(rgc = str_replace_all(rgc, "Greater Downtown Kirkland", "Kirkland Greater Downtown")) |>
  mutate(rgc = str_replace_all(rgc, "Redmond-Overlake", "Redmond Overlake")) |>
  mutate(rgc = str_replace_all(rgc, "Bellevue", "Bellevue Downtown")) |>
  pull() |>
  unique()

mic_names <- st_read_elmergeo(layer_name = "micen") |> 
  select("mic") |>
  st_drop_geometry() |>
  mutate(mic = str_replace_all(mic, "Paine Field / Boeing Everett", "Paine Field/Boeing Everett")) |>
  mutate(mic = str_replace_all(mic, "Sumner Pacific", "Sumner-Pacific")) |>
  mutate(mic = str_replace_all(mic, "Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton")) |>
  mutate(mic = str_replace_all(mic, "Cascade", "Cascade Industrial Center - Arlington/Marysville")) |>
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
  mutate(geography = str_replace_all(geography, "Greater Downtown Kirkland", "Kirkland Greater Downtown")) |>
  mutate(geography = str_replace_all(geography, "Redmond-Overlake", "Redmond Overlake")) |>
  mutate(geography = str_replace_all(geography, "Bellevue", "Bellevue Downtown")) |>
  mutate(geography = str_replace_all(geography, "Paine Field / Boeing Everett", "Paine Field/Boeing Everett")) |>
  mutate(geography = str_replace_all(geography, "Sumner Pacific", "Sumner-Pacific")) |>
  mutate(geography = str_replace_all(geography, "Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton")) |>
  mutate(geography = str_replace_all(geography, "Cascade", "Cascade Industrial Center - Arlington/Marysville")) |>
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
region <- rgc_modes |> 
  group_by(year, grouping, concept) |> 
  summarise(estimate = sum(estimate)) |>
  as_tibble() |> 
  mutate(geography = "Region", geography_type = "Region")

totals <- region |>  
  group_by(year, concept) |> 
  summarise(total = sum(estimate)) |> 
  as_tibble()

region <- left_join(region, totals,  by=c("year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total")

rm(totals)

# All RGCs
all_rgcs <- rgc_modes |> 
  filter(geography_type == rgc_title & geography != "Not in RGC") |> 
  group_by(year, grouping, concept) |> 
  summarise(estimate = sum(estimate)) |> 
  as_tibble() |> 
  mutate(geography = "All RGCs", geography_type = rgc_title)

totals <- all_rgcs |> 
  group_by(year, concept) |> 
  summarise(total = sum(estimate)) |> 
  as_tibble()

all_rgcs <- left_join(all_rgcs, totals,  by=c("year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total")

rm(totals)

# All MICs
all_mics <- rgc_modes |> 
  filter(geography_type == mic_title & geography != "Not in RGC") |> 
  group_by(year, grouping, concept) |> 
  summarise(estimate = sum(estimate)) |> 
  as_tibble() |> 
  mutate(geography = "All MICs", geography_type = mic_title)
totals <- all_mics |>  
  group_by(year, concept) |> 
  summarise(total = sum(estimate)) |> 
  as_tibble()

all_mics <- left_join(all_mics, totals,  by=c("year", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total")

rm(totals)

destination_mode_share <- bind_rows(centers, region, all_rgcs, all_mics)
ord <- unique(c("Region", "All RGCs", rgc_names, "All MICs", mic_names))
destination_mode_share <- destination_mode_share |> 
  mutate(geography = factor(geography, levels = ord)) |> 
  mutate(year = factor(year, levels=year_ord)) |>
  arrange(geography, grouping, year)

rm(dest_mode_order, centers, region, all_rgcs, all_mics, rgc_modes)

saveRDS(destination_mode_share |> filter(concept == "Work"), "data/destination_mode_share.rds")
