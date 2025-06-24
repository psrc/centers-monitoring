# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrcelmer)
library(sf)

# Inputs ---------------------------------------------------------------
rgc_title <- "Regional Growth Center (4/23/2024)"
mic_title <- "MIC (1/5/2024)"

industrial_years <- c("2010", "2015", "2020", "2022") 

# MIC Names ---------------------------------------------------------------
mic_names <- st_read_elmergeo(layer_name = "micen") |> 
  select("mic") |>
  st_drop_geometry() |>
  mutate(mic = str_replace_all(mic, "Paine Field / Boeing Everett", "Paine Field/Boeing Everett")) |>
  mutate(mic = str_replace_all(mic, "Sumner Pacific", "Sumner-Pacific")) |>
  mutate(mic = str_replace_all(mic, "Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton")) |>
  mutate(mic = str_replace_all(mic, "Cascade", "Cascade Industrial Center - Arlington/Marysville")) |>
  pull() |>
  unique()

# Data Ordering -----------------------------------------------------------
ord <- unique(c("Region", "All Centers", "All RGCs", "All MICs", mic_names))

land_ord <- c("Core Industrial", "Industrial-Commercial", "Aviation Operations",
              "Military", "Total Industrial", 
              "Limited Industrial", "Non-Industrial*", "Total Gross Acreage",
              "share")

year_ord <- c("2024","2023","2022","2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010")

# Data Processing ---------------------------------------------------------
vacancy_absorption <- read_csv("data/mic-vacancy-absorption.csv", show_col_types = FALSE)

industrial_land <- read_csv("data/mic-industrial-lands.csv", show_col_types = FALSE) |>
  pivot_longer(cols = !c(year, geography, geography_type), names_to = "grouping", values_to = "estimate") |>
  mutate(year = as.character(year)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography)) |>
  mutate(geography = factor(geography, levels = ord)) |>
  mutate(grouping = factor(grouping, levels = land_ord)) |>
  arrange(geography, grouping, year) |>
  filter(grouping != "share")

# Data for IL Pie Chart  
industrial_land_shares <- industrial_land |>
  filter(grouping %in% c("Total Industrial", "Limited Industrial", "Non-Industrial*", "Total Gross Acreage"))

total <- industrial_land_shares |> filter(grouping == "Total Gross Acreage") |> rename(total="estimate") |> select(-"grouping")

industrial_land_shares <- left_join(industrial_land_shares, total, by=c("year", "geography", "geography_type")) |>
  mutate(share = estimate / total, estimate = round(estimate, 0)) |>
  select(-"total") |>
  filter(grouping != "Total Gross Acreage")

rm(total)

# Data for IL Table
total <- industrial_land |> filter(grouping == "Total Gross Acreage") |> rename(total="estimate") |> select(-"grouping")

industrial_land <- left_join(industrial_land, total, by=c("year", "geography", "geography_type")) |>
  mutate(share = estimate / total, estimate = round(estimate, 0)) |>
  select(-"total")

rm(total)

# Data for Industrial Jobs
industrial_jobs <- read_csv("data/mic-industrial-jobs.csv", show_col_types = FALSE) |>
  filter(year %in% industrial_years) |>
  mutate(year = as.character(year)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography)) |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  filter(grouping %in% c("Industrial", "Non-industrial")) |>
  mutate(geography = factor(geography, levels = ord)) |>
  arrange(geography, grouping, year)

# Data Output -------------------------------------------------------------
saveRDS(industrial_land , "data/industrial_land.rds")
saveRDS(industrial_jobs , "data/industrial_jobs.rds")
saveRDS(industrial_land_shares , "data/industrial_land_shares.rds")
saveRDS(vacancy_absorption , "data/vacancy_absorption.rds")
