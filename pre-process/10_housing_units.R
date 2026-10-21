# Libraries -----------------------------------------------------------------
library(dplyr)
library(stringr)
library(tidyr)

un <- Sys.getenv("USERNAME")
process_dir <- file.path("C:/Users",str_to_lower(un),
                         "OneDrive - PSRC/coding/centers-monitoring")

source(file.path(process_dir, "pre-process", "0_general_inputs.R"))

# Housing Unit Change ------------------------------------------------------
pop_hh_hu_data <- readRDS(file.path(process_dir,
                                    "data",
                                    "population-housing.rds"))

hu = pop_hh_hu_data  |> 
  filter(grouping == "Housing Units") |>
  arrange(place_id, year) |>
  group_by(place_id) |>
  mutate(delta = round(estimate - lag(estimate), -1)) |>
  ungroup() |>
  mutate(grouping = "Housing Unit Change") |>
  drop_na() |>
  mutate(year = case_when(
    year == 2015 ~ "2010 to 2015",
    year == 2020 ~ "2015 to 2020",
    year == 2025 ~ "2020 to 2025")) |>
  mutate(estimate = delta, share = 1) |>
  select(-"delta") |>
  mutate(concept = "Housing Unit Change")

saveRDS(hu , 
        file.path(process_dir, 
                  "data", 
                  "housing_units.rds"))
