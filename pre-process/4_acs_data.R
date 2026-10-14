# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrcelmer)
library(tidycensus)
library(sf)
library(units)

un <- Sys.getenv("USERNAME")

process_dir <- file.path("C:/Users",
                         str_to_lower(un),
                         "OneDrive - PSRC/coding/centers-monitoring")

source(file.path(process_dir, 
                 "pre-process", 
                 "0_general_inputs.R")
       )

# Inputs ------------------------------------------------------------------
blockgroup_splits <- readRDS(file.path(process_dir, 
                                       "data", 
                                       "blockgroup_splits.rds")
                             )

tract_splits <- readRDS(file.path(process_dir, 
                                  "data", 
                                  "tract_splits.rds")
                        )

# ACS Variables -----------------------------------------------------------
variables <- read_csv(file.path(process_dir, 
                                "data", 
                                "acs_variables.csv"), 
                      show_col_types = FALSE)

county_variables <- unique(variables$variable)

blockgroup_variables <- variables |>
  filter(available_geography == "blockgroup") |>
  select("variable") |>
  distinct() |>
  pull()

tract_variables <- variables |>
  filter(available_geography == "tract") |>
  select("variable") |>
  distinct() |>
  pull()

grouping_order <- variables |>
  select("order", "grouping") |>
  arrange(order) |>
  distinct() |>
  select("grouping") |>
  pull()

split_data <- variables |>
  select("concept", "grouping", "split_type") |>
  distinct()

# Place Shapefiles -------------------------------------------------------
place_shape <- readRDS(file.path(process_dir, 
                                 "data", 
                                 "place_shape.rds")
                       ) |> 
  st_transform(spn)

all_names <- c(unique(place_shape$place_id))

rgc_names <- place_shape |>
  st_drop_geometry() |>
  filter(place_type == rgc_title) |>
  select("place_id") |>
  pull() |>
  unique()

mic_names <- place_shape |>
  st_drop_geometry() |>
  filter(place_type == mic_title) |>
  select("place_id") |>
  pull() |>
  unique()

# Levels for Factors ------------------------------------------------------
county_order <- c("Region-Region", "King-County", 
                  "Kitsap-County", "Pierce-County", 
                  "Snohomish-County")

place_order <- c(county_order, 
                 paste0("AllRGCs-", 
                        str_replace_all(rgc_title, " ", "")),
                 rgc_names,
                 paste0("AllMICs-", 
                        str_replace_all(mic_title, " ", "")),
                 mic_names)

# County Data ------------------------------------------------------
county_data <- NULL
for (y in census_years) {
  print(str_glue("Pulling County ACS data for {y}"))
  d <- get_acs(geography="county", state="Washington",
               county = c('King', 'Kitsap', 'Pierce', 'Snohomish'), 
               variables = county_variables, year = y, acs.type = 'acs5') 
  
  # Combine by Dashboard Grouping's
  d <- left_join(d, variables, by="variable") |>
    group_by(NAME, grouping, concept) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble()
  
  # PSRC Region 
  r <- d |>
    group_by(grouping, concept) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(NAME = "Region")
  
  d <- bind_rows(d, r)
  
  # Add totals  
  t <- d |>
    filter(str_detect(grouping, "Total")) |>
    select("NAME", "concept", total = "estimate")
  
  d <- left_join(d, t, by = c("NAME", "concept")) |>
    mutate(share = estimate / total) 
  
  # Final Cleanup
  d <- d |>
    mutate(place_name = str_remove_all(NAME, " County, Washington")) |>
    mutate(year = y) |>
    mutate(place_category = case_when(
      NAME == "Region" ~ "Region",
      NAME != "Region" ~ "County")) |>
    mutate(place_type = place_category) |>
    mutate(place_id = paste(str_replace_all(place_name, " ", ""), 
                            str_replace_all(place_category, " ", ""), 
                            sep = "-")
           ) |>
    select("year", "place_id", 
           "place_name", "place_category", 
           "place_type", "grouping", 
           "concept", "estimate", "share") |>
    mutate(grouping = factor(grouping, levels = grouping_order)) |>
    mutate(place_id = factor(place_id, levels = place_order)) |>
    arrange(place_id, grouping)
  
  if(is.null(county_data)) 
  {county_data <- d} else 
      {county_data <- bind_rows(county_data, d)}
  rm(d, r, t)
  
}

# Blockgroup Data ---------------------------------------------------------
blockgroup_data <- NULL
tract_data <- NULL

for (y in census_years) {
  
  if (y > 2012) {
    
    print(str_glue("Pulling Blockgroup ACS data for {y}"))
    d <- get_acs(geography="block group", state="Washington",
                 county = c('King', 'Kitsap', 'Pierce', 'Snohomish'), 
                 variables = blockgroup_variables, year = y, acs.type = 'acs5') 
    
    # Combine by Dashboard Grouping's
    d <- left_join(d, variables, by="variable") |>
      group_by(GEOID, grouping, concept) |>
      summarise(estimate = sum(estimate)) |>
      as_tibble()
    
    # Add totals  
    t <- d |>
      filter(str_detect(grouping, "Total")) |>
      select("GEOID", "concept", total = "estimate")
    
    d <- left_join(d, t, by = c("GEOID", "concept")) |>
      mutate(share = estimate / total) 
    
    # Final Cleanup
    d <- d |>
      mutate(geoid = GEOID) |>
      mutate(year = y) |>
      select("year", "geoid", "grouping", "concept", "estimate", "share") |>
      mutate(grouping = factor(grouping, levels = grouping_order)) |>
      arrange(geoid, grouping)
    
    d <- left_join(d, split_data, by=c("concept", "grouping"))
    
    if(is.null(blockgroup_data)) 
    {blockgroup_data <- d} else 
        {blockgroup_data <- bind_rows(blockgroup_data, d)}
    rm(d, t)
    
  } else {
    
    print(str_glue(
      "Blockgroup Data is not available via the Census API for {y}, 
      using Tract data"))
    
    print(str_glue("Pulling Tract ACS data for {y}"))
    d <- get_acs(geography="tract", state="Washington",
                 county = c('King', 'Kitsap', 'Pierce', 'Snohomish'), 
                 variables = blockgroup_variables, 
                 year = y, 
                 acs.type = 'acs5') 
    
    # Combine by Dashboard Grouping's
    d <- left_join(d, variables, by="variable") |>
      group_by(GEOID, grouping, concept) |>
      summarise(estimate = sum(estimate)) |>
      as_tibble()
    
    # Add totals  
    t <- d |>
      filter(str_detect(grouping, "Total")) |>
      select("GEOID", "concept", total = "estimate")
    
    d <- left_join(d, t, by = c("GEOID", "concept")) |>
      mutate(share = estimate / total) 
    
    # Final Cleanup
    d <- d |>
      mutate(geoid = GEOID) |>
      mutate(year = y) |>
      select("year", "geoid", "grouping", "concept", "estimate", "share") |>
      mutate(grouping = factor(grouping, levels = grouping_order)) |>
      arrange(geoid, grouping)
    
    d <- left_join(d, split_data, by=c("concept", "grouping"))
    
    if(is.null(tract_data)) 
    {tract_data <- d} else 
        {tract_data <- bind_rows(tract_data, d)}
    rm(d, t)

  } # end of census year condition check for 2012 or previous data
  
} # end of census year loop for block groups

# Tract Data --------------------------------------------------------------
for (y in census_years) {
  print(str_glue("Pulling Tract ACS data for {y}"))
  d <- get_acs(geography="tract", state="Washington",
               county = c('King', 'Kitsap', 'Pierce', 'Snohomish'), 
               variables = tract_variables, year = y, acs.type = 'acs5') 
  
  # Combine by Dashboard Grouping's
  d <- left_join(d, variables, by="variable") |>
    group_by(GEOID, grouping, concept) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble()
  
  # Add totals  
  t <- d |>
    filter(str_detect(grouping, "Total")) |>
    select("GEOID", "concept", total = "estimate")
  
  d <- left_join(d, t, by = c("GEOID", "concept")) |>
    mutate(share = estimate / total) 
  
  # Final Cleanup
  d <- d |>
    mutate(geoid = GEOID) |>
    mutate(year = y) |>
    select("year", "geoid", "grouping", "concept", "estimate", "share") |>
    mutate(grouping = factor(grouping, levels = grouping_order)) |>
    arrange(geoid, grouping)
  
  d <- left_join(d, split_data, by=c("concept", "grouping"))
  
  if(is.null(tract_data)) 
  {tract_data <- d} else 
      {tract_data <- bind_rows(tract_data, d)}
  rm(d, t)
  
} # end of census year loop for tracts

# Place Data from Blockgroups------------------------------------------------
place_data <- NULL
for(place in all_names) {
  
  df <- geography_estimate_from_splits(split_df = blockgroup_splits,
                                       estimate_df= blockgroup_data,
                                       geography_name = place)
  
  ifelse(is.null(place_data), 
         place_data <- df, 
         place_data <- bind_rows(place_data, df))
  rm(df)
} # end of place loop from block group data

# Place Data from Tracts----------------------------------------------------
for(place in all_names) {
  
  df <- geography_estimate_from_splits(split_df = tract_splits,
                                       estimate_df= tract_data,
                                       geography_name = place)
  
  place_data <- bind_rows(place_data, df)
  rm(df)
} # end of place loop from tract data

# Combine Place Data ------------------------------------------------------
a <- place_data |>
  group_by(year, place_type, grouping, concept) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(place_name = case_when(
    place_type == rgc_title ~ "All RGCs",
    place_type == mic_title ~ "All MICs"),
    place_id = paste(str_replace_all(place_name, " ", ""), 
                     str_replace_all(place_type, " ", ""), 
                     sep = "-"),
    place_category = case_when(
      place_type == rgc_title ~ "All RGCs",
      place_type == mic_title ~ "All MICs")
  )

t <- a |>
  filter(str_detect(grouping, "Total")) |>
  select("year", "place_id", "concept", total = "estimate")

a <- left_join(a, t, by = c("year", "place_id", "concept")) |>
  mutate(share = estimate / total) |>
  select(-"total")

combined_place_data <- bind_rows(county_data, place_data, a) |> 
  mutate(year = as.character(year)) |>
  mutate(place_id = factor(place_id, levels = place_order)) |> 
  mutate(grouping = factor(grouping, levels = grouping_order)) |>
  arrange(year, place_id, grouping)

# Manual Adjustments for Centers with No Population ------------------------
manual_data_overrides <- read_csv(
  "data/manual_centers_population_housing_data.csv", 
  show_col_types = FALSE) |>
  filter(manual_population == 0) |>
  select("place_id", "year") |>
  mutate(manual_override = 1,
         year = as.character(year))
  
final_data <- left_join(combined_place_data, 
                        manual_data_overrides, 
                        by=c("place_id","year")) |>
  mutate(estimate = case_when(
    !(is.na(manual_override)) ~ 0,
    (is.na(manual_override)) ~ estimate)) |>
  mutate(share = case_when(
    !(is.na(manual_override)) ~ 0,
    (is.na(manual_override)) ~ share)) |>
  select(-"manual_override")

saveRDS(final_data, file.path(process_dir, "data", "census_data.rds"))
