# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrcelmer)
library(sf)

# Inputs ------------------------------------------
census_years <- c(2010, 2015, 2020, 2024)
ofm_years <- c(2010, 2015, 2020, 2025)
jobs_years <- c(2010, 2015, 2020, 2025)
gtfs_year <- 2026
ofm_postcensal_year <- 2025
ofm_intercensal_year <- 2024
parcel_postcensal_year <- 2023
parcel_intercensal_year <- 2018

rgc_title <- "Regional Growth Center (3/26/2026)"
mic_title <- "MIC (7/10/2026)"

wgs84 <- 4326
spn <- 2285

options(dplyr.summarise.inform = FALSE)

# Functions ---------------------------------------------------------------
transit_stops_by_mode <- function(year, gtfs_feed, data_month = 4) {
  
  hct <- read_csv(file.path(process_dir, 
                            "data", 
                            "hct_ids.csv"), 
                  show_col_types = FALSE) 
  
  # Open Regional GTFS File and load into memory
  print(str_glue("Opening the {year} GTFS archive."))
  gtfs <- read_gtfs(path=gtfs_feed, 
                    files = c("trips","stops",
                              "stop_times", "routes", 
                              "shapes"))
  
  # Load Stops
  print(str_glue("Getting the {year} stops into a tibble." ))
  stops <- as_tibble(gtfs$stops) |> 
    mutate(stop_id = str_to_lower(stop_id)) |>
    select("stop_id", "stop_name", "stop_lat", "stop_lon")
  
  # Load Routes, add HCT modes and update names and agencies
  print(str_glue("Getting the {year} routes into a tibble." ))
  routes <- as_tibble(gtfs$routes) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("route_id", "agency_id",
           "route_short_name", "route_long_name", "route_type")
  
  print(str_glue("Adding High-Capacity Transit codes to the {year} routes"))
  routes <- left_join(routes, hct, by="route_id") |>
    mutate(type_code = case_when(
      is.na(type_code) ~ route_type,
      !(is.na(type_code)) ~ type_code)) |>
    mutate(route_name = case_when(
      is.na(route_name) ~ route_short_name,
      !(is.na(route_name)) ~ route_name)) |>
    mutate(type_name = case_when(
      is.na(type_name) ~ "Bus",
      !(is.na(type_name)) ~ type_name)) |>
    mutate(agency_name = case_when(
      !(is.na(agency_name)) ~ agency_name,
      is.na(agency_name) & agency_id == "29" ~ "Community Transit",
      is.na(agency_name) & agency_id == "97" ~ "Everett Transit",
      is.na(agency_name) & agency_id == "1" ~ "King County Metro",
      is.na(agency_name) & agency_id == "20" ~ "Kitsap Transit",
      is.na(agency_name) & agency_id == "19" ~ "InterCity Transit",
      is.na(agency_name) & agency_id == "3" ~ "Pierce Transit",
      is.na(agency_name) & agency_id == "40" ~ "Sound Transit")) |>
    select("route_id", "route_name", "type_name", "type_code", "agency_name")
  
  # Trips are used to get route id onto stop times
  print(str_glue(
    "Getting the {year} trips into a tibble to add route ID to stop times.")
    )
  
  trips <- as_tibble(gtfs$trips) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("trip_id", "route_id")
  
  trips <- left_join(trips, routes, by=c("route_id"))
  
  # Clean Up Stop Times to get routes and mode by stops served
  print(str_glue(
    "Getting the {year} stop times into a tibble to add route information.")
    )
  
  stoptimes <- as_tibble(gtfs$stop_times) |>
    mutate(stop_id = str_to_lower(stop_id)) |>
    select("trip_id", "stop_id")
  
  # Get Mode and agency from trips to stops
  print(str_glue("Getting unique stop list by modes for the {year}." ))
  stops_by_mode <- left_join(stoptimes, trips, by=c("trip_id")) |>
    select("stop_id", "type_code", "type_name", "agency_name") |>
    distinct()
  
  stops_by_mode <- left_join(stops_by_mode, stops, by=c("stop_id")) |>
    mutate(date=mdy(paste0(data_month,"-01-",year)))
  
  print(str_glue("All Done."))
  
  return(stops_by_mode)
  
}

transit_routes_by_mode <- function(year, gtfs_feed, data_month = 4) {
  
  hct <- read_csv(file.path(process_dir, 
                            "data", 
                            "hct_ids.csv"), 
                  show_col_types = FALSE) 
  
  # Open Regional GTFS File and load into memory
  print(str_glue("Opening the {year} GTFS archive."))
  gtfs <- read_gtfs(path=gtfs_feed, files = c("routes", "shapes", "trips"))
  
  # Load Routes, add HCT modes and update names and agencies
  print(str_glue("Getting the {year} routes into a tibble." ))
  routes <- as_tibble(gtfs$routes) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("route_id", "agency_id",
           "route_short_name", "route_long_name", "route_type")
  
  print(str_glue("Adding High-Capacity Transit codes to the {year} routes"))
  routes <- left_join(routes, hct, by="route_id") |>
    mutate(type_code = case_when(
      is.na(type_code) ~ route_type,
      !(is.na(type_code)) ~ type_code)) |>
    mutate(route_name = case_when(
      is.na(route_name) ~ route_short_name,
      !(is.na(route_name)) ~ route_name)) |>
    mutate(type_name = case_when(
      is.na(type_name) ~ "Bus",
      !(is.na(type_name)) ~ type_name)) |>
    mutate(agency_name = case_when(
      !(is.na(agency_name)) ~ agency_name,
      is.na(agency_name) & agency_id == "29" ~ "Community Transit",
      is.na(agency_name) & agency_id == "97" ~ "Everett Transit",
      is.na(agency_name) & agency_id == "1" ~ "King County Metro",
      is.na(agency_name) & agency_id == "20" ~ "Kitsap Transit",
      is.na(agency_name) & agency_id == "19" ~ "InterCity Transit",
      is.na(agency_name) & agency_id == "3" ~ "Pierce Transit",
      is.na(agency_name) & agency_id == "40" ~ "Sound Transit")) |>
    select("route_id", "route_name", "type_name", "type_code", "agency_name")
  
  # Load Route Shapes to get Mode information on layers
  route_lyr <- shapes_as_sf(gtfs$shapes)
  
  # Trips are used to get route id onto shapes
  print(str_glue(
    "Getting the {year} trips into a tibble to add route ID to stop times.")
    )
  
  trips <- as_tibble(gtfs$trips) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("route_id", "shape_id") |>
    distinct()
  
  route_lyr <- left_join(route_lyr, trips, by=c("shape_id")) |> drop_na()
  
  # Get Mode and agency from routes to shapes
  print(str_glue("Getting route details onto shapes for the {year}."))
  route_lyr <- left_join(route_lyr, routes, by=c("route_id")) |> 
    mutate(date=mdy(paste0(data_month,"-01-",year)))
  
  print(str_glue("All Done."))
  
  return(route_lyr)
  
}

get_parcel_data <- function(yrs) {
  
  parcels <- NULL
  for (y in yrs) {
    
    # Parcel population
    print(str_glue(
      "Loading {y} OFM based parcelized estimates of total population")
      )
    
    if (y >= 2020) {
      ofm_vintage <- ofm_postcensal_year
    } else {
      ofm_vintage <- ofm_intercensal_year
    }
    
    q <- paste0("SELECT parcel_dim_id, estimate_year, total_pop, household_pop, housing_units, occupied_housing_units from ofm.parcelized_saep_facts WHERE ofm_vintage = ", ofm_vintage, " AND estimate_year = ", y, "")
    p <- get_query(sql = q)
    
    # Parcel Dimensions
    if (y >= 2020) {
      parcel_yr <- parcel_postcensal_year
    } else {
      parcel_yr <- parcel_intercensal_year
    }
    
    print(str_glue("Loading {parcel_yr} parcel dimensions from Elmer"))
    q <- paste0("SELECT parcel_dim_id, parcel_id, block_group_geoid10, block_group_geoid20, tract_geoid10, tract_geoid20, x_coord_state_plane, y_coord_state_plane from small_areas.parcel_dim WHERE base_year = ", parcel_yr, " ")
    d <- get_query(sql = q) 
    
    # Add 2010 and 2020 Blockgroup and Tract IDs to Parcels
    p <- left_join(p, d, by="parcel_dim_id")
    
    if (is.null(parcels)) {parcels <- p} else {parcels <- bind_rows(parcels, p)}
    
  }
  
  return(parcels)
  
}

generate_census_splits <- function(census_geog, census_df, place_shape) {
  
  census_splits <- NULL
  place_nms <- unique(place_shape$place_id)
  
  for (yr in census_years) {
    
    if (yr < 2020) {census_yr <- 2010} else {census_yr <- 2020}
    if (yr < 2020) {geog_yr <- paste0(census_geog, "_geoid10")} else {geog_yr <- paste0(census_geog, "_geoid20")}
    
    for (place in place_nms) {
      print(str_glue("Calculating {census_geog} shares for {place} for the year {yr}"))
      geogs <- census_df |> filter(place_id == place & census_year == census_yr) |> distinct() |> select("geoid") |> pull()
      
      # Parcel Data for Year and Planning Geography
      p <- parcel_data |> 
        filter(estimate_year == yr & .data[[geog_yr]] %in% geogs) |>
        select(year = "estimate_year", "parcel_id", x = "x_coord_state_plane", y = "y_coord_state_plane", geoid = all_of(geog_yr), pop = "total_pop", hh_pop = "household_pop", hu = "housing_units", hh = "occupied_housing_units") |>
        mutate(geoid = as.character(geoid))
      
      # Census Geography Totals for  Census Geography that intersect the Planning Geography
      total <- p |>
        group_by(year, geoid) |>
        summarise(tot_pop = round(sum(pop),0), tot_hh_pop = round(sum(hh_pop),0), tot_hu = round(sum(hu),0), tot_hh = round(sum(hh),0)) |>
        as_tibble()
      
      p_lyr <- st_as_sf(p, coords = c("x", "y"), crs = spn)
      s_lyr <- place_shape |> filter(place_id == place)
      
      census_parcels <- suppressWarnings(st_intersection(p_lyr, s_lyr)) |> 
        st_drop_geometry() |>
        group_by(year, place_id, place_name, place_category, place_type, geoid) |>
        summarise(pop = round(sum(pop),0), hh_pop = round(sum(hh_pop),0), hu = round(sum(hu),0), hh = round(sum(hh),0)) |>
        as_tibble()
      
      # Census Geography shares for Planning Geography
      tbl <- left_join(census_parcels, total, by=c("year", "geoid")) |>
        mutate(percent_of_total_pop = pop/tot_pop) |>
        mutate(percent_of_household_pop = hh_pop/tot_hh_pop) |>
        mutate(percent_of_housing_units = hu/tot_hu) |>
        mutate(percent_of_occupied_housing_units = hh/tot_hh) |>
        mutate(percent_of_total_pop = replace_na(percent_of_total_pop, 0)) |>
        mutate(percent_of_household_pop = replace_na(percent_of_household_pop, 0)) |>
        mutate(percent_of_housing_units = replace_na(percent_of_housing_units, 0)) |>
        mutate(percent_of_occupied_housing_units = replace_na(percent_of_occupied_housing_units,0)) |>
        select("year", "geoid", "place_id", "place_name", "place_category", "place_type", "percent_of_total_pop", "percent_of_household_pop", "percent_of_housing_units", "percent_of_occupied_housing_units")
      
      if (is.null(census_splits)) {census_splits <- tbl} else {census_splits <- bind_rows(census_splits, tbl)}
    } # end of place loop
  } # end of census year loop
  
  return(census_splits)
}

geography_estimate_from_splits <- function(split_df, estimate_df, geography_name) {
  
  print(str_glue("Summarizing data for {geography_name}"))
  
  t <- split_df |> 
    filter(place_id == geography_name)
  
  c <- left_join(t, estimate_df, by=c("year", "geoid")) |>
    mutate(place_estimate = case_when(
      split_type == "percent_of_total_pop" ~ round(estimate * percent_of_total_pop,0),
      split_type == "percent_of_occupied_housing_units" ~ round(estimate * percent_of_occupied_housing_units,0),
      split_type == "percent_of_household_pop" ~ round(estimate * percent_of_household_pop,0),
      split_type == "percent_of_housing_units" ~ round(estimate * percent_of_housing_units,0))) |>
    group_by(year, place_id, place_name, place_category, place_type, grouping, concept) |>
    summarise(estimate = sum(place_estimate)) |>
    as_tibble() |>
    mutate(grouping = factor(grouping, levels = grouping_order)) |>
    arrange(year, grouping) |>
    drop_na()
  
  totals <- c |>
    filter(str_detect(grouping, "Total")) |>
    select("place_id", "year", "concept", total="estimate")
  
  c <- left_join(c, totals, by=c("place_id", "concept", "year")) |>
    mutate(share = estimate / total) |>
    select(-"total")
  
  return(c)
  
}
