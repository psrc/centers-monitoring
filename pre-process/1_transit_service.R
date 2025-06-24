# Libraries -----------------------------------------------------------------
library(tidyverse)
library(tidytransit)
library(psrcelmer)
library(sf)
library(units)

# Inputs ------------------------------------------------------------------
wgs84 <- 4326
spn <- 2285

rgc_title <- "Regional Growth Center (4/23/2024)"
mic_title <- "MIC (1/5/2024)"

gtfs_years <- c(2025)
gtfs_service <- "spring"

transit_ord <- c("All Transit Stops", "Bus", "Bus Rapid Transit", "Commuter Rail", "Ferry", "Light Rail, Monorail or Streetcar")
county_order <- c("Region", "King County", "Kitsap County", "Pierce County", "Snohomish County")

un <- Sys.getenv("USERNAME")
data_dir <- file.path("C:/Users",str_to_lower(un),"Puget Sound Regional Council", "RTP Data & Analysis - Data", "transit")

# Center Shapefiles -------------------------------------------------------
rgc_shape <- st_read_elmergeo(layer_name = "urban_centers") |>
  mutate(name = str_replace_all(name, "Greater Downtown Kirkland", "Kirkland Greater Downtown")) |>
  mutate(name = str_replace_all(name, "Redmond-Overlake", "Redmond Overlake")) |>
  mutate(name = str_replace_all(name, "Bellevue", "Bellevue Downtown")) |>
  select("name", "acres") |>
  st_transform(spn)

mic_shape <- st_read_elmergeo(layer_name = "micen") |>
  mutate(mic = str_replace_all(mic, "Paine Field / Boeing Everett", "Paine Field/Boeing Everett")) |>
  mutate(mic = str_replace_all(mic, "Sumner Pacific", "Sumner-Pacific")) |>
  mutate(mic = str_replace_all(mic, "Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton")) |>
  mutate(mic = str_replace_all(mic, "Cascade", "Cascade Industrial Center - Arlington/Marysville")) |>
  select(name="mic", "acres") |>
  st_transform(spn)

rgc_names <- st_read_elmergeo(layer_name = "urban_centers") |> 
  select("name") |> 
  st_drop_geometry() |> 
  mutate(name = str_replace_all(name, "Greater Downtown Kirkland", "Kirkland Greater Downtown")) |>
  mutate(name = str_replace_all(name, "Redmond-Overlake", "Redmond Overlake")) |>
  mutate(name = str_replace_all(name, "Bellevue", "Bellevue Downtown")) |>
  pull() |>
  unique()

mic_names <- st_read_elmergeo(layer_name = "micen") |> 
  select(name="mic") |>
  st_drop_geometry() |>
  mutate(name = str_replace_all(name, "Paine Field / Boeing Everett", "Paine Field/Boeing Everett")) |>
  mutate(name = str_replace_all(name, "Sumner Pacific", "Sumner-Pacific")) |>
  mutate(name = str_replace_all(name, "Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton")) |>
  mutate(name = str_replace_all(name, "Cascade", "Cascade Industrial Center - Arlington/Marysville")) |>
  pull() |>
  unique()

# Functions ---------------------------------------------------------------
transit_stops_by_mode <- function(year, service_change) {
  
  hct_file <- file.path(data_dir,"hct_ids.csv")
  hct <- read_csv(hct_file, show_col_types = FALSE) 
  
  if (tolower(service_change)=="spring") {data_month = "05"} else (data_month = "10")
  
  options(dplyr.summarise.inform = FALSE)
  gtfs_file <- file.path(data_dir,"gtfs",tolower(service_change),paste0(as.character(year),".zip"))
  
  # Open Regional GTFS File and load into memory
  print(str_glue("Opening the {service_change} {year} GTFS archive."))
  gtfs <- read_gtfs(path=gtfs_file, files = c("trips","stops","stop_times", "routes"))
  
  # Load Stops
  print(str_glue("Getting the {service_change} {year} stops into a tibble." ))
  stops <- as_tibble(gtfs$stops) |> 
    mutate(stop_id = str_to_lower(stop_id)) |>
    select("stop_id", "stop_name", "stop_lat", "stop_lon")
  
  # Load Routes, add HCT modes and update names and agencies
  print(str_glue("Getting the {service_change} {year} routes into a tibble." ))
  routes <- as_tibble(gtfs$routes) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("route_id", "agency_id","route_short_name", "route_long_name", "route_type")
  
  print(str_glue("Adding High-Capacity Transit codes to the {service_change} {year} routes"))
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
      is.na(agency_name) & str_detect(route_id, "communitytransit") ~ "Community Transit",
      is.na(agency_name) & agency_id %in% c("97", "7") ~ "Everett Transit",
      is.na(agency_name) & agency_id == "1" ~ "King County Metro",
      is.na(agency_name) & agency_id %in% c("4","20") ~ "Kitsap Transit",
      is.na(agency_name) & agency_id == "23" ~ "City of Seattle",
      is.na(agency_name) & agency_id %in% c("2", "3") ~ "Pierce Transit",
      is.na(agency_name) & agency_id == "51" ~ "Amtrak",
      is.na(agency_name) & agency_id == "95" ~ "Washington State Ferries",
      is.na(agency_name) & agency_id == "96" ~ "Seattle Center Monorail",
      is.na(agency_name) & agency_id == "19" ~ "Intercity Transit",
      is.na(agency_name) & agency_id %in% c("6","40") ~ "Sound Transit")) |>
    select("route_id", "route_name", "type_name", "type_code", "agency_name")
  
  # Trips are used to get route id onto stop times
  print(str_glue("Getting the {service_change} {year} trips into a tibble to add route ID to stop times." ))
  trips <- as_tibble(gtfs$trips) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("trip_id", "route_id")
  
  trips <- left_join(trips, routes, by=c("route_id"))
  
  # Clean Up Stop Times to get routes and mode by stops served
  print(str_glue("Getting the {service_change} {year} stop times into a tibble to add route information." ))
  stoptimes <- as_tibble(gtfs$stop_times) |>
    mutate(stop_id = str_to_lower(stop_id)) |>
    select("trip_id", "stop_id")
  
  # Get Mode and agency from trips to stops
  print(str_glue("Getting unique stop list by modes for the {service_change} {year}." ))
  stops_by_mode <- left_join(stoptimes, trips, by=c("trip_id")) |>
    select("stop_id", "type_code", "type_name", "agency_name") |>
    distinct()
  
  stops_by_mode <- left_join(stops_by_mode, stops, by=c("stop_id")) |>
    mutate(date=year)
  
  print(str_glue("All Done."))
  
  return(stops_by_mode)
  
}

transit_routes_by_mode <- function(year, service_change) {
  
  hct_file <- file.path(data_dir,"hct_ids.csv")
  hct <- read_csv(hct_file, show_col_types = FALSE) 
  
  if (tolower(service_change)=="spring") {data_month = "05"} else (data_month = "10")
  service_date <- ymd(paste0(year,"-",data_month,"-15"))
  
  options(dplyr.summarise.inform = FALSE)
  gtfs_file <- file.path(data_dir,"gtfs",tolower(service_change),paste0(as.character(year),".zip"))
  
  # Open Regional GTFS File and load into memory
  print(str_glue("Opening the {service_change} {year} GTFS archive."))
  gtfs <- read_gtfs(path=gtfs_file, files = c("trips","stops","stop_times", "routes", "shapes", "calendar"))
  
  # Load Routes, add HCT modes and update names and agencies
  print(str_glue("Getting the {service_change} {year} routes into a tibble." ))
  routes <- as_tibble(gtfs$routes) |> 
    mutate(route_id = str_to_lower(route_id)) |>
    select("route_id", "agency_id","route_short_name", "route_long_name", "route_type")
  
  print(str_glue("Adding High-Capacity Transit codes to the {service_change} {year} routes"))
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
      is.na(agency_name) & str_detect(route_id, "communitytransit") ~ "Community Transit",
      is.na(agency_name) & agency_id %in% c("97", "7") ~ "Everett Transit",
      is.na(agency_name) & agency_id == "1" ~ "King County Metro",
      is.na(agency_name) & agency_id %in% c("4","20") ~ "Kitsap Transit",
      is.na(agency_name) & agency_id == "23" ~ "City of Seattle",
      is.na(agency_name) & agency_id %in% c("2", "3") ~ "Pierce Transit",
      is.na(agency_name) & agency_id == "51" ~ "Amtrak",
      is.na(agency_name) & agency_id == "95" ~ "Washington State Ferries",
      is.na(agency_name) & agency_id == "96" ~ "Seattle Center Monorail",
      is.na(agency_name) & agency_id == "19" ~ "Intercity Transit",
      is.na(agency_name) & agency_id %in% c("6","40") ~ "Sound Transit")) |>
    select("route_id", "route_name", "type_name", "type_code", "agency_name")
  
  # Load Route Shapes to get Mode information on layers
  route_lyr <- shapes_as_sf(gtfs$shapes)
  
  # Trips are used to get route id onto shapes
  print(str_glue("Getting the {service_change} {year} trips into a tibble to add route ID to stop times." ))
  trips <- as_tibble(gtfs$trips) |> 
    #filter(service_id %in% calendar_ids) |>
    mutate(route_id = str_to_lower(route_id)) |>
    select("route_id", "shape_id", "direction_id") |>
    distinct()
  
  route_lyr <- left_join(route_lyr, trips, by=c("shape_id")) |> drop_na()
  
  # Trim to only include the longest route length if there are trip variations
  route_lyr <- route_lyr |>
    mutate(route_length = as.numeric(set_units(st_length(route_lyr), "miles")))
  
  longest_routes <- route_lyr |>
    st_drop_geometry() |>
    group_by(route_id, direction_id) |>
    summarise(route_length = max(route_length)) |>
    as_tibble()
  
  longest_routes <- left_join(longest_routes, route_lyr, by=c("route_id", "direction_id", "route_length")) |>
    select("shape_id") |>
    pull()
  
  route_lyr <- route_lyr |>
    filter(shape_id %in% longest_routes)
  
  # Get Mode and agency from routes to shapes
  print(str_glue("Getting route details onto shapes for the {service_change} {year}." ))
  route_lyr <- left_join(route_lyr, routes, by=c("route_id")) |> mutate(date=mdy(paste0(data_month,"-01-",year)))
  
  print(str_glue("All Done."))
  
  return(route_lyr)
  
}

# GTFS Data ---------------------------------------------------------------
transit_stops <- NULL
for(y in gtfs_years) {
  s <- transit_stops_by_mode(year = y, service_change = gtfs_service)
  if(is.null(transit_stops)) {transit_stops <- s} else {transit_stops <- bind_rows(transit_stops, s)}
  rm(s)
}

transit_stop_lyr <- transit_stops |> 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs=wgs84) |> 
  st_transform(spn) |>
  mutate(type_name = case_when(
    type_name == "ST Express" ~ "Bus",
    type_name == "BRT" ~ "Bus Rapid Transit",
    type_name %in% c("Streetcar", "Light Rail", "Monorail") ~ "Light Rail, Monorail or Streetcar",
    type_name %in% c("Passenger Ferry", "Auto Ferry") ~ "Ferry",
    type_name == "Commuter Rail" ~ "Commuter Rail",
    type_name == "Bus" ~ "Bus"))

transit_modes <- transit_stop_lyr |> st_drop_geometry() |> select(mode = "type_name") |> distinct() |> arrange(mode)

transit_route <- NULL
for(y in gtfs_years) {
  s <- transit_routes_by_mode(year = y, service_change = gtfs_service)
  if(is.null(transit_route)) {transit_route <- s} else {transit_route <- bind_rows(transit_route, s)}
  rm(s)
}

transit_route_lyr <- transit_route |> 
  st_transform(wgs84) |>
  mutate(type_name = case_when(
    type_name == "ST Express" ~ "Bus",
    type_name == "BRT" ~ "Bus Rapid Transit",
    type_name %in% c("Streetcar", "Light Rail", "Monorail") ~ "Light Rail, Monorail or Streetcar",
    type_name %in% c("Passenger Ferry", "Auto Ferry") ~ "Ferry",
    type_name == "Commuter Rail" ~ "Commuter Rail",
    type_name == "Bus" ~ "Bus"))

# Stops by Center ---------------------------------------------------------
rgc_stop_data <- NULL
# Summarize by RGC
for (rgc in rgc_names) {
  print(str_glue("Sumarzing stops for {rgc}."))
  
  # Stops that intersect Centers
  s_lyr <- rgc_shape |> filter(name == rgc)
  
  j_lyr <- st_intersection(transit_stop_lyr, s_lyr) |> 
    st_drop_geometry() |>
    select("name", mode = "type_name") |>
    mutate(stops = 1) |>
    group_by(mode) |>
    summarise(stops = round(sum(stops),0)) |>
    as_tibble()
  
  s <- left_join(transit_modes, j_lyr, by=c("mode")) |> mutate(geography = rgc) |> mutate(stops = replace_na(stops, 0))
  
  total <- s |> group_by(geography) |> summarise(stops = sum(stops)) |> as_tibble() |> mutate(mode = "All Transit Stops")
  
  s <- bind_rows(s, total) |> mutate(mode = factor(mode, levels = transit_ord)) |> arrange(mode) |> mutate(geography_type = rgc_title)
  
  if (is.null(rgc_stop_data)) {rgc_stop_data <- s} else {rgc_stop_data <- bind_rows(rgc_stop_data, s)}
  rm(j_lyr, s_lyr, s, total)
}

# Summarize for All RGCs
s <- rgc_stop_data |>
  group_by(mode) |>
  summarise(stops = round(sum(stops), 0)) |>
  as_tibble() |>
  mutate(geography = "All RGCs", geography_type = rgc_title)

rgc_stop_data <- bind_rows(rgc_stop_data, s) |> 
  mutate(geography_type = rgc_title) |>
  select("geography", "geography_type", "mode", "stops")
rm(s)

mic_stop_data <- NULL
# Summarize by MIC
for (mic in mic_names) {
  print(str_glue("Sumarzing stops for {mic}."))
  
  # Stops that intersect Centers
  s_lyr <- mic_shape |> filter(name == mic)
  
  j_lyr <- st_intersection(transit_stop_lyr, s_lyr) |> 
    st_drop_geometry() |>
    select("name", mode = "type_name") |>
    mutate(stops = 1) |>
    group_by(mode) |>
    summarise(stops = round(sum(stops),0)) |>
    as_tibble()
  
  s <- left_join(transit_modes, j_lyr, by=c("mode")) |> mutate(geography = mic) |> mutate(stops = replace_na(stops, 0))
  
  total <- s |> group_by(geography) |> summarise(stops = sum(stops)) |> as_tibble() |> mutate(mode = "All Transit Stops")
  
  s <- bind_rows(s, total) |> mutate(mode = factor(mode, levels = transit_ord)) |> arrange(mode) |> mutate(geography_type = mic_title)
  
  if (is.null(mic_stop_data)) {mic_stop_data <- s} else {mic_stop_data <- bind_rows(mic_stop_data, s)}
  rm(j_lyr, s_lyr, s, total)
}

# Summarize for All MICs
s <- mic_stop_data |>
  group_by(mode) |>
  summarise(stops = round(sum(stops), 0)) |>
  as_tibble() |>
  mutate(geography = "All MICs", geography_type = mic_title)

mic_stop_data <- bind_rows(mic_stop_data, s) |> 
  mutate(geography_type = mic_title) |>
  select("geography", "geography_type", "mode", "stops")
rm(s)

transit_stop_data <- bind_rows(rgc_stop_data, mic_stop_data)
rm(rgc_stop_data, mic_stop_data)

ord <- unique(c(county_order, "All RGCs", rgc_names, "All MICs", mic_names))
transit_stop_data <- transit_stop_data |>
  mutate(geography = factor(geography, levels = ord)) |> 
  arrange(geography, mode)

saveRDS(transit_stop_data, "data/transit_stop_data.rds")

transit_stop_lyr <- transit_stop_lyr |> st_transform(wgs84)
saveRDS(transit_stop_lyr, "data/transit_stop_lyr.rds")

saveRDS(transit_route_lyr, "data/transit_route_lyr.rds")

