# Libraries -----------------------------------------------------------------
library(tidyverse)
library(tidytransit)
library(psrcelmer)
library(sf)

un <- Sys.getenv("USERNAME")

process_dir <- file.path("C:/Users",
                         str_to_lower(un),
                         "OneDrive - PSRC/coding/centers-monitoring")

source(file.path(process_dir, 
                 "pre-process", 
                 "0_general_inputs.R")
       )

gtfs_file <- file.path("C:/Users",
                       str_to_lower(un),
                       "OneDrive - PSRC",
                       "gtfs_puget_sound_consolidated.zip")

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

grouping_order <- c("All Transit Stops", 
                    "Bus", 
                    "Bus Rapid Transit", 
                    "Commuter Rail", 
                    "Ferry", 
                    "Light Rail, Monorail or Streetcar")

# GTFS Data ---------------------------------------------------------------
transit_stops <- transit_stops_by_mode(year = gtfs_year,
                                       gtfs_feed = gtfs_file, 
                                       data_month = 9)

transit_stop_lyr <- transit_stops |> 
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs=wgs84) |> 
  st_transform(spn) |>
  mutate(type_name = case_when(
    type_name == "ST Express" ~ "Bus",
    type_name == "BRT" ~ "Bus Rapid Transit",
    type_name %in% c("Streetcar", "Light Rail", "Monorail") ~ 
      "Light Rail, Monorail or Streetcar",
    type_name %in% c("Passenger Ferry", "Auto Ferry") ~ "Ferry",
    type_name == "Commuter Rail" ~ "Commuter Rail",
    type_name == "Bus" ~ "Bus"))
    
transit_modes <- transit_stop_lyr |> 
  st_drop_geometry() |> 
  select(mode = "type_name") |> 
  distinct() |> 
  arrange(mode)

# Stops by Place -----------------------------------------------
place_stop_data <- NULL
for (place in all_names) {
  print(str_glue("Sumarzing stops for {place}."))
  
  # Stops that intersect Schools
  s_lyr <- place_shape |> filter(place_id == place)
  
  p_id <- unique(s_lyr$place_id)
  p_nm <- unique(s_lyr$place_name)
  p_ct <- unique(s_lyr$place_category)
  p_ty <- unique(s_lyr$place_type)
  
  j_lyr <- suppressWarnings(st_intersection(transit_stop_lyr, s_lyr)) |> 
    st_drop_geometry() |>
    select("place_name", mode = "type_name") |>
    mutate(stops = 1) |>
    group_by(place_name, mode) |>
    summarise(estimate = round(sum(stops),0)) |>
    as_tibble()
  
  s <- left_join(transit_modes, j_lyr, by=c("mode")) |> 
    mutate(place_id = p_id, 
           place_name = p_nm, 
           place_category = p_ct, 
           place_type = p_ty) |> 
    mutate(estimate = replace_na(estimate, 0))
  
  total <- s |> 
    group_by(place_id, place_name, place_category, place_type) |> 
    summarise(estimate = sum(estimate)) |> 
    as_tibble() |> 
    mutate(mode = "All Transit Stops")
  
  t <- total$estimate
  
  s <- bind_rows(s, total)  |>
    mutate(year = gtfs_year, 
           share = estimate / t, 
           concept = "Transit Stops by Mode") |>
    select("year", "place_id", "place_name", 
           "place_category", "place_type", 
           grouping = "mode", "concept", "estimate", "share")
  
  if (is.null(place_stop_data)) 
  {place_stop_data <- s} else 
      {place_stop_data <- bind_rows(place_stop_data, s)}
  rm(j_lyr, s_lyr, s, total, p_id, p_nm, p_ct, p_ty, t)
}

a <- place_stop_data |>
  group_by(place_type, grouping, concept) |>
  summarise(estimate = sum(estimate)) |>
  as_tibble() |>
  mutate(year = gtfs_year,
         place_name = case_when(
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
  filter(grouping == "All Transit Stops") |>
  select("place_id", total = "estimate")

a <- left_join(a, t, by=c("place_id")) |>
  mutate(share = estimate / total) |>
  select(-"total")

place_stop_data <- bind_rows(place_stop_data, a) |>
  mutate(year = as.character(year))

rm(a, t)

final_data <- place_stop_data |> 
  mutate(share = replace_na(share, 0)) |>
  mutate(place_id = factor(place_id, levels = place_order)) |> 
  mutate(grouping = factor(grouping, levels = grouping_order)) |>
  arrange(year, place_id, grouping)

saveRDS(final_data, file.path(process_dir, "data", "transit_stop_data.rds"))

transit_stop_lyr <- transit_stop_lyr |> 
  st_transform(wgs84) |>
  st_make_valid()

saveRDS(transit_stop_lyr, file.path(process_dir, 
                                    "data", 
                                    "transit_stop_lyr.rds"))

# Transit Routes ----------------------------------------------------------
transit_route <- transit_routes_by_mode(year = gtfs_year, 
                                        gtfs_feed = gtfs_file,
                                        data_month = 9)

transit_route_lyr <- transit_route |> 
  st_transform(wgs84) |>
  mutate(type_name = case_when(
    type_name == "ST Express" ~ "Bus",
    type_name == "BRT" ~ "Bus Rapid Transit",
    type_name %in% c("Streetcar", "Light Rail", "Monorail") ~ 
      "Light Rail, Monorail or Streetcar",
    type_name %in% c("Passenger Ferry", "Auto Ferry") ~ "Ferry",
    type_name == "Commuter Rail" ~ "Commuter Rail",
    type_name == "Bus" ~ "Bus")) |>
  st_make_valid()

# Trim the routes layer to only include the longest route variation for mapping
representative_route <- transit_route_lyr |> 
  mutate(length = st_length(geometry)) |>
  group_by(route_id) |>
  slice_max(order_by = length, n = 1, with_ties = FALSE) |>
  ungroup() |>
  filter(route_name != "Shuttle") |>
  drop_na()

saveRDS(representative_route, file.path(process_dir, 
                                        "data", 
                                        "transit_route_lyr.rds"))

