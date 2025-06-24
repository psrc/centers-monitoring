# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrcelmer)
library(sf)

# Basic Inputs ------------------------------------------------------------

# Silence the dplyr summarize message
options(dplyr.summarise.inform = FALSE)

# Population Years from OFM Parcelization
ofm_years <- c(2010, 2011, 2016, 2020, 2021, 2022, 2023, 2024)
hu_yrs <- c("2011", "2016", "2021", "2022", "2023", "2024")
max_ofm <- max(ofm_years)

# Make sure these match the boundary definition you want the data to be based on
rgc_title <- "regional_growth_center_2024_04_23"
mic_title <- "manufacturing_industrial_center_2024_04_23"

rgc_dashboard_title <- "Regional Growth Center (4/23/2024)"
mic_dashboard_title <- "MIC (1/5/2024)"

# Coordinate Reference Systems
wgs84 <- 4326
spn <- 2285

year_ord <- c("2024","2023","2022","2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010")

# Functions ---------------------------------------------------------------
process_ofm_data_for_centers <- function(yrs, dec = 0) {
  
  manual_data_overrides <- read_csv("data/manual_centers_population_housing_data.csv", show_col_types = FALSE)
  
  centers_population_housing <- NULL
  for (y in yrs) {
    
    # Parcel population
    print(str_glue("Loading {y} OFM based parcelized estimates of total population"))
    if (y < 2020) {
      ofm_vintage <- 2020
    } else {ofm_vintage <- max_ofm}
    
    q <- paste0("SELECT parcel_dim_id, estimate_year, total_pop, housing_units from ofm.parcelized_saep_facts WHERE ofm_vintage = ", ofm_vintage, " AND estimate_year = ", y, "")
    p <- get_query(sql = q)
    
    # Parcel Dimensions
    if (y >=2018) {parcel_yr <- 2018} else {parcel_yr <- 2014}
    print(str_glue("Loading {parcel_yr} parcel dimensions from Elmer"))
    q <- paste0("SELECT parcel_dim_id, parcel_id, county_name, ", rgc_title, ", ", mic_title, " from small_areas.parcel_dim WHERE base_year = ", parcel_yr, " ")
    d <- get_query(sql = q) |> 
      rename(rgc = all_of(rgc_title), mic = all_of(mic_title), county="county_name") |>
      # RGC Name Cleanup
      mutate(rgc = str_replace_all(rgc, "Greater Downtown Kirkland", "Kirkland Greater Downtown")) |>
      mutate(rgc = str_replace_all(rgc, "Redmond-Overlake", "Redmond Overlake")) |>
      mutate(rgc = str_replace_all(rgc, "Bellevue", "Bellevue Downtown")) |>
      mutate(rgc = str_replace_all(rgc, "Not in Center", "Not in a RGC")) |>
      # MIC Name Cleanup
      mutate(mic = str_replace_all(mic, "Paine Field / Boeing Everett", "Paine Field/Boeing Everett")) |>
      mutate(mic = str_replace_all(mic, "Sumner Pacific", "Sumner-Pacific")) |>
      mutate(mic = str_replace_all(mic, "Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton")) |>
      mutate(mic = str_replace_all(mic, "Cascade", "Cascade Industrial Center - Arlington/Marysville")) |>
      mutate(mic = str_replace_all(mic, "N/A", "Not in a MIC"))
    
    # Add MIC and RGC names to Parcels
    p <- left_join(p, d, by="parcel_dim_id")
    
    # RGC summaries
    print(str_glue("Summarizing RGC Data"))
    rgc_all <- p |>
      select(name="rgc", "total_pop", "housing_units") |>
      filter(name != "Not in a RGC") |>
      mutate(name= "All RGCs") |>
      group_by(name) |>
      summarise(population = round(sum(total_pop), dec), housing_units = round(sum(housing_units), dec)) |>
      as_tibble()
    
    rgc_individual <- p |>
      filter(rgc != "Not in a RGC") |>
      select(name="rgc", "total_pop", "housing_units") |>
      group_by(name) |>
      summarise(population = round(sum(total_pop), dec), housing_units = round(sum(housing_units), dec)) |>
      as_tibble()
    
    rgc <- bind_rows(rgc_all, rgc_individual) |>
      mutate(year = y, geography_type = "RGC")
    
    # MIC summaries
    print(str_glue("Summarizing MIC Data"))
    mic_all <- p |>
      select(name="mic", "total_pop", "housing_units") |>
      filter(name != "Not in a MIC") |>
      mutate(name= "All MICs") |>
      group_by(name) |>
      summarise(population = round(sum(total_pop), dec), housing_units = round(sum(housing_units), dec)) |>
      as_tibble()
    
    mic_individual <- p |>
      filter(mic != "Not in a MIC") |>
      select(name="mic", "total_pop", "housing_units") |>
      group_by(name) |>
      summarise(population = round(sum(total_pop), dec), housing_units = round(sum(housing_units), dec)) |>
      as_tibble()
    
    mic <- bind_rows(mic_all, mic_individual) |>
      mutate(year = y, geography_type = "MIC")
    
    print(str_glue("Combining Data and overwriting data with manual updates"))
    tbl <- bind_rows(rgc, mic) 
    tbl <- left_join(tbl, manual_data_overrides, by=c("name", "geography_type", "year")) |>
      mutate(pop = case_when(
        !(is.na(manual_population)) ~ manual_population,
        (is.na(manual_population)) ~ population)) |>
      mutate(hu = case_when(
        !(is.na(manual_housing_units)) ~ manual_housing_units,
        (is.na(manual_housing_units)) ~ housing_units)) |>
      select("name", population = "pop", housing_units = "hu", "year", "geography_type")
    
    if (is.null(centers_population_housing)) {centers_population_housing <- tbl} else {centers_population_housing <- bind_rows(centers_population_housing, tbl)}
    
  }
  
  print(str_glue("Formatting final table for dashboard processing"))
  final_tbl <- centers_population_housing |>
    mutate(geography_type = case_when(
      geography_type == "MIC" ~ mic_dashboard_title,
      geography_type == "RGC" ~ rgc_dashboard_title)) |>
    pivot_longer(cols = !c(name, year, geography_type), names_to = "grouping", values_to = "estimate") |>
    mutate(grouping = str_replace_all(grouping, "population", "Population")) |>
    mutate(grouping = str_replace_all(grouping, "housing_units", "Housing Units")) |>
    mutate(concept = "Center Overview", share = 1) |>
    mutate(year = factor(year, levels=year_ord)) |>
    select("year", geography = "name", "geography_type", "grouping", "estimate", "concept", "share")
  
  return(final_tbl)
  
}

# Centers Names & Types ---------------------------------------------------
rgc_names <- st_read_elmergeo(layer_name = "urban_centers") |> 
  select("name") |> 
  st_drop_geometry() |> 
  mutate(name = str_replace_all(name, "Redmond-Overlake", "Redmond Overlake")) |>
  mutate(name = str_replace_all(name, "Bellevue", "Bellevue Downtown")) |>
  mutate(name = str_replace_all(name, "Greater Downtown Kirkland", "Kirkland Greater Downtown")) |>
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

# Population and Housing Data for Centers ---------------------------------------------
population_housing <- process_ofm_data_for_centers(yrs =ofm_years, dec = -1)
saveRDS(population_housing, "data/pop_hsg_data.rds")

housing_unit_change <- population_housing |> 
  filter(grouping == "Housing Units" & year %in% hu_yrs) |>
  group_by(geography) |>
  mutate(delta = estimate-lag(estimate), data_year = paste0(lag(year),"-",year)) |>
  as_tibble() |>
  mutate(estimate = delta, concept = "New Net Housing Units") |>
  drop_na() |>
  select(-delta)

saveRDS(housing_unit_change, "data/housing_unit_data.rds")
