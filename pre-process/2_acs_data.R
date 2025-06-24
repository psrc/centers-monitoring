# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrccensus)
library(psrcelmer)
library(tidycensus)
library(sf)

# Inputs ------------------------------------------------------------------
analysis_years <- c(2013, 2018, 2023)
rgc_title <- "Regional Growth Center (4/23/2024)"
mic_title <- "MIC (1/5/2024)"
wgs84 <- 4326
spn <- 2285
options(dplyr.summarise.inform = FALSE)

# Factor Levels -----------------------------------------------------------
county_order <- c("Region", "King County", "Kitsap County", "Pierce County", "Snohomish County")

age_order <- c("Under 17", "18 to 34", "35 to 49", "50 to 64", "65+", "Total")

race_order <- c("American Indian\nand Alaska\nNative", "Asian", 
                "Black or African\nAmerican", "Hispanic or\nLatino", 
                "Native Hawaiian\nand Other\nPacific Islander",
                "Other", "Non-hispanic\nWhite", "Total")

income_order <- c("Less than\n$20,000", "$20,000 to\n$35,000", "$35,000 to\n$50,000",
                  "$50,000 to\n$75,000", "$75,000 to\n$100,000", "$100,000 to\n$150,000",
                  "$150,000 to\n$200,000", "$200,000 or\nmore", "Total")

tenure_order <- c("Renter", "Owner", "Total")

structure_order <- c("SF detached", "Moderate-low\ndensity", "Moderate-high\ndensity",
                     "High density", "Other", "Total")

burden_order <- c("Not cost\nburdened (<30%)", "Cost burdened\n(30-49.9%)",
                  "Severely cost\nburdened (50+%)", "Not computed", "Total")

education_order <- c("No high school\ndiploma", "High school", "Some college",
                     "Bachelor’s\ndegree","Graduate degree", "Total")

mode_order <- c("Drove\nAlone", "Carpooled", "Transit", "Bike", "Walk","Work from\nHome", "Other", "Total")

vehicle_order <- c("No vehicles","1 vehicle", "2 vehicles", "3 vehicles", "4 or more vehicles", "Total")

year_ord <- c("2024","2023","2022","2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010")

# Census Tables -----------------------------------------------------------
age_table <- "B01001"
race_table <- "B03002"
income_table <- "B19001"
tenure_table <- "B25003"
structure_table <- "B25024"
renter_burden_table <- "B25070"
owner_burden_table <- "B25091"
education_table <- "B15002"
mode_table <- "B08301"
vehicle_table <- "B08201"

# Variable Lookups --------------------------------------------------------
age_lookup <- data.frame(variable = c("B01001_002",
                                      "B01001_003", "B01001_004","B01001_005","B01001_006",
                                      "B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011", "B01001_012",
                                      "B01001_013", "B01001_014", "B01001_015",
                                      "B01001_016", "B01001_017", "B01001_018", "B01001_019",
                                      "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
                                      "B01001_026",
                                      "B01001_027", "B01001_028","B01001_029","B01001_030",
                                      "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035", "B01001_036",
                                      "B01001_037", "B01001_038", "B01001_039",
                                      "B01001_040", "B01001_041", "B01001_042", "B01001_043",
                                      "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049"),
                         gender = c("Male",
                                    "Male", "Male", "Male", "Male",
                                    "Male", "Male", "Male", "Male", "Male", "Male",
                                    "Male", "Male", "Male",
                                    "Male", "Male", "Male", "Male",
                                    "Male", "Male", "Male", "Male", "Male", "Male",
                                    "Female",
                                    "Female", "Female", "Female", "Female",
                                    "Female", "Female", "Female", "Female", "Female", "Female",
                                    "Female", "Female", "Female",
                                    "Female", "Female", "Female", "Female",
                                    "Female", "Female", "Female", "Female", "Female", "Female"),
                         grouping = c("Total",
                                      "Under 17", "Under 17", "Under 17", "Under 17",
                                      "18 to 34", "18 to 34", "18 to 34", "18 to 34", "18 to 34", "18 to 34",
                                      "35 to 49", "35 to 49", "35 to 49",
                                      "50 to 64", "50 to 64", "50 to 64", "50 to 64",
                                      "65+", "65+", "65+", "65+", "65+", "65+",
                                      "Total",
                                      "Under 17", "Under 17", "Under 17", "Under 17",
                                      "18 to 34", "18 to 34", "18 to 34", "18 to 34", "18 to 34", "18 to 34",
                                      "35 to 49", "35 to 49", "35 to 49",
                                      "50 to 64", "50 to 64", "50 to 64", "50 to 64",
                                      "65+", "65+", "65+", "65+", "65+", "65+"))

race_lookup <- data.frame(variable = c("B03002_001",
                                       "B03002_003",
                                       "B03002_004",
                                       "B03002_005",
                                       "B03002_006",
                                       "B03002_007",
                                       "B03002_008", "B03002_009",
                                       "B03002_012"),
                          grouping = c("Total",
                                       "Non-hispanic White",
                                       "Black or African American",
                                       "American Indian and Alaska Native",
                                       "Asian",
                                       "Native Hawaiian and Other Pacific Islander", 
                                       "Other", "Other",
                                       "Hispanic or Latino"))

income_lookup <- data.frame(variable = c("B19001_001",
                                         "B19001_002", "B19001_003","B19001_004",
                                         "B19001_005", "B19001_006", "B19001_007", 
                                         "B19001_008", "B19001_009", "B19001_010",
                                         "B19001_011", "B19001_012",
                                         "B19001_013", 
                                         "B19001_014", "B19001_015",
                                         "B19001_016", 
                                         "B19001_017"),
                            grouping = c("Total",
                                         "Less than $20,000", "Less than $20,000", "Less than $20,000",
                                         "$20,000 to $35,000", "$20,000 to $35,000", "$20,000 to $35,000",
                                         "$35,000 to $50,000", "$35,000 to $50,000", "$35,000 to $50,000",
                                         "$50,000 to $75,000", "$50,000 to $75,000",
                                         "$75,000 to $100,000", 
                                         "$100,000 to $150,000", "$100,000 to $150,000",
                                         "$150,000 to $200,000", 
                                         "$200,000 or more"))

tenure_lookup <- data.frame(variable = c("B25003_001",
                                         "B25003_002", 
                                         "B25003_003"),
                            grouping = c("Total",
                                         "Owner", 
                                         "Renter"))

structure_lookup <- data.frame(variable = c("B25024_001",
                                            "B25024_002",
                                            "B25024_003", "B25024_004", "B25024_005", "B25024_006",
                                            "B25024_007",
                                            "B25024_008", "B25024_009",
                                            "B25024_010", "B25024_011"),
                               grouping = c("Total",
                                            "SF detached",
                                            "Moderate-low density", "Moderate-low density", "Moderate-low density", "Moderate-low density",
                                            "Moderate-high density",
                                            "High density", "High density",
                                            "Other", "Other"))

renter_burden_lookup <- data.frame(variable = c("B25070_001",
                                                "B25070_002", "B25070_003", "B25070_004", "B25070_005", "B25070_006",
                                                "B25070_007", "B25070_008", "B25070_009",
                                                "B25070_010", 
                                                "B25070_011"),
                                   grouping = c("Total",
                                                "Not cost burdened (<30%)", "Not cost burdened (<30%)", "Not cost burdened (<30%)", "Not cost burdened (<30%)", "Not cost burdened (<30%)",
                                                "Cost burdened (30-49.9%)", "Cost burdened (30-49.9%)", "Cost burdened (30-49.9%)",
                                                "Severely cost burdened (50+%)",
                                                "Not computed"))

owner_burden_lookup <- data.frame(variable = c("B25091_002",
                                               "B25091_003", "B25091_004", "B25091_005", "B25091_006", "B25091_007",
                                               "B25091_008", "B25091_009", "B25091_010",
                                               "B25091_011",
                                               "B25091_012"),
                                  grouping = c("Total",
                                               "Not cost burdened (<30%)", "Not cost burdened (<30%)", "Not cost burdened (<30%)", "Not cost burdened (<30%)", "Not cost burdened (<30%)",
                                               "Cost burdened (30-49.9%)", "Cost burdened (30-49.9%)", "Cost burdened (30-49.9%)",
                                               "Severely cost burdened (50+%)",
                                               "Not computed"))

education_lookup <- data.frame(variable = c("B15002_001",
                                            "B15002_003", "B15002_004","B15002_005","B15002_006", "B15002_007", "B15002_008", "B15002_009", "B15002_010",
                                            "B15002_011",
                                            "B15002_012", "B15002_013", "B15002_014",
                                            "B15002_015",
                                            "B15002_016", "B15002_017", "B15002_018",
                                            "B15002_020", "B15002_021","B15002_022","B15002_023", "B15002_024", "B15002_025", "B15002_026", "B15002_027",
                                            "B15002_028",
                                            "B15002_029", "B15002_030", "B15002_031",
                                            "B15002_032",
                                            "B15002_033", "B15002_034", "B15002_035"),
                               grouping = c("Total",
                                            "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma",
                                            "High school",
                                            "Some college", "Some college", "Some college",
                                            "Bachelor’s degree",
                                            "Graduate degree", "Graduate degree", "Graduate degree",
                                            "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma", "No high school diploma",
                                            "High school",
                                            "Some college", "Some college", "Some college",
                                            "Bachelor’s degree",
                                            "Graduate degree", "Graduate degree", "Graduate degree"))

mode_lookup <- data.frame(variable = c("B08301_001",
                                       "B08301_003", 
                                       "B08301_004",
                                       "B08301_010",
                                       "B08301_016", "B08301_017", "B08301_020",
                                       "B08301_018", 
                                       "B08301_019",
                                       "B08301_021"),
                          grouping = c("Total",
                                       "Drove Alone",
                                       "Carpooled",
                                       "Transit",
                                       "Other", "Other", "Other",
                                       "Bike",
                                       "Walk",
                                       "Work from Home"))

vehicle_lookup <- data.frame(variable = c("B08201_001",
                                          "B08201_002",
                                          "B08201_003",
                                          "B08201_004",
                                          "B08201_005",
                                          "B08201_006"),
                             grouping = c("Total",
                                          "No vehicles",
                                          "1 vehicle", 
                                          "2 vehicles", 
                                          "3 vehicles", 
                                          "4 or more vehicles"))

# Functions ---------------------------------------------------------------
generate_blockgroup_splits <- function(y) {
  
  if (y >=2020) {
    ofm_vin <- y
    geog_yr <- 'blockgroup20'
    
  } else {
    ofm_vin <- 2020
    geog_yr <- 'blockgroup10'}
  
  if (y >=2018) {parcel_yr <- 2018} else {parcel_yr <- 2014}
  
  # Regional Growth Centers 
  print(str_glue("Getting Blockgroup splits from Elmer for {geog_yr} for {rgc_title} for the year {y}"))
  qr <- paste0("SELECT * FROM general.get_geography_splits('", geog_yr , "', '", rgc_title, "', ", y, ", ", ofm_vin,", ", parcel_yr,")")
  
  # Manufacturing & Industrial Centers 
  print(str_glue("Getting Blockgroup splits from Elmer for {geog_yr} for {mic_title} for the year {y}"))
  qm <- paste0("SELECT * FROM general.get_geography_splits('", geog_yr , "', '", mic_title, "', ", y, ", ", ofm_vin,", ", parcel_yr,")")
  
  rgc <- get_query(sql = qr, db_name = "Elmer") |> filter(planning_geog != "not in regional growth center")
  mic <- get_query(sql = qm, db_name = "Elmer") |> filter(planning_geog != "not in MIC")
  
  splits <- bind_rows(rgc, mic)
  
  splits <- splits |> 
    mutate(planning_geog = str_replace_all(planning_geog, "Greater Downtown Kirkland", "Kirkland Greater Downtown")) |>
    mutate(planning_geog = str_replace_all(planning_geog, "Redmond-Overlake", "Redmond Overlake")) |>
    mutate(planning_geog = str_replace_all(planning_geog, "Bellevue", "Bellevue Downtown")) |>
    mutate(planning_geog = str_replace_all(planning_geog,"Cascade", "Cascade Industrial Center - Arlington/Marysville")) |>
    mutate(planning_geog = str_replace_all(planning_geog,"Paine Field / Boeing Everett", "Paine Field/Boeing Everett")) |>
    mutate(planning_geog = str_replace_all(planning_geog,"Sumner Pacific", "Sumner-Pacific")) |>
    mutate(planning_geog = str_replace_all(planning_geog,"Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton"))
  
  return(splits)
  
}

generate_tract_splits <- function(y) {
  
  if (y >=2020) {
    ofm_vin <- y
    geog_yr <- 'tract20'
    
  } else {
    ofm_vin <- 2020
    geog_yr <- 'tract10'}
  
  if (y >=2018) {parcel_yr <- 2018} else {parcel_yr <- 2014}
  
  # Regional Growth Centers 
  print(str_glue("Getting tract splits from Elmer for {geog_yr} for {rgc_title} for the year {y}"))
  qr <- paste0("SELECT * FROM general.get_geography_splits('", geog_yr , "', '", rgc_title, "', ", y, ", ", ofm_vin,", ", parcel_yr,")")
  
  # Manufacturing & Industrial Centers 
  print(str_glue("Getting tract splits from Elmer for {geog_yr} for {mic_title} for the year {y}"))
  qm <- paste0("SELECT * FROM general.get_geography_splits('", geog_yr , "', '", mic_title, "', ", y, ", ", ofm_vin,", ", parcel_yr,")")
  
  rgc <- get_query(sql = qr, db_name = "Elmer") |> filter(planning_geog != "not in regional growth center")
  mic <- get_query(sql = qm, db_name = "Elmer") |> filter(planning_geog != "not in MIC")
  
  splits <- bind_rows(rgc, mic)
  
  splits <- splits |> 
    mutate(planning_geog = str_replace_all(planning_geog, "Greater Downtown Kirkland", "Kirkland Greater Downtown")) |>
    mutate(planning_geog = str_replace_all(planning_geog, "Redmond-Overlake", "Redmond Overlake")) |>
    mutate(planning_geog = str_replace_all(planning_geog, "Bellevue", "Bellevue Downtown")) |>
    mutate(planning_geog = str_replace_all(planning_geog,"Cascade", "Cascade Industrial Center - Arlington/Marysville")) |>
    mutate(planning_geog = str_replace_all(planning_geog,"Paine Field / Boeing Everett", "Paine Field/Boeing Everett")) |>
    mutate(planning_geog = str_replace_all(planning_geog,"Sumner Pacific", "Sumner-Pacific")) |>
    mutate(planning_geog = str_replace_all(planning_geog,"Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton"))
  
  return(splits)
  
}

geography_estimate_from_bg <- function(split_df, estimate_df, geography_type, split_type, geography_name) {
  
  if (geography_name %in% c("All RGCs", "All MICs")) {
    
    t <- split_df |> filter(planning_geog_type == geography_type) |> mutate(planning_geog = geography_name)
    
  } else {
    
    t <- split_df |> filter(planning_geog_type == geography_type & planning_geog == geography_name)
    
  }
  
  # Filter Blockgroup Splits to Geography
  t <- t |> select("year", "geoid", name = "planning_geog", share = all_of(split_type)) |> mutate(year = as.character(year))
  
  d <- estimate_df |> select("year","geoid", "grouping", "concept","estimate")
  
  c <- left_join(t, d, by=c("year", "geoid"), relationship = "many-to-many") |>
    mutate(place_estimate = round(estimate*share,0)) |>
    group_by(year, name, grouping, concept) |>
    summarise(estimate = sum(place_estimate)) |>
    as_tibble() |>
    rename(geography="name") |>
    mutate(geography_type = geography_type)
  
  totals <- c |>
    filter(grouping == "Total") |>
    select("geography", "year", "concept", total="estimate")
  
  c <- left_join(c, totals, by=c("geography", "year", "concept")) |>
    mutate(share = estimate / total) |>
    select(-"total")
  
  return(c)
  
}

acs_data_centers <- function(tbl, yrs, lookup, total, group, geog_ord, grouping_ord, geog_split, wrap_width, geog_census) {
  
  # County
  county <- get_acs_recs(geography="county", table.names = tbl, years = yrs, acs.type = 'acs5') 
  
  county <- left_join(county, lookup, by=c("variable")) |>
    filter(variable != total) |>
    filter(!(is.na(grouping))) |>
    select(geography="name", "estimate", "moe", "year", "grouping") |>
    group_by(geography, year, grouping) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(concept = group)
  
  totals <- county |> 
    filter(grouping == "Total") |> 
    select("geography", "year", "concept", total="estimate")
  
  county <- left_join(county, totals, by=c("geography", "year", "concept")) |>
    mutate(share = estimate / total) |>
    select(-"total") |>
    mutate(geography_type = "County") |>
    mutate(grouping = str_wrap(grouping, width = wrap_width)) |>
    mutate(geography = factor(geography, levels = geog_ord)) |>
    mutate(grouping = factor(grouping, levels = grouping_ord)) |>
    mutate(year = as.character(year)) |>
    arrange(geography, grouping, year)
  
  rm(totals)
  
  # Blockgroups or tracts
  geog_data <- get_acs_recs(geography=geog_census, table.names = tbl, years = yrs, acs.type = 'acs5') 
  
  geog_data <- left_join(geog_data, lookup, by=c("variable")) |>
    filter(variable != total) |>
    filter(!(is.na(grouping))) |>
    select(geoid="GEOID", "estimate", "moe", "year", "grouping") |>
    group_by(geoid, year, grouping) |>
    summarise(estimate = sum(estimate)) |>
    as_tibble() |>
    mutate(concept = group) |>
    mutate(geography_type = geog_census) |>
    mutate(grouping = str_wrap(grouping, width = wrap_width)) |>
    mutate(grouping = factor(grouping, levels = grouping_ord)) |>
    mutate(year = as.character(year)) |>
    arrange(geoid, grouping, year)
  
  if(geog_census == "block group") {splits <- blockgroup_splits}
  if(geog_census == "tract") {splits <- tract_splits}
  
  # Centers
  places <- NULL
  for(place in c(rgc_names, "All RGCs")) {
    df <- geography_estimate_from_bg(split_df = splits, geography_type = rgc_title, estimate_df=geog_data, split_type = geog_split, geography_name=place)
    ifelse(is.null(places), places <- df, places <- bind_rows(places, df))
    rm(df)
  }
  
  for(place in c(mic_names, "All MICs")) {
    df <- geography_estimate_from_bg(split_df = splits, geography_type = mic_title, estimate_df=geog_data, split_type = geog_split, geography_name=place)
    ifelse(is.null(places), places <- df, places <- bind_rows(places, df))
    rm(df)
  }
  
  df <- bind_rows(county, places)
  ord <- unique(c(geog_ord, "All RGCs", rgc_names, "All MICs", mic_names))
  df <- df |> 
    mutate(geography = factor(geography, levels = ord)) |> 
    mutate(year = factor(year, levels=year_ord)) |>
    mutate(estimate = round(estimate, -1)) |>
    arrange(geography, grouping, year)
  
  return(df)
  
}

# Center Shapefiles -------------------------------------------------------
rgc_shape <- st_read_elmergeo(layer_name = "urban_centers") |>
  st_transform(crs = spn) |>
  mutate(name = str_replace_all(name, "Greater Downtown Kirkland", "Kirkland Greater Downtown")) |>
  mutate(name = str_replace_all(name, "Redmond-Overlake", "Redmond Overlake")) |>
  mutate(name = str_replace_all(name, "Bellevue", "Bellevue Downtown")) |>
  select("name", "category", "acres") |>
  arrange(name)

rgc_names <- rgc_shape |> 
  select("name") |> 
  st_drop_geometry() |> 
  pull() |>
  unique() |>
  sort()

mic_shape <- st_read_elmergeo(layer_name = "micen") |>
  mutate(mic = str_replace_all(mic,"Cascade", "Cascade Industrial Center - Arlington/Marysville")) |>
  mutate(mic = str_replace_all(mic,"Paine Field / Boeing Everett", "Paine Field/Boeing Everett")) |>
  mutate(mic = str_replace_all(mic,"Sumner Pacific", "Sumner-Pacific")) |>
  mutate(mic = str_replace_all(mic,"Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton")) |>
  select(name="mic", "category", "acres")

mic_names <- mic_shape |> 
  select("name") |> 
  st_drop_geometry() |> 
  pull() |>
  unique() |>
  sort()

# Blockgroup and Tract Splits -------------------------------------------------------
blockgroup_splits <- NULL
for (yr in analysis_years) {
  splits <- generate_blockgroup_splits(y = yr)
  if (is.null(blockgroup_splits)) {blockgroup_splits <- splits} else {blockgroup_splits <- bind_rows(blockgroup_splits, splits)}
  rm(splits)
}

blockgroup_splits <- blockgroup_splits |>
  select(year = "ofm_estimate_year", geoid = "data_geog", "planning_geog_type", "planning_geog", "percent_of_total_pop", "percent_of_household_pop", "percent_of_housing_units", "percent_of_occupied_housing_units")

saveRDS(blockgroup_splits, "data/blockgroup_splits.rds")

tract_splits <- NULL
for (yr in analysis_years) {
  splits <- generate_tract_splits(y = yr)
  if (is.null(tract_splits)) {tract_splits <- splits} else {tract_splits <- bind_rows(tract_splits, splits)}
  rm(splits)
}

tract_splits <- tract_splits |>
  select(year = "ofm_estimate_year", geoid = "data_geog", "planning_geog_type", "planning_geog", "percent_of_total_pop", "percent_of_household_pop", "percent_of_housing_units", "percent_of_occupied_housing_units")

saveRDS(tract_splits, "data/tract_splits.rds")

# Age Distribution --------------------------------------------------------
population_by_age <- acs_data_centers(tbl = age_table,
                                      yrs = analysis_years,
                                      lookup = age_lookup,
                                      total = "B01001_001",
                                      group = "Population by Age Group",
                                      geog_ord = county_order,
                                      grouping_ord = age_order,
                                      geog_split = "percent_of_total_pop",
                                      wrap_width = 16,
                                      geog_census = "block group")

saveRDS(population_by_age, "data/population_by_age.rds")

# Race & Ethnicty ---------------------------------------------------------
population_by_race <- acs_data_centers(tbl = race_table,
                                       yrs = analysis_years,
                                       lookup = race_lookup,
                                       total = "B01001_001",
                                       group = "Population by Race & Ethnicity",
                                       geog_ord = county_order,
                                       grouping_ord = race_order,
                                       geog_split = "percent_of_total_pop",
                                       wrap_width = 16,
                                       geog_census = "block group")

saveRDS(population_by_race, "data/population_by_race.rds")

# Income ------------------------------------------------------------------
households_by_income <- acs_data_centers(tbl = income_table,
                                         yrs = analysis_years,
                                         lookup = income_lookup,
                                         total = "B01001_001",
                                         group = "Households by Income",
                                         geog_ord = county_order,
                                         grouping_ord = income_order,
                                         geog_split = "percent_of_occupied_housing_units",
                                         wrap_width = 11,
                                         geog_census = "block group")

saveRDS(households_by_income, "data/households_by_income.rds")

# Housing Tenure ----------------------------------------------------------
households_by_tenure <- acs_data_centers(tbl = tenure_table,
                                         yrs = analysis_years,
                                         lookup = tenure_lookup,
                                         total = "B01001_001",
                                         group = "Household Tenure",
                                         geog_ord = county_order,
                                         grouping_ord = tenure_order,
                                         geog_split = "percent_of_occupied_housing_units",
                                         wrap_width = 11,
                                         geog_census = "block group")

saveRDS(households_by_tenure, "data/households_by_tenure.rds")

# Structure Type ----------------------------------------------------------
housing_units_by_type <- acs_data_centers(tbl = structure_table,
                                         yrs = analysis_years,
                                         lookup = structure_lookup,
                                         total = "B01001_001",
                                         group = "Housing Unit Type",
                                         geog_ord = county_order,
                                         grouping_ord = structure_order,
                                         geog_split = "percent_of_occupied_housing_units",
                                         wrap_width = 15,
                                         geog_census = "block group")

saveRDS(housing_units_by_type, "data/housing_units_by_type.rds")

# Renter Cost Burden ----------------------------------------------------------
renter_cost_burden <- acs_data_centers(tbl = renter_burden_table,
                                       yrs = analysis_years,
                                       lookup = renter_burden_lookup,
                                       total = "B01001_001",
                                       group = "Renter Cost Burden",
                                       geog_ord = county_order,
                                       grouping_ord = burden_order,
                                       geog_split = "percent_of_occupied_housing_units",
                                       wrap_width = 15,
                                       geog_census = "block group")

saveRDS(renter_cost_burden, "data/renter_cost_burden.rds")

# Owner Cost Burden ----------------------------------------------------------
owner_cost_burden <- acs_data_centers(tbl = owner_burden_table,
                                      yrs = analysis_years,
                                      lookup = owner_burden_lookup,
                                      total = "B01001_001",
                                      group = "Owner Cost Burden",
                                      geog_ord = county_order,
                                      grouping_ord = burden_order,
                                      geog_split = "percent_of_occupied_housing_units",
                                      wrap_width = 15,
                                      geog_census = "block group")

saveRDS(owner_cost_burden, "data/owner_cost_burden.rds")

# Educational Attainment --------------------------------------------------
educational_attainment <- acs_data_centers(tbl = education_table,
                                           yrs = analysis_years,
                                           lookup = education_lookup,
                                           total = "B01001_001",
                                           group = "Educational Attainment",
                                           geog_ord = county_order,
                                           grouping_ord = education_order,
                                           geog_split = "percent_of_total_pop",
                                           wrap_width = 15,
                                           geog_census = "block group")

saveRDS(educational_attainment, "data/educational_attainment.rds")

# Mode Share to Work ------------------------------------------------------
mode_to_work <- acs_data_centers(tbl = mode_table,
                                 yrs = analysis_years,
                                 lookup = mode_lookup,
                                 total = "B01001_001",
                                 group = "Mode to Work",
                                 geog_ord = county_order,
                                 grouping_ord = mode_order,
                                 geog_split = "percent_of_occupied_housing_units",
                                 wrap_width = 10,
                                 geog_census = "block group")

saveRDS(mode_to_work, "data/mode_to_work.rds")

# Vehicles Available --------------------------------------------------------
households_by_vehicles <- acs_data_centers(tbl = vehicle_table,
                                           yrs = analysis_years,
                                           lookup = vehicle_lookup,
                                           total = "B01001_001",
                                           group = "Households by Vehicle Availability",
                                           geog_ord = county_order,
                                           grouping_ord = vehicle_order,
                                           geog_split = "percent_of_occupied_housing_units",
                                           wrap_width = 20,
                                           geog_census = "tract")

saveRDS(households_by_vehicles, "data/households_by_vehicles.rds")
