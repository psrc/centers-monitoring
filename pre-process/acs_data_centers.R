# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrccensus)
library(psrcelmer)
library(tidycensus)
library(sf)

# Inputs ------------------------------------------------------------------
pre_api_year <- 2011
api_years <- c(2016, 2021)
acs_pre2013_bg_dir <- "C:/coding/acs_blockgroups_pre2013"

# Factor Levels
county_order <- c("Region", "King County", "Kitsap County", "Pierce County", "Snohomish County")
age_order <- c("Under 17", "18 to 34", "35 to 49", "50 to 64", "65+", "Total")
race_order <- c("American Indian\nand Alaska\nNative", "Asian", 
                "Black or African\nAmerican", "Hispanic or\nLatino", 
                "Native Hawaiian\nand Other\nPacific Islander",
                "Other", "Non-hispanic\nWhite", "Total")
income_order <- c("Less than\n$20,000", "$20,000 to\n$35,000", "$35,000 to\n$50,000",
                  "$50,000 to\n$75,000", "$75,000 to\n$100,000", "$100,000 to\n$150,000",
                  "$150,000 to\n$200,000", "$200,000 or\nmore", "Total")
pop_hh_hu_order <- c("Population", "Households", "Housing Units")
tenure_order <- c("Renter", "Owner", "Total")

# Census Tables
age_table <- "B01001"
race_table <- "B03002"
income_table <- "B19001"
tenure_table <- "B25003"

# Employment Data
rgc_emp_file <- "data/rgc_covered_emp_2010_2021_revised_20230705.csv"
mic_emp_file <- "data/mic_covered_emp_2010_2021.csv"

# Pre-2013 ACS Blockgroup Files -------------------------------------------
age_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_", age_table, "_150.xlsx"))
race_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_", race_table, "_150.xlsx"))
income_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_", income_table, "_150.xlsx"))
tenure_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_", tenure_table, "_150.xlsx"))
structures_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_B25024_150.xlsx"))
education_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_B15002_150.xlsx"))

# Functions ---------------------------------------------------------------
centers_estimate_from_bg <- function(split_df=blockgroup_splits, estimate_df=blockgroups, center_type, split_type) {
  
  # Filter Blockgroup Splits to Center
  t <- split_df %>%
    filter(planning_geog_type == center_type & planning_geog == center) %>%
    select(year = "ofm_estimate_year", geography = "data_geog", name = "planning_geog", share = all_of(split_type)) %>%
    mutate(year = as.character(year))
  
  d <- estimate_df %>%
    select("year","geography", "grouping", "concept","estimate")
  
  c <- left_join(t, d, by=c("year", "geography")) %>%
    mutate(center_estimate = round(estimate*share,0)) %>%
    group_by(year, name, grouping, concept) %>%
    summarise(estimate = sum(center_estimate)) %>%
    as_tibble() %>%
    rename(geography="name") %>%
    mutate(geography_type = center_type)
  
  totals <- c %>%
    filter(grouping == "Total") %>%
    select("geography", "year", "concept", total="estimate")
  
  c <- left_join(c, totals, by=c("geography", "year", "concept")) %>%
    mutate(share = estimate / total) %>%
    select(-"total")
  
  return(c)
  
}

# Blockgroup Splits from Elmer --------------------------------------------
blockgroup_splits <- NULL
for (y in c(pre_api_year, api_years)) {
  
  if (y >=2020) {
    ofm_vin <- 2022
    geog_yr <- 'blockgroup20'} 
  
  else {
    ofm_vin <- 2020
    geog_yr <- 'blockgroup10'}
  
  if (y >=2018) {parcel_yr <- 2018} else {parcel_yr <- 2014}
  
  q <- paste0("SELECT * FROM general.get_geography_splits('",geog_yr,"', 'Regional Growth Center (6/22/2023)'," , y, ", ",ofm_vin,", ",parcel_yr,")")
  rgc <- get_query(sql = q, db_name = "Elmer")
  
  q <- paste0("SELECT * FROM general.get_geography_splits('",geog_yr,"', 'MIC (2022 RTP)'," , y, ", ",ofm_vin,", ",parcel_yr,")")
  mic <- get_query(sql = q, db_name = "Elmer")
  
  splits <- bind_rows(rgc, mic)
  
  if (is.null(blockgroup_splits)) {blockgroup_splits <- splits} else {blockgroup_splits <- bind_rows(blockgroup_splits, splits)}
  rm(rgc, mic, splits, q)
  
}

rm(y, ofm_vin, geog_yr, parcel_yr)

blockgroup_splits <- blockgroup_splits %>%
  mutate(planning_geog = gsub("Cascade", "Cascade Industrial Center - Arlington/Marysville", planning_geog)) %>%
  mutate(planning_geog = gsub("Bellevue", "Bellevue Downtown", planning_geog)) %>%
  mutate(planning_geog = gsub("Kent MIC", "Kent", planning_geog)) %>%
  mutate(planning_geog = gsub("Paine Field / Boeing Everett", "Paine Field/Boeing Everett", planning_geog)) %>%
  mutate(planning_geog = gsub("Sumner Pacific", "Sumner-Pacific", planning_geog)) %>%
  mutate(planning_geog = gsub("Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton", planning_geog)) %>%
  mutate(planning_geog = gsub("Redmond-Overlake", "Redmond Overlake", planning_geog))

# List of Centers ---------------------------------------------------------
rgc_names <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Regional_Growth_Centers/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>%
  select("name") %>%
  st_drop_geometry() %>%
  mutate(name = gsub("Redmond-Overlake", "Redmond Overlake", name)) %>%
  mutate(name = gsub("Bellevue", "Bellevue Downtown", name)) %>%
  pull() %>%
  unique()

mic_names <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Manufacturing_Industrial_Centers/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>%
  select("mic") %>%
  st_drop_geometry() %>%
  mutate(mic = gsub("Kent MIC", "Kent", mic)) %>%
  mutate(mic = gsub("Paine Field / Boeing Everett", "Paine Field/Boeing Everett", mic)) %>%
  mutate(mic = gsub("Sumner Pacific", "Sumner-Pacific", mic)) %>%
  mutate(mic = gsub("Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton", mic)) %>%
  mutate(mic = gsub("Cascade", "Cascade Industrial Center - Arlington/Marysville", mic)) %>%
  pull() %>%
  unique()

# Total Jobs from QCEW ----------------------------------------------------
rgc_jobs <- read_csv(rgc_emp_file) %>% 
  filter(center != "RGC Total") %>%
  mutate(Government = as.character(Government), `Public Education` = as.character(`Public Education`)) %>%
  pivot_longer(cols = !c(year,center), names_to = "grouping", values_to = "estimate") %>%
  rename(geography="center") %>%
  mutate(estimate = gsub("S", "*", estimate)) %>%
  mutate(geography = gsub("Redmond-Overlake", "Redmond Overlake", geography)) %>%
  mutate(geography = gsub("Bellevue", "Bellevue Downtown", geography)) %>%
  mutate(concept = "Employment", share=1, geography_type = "Regional Growth Center (6/22/2023)")

mic_jobs <- read_csv(mic_emp_file) %>% 
  filter(center != "MIC Total") %>%
  rename(`Public Education` = "Public_Education") %>%
  mutate(Const_Res = as.character(Const_Res), Government = as.character(Government), `Public Education` = as.character(`Public Education`)) %>%
  mutate(WTU = as.character(WTU), Total = as.character(Total)) %>%
  pivot_longer(cols = !c(year,center), names_to = "grouping", values_to = "estimate") %>%
  rename(geography="center") %>%
  mutate(estimate = gsub("S", "*", estimate)) %>%
  mutate(geography = gsub("Kent MIC", "Kent", geography)) %>%
  mutate(geography = gsub("Paine Field / Boeing Everett", "Paine Field/Boeing Everett", geography)) %>%
  mutate(geography = gsub("Sumner Pacific", "Sumner-Pacific", geography)) %>%
  mutate(geography = gsub("Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton", geography)) %>%
  mutate(geography = gsub("Cascade", "Cascade Industrial Center - Arlington/Marysville", geography)) %>%
  mutate(concept = "Employment", share=1, geography_type = "MIC (2022 RTP)")

centers_employment <- bind_rows(rgc_jobs, mic_jobs)
rm(rgc_jobs, mic_jobs)
saveRDS(centers_employment, "data/centers_employment.rds")

# Total Population from Parcelization -------------------------------------
parcel_facts <- get_table(schema='ofm', tbl_name='parcelized_saep_facts')
parcel_dims <- get_table(schema = 'small_areas', tbl_name = 'parcel_dim')

parcel_geo <- parcel_dims %>% 
  select("parcel_dim_id", "parcel_id", parcel_year="base_year", County="county_name", rgc="regional_growth_center_2023_06_22", mic="manufacturing_industrial_center_2022") %>%
  mutate(rgc = str_replace_all(rgc, "N/A", "Not in Center"), mic = str_replace_all(mic, "N/A","Not in Center")) %>%
  mutate(`Center Name` = case_when(
    rgc != "Not in Center" ~ rgc,
    mic != "Not in Center" ~ mic,
    rgc == "Not in Center" & mic == "Not in Center" ~ "Not in Center")) %>%
  mutate(`Center Type` = case_when(
    rgc != "Not in Center" ~ "Regional Growth Center",
    mic != "Not in Center" ~ "Manufacturing & Industrial Center",
    rgc == "Not in Center" & mic == "Not in Center" ~ ""))

ofm_years <- c(pre_api_year, api_years, 2022)
parcels <- left_join(parcel_facts, parcel_geo, by=c("parcel_dim_id")) %>%
  filter(estimate_year %in% ofm_years) 

rgc_data <- parcels %>%
  select(year="estimate_year", geography="rgc", "total_pop", "housing_units", "occupied_housing_units") %>%
  group_by(year, geography) %>%
  summarise(population=sum(total_pop), housing_units=sum(housing_units), households=sum(occupied_housing_units)) %>%
  as_tibble() %>%
  filter(geography != "Not in Center") %>%
  mutate(geography_type = "Regional Growth Center (6/22/2023)") %>%
  pivot_longer(cols = c(population, households, housing_units), names_to = "grouping", values_to = "estimate") %>%
  mutate(estimate = round(estimate,0), concept = "Center Overview", share=1) %>%
  mutate(geography = gsub("Redmond-Overlake", "Redmond Overlake", geography)) %>%
  mutate(geography = gsub("Bellevue", "Bellevue Downtown", geography)) %>%
  mutate(grouping = gsub("households", "Households", grouping)) %>%
  mutate(grouping = gsub("housing_units", "Housing Units", grouping)) %>%
  mutate(grouping = gsub("population", "Population", grouping)) %>%
  mutate(grouping = factor(grouping, levels = pop_hh_hu_order)) %>%
  arrange(geography, grouping, year)

mic_data <- parcels %>%
  select(year="estimate_year", geography="mic", "total_pop", "housing_units", "occupied_housing_units") %>%
  group_by(year, geography) %>%
  summarise(population=sum(total_pop), housing_units=sum(housing_units), households=sum(occupied_housing_units)) %>%
  as_tibble() %>%
  filter(geography != "Not in Center") %>%
  mutate(geography_type = "MIC (2022 RTP)") %>%
  pivot_longer(cols = c(population, households, housing_units), names_to = "grouping", values_to = "estimate") %>%
  mutate(estimate = round(estimate,0), concept = "Center Overview", share=1) %>%
  mutate(geography = gsub("Kent MIC", "Kent", geography)) %>%
  mutate(geography = gsub("Paine Field / Boeing Everett", "Paine Field/Boeing Everett", geography)) %>%
  mutate(geography = gsub("Sumner Pacific", "Sumner-Pacific", geography)) %>%
  mutate(geography = gsub("Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton", geography)) %>%
  mutate(geography = gsub("Cascade", "Cascade Industrial Center - Arlington/Marysville", geography)) %>%
  mutate(grouping = gsub("households", "Households", grouping)) %>%
  mutate(grouping = gsub("housing_units", "Housing Units", grouping)) %>%
  mutate(grouping = gsub("population", "Population", grouping)) %>%
  mutate(grouping = factor(grouping, levels = pop_hh_hu_order)) %>%
  arrange(geography, grouping, year)

center_pop_hh_hu <- bind_rows(rgc_data, mic_data)
rm(rgc_data, mic_data, parcels, parcel_geo)
saveRDS(center_pop_hh_hu, "data/center_pop_hh_hu.rds")

# Age Distribution --------------------------------------------------------
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

# County
county <- get_acs_recs(geography="county", table.names = age_table, years = c(pre_api_year, api_years), acs.type = 'acs5') 

county <- left_join(county, age_lookup, by=c("variable")) %>%
  filter(variable != "B01001_001") %>%
  select(geography="name", "estimate", "moe", "year", "gender", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Population by Age Group")

totals <- county %>%
  filter(grouping == "Total") %>%
  select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) %>%
  mutate(share = estimate / total) %>%
  select(-"total") %>%
  mutate(geography_type = "County") %>%
  mutate(geography = factor(geography, levels = county_order)) %>%
  mutate(grouping = factor(grouping, levels = age_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = age_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, age_lookup, by=c("variable")) %>%
  filter(variable != "B01001_001") %>%
  select(geography="GEOID", "estimate", "moe", "year", "gender", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Population by Age Group") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = factor(grouping, levels = age_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- readxl::read_excel(age_bg, sheet=paste0("ACS_",pre_api_year,"5_", age_table, "_150"), skip=7) %>%
  mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) %>%
  select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") %>%
  pivot_longer(!geoid, names_to = "label", values_to = "estimate") %>%
  filter(!(str_detect(label, "Margin of Error"))) %>%
  mutate(label = str_remove_all(label, "\\[Estimate\\] ")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) %>%
  mutate(grouping = case_when(
    # Total Population
    str_detect(label,"Total") ~ "Total",
    # Under 17
    str_detect(label,"Under 5 Years") ~ "Under 17",
    str_detect(label,"5 To 9 Years") ~ "Under 17",
    str_detect(label,"10 To 14 Years") ~ "Under 17",
    str_detect(label,"15 To 17 Years") ~ "Under 17",
    # 18 to 34
    str_detect(label,"18 And 19 Years") ~ "18 to 34",
    str_detect(label,"20 Years") ~ "18 to 34",
    str_detect(label,"21 Years") ~ "18 to 34",
    str_detect(label,"22 To 24 Years") ~ "18 to 34",
    str_detect(label,"25 To 29 Years") ~ "18 to 34",
    str_detect(label,"30 To 34 Years") ~ "18 to 34",
    # 35 to 49
    str_detect(label,"35 To 39 Years") ~ "35 to 49",
    str_detect(label,"40 To 44 Years") ~ "35 to 49",
    str_detect(label,"45 To 49 Years") ~ "35 to 49",
    # 50 to 65
    str_detect(label,"50 To 54 Years") ~ "50 to 64",
    str_detect(label,"55 To 59 Years") ~ "50 to 64",
    str_detect(label,"60 And 61 Years") ~ "50 to 64",
    str_detect(label,"62 To 64 Years") ~ "50 to 64",
    # over 65
    str_detect(label,"65 And 66 Years") ~ "65+",
    str_detect(label,"67 To 69 Years") ~ "65+",
    str_detect(label,"70 To 74 Years") ~ "65+",
    str_detect(label,"75 To 79 Years") ~ "65+",
    str_detect(label,"80 To 84 Years") ~ "65+",
    str_detect(label,"85 Years And Over") ~ "65+")) %>%
  drop_na() %>%
  mutate(county = substring(geoid, 1, 5)) %>%
  filter(county %in% c("53033", "53035", "53053", "53061")) %>%
  group_by(geoid, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  rename(geography="geoid") %>%
  mutate(concept = "Population by Age Group") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = factor(grouping, levels = age_order)) %>%
  mutate(year = as.character(pre_api_year)) %>%
  arrange(geography, grouping, year)

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) %>% arrange(geography, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
centers <- NULL
for(center in rgc_names) {
  
  df <- centers_estimate_from_bg(center_type = "Regional Growth Center (6/22/2023)", split_type = "percent_of_total_pop")
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

# Manufacturing and Industrial Centers
for(center in mic_names) {
  
  df <- centers_estimate_from_bg(center_type = "MIC (2022 RTP)", split_type = "percent_of_total_pop")
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

population_by_age <- bind_rows(county, centers)
rm(age_lookup, centers, blockgroups, county)
saveRDS(population_by_age, "data/population_by_age.rds")

# Race & Ethnicty ---------------------------------------------------------
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

county <- get_acs_recs(geography="county", table.names = race_table, years = c(pre_api_year, api_years), acs.type = 'acs5') 

county <- left_join(county, race_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="name", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Population by Race & Ethnicity")

totals <- county %>%
  filter(grouping == "Total") %>%
  select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) %>%
  mutate(share = estimate / total) %>%
  select(-"total") %>%
  mutate(geography_type = "County") %>%
  mutate(grouping = str_wrap(grouping, width=16)) %>%
  mutate(geography = factor(geography, levels = county_order)) %>%
  mutate(grouping = factor(grouping, levels = race_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = race_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, race_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="GEOID", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Population by Race & Ethnicity") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=16)) %>%
  mutate(grouping = factor(grouping, levels = race_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- readxl::read_excel(race_bg, sheet=paste0("ACS_",pre_api_year,"5_", race_table, "_150"), skip=7) %>%
  mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) %>%
  select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") %>%
  pivot_longer(!geoid, names_to = "label", values_to = "estimate") %>%
  filter(!(str_detect(label, "Margin of Error"))) %>%
  mutate(label = str_remove_all(label, "\\[Estimate\\] ")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) %>%
  mutate(grouping = case_when(
    # Total Population
    str_detect(label,"Total") ~ "Total",
    str_detect(label,"White Alone") ~ "Non-hispanic White",
    str_detect(label,"Black Or African American Alone") ~ "Black or African American",
    str_detect(label,"American Indian And Alaska Native Alone") ~ "American Indian and Alaska Native",
    str_detect(label,"Asian Alone") ~ "Asian",
    str_detect(label,"Native Hawaiian And Other Pacific Islander Alone") ~ "Native Hawaiian and Other Pacific Islander",
    str_detect(label,"Some Other Race Alone") ~ "Other",
    str_detect(label,"Two Or More Races") ~ "Other",
    label == "Hispanic Or Latino:" ~ "Hispanic or Latino")) %>%
  drop_na() %>%
  mutate(county = substring(geoid, 1, 5)) %>%
  filter(county %in% c("53033", "53035", "53053", "53061")) %>%
  group_by(geoid, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  rename(geography="geoid") %>%
  mutate(concept = "Population by Race & Ethnicity") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=16)) %>%
  mutate(grouping = factor(grouping, levels = race_order)) %>%
  mutate(year = as.character(pre_api_year)) %>%
  arrange(geography, grouping, year)

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) %>% arrange(geography, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
centers <- NULL
for(center in rgc_names) {
  
  df <- centers_estimate_from_bg(center_type = "Regional Growth Center (6/22/2023)", split_type = "percent_of_total_pop")
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

# Manufacturing and Industrial Centers
for(center in mic_names) {
  
  df <- centers_estimate_from_bg(center_type = "MIC (2022 RTP)", split_type = "percent_of_total_pop")
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

population_by_race <- bind_rows(county, centers)
rm(race_lookup, centers, blockgroups, county)
saveRDS(population_by_race, "data/population_by_race.rds")

# Income ------------------------------------------------------------------
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

county <- get_acs_recs(geography="county", table.names = income_table, years = c(pre_api_year, api_years), acs.type = 'acs5') 

county <- left_join(county, income_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="name", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Households by Income")

totals <- county %>%
  filter(grouping == "Total") %>%
  select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) %>%
  mutate(share = estimate / total) %>%
  select(-"total") %>%
  mutate(geography_type = "County") %>%
  mutate(grouping = str_wrap(grouping, width=11)) %>%
  mutate(geography = factor(geography, levels = county_order)) %>%
  mutate(grouping = factor(grouping, levels = income_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = income_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, income_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="GEOID", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Households by Income") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=11)) %>%
  mutate(grouping = factor(grouping, levels = income_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- readxl::read_excel(income_bg, sheet=paste0("ACS_",pre_api_year,"5_", income_table, "_150"), skip=7) %>%
  mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) %>%
  select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") %>%
  pivot_longer(!geoid, names_to = "label", values_to = "estimate") %>%
  filter(!(str_detect(label, "Margin of Error"))) %>%
  mutate(label = str_remove_all(label, "\\[Estimate\\] ")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) %>%
  mutate(grouping = case_when(
    # Total Population
    str_detect(label,"Total") ~ "Total",
    # Under $20K
    str_detect(label,"Less Than \\$10,000") ~ "Less than $20,000",
    str_detect(label,"\\$10,000 To \\$14,999") ~ "Less than $20,000",
    str_detect(label,"\\$15,000 To \\$19,999") ~ "Less than $20,000",
    # $20k to $35k
    str_detect(label,"\\$20,000 To \\$24,999") ~ "$20,000 to $35,000",
    str_detect(label,"\\$25,000 To \\$29,999") ~ "$20,000 to $35,000",
    str_detect(label,"\\$30,000 To \\$34,999") ~ "$20,000 to $35,000",
    # $35k to $50k
    str_detect(label,"\\$35,000 To \\$39,999") ~ "$35,000 to $50,000",
    str_detect(label,"\\$40,000 To \\$44,999") ~ "$35,000 to $50,000",
    str_detect(label,"\\$45,000 To \\$49,999") ~ "$35,000 to $50,000",
    # $50k to $75k
    str_detect(label,"\\$50,000 To \\$59,999") ~ "$50,000 to $75,000",
    str_detect(label,"\\$60,000 To \\$74,999") ~ "$50,000 to $75,000",
    # $75k to $100k
    str_detect(label,"\\$75,000 To \\$99,999") ~ "$75,000 to $100,000",
    # $100k to $150k
    str_detect(label,"\\$100,000 To \\$124,999") ~ "$100,000 to $150,000",
    str_detect(label,"\\$125,000 To \\$149,999") ~ "$100,000 to $150,000",
    # $150k to $200k
    str_detect(label,"\\$150,000 To \\$199,999") ~ "$150,000 to $200,000",
    # $200k or more
    str_detect(label,"\\$200,000 Or More") ~ "$200,000 or more"
  )) %>%
  mutate(county = substring(geoid, 1, 5)) %>%
  filter(county %in% c("53033", "53035", "53053", "53061")) %>%
  group_by(geoid, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  rename(geography="geoid") %>%
  mutate(concept = "Households by Income") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=11)) %>%
  mutate(grouping = factor(grouping, levels = income_order)) %>%
  mutate(year = as.character(pre_api_year)) %>%
  arrange(geography, grouping, year)

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) %>% arrange(geography, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
centers <- NULL
for(center in rgc_names) {
  
  df <- centers_estimate_from_bg(center_type = "Regional Growth Center (6/22/2023)", split_type = "percent_of_occupied_housing_units")
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

# Manufacturing and Industrial Centers
for(center in mic_names) {
  
  df <- centers_estimate_from_bg(center_type = "MIC (2022 RTP)", split_type = "percent_of_occupied_housing_units")
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

households_by_income <- bind_rows(county, centers)
rm(income_lookup, centers, blockgroups, county)
saveRDS(households_by_income, "data/households_by_income.rds")

# Housing Tenure ----------------------------------------------------------
tenure_lookup <- data.frame(variable = c("B25003_001",
                                         "B25003_002", 
                                         "B25003_003"),
                            grouping = c("Total",
                                         "Owner", 
                                         "Renter"))

county <- get_acs_recs(geography="county", table.names = tenure_table, years = c(pre_api_year, api_years), acs.type = 'acs5') 

county <- left_join(county, tenure_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="name", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Household Tenure")

totals <- county %>%
  filter(grouping == "Total") %>%
  select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) %>%
  mutate(share = estimate / total) %>%
  select(-"total") %>%
  mutate(geography_type = "County") %>%
  mutate(grouping = str_wrap(grouping, width=11)) %>%
  mutate(geography = factor(geography, levels = county_order)) %>%
  mutate(grouping = factor(grouping, levels = tenure_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = tenure_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, tenure_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="GEOID", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Household Tenure") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=11)) %>%
  mutate(grouping = factor(grouping, levels = tenure_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- readxl::read_excel(tenure_bg, sheet=paste0("ACS_",pre_api_year,"5_", tenure_table, "_150"), skip=7) %>%
  mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) %>%
  select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") %>%
  pivot_longer(!geoid, names_to = "label", values_to = "estimate") %>%
  filter(!(str_detect(label, "Margin of Error"))) %>%
  mutate(label = str_remove_all(label, "\\[Estimate\\] ")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) %>%
  mutate(grouping = case_when(
    # Total Population
    str_detect(label,"Total") ~ "Total",
    # Owner
    str_detect(label,"Owner") ~ "Owner",
    # Renter
    str_detect(label,"Renter") ~ "Renter")) %>%
  mutate(county = substring(geoid, 1, 5)) %>%
  filter(county %in% c("53033", "53035", "53053", "53061")) %>%
  group_by(geoid, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  rename(geography="geoid") %>%
  mutate(concept = "Household Tenure") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=11)) %>%
  mutate(grouping = factor(grouping, levels = tenure_order)) %>%
  mutate(year = as.character(pre_api_year)) %>%
  arrange(geography, grouping, year)

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) %>% arrange(geography, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
centers <- NULL
for(center in rgc_names) {
  
  df <- centers_estimate_from_bg(center_type = "Regional Growth Center (6/22/2023)", split_type = "percent_of_occupied_housing_units")
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

# Manufacturing and Industrial Centers
for(center in mic_names) {
  
  df <- centers_estimate_from_bg(center_type = "MIC (2022 RTP)", split_type = "percent_of_occupied_housing_units")
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

households_by_tenure <- bind_rows(county, centers)
rm(tenure_lookup, centers, blockgroups, county)
saveRDS(households_by_tenure, "data/households_by_tenure.rds")















# Educational Attainment --------------------------------------------------
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

# County
county <- get_acs_recs(geography="county", table.names = "B15002", years = c(pre_api_years, api_years), acs.type = 'acs5') %>%
  mutate(label = str_remove_all(label, "Estimate!!Total!!")) %>%
  mutate(label = str_remove_all(label, ":")) %>%
  mutate(label = str_replace_all(label, "!!", " ")) %>%
  mutate(label = str_remove_all(label, "Male ")) %>%
  mutate(label = str_remove_all(label, "Female ")) %>%
  mutate(label = str_remove_all(label, "Estimate ")) %>%
  mutate(label = str_trim(label, "both")) %>%
  select(-"state") %>%
  mutate(label = str_remove_all(label, "Total ")) %>%
  mutate(concept = str_to_title(concept)) %>%
  filter(label !="Female") %>%
  filter(label !="Male")

county <- left_join(county, education_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  group_by(name, year, grouping) %>%
  summarise(estimate = sum(estimate),  moe = round(moe_sum(moe, estimate), 0)) %>%
  as_tibble() %>%
  mutate(metric = "Educational Attainment", geography_type = "County")

totals <- county %>%
  select("name", "grouping", "year", total="estimate") %>%
  filter(grouping == "Total") %>%
  select(-"grouping")

county_edu <- left_join(county, totals, by=c("name", "year")) %>%
  mutate(share = estimate / total) %>%
  select("name", geography="geography_type", "grouping", "year", "estimate", "moe", "share")

county_edu <- county_edu %>%
  select(-"share") %>%
  rename(`Estimate:`="estimate", `MoE:`="moe") %>%
  pivot_wider(names_from = "grouping", values_from = c("Estimate:", "MoE:"), names_sep = " ")

rm(totals, county)

# Blockgroup 2013 onward
edu_post_2013 <- get_acs_recs(geography="block group", table.names = "B15002", years = api_years, acs.type = 'acs5') %>%
  mutate(label = str_remove_all(label, "Estimate!!Total!!")) %>%
  mutate(label = str_remove_all(label, ":")) %>%
  mutate(label = str_replace_all(label, "!!", " ")) %>%
  mutate(label = str_remove_all(label, "Male ")) %>%
  mutate(label = str_remove_all(label, "Female ")) %>%
  mutate(label = str_remove_all(label, "Estimate ")) %>%
  mutate(label = str_trim(label, "both")) %>%
  select(-"state") %>%
  mutate(label = str_remove_all(label, "Total ")) %>%
  mutate(concept = str_to_title(concept)) %>%
  filter(label !="Female") %>%
  filter(label !="Male")

edu_post_2013 <- left_join(edu_post_2013, education_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  group_by(GEOID, year, grouping) %>%
  summarise(estimate = sum(estimate),  moe = round(moe_sum(moe, estimate), 0)) %>%
  as_tibble() %>%
  rename(geoid="GEOID") %>%
  pivot_wider(names_from = "grouping", values_from = c("estimate", "moe"))

# Blockgroup pre 2013
est_pre_2013 <- readxl::read_excel(education_2010, sheet="ACS_20105_B15002_150", skip=7) %>%
  mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) %>%
  select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") %>%
  pivot_longer(!geoid, names_to = "label", values_to = "estimate") %>%
  filter(!(str_detect(label, "Margin of Error"))) %>%
  mutate(label = str_remove_all(label, "\\[Estimate\\] ")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) %>%
  mutate(ID = row_number())

moe_pre_2013 <- readxl::read_excel(education_2010, sheet="ACS_20105_B15002_150", skip=7) %>%
  mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) %>%
  select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") %>%
  pivot_longer(!geoid, names_to = "label", values_to = "moe") %>%
  filter((str_detect(label, "Margin of Error"))) %>%
  mutate(label = str_remove_all(label, "\\[Margin of Error\\] ")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9][0-9]")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) %>%
  mutate(ID = row_number())

edu_pre_2013 <- left_join(est_pre_2013, moe_pre_2013, by=c("ID", "geoid", "label")) %>%
  select(-"ID") %>%
  filter(label !="Female:") %>%
  filter(label !="Male:") %>%
  mutate(grouping = case_when(
    # Total Population
    str_detect(label,"Total") ~ "Total",
    # No High School
    str_detect(label,"No Schooling Completed") ~ "No high school diploma",
    str_detect(label,"Nursery To 4th Grade") ~ "No high school diploma",
    str_detect(label,"5th And 6th Grade") ~ "No high school diploma",
    str_detect(label,"7th And 8th Grade") ~ "No high school diploma",
    str_detect(label,"9th Grade") ~ "No high school diploma",
    str_detect(label,"10th Grade") ~ "No high school diploma",
    str_detect(label,"11th Grade") ~ "No high school diploma",
    str_detect(label,"12th Grade, No Diploma") ~ "No high school diploma",
    # High School
    str_detect(label,"High School Graduate, Ged, Or Alternative") ~ "High school",
    # Some college
    str_detect(label,"Some College, Less Than 1 Year") ~ "Some college",
    str_detect(label,"Some College, 1 Or More Years, No Degree") ~ "Some college",
    str_detect(label,"Associate's Degree") ~ "Some college",
    # Bachelors Degree
    str_detect(label,"Bachelor's Degree") ~ "Bachelor’s degree",
    # Graduate degree
    str_detect(label,"Master's Degree") ~ "Graduate degree",
    str_detect(label,"Professional School Degree") ~ "Graduate degree",
    str_detect(label,"Doctorate Degree") ~ "Graduate degree")) %>%
  drop_na() %>% 
  select(-label) %>%
  mutate(county = substring(geoid, 1, 5)) %>%
  filter(county %in% c("53033", "53035", "53053", "53061")) %>%
  group_by(geoid, grouping) %>%
  summarise(estimate = sum(estimate), moe = round(moe_sum(moe, estimate), 0)) %>%
  as_tibble() %>%
  pivot_wider(names_from = "grouping", values_from = c("estimate", "moe")) %>%
  mutate(year = 2010)

# Join Data with BLockgroups by MIC with Shares
bg_edu <- bind_rows(edu_pre_2013, edu_post_2013)
mic_edu <- left_join(mic_splits, bg_edu, by=c("geoid", "year"))  
rm(bg_edu, edu_pre_2013, edu_post_2013, est_pre_2013, moe_pre_2013)

# Summarize Population by Education for MIC's and MIC buffers
mic_edu <- mic_edu %>%
  mutate(`Estimate: No high school diploma` = round(`estimate_No high school diploma` * pop_share, 0)) %>%
  mutate(`Estimate: High school` = round(`estimate_High school` * pop_share, 0)) %>%
  mutate(`Estimate: Some college` = round(`estimate_Some college` * pop_share, 0)) %>%
  mutate(`Estimate: Bachelor’s degree` = round(`estimate_Bachelor’s degree` * pop_share, 0)) %>%
  mutate(`Estimate: Graduate degree` = round(`estimate_Graduate degree` * pop_share, 0)) %>%
  mutate(`Estimate: Total` = round(`estimate_Total` * pop_share, 0)) %>%
  mutate(`MoE: No high school diploma` = round(`moe_No high school diploma` * pop_share, 0)) %>%
  mutate(`MoE: High school` = round(`moe_High school` * pop_share, 0)) %>%
  mutate(`MoE: Some college` = round(`moe_Some college` * pop_share, 0)) %>%
  mutate(`MoE: Bachelor’s degree` = round(`moe_Bachelor’s degree` * pop_share, 0)) %>%
  mutate(`MoE: Graduate degree` = round(`moe_Graduate degree` * pop_share, 0)) %>%
  mutate(`MoE: Total` = round(`moe_Total` * pop_share, 0)) %>%
  group_by(year, name, geography) %>%
  summarise(`Estimate: No high school diploma` = sum(`Estimate: No high school diploma`), 
            `Estimate: High school` = sum(`Estimate: High school`), 
            `Estimate: Some college` = sum(`Estimate: Some college`), 
            `Estimate: Bachelor’s degree` = sum(`Estimate: Bachelor’s degree`),
            `Estimate: Graduate degree` = sum(`Estimate: Graduate degree`), 
            `Estimate: Total` = sum(`Estimate: Total`),
            `MoE: No high school diploma` = round(moe_sum(`MoE: No high school diploma`, `Estimate: No high school diploma`), 0), 
            `MoE: High school` = round(moe_sum(`MoE: High school`, `Estimate: High school`), 0), 
            `MoE: Some college` = round(moe_sum(`MoE: Some college`, `Estimate: Some college`), 0), 
            `MoE: Bachelor’s degree` = round(moe_sum(`MoE: Bachelor’s degree`, `Estimate: Bachelor’s degree`), 0),
            `MoE: Graduate degree` = round(moe_sum(`MoE: Graduate degree`, `Estimate: Graduate degree`), 0), 
            `MoE: Total` = round(moe_sum(`MoE: Total`, `Estimate: Total`), 0)) %>%
  as_tibble()

mic_edu <- bind_rows(mic_edu, county_edu)
write_csv(mic_edu, "output/mic_edu_groups.csv")
rm(county_edu)

# Structure Type ----------------------------------------------------------
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

# County
county <- get_acs_recs(geography="county", table.names = "B25024", years = c(pre_api_years, api_years), acs.type = 'acs5') %>%
  mutate(label = str_remove_all(label, "Estimate!!Total!!")) %>%
  mutate(label = str_remove_all(label, ":")) %>%
  mutate(label = str_replace_all(label, "!!", " ")) %>%
  mutate(label = str_remove_all(label, "Estimate ")) %>%
  mutate(label = str_trim(label, "both")) %>%
  select(-"state") %>%
  mutate(label = str_remove_all(label, "Total ")) %>%
  mutate(concept = str_to_title(concept))

county <- left_join(county, structure_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  group_by(name, year, grouping) %>%
  summarise(estimate = sum(estimate),  moe = round(moe_sum(moe, estimate), 0)) %>%
  as_tibble() %>%
  mutate(metric = "Housing Structure Type", geography_type = "County")

totals <- county %>%
  select("name", "grouping", "year", total="estimate") %>%
  filter(grouping == "Total") %>%
  select(-"grouping")

county_structures <- left_join(county, totals, by=c("name", "year")) %>%
  mutate(share = estimate / total) %>%
  select("name", geography="geography_type", "grouping", "year", "estimate", "moe", "share")

county_structures <- county_structures %>%
  select(-"share") %>%
  rename(`Estimate:`="estimate", `MoE:`="moe") %>%
  pivot_wider(names_from = "grouping", values_from = c("Estimate:", "MoE:"), names_sep = " ")

rm(totals, county)

# Blockgroup 2013 onward
structures_post_2013 <- get_acs_recs(geography="block group", table.names = "B25024", years = api_years, acs.type = 'acs5') %>%
  mutate(label = str_remove_all(label, "Estimate!!Total!!")) %>%
  mutate(label = str_remove_all(label, ":")) %>%
  mutate(label = str_replace_all(label, "!!", " ")) %>%
  mutate(label = str_remove_all(label, "Estimate ")) %>%
  mutate(label = str_trim(label, "both")) %>%
  select(-"state") %>%
  mutate(label = str_remove_all(label, "Total ")) %>%
  mutate(concept = str_to_title(concept))

structures_post_2013 <- left_join(structures_post_2013, structure_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  group_by(GEOID, year, grouping) %>%
  summarise(estimate = sum(estimate),  moe = round(moe_sum(moe, estimate), 0)) %>%
  as_tibble() %>%
  rename(geoid="GEOID") %>%
  pivot_wider(names_from = "grouping", values_from = c("estimate", "moe"))

# Blockgroup pre 2013
est_pre_2013 <- readxl::read_excel(structures_2010, sheet="ACS_20105_B25024_150", skip=7) %>%
  mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) %>%
  select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") %>%
  pivot_longer(!geoid, names_to = "label", values_to = "estimate") %>%
  filter(!(str_detect(label, "Margin of Error"))) %>%
  mutate(label = str_remove_all(label, "\\[Estimate\\] ")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) %>%
  mutate(ID = row_number())

moe_pre_2013 <- readxl::read_excel(structures_2010, sheet="ACS_20105_B25024_150", skip=7) %>%
  mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) %>%
  select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") %>%
  pivot_longer(!geoid, names_to = "label", values_to = "moe") %>%
  filter((str_detect(label, "Margin of Error"))) %>%
  mutate(label = str_remove_all(label, "\\[Margin of Error\\] ")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9][0-9]")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) %>%
  mutate(ID = row_number())

structures_pre_2013 <- left_join(est_pre_2013, moe_pre_2013, by=c("ID", "geoid", "label")) %>%
  select(-"ID") %>%
  mutate(grouping = case_when(
    # Total
    str_detect(label,"Total") ~ "Total",
    # High Density
    str_detect(label,"20 To 49") ~ "High density",
    str_detect(label,"50 Or More") ~ "High density",
    # Low Density
    str_detect(label,"1, Detached") ~ "SF detached",
    # Moderate Low
    str_detect(label,"1, Attached") ~ "Moderate-low density",
    str_detect(label,"2") ~ "Moderate-low density",
    str_detect(label,"3 Or 4") ~ "Moderate-low density",
    str_detect(label,"5 To 9") ~ "Moderate-low density",
    # Moderate High
    str_detect(label,"10 To 19") ~ "Moderate-high density",
    # Other
    str_detect(label,"Mobile Home") ~ "Other",
    str_detect(label,"Boat, Rv, Van, Etc.") ~ "Other")) %>%
  drop_na() %>% 
  select(-label) %>%
  mutate(county = substring(geoid, 1, 5)) %>%
  filter(county %in% c("53033", "53035", "53053", "53061")) %>%
  group_by(geoid, grouping) %>%
  summarise(estimate = sum(estimate), moe = round(moe_sum(moe, estimate), 0)) %>%
  as_tibble() %>%
  pivot_wider(names_from = "grouping", values_from = c("estimate", "moe")) %>%
  mutate(year = 2010)

# Join Data with BLockgroups by MIC with Shares
bg_structures <- bind_rows(structures_pre_2013, structures_post_2013)
mic_structures <- left_join(mic_splits, bg_structures, by=c("geoid", "year"))  
rm(bg_structures, structures_pre_2013, structures_post_2013, est_pre_2013, moe_pre_2013)

# Summarize Population by Education for MIC's and MIC buffers
mic_structures <- mic_structures %>%
  mutate(`Estimate: SF detached` = round(`estimate_SF detached` * hu_share, 0)) %>%
  mutate(`Estimate: Moderate-low density` = round(`estimate_Moderate-low density` * hu_share, 0)) %>%
  mutate(`Estimate: Moderate-high density` = round(`estimate_Moderate-high density` * hu_share, 0)) %>%
  mutate(`Estimate: High density` = round(`estimate_High density` * hu_share, 0)) %>%
  mutate(`Estimate: Other` = round(`estimate_Other` * hu_share, 0)) %>%
  mutate(`Estimate: Total` = round(`estimate_Total` * hu_share, 0)) %>%
  mutate(`MoE: SF detached` = round(`moe_SF detached` * hu_share, 0)) %>%
  mutate(`MoE: Moderate-low density` = round(`moe_Moderate-low density` * hu_share, 0)) %>%
  mutate(`MoE: Moderate-high density` = round(`moe_Moderate-high density` * hu_share, 0)) %>%
  mutate(`MoE: High density` = round(`moe_High density` * hu_share, 0)) %>%
  mutate(`MoE: Other` = round(`moe_Other` * hu_share, 0)) %>%
  mutate(`MoE: Total` = round(`moe_Total` * hu_share, 0)) %>%
  group_by(year, name, geography) %>%
  summarise(`Estimate: SF detached` = sum(`Estimate: SF detached`), 
            `Estimate: Moderate-low density` = sum(`Estimate: Moderate-low density`), 
            `Estimate: Moderate-high density` = sum(`Estimate: Moderate-high density`), 
            `Estimate: High density` = sum(`Estimate: High density`),
            `Estimate: Other` = sum(`Estimate: Other`), 
            `Estimate: Total` = sum(`Estimate: Total`),
            `MoE: SF detached` = round(moe_sum(`MoE: SF detached`, `Estimate: SF detached`), 0), 
            `MoE: Moderate-low density` = round(moe_sum(`MoE: Moderate-low density`, `Estimate: Moderate-low density`), 0), 
            `MoE: Moderate-high density` = round(moe_sum(`MoE: Moderate-high density`, `Estimate: Moderate-high density`), 0), 
            `MoE: High density` = round(moe_sum(`MoE: High density`, `Estimate: High density`), 0),
            `MoE: Other` = round(moe_sum(`MoE: Other`, `Estimate: Other`), 0), 
            `MoE: Total` = round(moe_sum(`MoE: Total`, `Estimate: Total`), 0)) %>%
  as_tibble()

mic_structures <- bind_rows(mic_structures, county_structures)
write_csv(mic_structures, "output/mic_structures_groups.csv")
rm(county_structures)



# Final Organization and Output -------------------------------------------
#temp <- mic_tenure
#write_csv(mic_tenure, "output/mic_tenure_groups.csv")

