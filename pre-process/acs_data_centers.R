# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrccensus)
library(psrcelmer)
library(tidycensus)
library(sf)

# Inputs ------------------------------------------------------------------
wgs84 <- 4326
spn <- 32148

pre_api_year <- 2011
api_years <- c(2016, 2021)
acs_pre2013_bg_dir <- "C:/coding/acs_blockgroups_pre2013"
base_model_yr <- 2018

year_ord <- c("2022","2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010")
rgc_title <- "Regional Growth Center (6/22/2023)"
mic_title <- "MIC (2022 RTP)"

srt_ids <- c('kcm_100340', 'kcm_102638')
lrt_ids <- c('kcm_100479', 'st_100479', 'st_TLINK')
crt_ids <- c('st_SNDR_EV','st_SNDR_TL','st_AMTK', 'st_SNDR_S', 'st_SNDR_N')
brt_ids <- c('ct_701','ct_702','kcm_100512', 'kcm_102548', 'kcm_102576', 'kcm_102581', 'kcm_102615', 'kcm_102619', 'KC_102736')
pof_ids <- c('kcm_100336', 'kcm_100337','kt_Ferry', 'kt_Annapolis', 'kt_Kitsap Fast Ferry')
fry_ids <- c('wsf_128', 'wsf_145', 'wsf_1621', 'wsf_2022', 'wsf_209', 'wsf_37','wsf_47', 'wsf_229',
             'wsf_2116', 'wsf_2220','wsf_514', 'wsf_73', 'wsf_74', 'wsf_812', 'wsf_920', 'wsf_922')
hct_ids <- c(srt_ids, lrt_ids, crt_ids, brt_ids, pof_ids, fry_ids)

st_express <- c("510","511","512","513","522","532","535","540","541","542","544","545",
                "550","554","555","556","560","566","567","574","577", "578", "580","586", 
                "590","592","594","595","596")

brt_routes <- c("701","702", "Swift", "Swift Blue", "Swift Green",
                "A Line", "B Line", "C Line", "D Line", "E Line", "F Line", "G Line", "H Line")

rgc_title <- "Regional Growth Center (6/22/2023)"
mic_title <- "MIC (2022 RTP)"

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
structure_order <- c("SF detached", "Moderate-low\ndensity", "Moderate-high\ndensity",
                     "High density", "Other", "Total")
burden_order <- c("Not cost\nburdened (<30%)", "Cost burdened\n(30-49.9%)",
                  "Severely cost\nburdened (50+%)", "Not computed", "Total")
education_order <- c("No high school\ndiploma", "High school", "Some college",
                     "Bachelor’s\ndegree","Graduate degree", "Total")
mode_order <- c("Drove\nAlone", "Carpooled", "Transit", "Bike", "Walk","Work from\nHome", "Other", "Total")

# Census Tables
age_table <- "B01001"
race_table <- "B03002"
income_table <- "B19001"
tenure_table <- "B25003"
structure_table <- "B25024"
renter_burden_table <- "B25070"
owner_burden_table <- "B25091"
education_table <- "B15002"
mode_table <- "B08301"

# Employment Data
rgc_emp_file <- "data/rgc_covered_emp_2010_2022.csv"
mic_emp_file <- "data/mic_covered_emp_2010_2022.csv"

# RGS Mode Destination Mode Share
rgc_dest_mod_file <- "data/mode_share_rgc.csv"

# Pre-2013 ACS Blockgroup Files -------------------------------------------
age_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_", age_table, "_150.xlsx"))
race_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_", race_table, "_150.xlsx"))
income_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_", income_table, "_150.xlsx"))
tenure_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_", tenure_table, "_150.xlsx"))
structure_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_", structure_table, "_150.xlsx"))
renter_burden_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_", renter_burden_table, "_150.xlsx"))
owner_burden_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_", owner_burden_table, "_150_v2.xlsx"))
education_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_", education_table, "_150.xlsx"))
mode_bg <- file.path(acs_pre2013_bg_dir, paste0("acs_",pre_api_year,"5_", mode_table, "_150.xlsx"))

# Functions ---------------------------------------------------------------
centers_estimate_from_bg <- function(split_df=blockgroup_splits, estimate_df=blockgroups, center_type, split_type, center_name) {
  
  if (center_name == "All Centers" & center_type == "Regional Growth Center (6/22/2023)") {cn <- "not in regional growth center"}
  if (center_name == "All Centers" & center_type == "MIC (2022 RTP)") {cn <- "not in mic"}

  if (center_name == "All Centers") {
    
    t <- split_df %>% filter(planning_geog_type == center_type & planning_geog != cn) %>% mutate(planning_geog = "All Centers")
    
  } else {
    
    t <- split_df %>% filter(planning_geog_type == center_type & planning_geog == center_name)
    
  }
  
  # Filter Blockgroup Splits to Center
  t <- t %>%
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
rgc_jobs <- read_csv(rgc_emp_file) |>
  filter(center != "RGC Total") |>
  mutate(Government = as.character(Government), `Public Education` = as.character(`Public Education`)) |>
  pivot_longer(cols = !c(year,center), names_to = "grouping", values_to = "estimate") |>
  rename(geography="center") |>
  mutate(estimate = gsub("S", "*", estimate)) |>
  mutate(geography = gsub("Redmond-Overlake", "Redmond Overlake", geography)) |>
  mutate(geography = gsub("Bellevue", "Bellevue Downtown", geography)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  mutate(grouping = gsub("Const/Res", "Construction / Resources", grouping)) |>
  mutate(grouping = gsub("WTU", "Wholesale, Transportation & Utilties", grouping)) |>
  mutate(grouping = gsub("FIRE", "Finance, Insurance & Real Estate", grouping)) |>
  mutate(concept = "Employment", share=1, geography_type = "Regional Growth Center (6/22/2023)") |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography))

mic_jobs <- read_csv(mic_emp_file) |> 
  filter(center != "MIC Total") |>
  rename(`Public Education` = "Public_Education") |>
  mutate(Const_Res = as.character(Const_Res), Government = as.character(Government), `Public Education` = as.character(`Public Education`)) |>
  mutate(WTU = as.character(WTU), Total = as.character(Total)) |>
  pivot_longer(cols = !c(year,center), names_to = "grouping", values_to = "estimate") |>
  rename(geography="center") |>
  mutate(estimate = gsub("S", "*", estimate)) |>
  mutate(geography = gsub("Kent MIC", "Kent", geography)) |>
  mutate(geography = gsub("Paine Field / Boeing Everett", "Paine Field/Boeing Everett", geography)) |>
  mutate(geography = gsub("Sumner Pacific", "Sumner-Pacific", geography)) |>
  mutate(geography = gsub("Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton", geography)) |>
  mutate(geography = gsub("Cascade", "Cascade Industrial Center - Arlington/Marysville", geography)) |>
  mutate(grouping = gsub("Const_Res", "Construction / Resources", grouping)) |>
  mutate(grouping = gsub("WTU", "Wholesale, Transportation & Utilties", grouping)) |>
  mutate(grouping = gsub("FIRE", "Finance, Insurance & Real Estate", grouping)) |>
  mutate(concept = "Employment", share=1, geography_type = "MIC (2022 RTP)") |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography))

centers_employment <- bind_rows(rgc_jobs, mic_jobs) |> mutate(estimate = as.integer(estimate))

# Get Totals for Shares
totals <- centers_employment |> filter(grouping == "Total") |> select("year", "geography", "geography_type", total="estimate")
centers_employment <- left_join(centers_employment, totals, by=c("year", "geography", "geography_type")) |> mutate(share = estimate/total) |> select(-"total")
saveRDS(centers_employment, "data/centers_employment.rds")
rm(rgc_jobs, mic_jobs, totals)

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
rm(rgc_data, mic_data, parcels)
saveRDS(center_pop_hh_hu, "data/center_pop_hh_hu.rds")

# Housing Units -----------------------------------------------------------
parcels <- left_join(parcel_facts, parcel_geo, by=c("parcel_dim_id")) 

rgc_data <- parcels %>%
  select(year="estimate_year", geography="rgc", "housing_units") %>%
  group_by(year, geography) %>%
  summarise(housing_units=sum(housing_units)) %>%
  as_tibble() %>%
  filter(geography != "Not in Center") %>%
  mutate(geography_type = "Regional Growth Center (6/22/2023)") %>%
  pivot_longer(cols = c(housing_units), names_to = "grouping", values_to = "estimate") %>%
  mutate(estimate = round(estimate,0), concept = "Total Housing Units", share=1) %>%
  mutate(geography = gsub("Redmond-Overlake", "Redmond Overlake", geography)) %>%
  mutate(geography = gsub("Bellevue", "Bellevue Downtown", geography)) %>%
  mutate(grouping = gsub("housing_units", "Housing Units", grouping)) %>%
  arrange(geography, grouping, year)

mic_data <- parcels %>%
  select(year="estimate_year", geography="mic", "housing_units") %>%
  group_by(year, geography) %>%
  summarise(housing_units=sum(housing_units)) %>%
  as_tibble() %>%
  filter(geography != "Not in Center") %>%
  mutate(geography_type = "MIC (2022 RTP)") %>%
  pivot_longer(cols = c(housing_units), names_to = "grouping", values_to = "estimate") %>%
  mutate(estimate = round(estimate,0), concept = "Total Housing Units", share=1) %>%
  mutate(geography = gsub("Kent MIC", "Kent", geography)) %>%
  mutate(geography = gsub("Paine Field / Boeing Everett", "Paine Field/Boeing Everett", geography)) %>%
  mutate(geography = gsub("Sumner Pacific", "Sumner-Pacific", geography)) %>%
  mutate(geography = gsub("Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton", geography)) %>%
  mutate(geography = gsub("Cascade", "Cascade Industrial Center - Arlington/Marysville", geography)) %>%
  mutate(grouping = gsub("housing_units", "Housing Units", grouping)) %>%
  arrange(geography, grouping, year)

center_hu <- bind_rows(rgc_data, mic_data)

center_hu <- center_hu %>%
  group_by(geography) %>%
  mutate(delta = estimate - lag(estimate)) %>%
  as_tibble() %>%
  drop_na()

rm(rgc_data, mic_data, parcels)
saveRDS(center_hu, "data/center_hu.rds")

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
for(center in c(rgc_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "Regional Growth Center (6/22/2023)", split_type = "percent_of_total_pop", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

# Manufacturing and Industrial Centers
for(center in c(mic_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "MIC (2022 RTP)", split_type = "percent_of_total_pop", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

population_by_age <- bind_rows(county, centers)
rm(age_lookup, centers, blockgroups, county)
ord <- unique(c("Region", "All Centers", rgc_names, mic_names))
population_by_age <- population_by_age %>% mutate(geography = factor(geography, levels = ord)) %>% arrange(geography, grouping, year)
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
for(center in c(rgc_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "Regional Growth Center (6/22/2023)", split_type = "percent_of_total_pop", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

# Manufacturing and Industrial Centers
for(center in c(mic_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "MIC (2022 RTP)", split_type = "percent_of_total_pop", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

population_by_race <- bind_rows(county, centers)
rm(race_lookup, centers, blockgroups, county)
ord <- unique(c("Region", "All Centers", rgc_names, mic_names))
population_by_race <- population_by_race %>% mutate(geography = factor(geography, levels = ord)) %>% arrange(geography, grouping, year)
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
for(center in c(rgc_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "Regional Growth Center (6/22/2023)", split_type = "percent_of_occupied_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

# Manufacturing and Industrial Centers
for(center in c(mic_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "MIC (2022 RTP)", split_type = "percent_of_occupied_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

households_by_income <- bind_rows(county, centers)
rm(income_lookup, centers, blockgroups, county)
ord <- unique(c("Region", "All Centers", rgc_names, mic_names))
households_by_income <- households_by_income %>% mutate(geography = factor(geography, levels = ord)) %>% arrange(geography, grouping, year)
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
for(center in c(rgc_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "Regional Growth Center (6/22/2023)", split_type = "percent_of_occupied_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

# Manufacturing and Industrial Centers
for(center in c(mic_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "MIC (2022 RTP)", split_type = "percent_of_occupied_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

households_by_tenure <- bind_rows(county, centers)
rm(tenure_lookup, centers, blockgroups, county)
ord <- unique(c("Region", "All Centers", rgc_names, mic_names))
households_by_tenure <- households_by_tenure %>% mutate(geography = factor(geography, levels = ord)) %>% arrange(geography, grouping, year)
saveRDS(households_by_tenure, "data/households_by_tenure.rds")

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

county <- get_acs_recs(geography="county", table.names = structure_table, years = c(pre_api_year, api_years), acs.type = 'acs5') 

county <- left_join(county, structure_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="name", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Housing Unit Type")

totals <- county %>%
  filter(grouping == "Total") %>%
  select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) %>%
  mutate(share = estimate / total) %>%
  select(-"total") %>%
  mutate(geography_type = "County") %>%
  mutate(grouping = str_wrap(grouping, width=15)) %>%
  mutate(geography = factor(geography, levels = county_order)) %>%
  mutate(grouping = factor(grouping, levels = structure_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = structure_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, structure_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="GEOID", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Housing Unit Type") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=15)) %>%
  mutate(grouping = factor(grouping, levels = structure_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- readxl::read_excel(structure_bg, sheet=paste0("ACS_",pre_api_year,"5_", structure_table, "_150"), skip=7) %>%
  mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) %>%
  select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") %>%
  pivot_longer(!geoid, names_to = "label", values_to = "estimate") %>%
  filter(!(str_detect(label, "Margin of Error"))) %>%
  mutate(label = str_remove_all(label, "\\[Estimate\\] ")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) %>%
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
  mutate(county = substring(geoid, 1, 5)) %>%
  filter(county %in% c("53033", "53035", "53053", "53061")) %>%
  group_by(geoid, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  rename(geography="geoid") %>%
  mutate(concept = "Housing Unit Type") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=15)) %>%
  mutate(grouping = factor(grouping, levels = structure_order)) %>%
  mutate(year = as.character(pre_api_year)) %>%
  arrange(geography, grouping, year)

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) %>% arrange(geography, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
centers <- NULL
for(center in c(rgc_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "Regional Growth Center (6/22/2023)", split_type = "percent_of_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

# Manufacturing and Industrial Centers
for(center in c(mic_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "MIC (2022 RTP)", split_type = "percent_of_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

housing_units_by_type <- bind_rows(county, centers)
rm(structure_lookup, centers, blockgroups, county)
ord <- unique(c("Region", "All Centers", rgc_names, mic_names))
housing_units_by_type <- housing_units_by_type %>% mutate(geography = factor(geography, levels = ord)) %>% arrange(geography, grouping, year)
saveRDS(housing_units_by_type, "data/housing_units_by_type.rds")

# Renter Cost Burden ----------------------------------------------------------
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

county <- get_acs_recs(geography="county", table.names = renter_burden_table, years = c(pre_api_year, api_years), acs.type = 'acs5') 

county <- left_join(county, renter_burden_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="name", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Renter Cost Burden")

totals <- county %>%
  filter(grouping == "Total") %>%
  select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) %>%
  mutate(share = estimate / total) %>%
  select(-"total") %>%
  mutate(geography_type = "County") %>%
  mutate(grouping = str_wrap(grouping, width=15)) %>%
  mutate(geography = factor(geography, levels = county_order)) %>%
  mutate(grouping = factor(grouping, levels = burden_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = renter_burden_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, renter_burden_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="GEOID", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Renter Cost Burden") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=15)) %>%
  mutate(grouping = factor(grouping, levels = burden_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- readxl::read_excel(renter_burden_bg, sheet=paste0("ACS_",pre_api_year,"5_", renter_burden_table, "_150"), skip=7) %>%
  mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) %>%
  select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") %>%
  pivot_longer(!geoid, names_to = "label", values_to = "estimate") %>%
  filter(!(str_detect(label, "Margin of Error"))) %>%
  mutate(label = str_remove_all(label, "\\[Estimate\\] ")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) %>%
  mutate(grouping = case_when(
    # Total
    str_detect(label,"Total") ~ "Total",
    # Not Burdened
    str_detect(label,"Less Than 10.0 Percent") ~ "Not cost burdened (<30%)",
    str_detect(label,"10.0 To 14.9 Percent") ~ "Not cost burdened (<30%)",
    str_detect(label,"15.0 To 19.9 Percent") ~ "Not cost burdened (<30%)",
    str_detect(label,"20.0 To 24.9 Percent") ~ "Not cost burdened (<30%)",
    str_detect(label,"25.0 To 29.9 Percent") ~ "Not cost burdened (<30%)",
    # Cost Burdened
    str_detect(label,"30.0 To 34.9 Percent") ~ "Cost burdened (30-49.9%)",
    str_detect(label,"35.0 To 39.9 Percent") ~ "Cost burdened (30-49.9%)",
    str_detect(label,"40.0 To 49.9 Percent") ~ "Cost burdened (30-49.9%)",
    # Severely cost burdened
    str_detect(label,"50.0 Percent Or More") ~ "Severely cost burdened (50+%)",
    # Not computed
    str_detect(label,"Not Computed") ~ "Not computed")) %>%
  mutate(county = substring(geoid, 1, 5)) %>%
  filter(county %in% c("53033", "53035", "53053", "53061")) %>%
  group_by(geoid, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  rename(geography="geoid") %>%
  mutate(concept = "Renter Cost Burden") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=15)) %>%
  mutate(grouping = factor(grouping, levels = burden_order)) %>%
  mutate(year = as.character(pre_api_year)) %>%
  arrange(geography, grouping, year)

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) %>% arrange(geography, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
centers <- NULL
for(center in c(rgc_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "Regional Growth Center (6/22/2023)", split_type = "percent_of_occupied_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

# Manufacturing and Industrial Centers
for(center in c(mic_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "MIC (2022 RTP)", split_type = "percent_of_occupied_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

renter_cost_burden <- bind_rows(county, centers) %>% mutate(share = replace_na(share, 0))
rm(renter_burden_lookup, centers, blockgroups, county)

# Owner Cost Burden ----------------------------------------------------------
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

county <- get_acs_recs(geography="county", table.names = owner_burden_table, years = c(pre_api_year, api_years), acs.type = 'acs5') 

county <- left_join(county, owner_burden_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="name", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Owner Cost Burden")

totals <- county %>%
  filter(grouping == "Total") %>%
  select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) %>%
  mutate(share = estimate / total) %>%
  select(-"total") %>%
  mutate(geography_type = "County") %>%
  mutate(grouping = str_wrap(grouping, width=15)) %>%
  mutate(geography = factor(geography, levels = county_order)) %>%
  mutate(grouping = factor(grouping, levels = burden_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = owner_burden_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, owner_burden_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="GEOID", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Owner Cost Burden") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=15)) %>%
  mutate(grouping = factor(grouping, levels = burden_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- readxl::read_excel(owner_burden_bg, sheet=paste0("ACS_",pre_api_year,"5_", owner_burden_table, "_150"), skip=7) %>%
  mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) %>%
  select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") %>%
  pivot_longer(!geoid, names_to = "label", values_to = "estimate") %>%
  filter(!(str_detect(label, "Margin of Error"))) %>%
  mutate(label = str_remove_all(label, "\\[Estimate\\] ")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) %>%
  mutate(grouping = case_when(
    # Total
    str_detect(label,"Housing Units With A Mortgage") ~ "Total",
    # Not Burdened
    str_detect(label,"Less Than 10.0 Percent") ~ "Not cost burdened (<30%)",
    str_detect(label,"10.0 To 14.9 Percent") ~ "Not cost burdened (<30%)",
    str_detect(label,"15.0 To 19.9 Percent") ~ "Not cost burdened (<30%)",
    str_detect(label,"20.0 To 24.9 Percent") ~ "Not cost burdened (<30%)",
    str_detect(label,"25.0 To 29.9 Percent") ~ "Not cost burdened (<30%)",
    # Cost Burdened
    str_detect(label,"30.0 To 34.9 Percent") ~ "Cost burdened (30-49.9%)",
    str_detect(label,"35.0 To 39.9 Percent") ~ "Cost burdened (30-49.9%)",
    str_detect(label,"40.0 To 49.9 Percent") ~ "Cost burdened (30-49.9%)",
    # Severely cost burdened
    str_detect(label,"50.0 Percent Or More") ~ "Severely cost burdened (50+%)",
    # Not computed
    str_detect(label,"Not Computed") ~ "Not computed")) %>%
  drop_na() %>% 
  mutate(county = substring(geoid, 1, 5)) %>%
  filter(county %in% c("53033", "53035", "53053", "53061")) %>%
  group_by(geoid, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  rename(geography="geoid") %>%
  mutate(concept = "Owner Cost Burden") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=15)) %>%
  mutate(grouping = factor(grouping, levels = burden_order)) %>%
  mutate(year = as.character(pre_api_year)) %>%
  arrange(geography, grouping, year)

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) %>% arrange(geography, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
centers <- NULL
for(center in c(rgc_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "Regional Growth Center (6/22/2023)", split_type = "percent_of_occupied_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

# Manufacturing and Industrial Centers
for(center in c(mic_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "MIC (2022 RTP)", split_type = "percent_of_occupied_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

owner_cost_burden <- bind_rows(county, centers) %>% mutate(share = replace_na(share, 0))
rm(owner_burden_lookup, centers, blockgroups, county)

cost_burden <- bind_rows(owner_cost_burden, renter_cost_burden)
ord <- unique(c("Region", "All Centers", rgc_names, mic_names))
cost_burden <- cost_burden %>% mutate(geography = factor(geography, levels = ord)) %>% arrange(geography, grouping, year)
saveRDS(cost_burden, "data/cost_burden.rds")

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

county <- get_acs_recs(geography="county", table.names = education_table, years = c(pre_api_year, api_years), acs.type = 'acs5') 

county <- left_join(county, education_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="name", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Educational Attainment")

totals <- county %>%
  filter(grouping == "Total") %>%
  select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) %>%
  mutate(share = estimate / total) %>%
  select(-"total") %>%
  mutate(geography_type = "County") %>%
  mutate(grouping = str_wrap(grouping, width=15)) %>%
  mutate(geography = factor(geography, levels = county_order)) %>%
  mutate(grouping = factor(grouping, levels = education_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = education_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, education_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="GEOID", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Educational Attainment") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=15)) %>%
  mutate(grouping = factor(grouping, levels = education_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- readxl::read_excel(education_bg, sheet=paste0("ACS_",pre_api_year,"5_", education_table, "_150"), skip=7) %>%
  mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) %>%
  select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") %>%
  pivot_longer(!geoid, names_to = "label", values_to = "estimate") %>%
  filter(!(str_detect(label, "Margin of Error"))) %>%
  mutate(label = str_remove_all(label, "\\[Estimate\\] ")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) %>%
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
  mutate(county = substring(geoid, 1, 5)) %>%
  filter(county %in% c("53033", "53035", "53053", "53061")) %>%
  group_by(geoid, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  rename(geography="geoid") %>%
  mutate(concept = "Educational Attainment") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=15)) %>%
  mutate(grouping = factor(grouping, levels = education_order)) %>%
  mutate(year = as.character(pre_api_year)) %>%
  arrange(geography, grouping, year)

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) %>% arrange(geography, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
centers <- NULL
for(center in c(rgc_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "Regional Growth Center (6/22/2023)", split_type = "percent_of_occupied_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

# Manufacturing and Industrial Centers
for(center in c(mic_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "MIC (2022 RTP)", split_type = "percent_of_occupied_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

educational_attainment <- bind_rows(county, centers) %>% mutate(share = replace_na(share, 0))
rm(education_lookup, centers, blockgroups, county)
ord <- unique(c("Region", "All Centers", rgc_names, mic_names))
educational_attainment <- educational_attainment %>% mutate(geography = factor(geography, levels = ord)) %>% arrange(geography, grouping, year)
saveRDS(educational_attainment, "data/educational_attainment.rds")

# Mode Share to Work ------------------------------------------------------
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

county <- get_acs_recs(geography="county", table.names = mode_table, years = c(pre_api_year, api_years), acs.type = 'acs5') 

county <- left_join(county, mode_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="name", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Mode to Work")

totals <- county %>%
  filter(grouping == "Total") %>%
  select("geography", "year", "concept", total="estimate")

county <- left_join(county, totals, by=c("geography", "year", "concept")) %>%
  mutate(share = estimate / total) %>%
  select(-"total") %>%
  mutate(geography_type = "County") %>%
  mutate(grouping = str_wrap(grouping, width=10)) %>%
  mutate(geography = factor(geography, levels = county_order)) %>%
  mutate(grouping = factor(grouping, levels = mode_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

rm(totals)

# Blockgroup 2013 onward
bg_post_2013 <- get_acs_recs(geography="block group", table.names = mode_table, years = api_years, acs.type = 'acs5') 

bg_post_2013 <- left_join(bg_post_2013, mode_lookup, by=c("variable")) %>%
  filter(!(is.na(grouping))) %>%
  select(geography="GEOID", "estimate", "year", "grouping") %>%
  group_by(geography, year, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(concept = "Mode to Work") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=10)) %>%
  mutate(grouping = factor(grouping, levels = mode_order)) %>%
  mutate(year = as.character(year)) %>%
  arrange(geography, grouping, year)

# Blockgroup pre 2013
bg_pre_2013 <- readxl::read_excel(mode_bg, sheet=paste0("ACS_",pre_api_year,"5_", mode_table, "_150"), skip=7) %>%
  mutate(geoid = as.character(str_remove_all(`Geographic Identifier`, "15000US"))) %>%
  select(-"Geographic Identifier",-"Table ID", -"Title", -"Universe", -"Summary Level Type", -"Summary Level", -"Area Name", -"ACS Dataset") %>%
  pivot_longer(!geoid, names_to = "label", values_to = "estimate") %>%
  filter(!(str_detect(label, "Margin of Error"))) %>%
  mutate(label = str_remove_all(label, "\\[Estimate\\] ")) %>%
  mutate(label = str_remove_all(label, "\\...[0-9][0-9]")) %>%
  mutate(grouping = case_when(
    # Total Population
    str_detect(label,"Total") ~ "Total",
    # Drove Alone
    str_detect(label,"Drove Alone") ~ "Drove Alone",
    # Carpool
    str_detect(label,"Carpooled") ~ "Carpooled",
    # Transit
    str_detect(label,"Public Transportation \\(Excluding Taxicab\\)") ~ "Transit",
    # Bike
    str_detect(label,"Bicycle") ~ "Bike",
    # Walk
    str_detect(label,"Walked") ~ "Walk",
    # Work from Home
    str_detect(label,"Worked At Home") ~ "Work from Home",
    # Other
    str_detect(label,"Taxicab") ~ "Other",
    str_detect(label,"Motorcycle") ~ "Other",
    str_detect(label,"Other Means") ~ "Other")) %>%
  drop_na() %>% 
  mutate(county = substring(geoid, 1, 5)) %>%
  filter(county %in% c("53033", "53035", "53053", "53061")) %>%
  group_by(geoid, grouping) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  rename(geography="geoid") %>%
  mutate(concept = "Mode to Work") %>%
  mutate(geography_type = "Blockgroup") %>%
  mutate(grouping = str_wrap(grouping, width=10)) %>%
  mutate(grouping = factor(grouping, levels = mode_order)) %>%
  mutate(year = as.character(pre_api_year)) %>%
  arrange(geography, grouping, year)

# Combine Blockgroup Data
blockgroups <- bind_rows(bg_pre_2013, bg_post_2013) %>% arrange(geography, grouping, year)
rm(bg_pre_2013, bg_post_2013)

# Regional Growth Centers
centers <- NULL
for(center in c(rgc_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "Regional Growth Center (6/22/2023)", split_type = "percent_of_occupied_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

# Manufacturing and Industrial Centers
for(center in c(mic_names, "All Centers")) {
  
  df <- centers_estimate_from_bg(center_type = "MIC (2022 RTP)", split_type = "percent_of_occupied_housing_units", center_name=center)
  ifelse(is.null(centers), centers <- df, centers <- bind_rows(centers, df))
  rm(df)
  
}

mode_to_work <- bind_rows(county, centers) %>% mutate(share = replace_na(share, 0))
rm(mode_lookup, centers, blockgroups, county)
ord <- unique(c("Region", "All Centers", rgc_names, mic_names))
mode_to_work <- mode_to_work %>% mutate(geography = factor(geography, levels = ord)) %>% arrange(geography, grouping, year)
saveRDS(mode_to_work, "data/mode_to_work.rds")

# Destination Mode Share --------------------------------------------------
dest_mode_order <- c("Drove\nAlone", "Carpooled", "Transit", "Bike", "Walk", "TNC")

rgcs <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Regional_Growth_Centers/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>%
  select("name") %>%
  st_drop_geometry() %>%
  pull() %>%
  unique()

mics <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Manufacturing_Industrial_Centers/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>%
  select("mic") %>%
  st_drop_geometry() %>%
  pull() %>%
  unique()

rgc_modes <- read_csv(rgc_dest_mod_file) %>% 
  drop_na %>%
  mutate(concept = case_when(
    dpurp == "Work" ~ "Work",
    TRUE ~ "Non-Work")) %>%
  mutate(grouping = case_when(
    mode == "SOV" ~ "Drove Alone",
    mode %in% c("HOV2", "HOV3+") ~ "Carpooled",
    mode == "Bike" ~ "Bike",
    mode %in% c("School Bus", "Transit") ~ "Transit",
    mode == "TNC" ~ "TNC",
    mode == "Walk" ~ "Walk")) %>%
  mutate(year = as.character(base_model_yr)) %>% 
  select(geography="hh_rgc", "year", "grouping", estimate="trexpfac", "concept") %>%
  group_by(geography, year, grouping, concept) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>%
  mutate(geography_type = case_when(
    geography %in% mics ~ mic_title,
    geography %in% rgcs ~ rgc_title,
    TRUE ~ "Not in a Center")) %>%
  mutate(geography = gsub("Redmond-Overlake", "Redmond Overlake", geography)) %>%
  mutate(geography = gsub("Bellevue", "Bellevue Downtown", geography)) %>%
  mutate(geography = gsub("Kent MIC", "Kent", geography)) %>%
  mutate(geography = gsub("Paine Field / Boeing Everett", "Paine Field/Boeing Everett", geography)) %>%
  mutate(geography = gsub("Sumner Pacific", "Sumner-Pacific", geography)) %>%
  mutate(geography = gsub("Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton", geography)) %>%
  mutate(geography = gsub("Cascade", "Cascade Industrial Center - Arlington/Marysville", geography)) %>%
  mutate(grouping = str_wrap(grouping, width=10)) %>%
  mutate(grouping = factor(grouping, levels = dest_mode_order)) %>%
  arrange(geography, grouping, year)

# RGC and MIC's
centers <- rgc_modes %>%
  filter(geography != "Not in RGC")

totals <- centers %>%
  group_by(geography, year, concept, geography_type) %>%
  summarise(total = sum(estimate)) %>%
  as_tibble()

centers <- left_join(centers, totals,  by=c("geography", "year", "concept", "geography_type")) %>%
  mutate(share = estimate / total) %>%
  select(-"total")

rm(totals)

# Region
region <- rgc_modes %>%
  group_by(year, grouping, concept) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>% 
  mutate(geography = "Region", geography_type = "Region")

totals <- region %>%
  group_by(year, concept) %>%
  summarise(total = sum(estimate)) %>%
  as_tibble()

region <- left_join(region, totals,  by=c("year", "concept")) %>%
  mutate(share = estimate / total) %>%
  select(-"total")

rm(totals)

# All RGCs
all_rgcs <- rgc_modes %>%
  filter(geography_type == rgc_title & geography != "Not in RGC") %>%
  group_by(year, grouping, concept) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>% 
  mutate(geography = "All Centers", geography_type = rgc_title)

totals <- all_rgcs %>%
  group_by(year, concept) %>%
  summarise(total = sum(estimate)) %>%
  as_tibble()

all_rgcs <- left_join(all_rgcs, totals,  by=c("year", "concept")) %>%
  mutate(share = estimate / total) %>%
  select(-"total")

rm(totals)

# All MICs
all_mics <- rgc_modes %>%
  filter(geography_type == mic_title & geography != "Not in RGC") %>%
  group_by(year, grouping, concept) %>%
  summarise(estimate = sum(estimate)) %>%
  as_tibble() %>% 
  mutate(geography = "All Centers", geography_type = mic_title)

totals <- all_mics %>%
  group_by(year, concept) %>%
  summarise(total = sum(estimate)) %>%
  as_tibble()

all_mics <- left_join(all_mics, totals,  by=c("year", "concept")) %>%
  mutate(share = estimate / total) %>%
  select(-"total")

rm(totals)

destination_mode_share <- bind_rows(centers, region, all_rgcs, all_mics)
ord <- unique(c("Region", "All Centers", rgc_names, mic_names))
destination_mode_share <- destination_mode_share %>% mutate(geography = factor(geography, levels = ord)) %>% arrange(geography, grouping, year)
rm(dest_mode_order, centers, region, all_rgcs, all_mics, rgc_modes)
saveRDS(destination_mode_share, "data/destination_mode_share.rds")

# Transit Service ---------------------------------------------------------
stops <- read_csv("data/stops.txt") |> 
  select(stop_id,stop_lat,stop_lon)

stops_layer <- st_as_sf(stops, coords = c("stop_lon","stop_lat"), crs = wgs84) |>
  st_transform(spn)

rgc <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Regional_Growth_Centers/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") |>
  select(rgc="name") |>
  mutate(rgc = gsub("Redmond-Overlake", "Redmond Overlake", rgc)) |>
  mutate(rgc = gsub("Bellevue", "Bellevue Downtown", rgc)) |>
  st_transform(spn)

mic <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Manufacturing_Industrial_Centers/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") |>
  select("mic") %>%
  mutate(mic = gsub("Kent MIC", "Kent", mic)) |>
  mutate(mic = gsub("Paine Field / Boeing Everett", "Paine Field/Boeing Everett", mic)) |>
  mutate(mic = gsub("Sumner Pacific", "Sumner-Pacific", mic)) |>
  mutate(mic = gsub("Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton", mic)) |>
  mutate(mic = gsub("Cascade", "Cascade Industrial Center - Arlington/Marysville", mic)) |>
  st_transform(spn)

r <- st_intersection(stops_layer, rgc) |> st_drop_geometry()
m <- st_intersection(stops_layer, mic) |> st_drop_geometry()

stops <- left_join(stops, r, by=c("stop_id"))
stops <- left_join(stops, m, by=c("stop_id"))
stops <- stops |> mutate(rgc = replace_na(rgc, "Not in Center")) |> mutate(mic = replace_na(mic, "Not in Center"))
rm(stops_layer, rgc, mic, r, m)

# Clean Up Routes
routes <- read_csv("data/routes.txt") |>
  mutate(route_short_name=as.character(route_short_name)) |>
  mutate(route_short_name=str_replace_all(route_short_name, "\\.0","")) |>
  # Cleanup Agency Names for ST Express Routes
  mutate(agency_name=case_when(
    route_short_name %in% st_express ~ "Sound Transit",
    !(route_short_name %in% st_express) ~ agency_id)) |>
  # Set Agency Names
  mutate(agency_name=case_when(
    agency_name %in% c("29") ~ "Community Transit",
    agency_name %in% c("3") ~ "Pierce Transit",
    agency_name %in% c("97") ~ "Everett Transit",
    agency_name %in% c("EOS","23") ~ "City of Seattle",
    agency_name %in% c("KCM","1") ~ "King County Metro",
    agency_name %in% c("KMD") ~ "King County Marine Division",
    agency_name %in% c("kt") ~ "Kitsap Transit",
    agency_name %in% c("Sound Transit","","40","ST") ~ "Sound Transit",
    agency_name %in% c("95","WSF") ~ "Washington State Ferries")) |>
  # Get Route Description and Long Names Consistent
  mutate(route_long_name=case_when(
    is.na(route_long_name) ~ route_desc,
    !(is.na(route_long_name)) ~ route_long_name)) |>
  # Flag BRT Routes
  mutate(transit_type=case_when(
    route_short_name %in% brt_routes ~ "BRT",
    !(route_short_name %in% brt_routes) ~ "0")) |>
  mutate(transit_type=case_when(
    transit_type == "BRT" ~ "BRT",
    route_type == 0 ~ "Light Rail or Streetcar",
    route_type == 2 ~ "Commuter Rail",
    route_type == 3 ~ "Bus",
    route_type == 4 ~ "Ferry")) %>%
  # Remove Extra Attributes
  select("route_id", "route_short_name", "route_long_name", "agency_name", "transit_type")

# Add transit modes served to each stop: BRT, LRT, CRT, Ferry, and Bus
stoptimes <- read_csv("data/stop_times.txt") |> select("trip_id", "stop_id")
trips <- read_csv("data/trips.txt") |> select("trip_id", "route_id")
trips <- left_join(trips, routes, by=c("route_id"))
trip_stops <- left_join(stoptimes, trips, by=c("trip_id")) |> select("stop_id", "transit_type") |> distinct()

# BRT
s <- trip_stops |> filter(transit_type == "BRT") |> rename(brt="transit_type")
stops <- left_join(stops, s , by="stop_id") 

# Bus
s <- trip_stops |> filter(transit_type == "Bus") |> rename(bus="transit_type")
stops <- left_join(stops, s , by="stop_id") 

# Commuter Rail
s <- trip_stops |> filter(transit_type == "Commuter Rail") |> rename(crt="transit_type")
stops <- left_join(stops, s , by="stop_id") 

# Ferry
s <- trip_stops |> filter(transit_type == "Ferry") |> rename(ferry="transit_type")
stops <- left_join(stops, s , by="stop_id") 

# Light Rail or Streetcar
s <- trip_stops |> filter(transit_type == "Light Rail or Streetcar") |> rename(lrt="transit_type")
stops <- left_join(stops, s , by="stop_id") 

stops_layer <- st_as_sf(stops, coords = c("stop_lon","stop_lat"), crs = wgs84)
rm(s, routes, stoptimes, trip_stops, trips, stops)
saveRDS(stops_layer, "data/stops_layer.rds")
