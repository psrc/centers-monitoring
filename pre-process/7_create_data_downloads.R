# Libraries -----------------------------------------------------------------
library(tidyverse)
library(psrcelmer)
library(sf)
library(openxlsx)

wgs84 <- 4326
spn <- 32148

# Functions ---------------------------------------------------------------
create_public_spreadsheet <- function(table_list, place_name) {
  
  hs <- createStyle(
    fontColour = "black",
    border = "bottom",
    fgFill = "#00a7a0",
    halign = "center",
    valign = "center",
    textDecoration = "bold"
  )
  
  ns <- createStyle(
    fontName = "Poppins",
    fontColour = "black",
    fontSize = 11,
    halign = "left",
    valign = "center",
  )
  
  table_idx <- 1
  sheet_idx <- 2
  
  wb <- loadWorkbook("data/metadata.xlsx")
  
  # Set Font Style
  addStyle(wb = wb, sheet = "Data Notes", style = ns, rows = 1:2, cols = 2)
  
  writeData(
    wb = wb,
    sheet = "Data Notes",
    x = place_name,
    xy = c(2,1))
  
  writeData(
    wb = wb,
    sheet = "Data Notes",
    x = Sys.Date(),
    xy = c(2,2))
  
  for (i in table_list) {
    for (j in names(table_list)) {
      if (names(table_list)[table_idx] == j) {
        
        addWorksheet(wb, sheetName = j)
        writeDataTable(wb, sheet = sheet_idx, x = i, tableStyle = "none", headerStyle = hs, withFilter = FALSE)
        setColWidths(wb, sheet = sheet_idx, cols = 1:length(i), widths = "auto")
        freezePane(wb, sheet = sheet_idx, firstRow = TRUE)
        
      } else {next}
    }
    if (table_idx < length(table_list)) {
      
      table_idx <- table_idx + 1
      sheet_idx <- sheet_idx + 1
      
    } else {break}
  }
  
  return(wb)
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

center_names <- c(rgc_names, mic_names)

# Center Data ---------------------------------------------------------------
pop_hh_hu_data <- readRDS("data/pop_hsg_data.rds")
unit_data <- readRDS("data/housing_unit_data.rds")
age_data <- readRDS("data/population_by_age.rds") 
race_data <- readRDS("data/population_by_race.rds")
income_data <- readRDS("data/households_by_income.rds") 
education_data <- readRDS("data/educational_attainment.rds") 
employment_data <- readRDS("data/centers_employment.rds")
tenure_data <- readRDS("data/households_by_tenure.rds") 
type_data <- readRDS("data/housing_units_by_type.rds") 
renter_burden_data <- readRDS("data/renter_cost_burden.rds")
owner_burden_data <- readRDS("data/owner_cost_burden.rds")
mode_data <- readRDS("data/mode_to_work.rds")
destination_mode_data <- readRDS("data/destination_mode_share.rds")
transit_stop_data <- readRDS("data/transit_stop_data.rds")
transit_stop_lyr <- readRDS("data/transit_stop_lyr.rds")
transit_route_lyr <- readRDS("data/transit_route_lyr.rds")

intersection_density <- read_csv("data/center_intersection_density.csv", show_col_types = FALSE)
centers_info <- read_csv("data/centers_information.csv", show_col_types = FALSE)
source_info <- read_csv("data/source_information.csv", show_col_types = FALSE)

industrial_land <- readRDS("data/industrial_land.rds")
industrial_jobs <- readRDS("data/industrial_jobs.rds")
industrial_land_shares <- readRDS("data/industrial_land_shares.rds")
vacancy_absorption <- readRDS("data/vacancy_absorption.rds")

# Create Spreadsheet ------------------------------------------------------
for(center_name in center_names) {
  
  tn <- create_public_spreadsheet(table_list = list("Population" = pop_hh_hu_data  |> filter(geography %in% c(center_name) & grouping == "Population"), 
                                                    "Age" = age_data  |> filter(geography %in% c(center_name)),
                                                    "Race" = race_data  |> filter(geography %in% c(center_name)),
                                                    "Household Income" = income_data  |> filter(geography %in% c(center_name)),
                                                    "Educational Attainment" = education_data  |> filter(geography %in% c(center_name)),
                                                    "Jobs" = employment_data |> filter(geography %in% c(center_name)),
                                                    "Housing Units" = pop_hh_hu_data  |> filter(geography %in% c(center_name) & grouping == "Housing Units"),
                                                    "Net Housing Units" = unit_data  |> filter(geography %in% c(center_name)),
                                                    "Housing Tenure" = tenure_data  |> filter(geography %in% c(center_name)),
                                                    "Housing Type" = type_data  |> filter(geography %in% c(center_name)),
                                                    "Renter Cost Burden" = renter_burden_data  |> filter(geography %in% c(center_name)),
                                                    "Owner Cost Burden" = owner_burden_data  |> filter(geography %in% c(center_name)),
                                                    "Transit Stops" = transit_stop_data |> st_drop_geometry() |> filter(geography %in% c(center_name)),
                                                    "Resident Mode Share" = mode_data  |> filter(geography %in% c(center_name)),
                                                    "Destination Mode Share" = destination_mode_data  |> filter(geography %in% c(center_name)),
                                                    "Intersection Density" = intersection_density |> filter(name %in% c(center_name))
  ), place_name = center_name)
  
  saveWorkbook(tn, file = paste0("data-downloads/", str_to_lower(str_replace_all(str_replace_all(center_name, "\\/", "_"), " ", "_")), "_data.xlsx"), overwrite = TRUE)

}




