# Libraries -----------------------------------------------------------------
library(dplyr)
library(stringr)
library(sf)
library(openxlsx)

un <- Sys.getenv("USERNAME")
process_dir <- file.path("C:/Users",
                         str_to_lower(un),
                         "OneDrive - PSRC/coding/centers-monitoring")

source(file.path(process_dir, 
                 "pre-process", 
                 "0_general_inputs.R"))

# Place Shapefiles -------------------------------------------------------
place_shape <- readRDS(file.path(process_dir, 
                                 "data", 
                                 "place_shape.rds"))

mic_names <- place_shape |> 
  st_drop_geometry() |> 
  filter(place_type == mic_title) |> 
  select("place_name") |> 
  pull() |> 
  unique()

rgc_names <- place_shape |> 
  st_drop_geometry() |> 
  filter(place_type == rgc_title) |> 
  select("place_name") |> 
  pull() |> 
  unique()

# Functions ---------------------------------------------------------------
create_public_spreadsheet <- function(table_list, center_name) {
  
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
    x = center_name,
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
        writeDataTable(wb, 
                       sheet = sheet_idx, 
                       x = i, 
                       tableStyle = "none", 
                       headerStyle = hs, 
                       withFilter = FALSE)
        setColWidths(wb, 
                     sheet = sheet_idx, 
                     cols = 1:length(i), 
                     widths = "auto")
        freezePane(wb, 
                   sheet = sheet_idx, 
                   firstRow = TRUE)
        
      } else {next}
    }
    if (table_idx < length(table_list)) {
      
      table_idx <- table_idx + 1
      sheet_idx <- sheet_idx + 1
      
    } else {break}
  }
  
  return(wb)
}

# Center Data  Files ---------------------------------------------------------
pop_hh_hu_data <- readRDS(file.path(process_dir,
                                    "data",
                                    "population-housing.rds"))

housing_units_data <- readRDS(file.path(process_dir,
                                    "data",
                                    "housing_units.rds"))

acs_data <- readRDS(file.path(process_dir,
                              "data",
                              "census_data.rds"))

employment_data <- readRDS(file.path(process_dir,
                                     "data",
                                     "covered_employment.rds"))

destination_mode_data <- readRDS(file.path(process_dir,
                                           "data",
                                           "destination_mode_share.rds"))

transit_stop_data <- readRDS(file.path(process_dir,
                                       "data",
                                       "transit_stop_data.rds"))
                                       
intersection_density <- readRDS(file.path(process_dir,
                                          "data",
                                          "intersection_density.rds"))

industrial_land <- readRDS(file.path(process_dir,
                                     "data",
                                     "industrial_land.rds"))

industrial_jobs <- readRDS(file.path(process_dir,
                                     "data",
                                     "industrial_jobs.rds"))

industrial_land_shares <- readRDS(file.path(process_dir,
                                            "data",
                                            "industrial_land_shares.rds"))

# Create RGC Spreadsheet ------------------------------------------------------
for(nm in rgc_names) {
  
  tn <- create_public_spreadsheet(
    table_list = list(
      "Population" = pop_hh_hu_data  |> 
        filter(place_name == nm & grouping == "Population"),
      
      "Age" = acs_data  |> 
        filter(place_name == nm & concept == "Population by Age Group"),
      
      "Race" = acs_data  |> 
        filter(place_name == nm & concept == "Population by Race & Ethnicity"),
      
      "Household Income" = acs_data  |> 
        filter(place_name == nm & concept == "Households by Income"),
      
      "Educational Attainment" = acs_data  |> 
        filter(place_name == nm & concept == "Educational Attainment"),
      
      "Jobs" = employment_data |> 
        filter(place_name == nm),
      
      "Housing Units" = pop_hh_hu_data  |> 
        filter(place_name == nm & grouping == "Housing Units"),
      
      "Net Housing Units" = housing_units_data  |> 
        filter(place_name == nm),
      
      "Housing Tenure" = acs_data  |> 
        filter(place_name == nm & concept == "Household Tenure"),
      
      "Housing Type" = acs_data  |> 
        filter(place_name == nm & concept == "Housing Unit Type"),
      
      "Renter Cost Burden" = acs_data  |> 
        filter(place_name == nm & concept == "Renter Cost Burden"),
      
      "Owner Cost Burden" = acs_data  |> 
        filter(place_name == nm & concept == "Owner Cost Burden"),

      "Transit Stops" = transit_stop_data |> 
        filter(place_name == nm),
      
      "Resident Mode Share" = acs_data  |> 
        filter(place_name == nm & concept == "Mode to Work"),
      
      "Destination Mode Share" = destination_mode_data  |> 
        filter(place_name == nm),
      
      "Vehicle Availability" = acs_data  |> 
        filter(place_name == nm & 
                 concept == "Households by Vehicle Availability"),
      
      "Intersection Density" = intersection_density |> 
        filter(place_name == nm)), 
    
    center_name = nm)
  
  saveWorkbook(tn, file = file.path(process_dir,"data-downloads", 
                                    paste0(str_to_lower(
                                      str_replace_all(
                                        str_replace_all(nm, "\\/", "_"),
                                        " ", "_")),
                                      "_data.xlsx")
                                    ), 
                                    overwrite = TRUE)

}

# Create MIC Spreadsheet ------------------------------------------------------
for(nm in mic_names) {
  
  tn <- create_public_spreadsheet(
    table_list = list(
      
      "Jobs" = employment_data |> 
        filter(place_name == nm),
      
      "Industrial Jobs" = industrial_jobs |> 
        filter(place_name == nm),
      
      "Population" = pop_hh_hu_data  |> 
        filter(place_name == nm & grouping == "Population"),
      
      "Housing Units" = pop_hh_hu_data  |> 
        filter(place_name == nm & grouping == "Housing Units"),
      
      "Transit Stops" = transit_stop_data |> 
        filter(place_name == nm),
      
      "Destination Mode Share" = destination_mode_data  |> 
        filter(place_name == nm),
      
      "Industrial Land" = industrial_land |> 
        filter(place_name == nm),
      
      "Industrial Land Shares" = industrial_land_shares |> 
        filter(place_name == nm)), 
    
    center_name = nm)
  
  saveWorkbook(tn, file = file.path(process_dir,"data-downloads", 
                                    paste0(str_to_lower(
                                      str_replace_all(
                                        str_replace_all(nm, "\\/", "_"),
                                        " ", "_")),
                                      "_data.xlsx")
  ), 
  overwrite = TRUE)
  
}