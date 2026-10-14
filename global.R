# Packages ----------------------------------------------------------------

# Packages for Interactive Web application
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinycssloaders)
library(shinyBS)

# Packages for Data Cleaning/Processing
library(tidyverse)

# Packages for Chart Creation
library(psrcplot)
library(plotly)
library(echarts4r)

# Packages for Table Creation
library(DT)
library(scales)

# Packages for Maps
library(sf)
library(leaflet)

# Package for Excel Data Creation
library(openxlsx)

# Inputs ------------------------------------------------------------------
wgs84 <- 4326
spn <- 2285

current_census_yr <- 2024
current_employment_yr <- 2025
census_years <- c(2010, 2015, 2020, 2024)
pop_hsg_yrs <- c("2010", "2015", "2020", "2025")
hu_yrs <- c("2010 to 2015", "2015 to 2020", "2020 to 2025")
industrial_years <- c("2010", "2015", "2020", "2025") 
model_base_yr <- "2023"

year_ord <- c("2025","2024","2023","2022","2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010")

gtfs_year <- "2026"
gtfs_service <- "Fall"

rgc_title <- "Regional Growth Center (3/26/2026)"
mic_title <- "MIC (7/10/2026)"

# Run Modules files -------------------------------------------------------
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)
source("functions.R")

# Center Data ---------------------------------------------------------------

# OFM Based Output
pop_hh_hu_data <- readRDS("data/population-housing.rds")
unit_data <- readRDS("data/housing_units.rds")

# ACS Based Output
acs_data <- readRDS("data/census_data.rds") |>
  filter(!(str_detect(grouping, "Total"))) |>
  mutate(year = factor(year, levels = year_ord))

age_data <- acs_data  |> 
  filter(concept == "Population by Age Group")

race_data <- acs_data  |> 
  filter(concept == "Population by Race & Ethnicity")

income_data <- acs_data  |> 
  filter(concept == "Households by Income")

education_data <- acs_data  |> 
  filter(concept == "Educational Attainment")

tenure_data <- acs_data  |> 
  filter(concept == "Household Tenure")

type_data <- acs_data  |> 
  filter(concept == "Housing Unit Type")

renter_burden_data <- acs_data  |> 
  filter(concept == "Renter Cost Burden")

owner_burden_data <- acs_data  |> 
  filter(concept == "Owner Cost Burden")

mode_data <- acs_data  |> 
  filter(concept == "Mode to Work")

vehicle_availability_data <- acs_data  |> 
  filter(concept == "Households by Vehicle Availability")

# Model Data
destination_mode_data <- readRDS("data/destination_mode_share.rds")
intersection_density <- readRDS("data/intersection_density.rds")

# GTFS Data
transit_stop_data <- readRDS("data/transit_stop_data.rds")
transit_stop_lyr <- readRDS("data/transit_stop_lyr.rds")
transit_route_lyr <- readRDS("data/transit_route_lyr.rds")

# Jobs Data
employment_data <- readRDS("data/covered_employment.rds")
industrial_land <- readRDS("data/industrial_land.rds")
industrial_jobs <- readRDS("data/industrial_jobs.rds")
industrial_land_shares <- readRDS("data/industrial_land_shares.rds")

# General Information
centers_info <- read_csv("data/centers_information.csv", show_col_types = FALSE)
source_info <- read_csv("data/source_information.csv", show_col_types = FALSE)

# Center Boundaries --------------------------------------------------------------
rgc_shape <- readRDS("data/rgc_shape.rds")

rgc_names <- rgc_shape |> 
  st_drop_geometry() |> 
  select("place_name") |> 
  arrange(place_name) |> 
  distinct() |> 
  pull()

random_rgc <- rgc_names[[sample(1:length(rgc_names), 1)]]

mic_shape <- readRDS("data/mic_shape.rds")

mic_names <- mic_shape |>
  st_drop_geometry() |>
  select("place_name") |>
  arrange(place_name) |>
  distinct()|>
  pull()

random_mic <- mic_names[[sample(1:length(mic_names), 1)]]
