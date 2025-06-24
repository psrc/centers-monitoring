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
spn <- 32148

current_census_yr <- 2023
current_employment_yr <- 2023
census_years <- c(current_census_yr-10, current_census_yr-5, current_census_yr)
pop_hsg_yrs <- c("2010", "2020", "2022", "2023", "2024")
hu_yrs <- c("2011", "2016", "2021", "2022", "2023", "2024")
industrial_years <- c("2010", "2015", "2020", "2022") 
year_ord <- c("2024","2023","2022","2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010")

gtfs_year <- "2025"
gtfs_service <- "Spring"

rgc_title <- "Regional Growth Center (4/23/2024)"
mic_title <- "MIC (1/5/2024)"

# Run Modules files -------------------------------------------------------
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)
source("functions.R")

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

# Center Boundaries --------------------------------------------------------------
rgc_shape <- readRDS("data/rgc_shape.rds")

rgc_names <- rgc_shape |> 
  st_drop_geometry() |> 
  select("name") |> 
  arrange(name) |> 
  distinct() |> 
  pull()

random_rgc <- rgc_names[[sample(1:length(rgc_names), 1)]]

mic_shape <- readRDS("data/mic_shape.rds")

mic_names <- mic_shape |>
  st_drop_geometry() |>
  select("name") |>
  arrange(name) |>
  distinct()|>
  pull()

random_mic <- mic_names[[sample(1:length(mic_names), 1)]]
