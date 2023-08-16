# Packages ----------------------------------------------------------------

# Packages for Interactive Web application
library(shiny)
library(shinydashboard)
library(bs4Dash)

# Packages for Data Cleaning/Processing
library(tidyverse)

# Packages for Chart Creation
library(psrcplot)
library(plotly)
library(echarts4r)

# Packages for Table Creation
library(DT)

# Packages for Maps
library(sf)
library(leaflet)

wgs84 <- 4326
spn <- 32148

current_census_yr <- (lubridate::year(Sys.Date())-2)
census_years <- c(current_census_yr-10, current_census_yr-5, current_census_yr)
year_ord <- c("2022","2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010")

# Run Modules files -------------------------------------------------------
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)
source("functions.R")

# Inputs ---------------------------------------------------------------
rgc_title <- "Regional Growth Center (6/22/2023)"
mic_title <- "MIC (2022 RTP)"

# Demographic Metrics
pop_hh_hu_data <- readRDS("data/center_pop_hh_hu.rds") %>% mutate(data_year = factor(year, levels=year_ord))
age_data <- readRDS("data/population_by_age.rds") %>% mutate(data_year = factor(year, levels=year_ord))
race_data <- readRDS("data/population_by_race.rds") %>% mutate(data_year = factor(year, levels=year_ord))
income_data <- readRDS("data/households_by_income.rds") %>% mutate(data_year = factor(year, levels=year_ord))
employment_data <- readRDS("data/centers_employment.rds") %>% mutate(data_year = factor(year, levels=year_ord))

# Housing Data Metrics
tenure_data <- readRDS("data/households_by_tenure.rds") %>% mutate(data_year = factor(year, levels=year_ord))
type_data <- readRDS("data/housing_units_by_type.rds") %>% mutate(data_year = factor(year, levels=year_ord))
burden_data <- readRDS("data/cost_burden.rds") %>% mutate(data_year = factor(year, levels=year_ord))

# Centers Information
centers_info <- read_csv("data/centers_information.csv", show_col_types = FALSE)

mic_names <- age_data %>% filter(geography_type== mic_title) %>% select("geography") %>% arrange(geography) %>% distinct() %>% pull()

# Shapefiles --------------------------------------------------------------
rgc_shape <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Regional_Growth_Centers/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") %>%
  mutate(name = gsub("Bellevue", "Bellevue Downtown", name)) %>%
  mutate(name = gsub("Redmond-Overlake", "Redmond Overlake", name)) %>%
  select("name", "acres")

rgc_names <- rgc_shape %>% st_drop_geometry() %>% select("name") %>% arrange(name) %>% distinct() %>% pull()
random_rgc <- rgc_names[[sample(1:length(rgc_names), 1)]]
