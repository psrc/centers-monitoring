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

# Packaage for Excel Data Creation
library(openxlsx)

wgs84 <- 4326
spn <- 32148

current_census_yr <- (lubridate::year(Sys.Date())-2)
current_employment_yr <- 2022
census_years <- c(current_census_yr-10, current_census_yr-5, current_census_yr)
ofm_years <- c(2011, 2016, 2021, 2022)
industrial_years <- c(2010, 2015, 2020) 
year_ord <- c("2022","2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010")

# Run Modules files -------------------------------------------------------
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)
source("functions.R")

# Inputs ---------------------------------------------------------------
rgc_title <- "Regional Growth Center (6/22/2023)"
mic_title <- "MIC (2022 RTP)"

# Demographic Metrics
pop_hh_hu_data <- readRDS("data/center_pop_hh_hu.rds") |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography))

age_data <- readRDS("data/population_by_age.rds") |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography))

race_data <- readRDS("data/population_by_race.rds") |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography))

income_data <- readRDS("data/households_by_income.rds") |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography))

education_data <- readRDS("data/educational_attainment.rds") |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography))

employment_data <- readRDS("data/centers_employment.rds") 

# Housing Data Metrics
tenure_data <- readRDS("data/households_by_tenure.rds") |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography))

type_data <- readRDS("data/housing_units_by_type.rds") |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography))

burden_data <- readRDS("data/cost_burden.rds") |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography)) 

unit_data <- readRDS("data/center_hu.rds") |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography))

renter_burden_data <- burden_data |> filter(concept == "Renter Cost Burden")

owner_burden_data <- burden_data |> filter(concept == "Owner Cost Burden")

# Transportation Metrics
mode_data <- readRDS("data/mode_to_work.rds") |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography))

destination_mode_data <- readRDS("data/destination_mode_share.rds") |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", geography)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography)) |>
  filter(concept == "Work") |>
  filter(!(geography %in% c("Puget Sound Industrial Center - Bremerton", "North Tukwila", "Port of Tacoma", "Sumner-Pacific")))

transit_stop_data <- readRDS("data/stops_layer.rds") |>
  mutate(rgc = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", rgc))

# Centers Information
centers_info <- read_csv("data/centers_information.csv", show_col_types = FALSE)
intersection_density <- read_csv("data/center_intersection_density.csv", show_col_types = FALSE)

# Source information
source_info <- read_csv("data/source_information.csv", show_col_types = FALSE)

# Shapefiles --------------------------------------------------------------
rgc_shape <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Regional_Growth_Centers/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") |>
  mutate(name = gsub("Bellevue", "Bellevue Downtown", name)) |>
  mutate(name = gsub("Redmond-Overlake", "Redmond Overlake", name)) |>
  mutate(name = gsub("Greater Downtown Kirkland", "Kirkland Greater Downtown", name)) |>
  select("name", "acres")

rgc_names <- rgc_shape |> st_drop_geometry() |> select("name") |> arrange(name) |> distinct() |> pull()
random_rgc <- rgc_names[[sample(1:length(rgc_names), 1)]]

mic_shape <- st_read("https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services/Manufacturing_Industrial_Centers/FeatureServer/0/query?where=0=0&outFields=*&f=pgeojson") |>
  mutate(mic = gsub("Kent MIC", "Kent", mic)) |>
  mutate(mic = gsub("Paine Field / Boeing Everett", "Paine Field/Boeing Everett", mic)) |>
  mutate(mic = gsub("Sumner Pacific", "Sumner-Pacific", mic)) %>%
  mutate(mic = gsub("Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton", mic)) |>
  mutate(mic = gsub("Cascade", "Cascade Industrial Center - Arlington/Marysville", mic)) |>
  select(name="mic", "acres")

mic_names <- mic_shape %>% st_drop_geometry() %>% select("name") %>% arrange(name) %>% distinct() %>% pull()
random_mic <- mic_names[[sample(1:length(mic_names), 1)]]

# MIC Measures ------------------------------------------------------------
ord <- unique(c("Region", "All Centers", "All RGCs", "All MICs", mic_names))

vacancy_absorption <- read_csv("data/mic-vacancy-absorption.csv", show_col_types = FALSE)

land_ord <- c("Core Industrial", "Industrial-Commercial", "Aviation Operations",
              "Military", "Total Industrial", 
              "Limited Industrial", "Non-Industrial*", "Total Gross Acreage",
              "share")

industrial_land <- read_csv("data/mic-industrial-lands.csv", show_col_types = FALSE) |>
  pivot_longer(cols = !c(year, geography, geography_type), names_to = "grouping", values_to = "estimate") |>
  mutate(year = as.character(year)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography)) |>
  mutate(geography = factor(geography, levels = ord)) |>
  mutate(grouping = factor(grouping, levels = land_ord)) |>
  arrange(geography, grouping, year) |>
  filter(grouping != "share")

# Data for IL Pie Chart  
industrial_land_shares <- industrial_land |>
  filter(grouping %in% c("Total Industrial", "Limited Industrial", "Non-Industrial*", "Total Gross Acreage"))

total <- industrial_land_shares |> filter(grouping == "Total Gross Acreage") |> rename(total="estimate") |> select(-"grouping")

industrial_land_shares <- left_join(industrial_land_shares, total, by=c("year", "geography", "geography_type")) |>
  mutate(share = estimate / total, estimate = round(estimate, 0)) |>
  select(-"total") |>
  filter(grouping != "Total Gross Acreage")

rm(total)

# Data for IL Table
total <- industrial_land |> filter(grouping == "Total Gross Acreage") |> rename(total="estimate") |> select(-"grouping")

industrial_land <- left_join(industrial_land, total, by=c("year", "geography", "geography_type")) |>
  mutate(share = estimate / total, estimate = round(estimate, 0)) |>
  select(-"total")

rm(total)

# Data for Industrial Jobs
industrial_jobs <- read_csv("data/mic-industrial-jobs.csv", show_col_types = FALSE) |>
  filter(year %in% industrial_years) |>
  mutate(year = as.character(year)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography)) |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  filter(grouping %in% c("Industrial", "Non-industrial")) |>
  mutate(geography = factor(geography, levels = ord)) |>
  arrange(geography, grouping, year)
