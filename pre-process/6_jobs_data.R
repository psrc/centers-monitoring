# Libraries -----------------------------------------------------------------
library(tidyverse)

# Basic Inputs ------------------------------------------------------------
un <- Sys.getenv("USERNAME")
process_dir <- file.path("C:/Users",
                         str_to_lower(un),
                         "OneDrive - PSRC/coding/centers-monitoring")

source(file.path(process_dir, "pre-process", "0_general_inputs.R"))

# Employment Data
place_emp_file <- file.path(process_dir, 
                            "data", 
                            "centers_covered_emp_2010_2015_2020_2025.csv")

place_jobs <- read_csv(place_emp_file) |>
  mutate(Government = as.character(Government), 
         `Public Education` = as.character(`Public Education`)) |>
  pivot_longer(cols = !c(year, place_id, place_name, place_type, place_category), 
               names_to = "grouping", 
               values_to = "estimate") |>
  mutate(estimate = gsub("S", 
                         "*", 
                         estimate)) |>
  mutate(grouping = gsub("Const/Res", 
                         "Construction / Resources", 
                         grouping)) |>
  mutate(grouping = gsub("WTU", 
                         "Wholesale, 
                         Transportation & Utilities", 
                         grouping)) |>
  mutate(grouping = gsub("FIRE", 
                         "Finance, 
                         Insurance & Real Estate", 
                         grouping)) |>
  mutate(concept = "Employment", share=1) |>
  mutate(estimate = round(as.integer(estimate), -1))

# Get Totals for Shares
totals <- place_jobs |> 
  filter(grouping == "Total Jobs") |> 
  select("year", "place_id", total="estimate")

place_jobs <- left_join(place_jobs, totals, by=c("year", "place_id")) |> 
  mutate(share = estimate/total) |> 
  select(-"total") |>
  mutate(year = as.character(year))

saveRDS(place_jobs, file.path(process_dir, "data", "covered_employment.rds"))
