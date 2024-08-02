# Libraries -----------------------------------------------------------------
library(tidyverse)

# Basic Inputs ------------------------------------------------------------
rgc_title <- "Regional Growth Center (12/12/2023)"
mic_title <- "MIC (1/5/2024)"

# Employment Data
rgc_emp_file <- "data/rgc_covered_emp_2010_2023.csv"
mic_emp_file <- "data/mic_covered_emp_2010_2023.csv"

year_ord <- c("2023","2022","2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010")

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
  mutate(grouping = gsub("WTU", "Wholesale, Transportation & Utilities", grouping)) |>
  mutate(grouping = gsub("FIRE", "Finance, Insurance & Real Estate", grouping)) |>
  mutate(concept = "Employment", share=1, geography_type = rgc_title) |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography)) |>
  mutate(estimate = round(as.integer(estimate), -1))

mic_jobs <- read_csv(mic_emp_file) |> 
  filter(center != "MIC Total") |>
  mutate(`Const/Res` = as.character(`Const/Res`), Government = as.character(Government), `Public Education` = as.character(`Public Education`)) |>
  mutate(WTU = as.character(WTU), Total = as.character(Total)) |>
  pivot_longer(cols = !c(year,center), names_to = "grouping", values_to = "estimate") |>
  rename(geography="center") |>
  mutate(estimate = gsub("S", "*", estimate)) |>
  mutate(geography = gsub("Kent MIC", "Kent", geography)) |>
  mutate(geography = gsub("Paine Field / Boeing Everett", "Paine Field/Boeing Everett", geography)) |>
  mutate(geography = gsub("Sumner Pacific", "Sumner-Pacific", geography)) |>
  mutate(geography = gsub("Puget Sound Industrial Center- Bremerton", "Puget Sound Industrial Center - Bremerton", geography)) |>
  mutate(geography = gsub("Cascade", "Cascade Industrial Center - Arlington/Marysville", geography)) |>
  mutate(grouping = gsub("Const/Res", "Construction / Resources", grouping)) |>
  mutate(grouping = gsub("WTU", "Wholesale, Transportation & Utilities", grouping)) |>
  mutate(grouping = gsub("FIRE", "Finance, Insurance & Real Estate", grouping)) |>
  mutate(concept = "Employment", share=1, geography_type = mic_title) |>
  mutate(data_year = factor(year, levels=year_ord)) |>
  mutate(geography = case_when(
    geography == "All Centers" & geography_type == rgc_title ~ "All RGCs",
    geography == "All Centers" & geography_type == mic_title ~ "All MICs",
    geography != "All Centers" ~ geography)) |>
  mutate(estimate = round(as.integer(estimate), -1))

centers_employment <- bind_rows(rgc_jobs, mic_jobs)

# Get Totals for Shares
totals <- centers_employment |> filter(grouping == "Total") |> select("year", "geography", "geography_type", total="estimate")
centers_employment <- left_join(centers_employment, totals, by=c("year", "geography", "geography_type")) |> mutate(share = estimate/total) |> select(-"total")
saveRDS(centers_employment, "data/centers_employment.rds")
