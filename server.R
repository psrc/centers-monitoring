# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  footer_server('psrcfooter')
  
  leftpanel_server('rgcleftpanel',
                   contact_name = "Maggie Moore",
                   contact_phone = "206-464-6171",
                   contact_email = "mmoore@psrc.org",
                   contact_title = "Senior Planner",
                   photo_filename = "redmondconnector.jpeg")

  leftpanel_server('micleftpanel',
                   contact_name = "Maggie Moore",
                   contact_phone = "206-464-6171",
                   contact_email = "mmoore@psrc.org",
                   contact_title = "Senior Planner",
                   photo_filename = "portoftacomaindustrialland.jpg")
    
  banner_server('rgcBanner', 
                banner_title = "Regional Growth Centers", 
                banner_subtitle = "Centers",
                banner_url = "https://www.psrc.org/our-work/centers")
  
  banner_server('micBanner', 
                banner_title = "Manufacturing and Industrial Centers", 
                banner_subtitle = "Centers",
                banner_url = "https://www.psrc.org/our-work/centers")
  
  demographics_server('rgcDemographics',
                      center_name = reactive(input$RGC),
                      center_type = rgc_title)
  
  demographics_server('micDemographics',
                      center_name = reactive(input$MIC),
                      center_type = mic_title)
  
  transportation_server('rgcTransportation',
                      center_name = reactive(input$RGC),
                      center_type = rgc_title,
                      center_desc = "rgc")
  
  transportation_server('micTransportation',
                      center_name = reactive(input$MIC),
                      center_type = mic_title,
                      center_desc = "mic")
  
# Center Maps -------------------------------------------
  output$rgc_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl=FALSE)) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addPolygons(data = rgc_shape %>% filter(name %in% input$RGC),
                  fillColor = "76787A",
                  weight = 4,
                  opacity = 1.0,
                  color = "#91268F",
                  dashArray = "4",
                  fillOpacity = 0.0)
  })
  
  output$mic_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl=FALSE)) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addPolygons(data = mic_shape %>% filter(name %in% input$MIC),
                  fillColor = "76787A",
                  weight = 4,
                  opacity = 1.0,
                  color = "#91268F",
                  dashArray = "4",
                  fillOpacity = 0.0)
  })

# Center Summary Data -----------------------------------------------------

  rgc_descritpion_filter <- reactive({
   centers_info |>
     filter(rgc_mic=="Regional Growth Center" & name == input$RGC) |>
     select("information") |>
     pull()
  })

  output$RGCDescription <- renderText(rgc_descritpion_filter())
  output$rgc_summary_table <- DT::renderDataTable({create_rgc_summary_table(center_name = input$RGC, yr = 2021)})
  
  mic_descritpion_filter <- reactive({
   centers_info |>
     filter(rgc_mic=="Manufacturing Industrial Center" & name == input$MIC) |>
     select("information") |>
     pull()
  })
  
  output$MICDescription <- renderText(mic_descritpion_filter())
  output$mic_summary_table <- DT::renderDataTable(create_mic_summary_table(center_name = input$MIC, yr = 2021))

  output$lu_map <- renderImage({
    
    ifelse(input$RGC == "Seattle First Hill/Capitol Hill", imgfn <- "Seattle First Hill Capitol Hill.jpg", imgfn <- paste0(input$RGC, '.jpg'))
    
    filename <- normalizePath(file.path('./www', imgfn))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         width = 400,
         height = 400,
         alt = paste("Land use map for", input$RGC))
    
  }, deleteFile = FALSE)
  

  rgc_summary_data <- reactive({
    
    create_public_spreadsheet(table_list = list("Population" = pop_hh_hu_data |> select(-"year") |> filter(geography %in% c(input$RGC) & geography_type %in% c(rgc_title) & grouping == "Population"), 
                                                "Age" = age_data |> select(-"year") |> filter(geography %in% c(input$RGC) & geography_type %in% c(rgc_title)),
                                                "Race" = race_data |> select(-"year") |> filter(geography %in% c(input$RGC) & geography_type %in% c(rgc_title)),
                                                "Household Income" = income_data |> select(-"year") |> filter(geography %in% c(input$RGC) & geography_type %in% c(rgc_title)),
                                                "Educational Attainment" = education_data |> select(-"year") |> filter(geography %in% c(input$RGC) & geography_type %in% c(rgc_title)),
                                                "Jobs" = employment_data|> select(-"year") |> filter(geography %in% c(input$RGC) & geography_type %in% c(rgc_title)),
                                                "Housing Units" = pop_hh_hu_data |> select(-"year") |> filter(geography %in% c(input$RGC) & geography_type %in% c(rgc_title) & grouping == "Housing Units"),
                                                "Net Housing Units" = unit_data |> select(-"year") |> filter(geography %in% c(input$RGC) & geography_type %in% c(rgc_title)),
                                                "Housing Tenure" = tenure_data |> select(-"year") |> filter(geography %in% c(input$RGC) & geography_type %in% c(rgc_title)),
                                                "Housing Type" = type_data |> select(-"year") |> filter(geography %in% c(input$RGC) & geography_type %in% c(rgc_title)),
                                                "Renter Cost Burden" = burden_data |> select(-"year") |> filter(concept == "Renter Cost Burden" & geography %in% c(input$RGC) & geography_type %in% c(rgc_title)),
                                                "Owner Cost Burden" = burden_data |> select(-"year") |> filter(concept == "Owner Cost Burden" & geography %in% c(input$RGC) & geography_type %in% c(rgc_title)),
                                                "Transit Stops" = transit_stop_data |> st_drop_geometry() |> filter(rgc %in% c(input$RGC)) |> select(-"mic"),
                                                "Resident Mode Share" = mode_data |> select(-"year") |> filter(geography %in% c(input$RGC) & geography_type %in% c(rgc_title)),
                                                "Destination Mode Share" = destination_mode_data |> select(-"year") |> filter(geography %in% c(input$RGC) & geography_type %in% c(rgc_title)),
                                                "Intersection Density" = intersection_density |> filter(name %in% c(input$RGC))
                                                ), place_name = input$RGC)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$RGC,"_summary_data.xlsx")},
    content <- function(file) {
      saveWorkbook(rgc_summary_data(), file = file)},
    contentType = "application/Excel"
  )
  

  output$urban_form_table <- DT::renderDataTable({create_rgc_urban_form_table(center_name = input$RGC)})
  
  output$rgc_hu_chart <- renderEcharts4r({
    
    echart_column_chart(df = pop_hh_hu_data %>% filter(geography_type == rgc_title, geography == input$RGC & grouping == "Housing Units"),
                        x = "data_year", y = "estimate", tog = "grouping", title = "Total Housing Units",
                        dec = 0, esttype = "number", color = "blues")
    
  })
  
  output$rgc_hu_table <- DT::renderDataTable({create_single_group_table(df = pop_hh_hu_data, rgc_name = input$RGC, data_yrs = ofm_years, dec = 0, group = "Housing Units")})
  
  output$rgc_hu_change_chart <- renderEcharts4r({
    
    echart_column_chart(df = unit_data %>% 
                          filter(geography_type %in% c(rgc_title) & geography %in% c(input$RGC) & year %in% ofm_years) %>% 
                          mutate(concept="New Net Housing Units") %>% 
                          mutate(delta = estimate-lag(estimate), data_year = paste0(lag(data_year),"-",data_year)) %>%
                          drop_na(),
                        x = "data_year", y = "delta", tog = "concept", title = "New Net Housing Units",
                        dec = 0, esttype = "number", color = "purples")
    
  })
  
  output$rgc_hu_change_table <- DT::renderDataTable({create_change_table(df = unit_data %>% 
                                                                           filter(geography_type %in% c(rgc_title) & geography %in% c(input$RGC) & year %in% ofm_years) %>% 
                                                                           mutate(delta = estimate-lag(estimate), data_year = paste0(lag(data_year),"-",data_year)) %>%
                                                                           drop_na(), 
                                                                         yr = "data_year",val="delta", nm="New Net Housing Units")})
  
  output$rgc_tenure_chart <- renderEcharts4r({
    
    echart_multi_column_chart(df = tenure_data %>% filter(geography_type %in% c(rgc_title, "County") & geography %in% c(input$RGC, "Region", "All Centers") & grouping != "Total"),
                              x = "grouping", y = "share", fill="geography", tog = "data_year", 
                              dec = 0, esttype = "percent", color = "jewel")
  })
  
  output$rgc_tenure_table <- DT::renderDataTable({create_multi_year_table(df = tenure_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
  output$rgc_type_chart <- renderEcharts4r({
    
    echart_multi_bar_chart(df = type_data %>% 
                             filter(geography_type %in% c(rgc_title, "County") & geography %in% c(input$RGC, "Region", "All Centers") & grouping != "Total") %>%
                             arrange(desc(grouping)),
                           x = "grouping", y = "share", fill="geography", tog = "data_year", 
                           dec = 0, esttype = "percent", color = "jewel")
  })
  
  output$rgc_type_table <- DT::renderDataTable({create_multi_year_table(df = type_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
  output$rgc_renter_burden_chart <- renderEcharts4r({
    
    echart_multi_column_chart(df = burden_data %>% 
                                filter(geography_type %in% c(rgc_title, "County") & geography %in% c(input$RGC, "Region", "All Centers") & grouping != "Total" & concept == "Renter Cost Burden"),
                              x = "grouping", y = "share", fill="geography", tog = "data_year", 
                              dec = 0, esttype = "percent", color = "jewel")
  })
  
  output$rgc_renter_burden_table <- DT::renderDataTable({create_multi_year_table(df = renter_burden_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
  output$rgc_owner_burden_chart <- renderEcharts4r({
    
    echart_multi_column_chart(df = burden_data %>% 
                                filter(geography_type %in% c(rgc_title, "County") & geography %in% c(input$RGC, "Region", "All Centers") & grouping != "Total" & concept == "Owner Cost Burden"),
                              x = "grouping", y = "share", fill="geography", tog = "data_year", 
                              dec = 0, esttype = "percent", color = "jewel")
    
  })
  
  output$rgc_owner_burden_table <- DT::renderDataTable({create_multi_year_table(df = owner_burden_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
  
  output$rgc_jobs_chart <- renderEcharts4r({
    
    echart_column_chart(df = employment_data |> 
                          filter(geography_type == rgc_title, geography == input$RGC & grouping == "Total") |>
                          mutate(estimate = as.integer(estimate)) |>
                          filter(data_year %in% ofm_years),
                        x = "data_year", y = "estimate", tog = "grouping", title = "Total Employment",
                        dec = 0, esttype = "number", color = "purples")
    
  })
  
  output$rgc_job_sectors_table <- DT::renderDataTable({create_rgc_jobs_by_sector_table(center_name = input$RGC)})
  
    
})    



