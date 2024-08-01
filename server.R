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
                banner_title = "Manufacturing/Industrial Centers", 
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
  
  housing_server('rgcHousing',
                 center_name = reactive(input$RGC),
                 center_type = rgc_title)
  
  housing_server('micHousing',
                 center_name = reactive(input$MIC),
                 center_type = mic_title)
  
  jobs_server('rgcEmployment',
              center_name = reactive(input$RGC),
              center_type = rgc_title)
  
  jobs_server('micEmployment',
              center_name = reactive(input$MIC),
              center_type = mic_title)
  
  overview_server('rgcOverview',
              center_name = reactive(input$RGC),
              center_type = rgc_title)
  
  overview_server('micOverview',
              center_name = reactive(input$MIC),
              center_type = mic_title)
  
  form_server('rgcForm',
              center_name = reactive(input$RGC),
              center_type = rgc_title)
  
  form_server('micForm',
              center_name = reactive(input$MIC),
              center_type = mic_title)
  
  source_server('rgcSource', center_type = rgc_title)
  
  source_server('micSource', center_type = mic_title)
  
# Center Summary Data -----------------------------------------------------
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
  
  
  mic_summary_data <- reactive({
    
    create_public_spreadsheet(table_list = list("Jobs" = employment_data|> select(-"year") |> filter(geography %in% c(input$MIC) & geography_type %in% c(mic_title)),
                                                "Industrial Jobs" = industrial_jobs |> filter(geography %in% c(input$MIC) & geography_type %in% c(mic_title) & data_year %in% pop_hsg_yrs),
                                                "Population" = pop_hh_hu_data |> select(-"year") |> filter(geography %in% c(input$MIC) & geography_type %in% c(mic_title) & grouping == "Population"), 
                                                "Housing Units" = pop_hh_hu_data |> select(-"year") |> filter(geography %in% c(input$MIC) & geography_type %in% c(mic_title) & grouping == "Housing Units"),
                                                "Transit Stops" = transit_stop_data |> st_drop_geometry() |> filter(mic %in% c(input$MIC)) |> select(-"rgc"),
                                                "Destination Mode Share" = destination_mode_data |> select(-"year") |> filter(geography %in% c(input$MIC) & geography_type %in% c(mic_title)),
                                                "Industrial Land" = industrial_land |> filter(geography %in% c(input$MIC) & !(grouping %in% c("Vacant", "Re-developable", "Available")))
    ), place_name = input$MIC)
  })
  
  output$downloadrgcData <- downloadHandler(
    filename = function() {
      paste0(input$RGC,"_summary_data.xlsx")},
    content <- function(file) {
      saveWorkbook(rgc_summary_data(), file = file)},
    contentType = "application/Excel"
  )
  
  output$downloadmicData <- downloadHandler(
    filename = function() {
      paste0(input$MIC,"_summary_data.xlsx")},
    content <- function(file) {
      saveWorkbook(mic_summary_data(), file = file)},
    contentType = "application/Excel"
  )

})    



