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
    
  leftpanel_server('overviewleftpanel',
                   contact_name = "Craig Helmann",
                   contact_phone = "206-389-2889",
                   contact_email = "chelmann@psrc.org",
                   contact_title = "Director of Data",
                   photo_filename = "20210617_psrc_1576.png")
  
  banner_server('overviewBanner', 
                banner_title = "Centers Monitoring", 
                banner_subtitle = "VISION 2050",
                banner_url = "https://www.psrc.org/planning-2050/vision-2050")
  
  banner_server('rgcBanner', 
                banner_title = "Regional Growth Centers", 
                banner_subtitle = "Centers",
                banner_url = "https://www.psrc.org/our-work/centers")
  
  banner_server('micBanner', 
                banner_title = "Manufacturing and Industrial Centers", 
                banner_subtitle = "Centers",
                banner_url = "https://www.psrc.org/our-work/centers")
  

# Fatal Collisions by Geography -------------------------------------------

  rgc_filter <- reactive({
    data %>% 
      filter(geography=="Regional Growth Center" & name == input$RGC) %>%
      mutate(year = as.character(year), `Total Population` = round(total_population,0))
  })
  
  rgc_year_filter <- reactive({
    centers_info %>%
      filter(rgc_mic=="Regional Growth Center" & name == input$RGC) %>%
      select("designation_year") %>%
      pull()
  })
  
  rgc_descritpion_filter <- reactive({
    centers_info %>%
      filter(rgc_mic=="Regional Growth Center" & name == input$RGC) %>%
      select("information") %>%
      pull()
  })
  
  rgc_type_filter <- reactive({
    centers_info %>%
      filter(rgc_mic=="Regional Growth Center" & name == input$RGC) %>%
      select("center_type") %>%
      pull()
  })
  
  rgc_county <- reactive({
    centers_info %>%
      filter(rgc_mic=="Regional Growth Center" & name == input$RGC) %>%
      select("county") %>%
      pull()
  })
  
  output$lu_map <- renderImage({
    
    ifelse(input$RGC == "Seattle First Hill/Capitol Hill", imgfn <- "Seattle First Hill Capitol Hill.jpg", imgfn <- paste0(input$RGC, '.jpg'))
    
    filename <- normalizePath(file.path('./www', imgfn))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         width = 400,
         height = 400,
         alt = paste("Land use map for", input$RGC))
    
  }, deleteFile = FALSE)
  
  output$rgc_population_chart <- renderEcharts4r({
    rgc_filter() %>%
      e_charts(year) %>%
      e_bar(`Total Population`, stack = "grp") %>%
      e_color(psrc_colors$obgnpgy_5) %>%
      e_toolbox_feature("dataView") %>%
      e_toolbox_feature("saveAsImage") %>%
      e_tooltip(trigger = "axis") %>%
      e_x_axis(axisTick=list(show = FALSE)) %>%
      e_show_loading() %>%
      e_legend(show = FALSE, bottom=0) %>%
      e_title(text="Total Population (including Group Quarters)",
              link="https://ofm.wa.gov/washington-data-research/population-demographics/population-estimates/small-area-estimates-program")
    
  })
  
  mic_filter <- reactive({
    data %>% 
      filter(geography=="Manufacturing & Industrial Center" & name == input$MIC) %>%
      mutate(year = as.character(year), `Total Population` = round(total_population,0))
  })
  
  
  output$mic_population_chart <- renderEcharts4r({
    mic_filter() %>%
      e_charts(year) %>%
      e_bar(`Total Population`, stack = "grp") %>%
      e_color(psrc_colors$obgnpgy_5) %>%
      e_toolbox_feature("dataView") %>%
      e_toolbox_feature("saveAsImage") %>%
      e_tooltip(trigger = "axis") %>%
      e_x_axis(axisTick=list(show = FALSE)) %>%
      e_show_loading() %>%
      e_legend(show = FALSE, bottom=0) %>%
      e_title(text="Total Population (including Group Quarters)",
              link="https://ofm.wa.gov/washington-data-research/population-demographics/population-estimates/small-area-estimates-program")
    
  })
  
  output$RGCDesignationYear <- renderText({rgc_year_filter()})
  output$RGCType <- renderText({rgc_type_filter()})
  output$RGCDescription <- renderText({rgc_descritpion_filter()})
  
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
  
  output$rgc_pop_chart <- renderEcharts4r({
    
    echart_column_chart(df = pop_hh_hu_data %>% filter(geography_type == rgc_title, geography == input$RGC & grouping == "Population"),
                        x = "data_year", y = "estimate", tog = "grouping", title = "Total Population",
                        dec = 0, esttype = "number", color = "oranges")
    
  })
  
  output$rgc_pop_table <- DT::renderDataTable({create_single_group_table(df = pop_hh_hu_data, rgc_name = input$RGC, data_yrs = ofm_years, dec = 0, group = "Population")})
  
  output$rgc_age_chart <- renderEcharts4r({
    
      echart_multi_column_chart(df = age_data %>% filter(geography_type %in% c(rgc_title, "County") & geography %in% c(input$RGC, "Region", "All Centers") & grouping != "Total"),
                              x = "grouping", y = "share", fill="geography", tog = "data_year", 
                              dec = 0, esttype = "percent", color = "jewel")
    
  })
  
  output$rgc_age_table <- DT::renderDataTable({create_multi_group_table(df = age_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
  output$rgc_race_chart <- renderEcharts4r({
    
    echart_multi_bar_chart(df = race_data %>% 
                             filter(geography_type %in% c(rgc_title, "County") & geography %in% c(input$RGC, "Region", "All Centers") & grouping != "Total") %>%
                             arrange(desc(grouping)),
                           x = "grouping", y = "share", fill="geography", tog = "data_year",
                           dec = 0, esttype = "percent", color = "jewel")
    
    
  })
  
  output$rgc_race_table <- DT::renderDataTable({create_multi_group_table(df = race_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
  output$rgc_income_chart <- renderEcharts4r({
    
    echart_multi_column_chart(df = income_data %>% filter(geography_type %in% c(rgc_title, "County") & geography %in% c(input$RGC, "Region", "All Centers") & grouping != "Total"),
                              x = "grouping", y = "share", fill="geography", tog = "data_year", 
                              dec = 0, esttype = "percent", color = "jewel")
  })
  
  output$rgc_income_table <- DT::renderDataTable({create_multi_group_table(df = income_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
  output$rgc_education_chart <- renderEcharts4r({
    
    echart_multi_bar_chart(df = education_data %>% filter(geography_type %in% c(rgc_title, "County") & geography %in% c(input$RGC, "Region", "All Centers") & grouping != "Total"),
                              x = "grouping", y = "share", fill="geography", tog = "data_year", 
                              dec = 0, esttype = "percent", color = "jewel")
    
  })
  
  output$rgc_education_table <- DT::renderDataTable({create_multi_group_table(df = education_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
  output$summary_table <- DT::renderDataTable({create_rgc_summary_table(center_name = input$RGC, yr = 2021)})
  
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
  
  output$rgc_tenure_table <- DT::renderDataTable({create_multi_group_table(df = tenure_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
  output$rgc_type_chart <- renderEcharts4r({
    
    echart_multi_bar_chart(df = type_data %>% 
                             filter(geography_type %in% c(rgc_title, "County") & geography %in% c(input$RGC, "Region", "All Centers") & grouping != "Total") %>%
                             arrange(desc(grouping)),
                           x = "grouping", y = "share", fill="geography", tog = "data_year", 
                           dec = 0, esttype = "percent", color = "jewel")
  })
  
  output$rgc_type_table <- DT::renderDataTable({create_multi_group_table(df = type_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
  output$rgc_renter_burden_chart <- renderEcharts4r({
    
    echart_multi_column_chart(df = burden_data %>% 
                                filter(geography_type %in% c(rgc_title, "County") & geography %in% c(input$RGC, "Region", "All Centers") & grouping != "Total" & concept == "Renter Cost Burden"),
                              x = "grouping", y = "share", fill="geography", tog = "data_year", 
                              dec = 0, esttype = "percent", color = "jewel")
  })
  
  output$rgc_renter_burden_table <- DT::renderDataTable({create_multi_group_table(df = renter_burden_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
  output$rgc_owner_burden_chart <- renderEcharts4r({
    
    echart_multi_column_chart(df = burden_data %>% 
                                filter(geography_type %in% c(rgc_title, "County") & geography %in% c(input$RGC, "Region", "All Centers") & grouping != "Total" & concept == "Owner Cost Burden"),
                              x = "grouping", y = "share", fill="geography", tog = "data_year", 
                              dec = 0, esttype = "percent", color = "jewel")
    
  })
  
  output$rgc_owner_burden_table <- DT::renderDataTable({create_multi_group_table(df = owner_burden_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
  output$rgc_resident_mode_chart <- renderEcharts4r({
    
    echart_multi_column_chart(df = mode_data %>% 
                                filter(geography_type %in% c(rgc_title, "County") & geography %in% c(input$RGC, "Region", "All Centers") & grouping != "Total"),
                              x = "grouping", y = "share", fill="geography", tog = "data_year", 
                              dec = 0, esttype = "percent", color = "jewel")
  })
  
  output$rgc_resident_mode_table <- DT::renderDataTable({create_multi_group_table(df = mode_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
  output$rgc_destination_mode_chart <- renderEcharts4r({
    
    echart_multi_column_chart(df = destination_mode_data %>% 
                                filter(geography_type %in% c(rgc_title, "Region") & geography %in% c(input$RGC, "Region", "All Centers") & grouping != "Total"),
                              x = "grouping", y = "share", fill="geography", tog = "concept", 
                              dec = 0, esttype = "percent", color = "jewel")
  })
  
  #output$rgc_resident_mode_table <- DT::renderDataTable({create_multi_group_table(df = mode_data, rgc_name = input$RGC, data_yrs = as.character(census_years), dec = 1)})
  
    
})    



