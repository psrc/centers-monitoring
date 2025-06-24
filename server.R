shinyServer(function(input, output, session) {
  
  footer_server('psrcfooter')
  
  leftpanel_server('rgcleftpanel',
                   contact_name = "Liz Underwood-Bultmann, AICP",
                   contact_phone = "206-464-6174",
                   contact_email = "lunderwood-bultmann@psrc.org",
                   contact_title = "Principal Planner",
                   photo_filename = "redmondconnector.jpeg")

  leftpanel_server('micleftpanel',
                   contact_name = "Liz Underwood-Bultmann, AICP",
                   contact_phone = "206-464-6174",
                   contact_email = "lunderwood-bultmann@psrc.org",
                   contact_title = "Principal Planner",
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
  
  output$downloadrgcData <- downloadHandler(
    filename = function() {paste0(str_to_lower(str_replace_all(str_replace_all(input$RGC, "\\/", "_"), " ", "_")), "_data.xlsx")},
    content <- function(file) {file.copy(paste0("data-downloads/", str_to_lower(str_replace_all(str_replace_all(input$RGC, "\\/", "_"), " ", "_")), "_data.xlsx"),file)},
    contentType = "application/Excel"
  )
  
  output$downloadmicData <- downloadHandler(
    filename = function() {paste0(str_to_lower(str_replace_all(str_replace_all(input$MIC, "\\/", "_"), " ", "_")), "_data.xlsx")},
    content <- function(file) {file.copy(paste0("data-downloads/", str_to_lower(str_replace_all(str_replace_all(input$MIC, "\\/", "_"), " ", "_")), "_data.xlsx"),file)},
    contentType = "application/Excel"
  )
  

})    

