# Employment tabPanel

form_ui <- function(id) {
  ns <- NS(id)
  
  tagList( 
    uiOutput(ns('aformtab'))
  )
  
}

form_server <- function(id, center_name, center_type) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    zoning_map_filename <- reactive({
      
      if(center_type == rgc_title) {
           
         ifelse(center_name() == "Seattle First Hill/Capitol Hill", imgfn <- "Seattle First Hill_Capitol Hill.jpg", imgfn <- paste0(center_name(), '.jpg'))
         
      } else {
        
        ifelse(center_name() == "Paine Field/Boeing Everett", imgfn <- "Paine Field_Boeing Everett.jpg", imgfn <- paste0(center_name(), '.jpg'))
          
      }
       
      normalizePath(file.path('./www', imgfn))
      
    })
    
    # Tables and Charts
    output$lu_map <- renderImage({
      
    # Return a list containing the filename and alt text
    list(src = zoning_map_filename(),
         width = 400,
         height = 400,
         alt = paste("Land use map for", center_name()))
      
    }, deleteFile = FALSE)
    
    if (center_type == rgc_title) {
      
      output$urban_form_table <- DT::renderDataTable(create_rgc_urban_form_table(center_name = center_name()))
      
    }
    
    if (center_type == mic_title) {
      
      output$industrial_land_table <- DT::renderDataTable({create_multi_year_table(df = industrial_land |> filter(place_name==center_name() & !(grouping %in% c("Vacant", "Re-developable", "Available"))), 
                                                                                   rgc_name = center_name(), data_yrs = model_base_yr, dec = 0, center_type = center_type)})
      
      output$net_industial_land_chart <- renderEcharts4r(echart_pie_chart(df = industrial_land_shares |> filter(place_name==center_name()), value="estimate", fill="grouping", color="jewel", esttype="number", dec=0))
      
    }

    # Tab layout
    output$aformtab <- renderUI({
      
      if (center_type == rgc_title) {
        
        tagList(
          
          hr(),
          
          strong(tags$div(class="chart_title","Land Use Map")),
          fluidRow(column(7, imageOutput(ns("lu_map"))),
                   column(5, br(), br(), div(img(src="legend.png", width = "45%", style = "padding-left: 0px;")))),
          tags$div(class="chart_source","Source: PSRC UrbanSim Base Year Dataset"),
          br(),
          fluidRow(column(12,dataTableOutput(ns("urban_form_table")))),
          tags$div(class="chart_source","Source: 2023 PSRC SoundCast Activity Based Model"),
          hr(style = "border-top: 1px solid #000000;")
        
      )
        
      } else {
        
        tagList(
          
          hr(),
          
          strong(tags$div(class="chart_title","Industrial Land Types")),
          fluidRow(column(7, imageOutput(ns("lu_map"))),
                   column(5, br(), br(), br(), br(), div(img(src="IL_lu_legend.png", width = "50%", style = "padding-left: 0px;")))),
          tags$div(class="chart_source","Source: PSRC 2024 Industrial Lands Inventory"),
          hr(style = "border-top: 1px solid #000000;"),
          
          strong(tags$div(class="chart_title","Industrial Land by Type (acres)")),
          fluidRow(column(6, dataTableOutput(ns("industrial_land_table"))),
                   column(6, echarts4rOutput(ns("net_industial_land_chart")))),
          tags$div(class="chart_source","Source: PSRC 2024 Industrial Lands Inventory"),
          hr(style = "border-top: 1px solid #000000;")
          
          
        )
        
        
      }
      
    })
    
  }) # end moduleServer
  
}
