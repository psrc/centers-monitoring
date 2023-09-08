# Transportation tabPanel

transportation_ui <- function(id) {
  ns <- NS(id)
  
  tagList( 
    uiOutput(ns('atransportationtab'))
  )
  
}

transportation_server <- function(id, center_name, center_type, center_desc) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    # Tables and Charts
    
    if (center_type == rgc_title) {
      
      output$resident_mode_chart <- renderEcharts4r({
        echart_multi_column_chart(df = mode_data %>% 
                                    filter(geography_type %in% c(center_type, "County") & geography %in% c(center_name(), "Region", "All Centers") & grouping != "Total"),
                                  x = "grouping", y = "share", fill="geography", tog = "data_year", 
                                  dec = 0, esttype = "percent", color = "jewel")
        })
    
      output$resident_mode_table <- DT::renderDataTable({create_multi_year_table(df = mode_data, rgc_name = center_name(), data_yrs = as.character(census_years), dec = 1, center_type = center_type)})
      
    }
    
    output$destination_mode_chart <- renderEcharts4r({
      
      echart_multi_column_chart(df = destination_mode_data %>% 
                                  filter(geography_type %in% c(center_type, "Region") & geography %in% c(center_name(), "Region", "All Centers") & grouping != "Total"),
                                x = "grouping", y = "share", fill="geography", tog = "concept", 
                                dec = 0, esttype = "percent", color = "jewel")
    })
    
    output$destination_mode_table <- DT::renderDataTable({create_multi_group_table(df = destination_mode_data, rgc_name = center_name(), grp = "concept", dec = 1, center_type = center_type)})
    
    output$stop_table <- DT::renderDataTable({create_transit_stop_table(center_name = center_name(), center_type = center_desc)})
    
    output$stop_map <- renderLeaflet({create_transit_map(center_name = center_name(), center_type = center_type, center_desc = center_desc)})
    
    # Tab layout
    output$atransportationtab <- renderUI({
      
      if (center_type == rgc_title) {
      
        tagList(
        
          hr(),
        
          # Transit Stop Map and Table
          fluidRow(column(6, leafletOutput(ns("stop_map"))),
                  column(6, strong("Transit Service"),
                          br(),
                          dataTableOutput(ns("stop_table")),
                          br(),
                          tags$div(class="chart_source","Source: Spring GTFS Service")
                  )),
          fluidRow(column(12, div(img(src="transit-legend.png", width = "75%", style = "padding-left: 0px;")))),
         br(),
        
          # Resident Mode Share
          strong(tags$div(class="chart_title","Mode to Work for Residents")),
          fluidRow(column(12,echarts4rOutput(ns("resident_mode_chart")))),
          br(),
          fluidRow(column(12, dataTableOutput(ns("resident_mode_table")))),
          tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B08301"),
          hr(style = "border-top: 1px solid #000000;"),
          
        
          # Destination Mode Share
          strong(tags$div(class="chart_title","Destination Mode Share")),
          fluidRow(column(12,echarts4rOutput(ns("destination_mode_chart")))),
          br(),
          fluidRow(column(12, dataTableOutput(ns("destination_mode_table")))),
          tags$div(class="chart_source","Source: 2018 PSRC SoundCast Activity Based Model"),
          hr(style = "border-top: 1px solid #000000;")
        )
        
      } else {
        
        tagList(
          
          hr(),
          
          # Transit Stop Map and Table
          fluidRow(column(6, leafletOutput(ns("stop_map"))),
                   column(6, strong("Transit Service"),
                          br(),
                          dataTableOutput(ns("stop_table")),
                          br(),
                          tags$div(class="chart_source","Source: Spring GTFS Service")
                   )),
          fluidRow(column(12, div(img(src="transit-legend.png", width = "75%", style = "padding-left: 0px;")))),
          br(),
          
          # Destination Mode Share
          strong(tags$div(class="chart_title","Destination Mode Share")),
          fluidRow(column(12,echarts4rOutput(ns("destination_mode_chart")))),
          br(),
          fluidRow(column(12, dataTableOutput(ns("destination_mode_table")))),
          tags$div(class="chart_source","Source: 2018 PSRC SoundCast Activity Based Model"),
          hr(style = "border-top: 1px solid #000000;")
        )
        
      }
      
    })
    
  }) # end moduleServer
  
}
