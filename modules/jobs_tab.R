# Employment tabPanel

jobs_ui <- function(id) {
  ns <- NS(id)
  
  tagList( 
    uiOutput(ns('ajobstab'))
  )
  
}

jobs_server <- function(id, center_name, center_type) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    # Tables and Charts
    output$jobs_chart <- renderEcharts4r({
      
      echart_column_chart(df = employment_data |> 
                            filter(geography_type == center_type, geography == center_name() & grouping == "Total") |>
                            mutate(estimate = as.integer(estimate)) |>
                            filter(data_year %in% ofm_years),
                          x = "data_year", y = "estimate", tog = "grouping", title = "Total Employment",
                          dec = 0, esttype = "number", color = "purples")
      
    })
    
    output$job_sectors_table <- DT::renderDataTable(create_rgc_jobs_by_sector_table(center_name = center_name(), center_type=center_type))
  
    # Tab layout
    output$ajobstab <- renderUI({
      
      tagList(
        
        hr(),
        
        # Total Jobs and Jobs by Sector
        fluidRow(column(6, echarts4rOutput(ns("jobs_chart"))),
                 column(6, strong("Jobs by Sector"),
                        br(),
                        dataTableOutput(ns("job_sectors_table")),
                        br(),
                        tags$div(class="chart_source","* Employment data is suppressed")
                 )),
        
        hr(style = "border-top: 1px solid #000000;")
        
        
      )
      
    })
    
  }) # end moduleServer
  
}
