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
                            filter(data_year %in% pop_hsg_yrs),
                          x = "data_year", y = "estimate", tog = "grouping", title = "Total Employment",
                          dec = 0, esttype = "number", color = "purples")
      
    })
    
    output$job_sectors_table <- DT::renderDataTable(create_rgc_jobs_by_sector_table(center_name = center_name(), center_type=center_type))
    
    if (center_type == mic_title) {
      
      output$industrial_jobs_chart <- renderEcharts4r({
        
        echart_multi_column_chart(df = industrial_jobs |> filter(geography %in% c(center_name(), "Region", "All Centers", "All RGCs", "All MICs") & data_year %in% pop_hsg_yrs),
                                  x = "grouping", y = "share", tog = "data_year", fill="geography",
                                  dec = 0, esttype = "percent", color = "jewel")
        })
      
      output$industrial_table <- DT::renderDataTable({create_multi_year_table(df = industrial_jobs, rgc_name = center_name(), data_yrs = as.character(pop_hsg_yrs), dec = 1, center_type = center_type)})
      
    }
  
    # Tab layout
    output$ajobstab <- renderUI({
      
      if (center_type == rgc_title) {
        
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
      } else {
        
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
                  hr(style = "border-top: 1px solid #000000;"),
          
          # Industrial Jobs
          strong(tags$div(class="chart_title","Share of Industrial Jobs")),
          fluidRow(column(12, echarts4rOutput(ns("industrial_jobs_chart")))),
          br(),
          fluidRow(column(12, dataTableOutput(ns("industrial_table")))),
          br(),
          tags$div(class="chart_source","Source: PSRC Covered Employment Estimates"),
          hr(style = "border-top: 1px solid #000000;")
          
          )
      }
      
    })
    
  }) # end moduleServer
  
}
