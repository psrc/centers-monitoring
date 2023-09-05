# Display Overview Data for a Center

overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList( 
    uiOutput(ns('aoverviewtab'))
  )
  
}

overview_server <- function(id, center_name, center_type) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    # Tables and Charts
    output$center_map <- renderLeaflet(create_center_map(center_name=center_name(), center_type = center_type))
    output$center_summary_table <- DT::renderDataTable(create_summary_table(center_name=center_name(), center_type = center_type, yr = current_census_yr))
    output$center_description <- renderText(pull_center_information(center_name=center_name(), center_type = center_type))
    
    # Tab layout
    output$aoverviewtab <- renderUI({
      
      tagList(
        
        # Center Summary Data
        fluidRow(column(6, leafletOutput(ns("center_map"))),
                 column(6, strong("Summary Statistics"),
                        br(),
                        dataTableOutput(ns("center_summary_table")),
                        br(),
                        tags$div(class="chart_source","* Employment data is suppressed")
                 )),
        br(),
        fluidRow(column(12, strong("Description:"),
                        br(),
                        textOutput(ns("center_description")))),
        br()
        
      )
      
    })
    
  }) # end moduleServer
  
}
