# Display Overview Data for a Center

source_ui <- function(id) {
  ns <- NS(id)
  
  tagList( 
    uiOutput(ns('asourcetab'))
  )
  
}

source_server <- function(id, center_type) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    # Tables
    output$center_source_table <- DT::renderDataTable(create_source_table(d=source_info, center_type = center_type))
      
    # Tab layout
    output$asourcetab <- renderUI({
      
      tagList(
        
        # Center Source Data
        strong(tags$div(class="chart_title","Data Sources")),
        fluidRow(column(12, dataTableOutput(ns("center_source_table")))),
        br()
      )
      
    })
    
  }) # end moduleServer
  
}
