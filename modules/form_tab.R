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
           
         ifelse(center_name() == "Seattle First Hill/Capitol Hill", imgfn <- "Seattle First Hill Capitol Hill.jpg", imgfn <- paste0(center_name(), '.jpg'))
         
      } else {
          
        if(center_name() == "Cascade Industrial Center - Arlington/Marysville") {imgfn <- "Cascade Industrial Center - Arlington-Marysville.jpg"}
        if(center_name() == "Kent") {imgfn <- "Kent-MIC.jpg"}
        if(center_name() == "Paine Field/Boeing Everett") {imgfn <- "Paine Field-Boeing Everett.jpg"}
        if(!(center_name() %in% c("Cascade Industrial Center - Arlington/Marysville", "Kent", "Paine Field/Boeing Everett"))) {imgfn <- paste0(center_name(), '.jpg')}
        
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
     
    output$urban_form_table <- DT::renderDataTable(create_rgc_urban_form_table(center_name = center_name()))

    # Tab layout
    output$aformtab <- renderUI({
      
      tagList(
        
        hr(),
        
        strong(tags$div(class="chart_title","Zoning Map")),
        fluidRow(column(7, imageOutput(ns("lu_map"))),
                 column(5, div(img(src="legend.png", width = "75%", style = "padding-left: 0px;")))),
        
        br(),
        fluidRow(column(12,dataTableOutput(ns("urban_form_table")))),
        hr(style = "border-top: 1px solid #000000;")
        
      )
      
    })
    
  }) # end moduleServer
  
}
