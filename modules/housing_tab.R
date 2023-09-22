# Housing tabPanel

housing_ui <- function(id) {
  ns <- NS(id)
  
  tagList( 
    uiOutput(ns('ahousingtab'))
  )
  
}

housing_server <- function(id, center_name, center_type) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    # Tables and Charts
    output$hu_chart <- renderEcharts4r({
      
      echart_column_chart(df = pop_hh_hu_data |> filter(geography_type == center_type, geography == center_name() & grouping == "Housing Units"),
                          x = "data_year", y = "estimate", tog = "grouping", title = "Total Housing Units",
                          dec = 0, esttype = "number", color = "blues")
      
    })
    
    output$hu_table <- DT::renderDataTable(create_single_group_table(df = pop_hh_hu_data, rgc_name = center_name(), data_yrs = ofm_years, dec = 0, group = "Housing Units", center_type = center_type))
    
    output$hu_change_chart <- renderEcharts4r({
      
      echart_column_chart(df = unit_data |>
                            filter(geography_type %in% c(center_type) & geography %in% c(center_name()) & year %in% ofm_years) |>
                            mutate(concept="New Net Housing Units") |> 
                            mutate(delta = estimate-lag(estimate), data_year = paste0(lag(data_year),"-",data_year)) |>
                            drop_na(),
                          x = "data_year", y = "delta", tog = "concept", title = "New Net Housing Units",
                          dec = 0, esttype = "number", color = "purples")
      
    })
    
    output$hu_change_table <- DT::renderDataTable({create_change_table(df = unit_data |>
                                                                         filter(geography_type %in% c(center_type) & geography %in% c(center_name()) & year %in% ofm_years) |>
                                                                         mutate(delta = estimate-lag(estimate), data_year = paste0(lag(data_year),"-",data_year)) |>
                                                                         drop_na(), 
                                                                       yr = "data_year",val="delta", nm="New Net Housing Units")})
    
    output$tenure_chart <- renderEcharts4r({
      
      echart_multi_column_chart(df = tenure_data |> filter(geography_type %in% c(center_type, "County") & geography %in% c(center_name(), "Region", "All Centers") & grouping != "Total"),
                                x = "grouping", y = "share", fill="geography", tog = "data_year", 
                                dec = 0, esttype = "percent", color = "jewel")
    })
    
    output$tenure_table <- DT::renderDataTable(create_multi_year_table(df = tenure_data, rgc_name = center_name(), data_yrs = as.character(census_years), dec = 1, center_type = center_type))
    
    output$type_chart <- renderEcharts4r({
      
      echart_multi_bar_chart(df = type_data |>
                               filter(geography_type %in% c(center_type, "County") & geography %in% c(center_name(), "Region", "All Centers") & grouping != "Total") |>
                               arrange(desc(grouping)),
                             x = "grouping", y = "share", fill="geography", tog = "data_year", 
                             dec = 0, esttype = "percent", color = "jewel")
    })
    
    output$type_table <- DT::renderDataTable(create_multi_year_table(df = type_data, rgc_name = center_name(), data_yrs = as.character(census_years), dec = 1, center_type = center_type))
    
    if (center_type == rgc_title) {
    
      output$renter_burden_chart <- renderEcharts4r({
      
       echart_multi_column_chart(df = burden_data |>
                                    filter(geography_type %in% c(center_type, "County") & geography %in% c(center_name(), "Region", "All Centers") & grouping != "Total" & concept == "Renter Cost Burden"),
                                  x = "grouping", y = "share", fill="geography", tog = "data_year", 
                                  dec = 0, esttype = "percent", color = "jewel")
      })
    
      output$renter_burden_table <- DT::renderDataTable(create_multi_year_table(df = renter_burden_data, rgc_name = center_name(), data_yrs = as.character(census_years), dec = 1, center_type = center_type))
    
      output$owner_burden_chart <- renderEcharts4r({
      
        echart_multi_column_chart(df = burden_data |>
                                    filter(geography_type %in% c(center_type, "County") & geography %in% c(center_name(), "Region", "All Centers") & grouping != "Total" & concept == "Owner Cost Burden"),
                                  x = "grouping", y = "share", fill="geography", tog = "data_year", 
                                  dec = 0, esttype = "percent", color = "jewel")
      
      })
    
      output$owner_burden_table <- DT::renderDataTable(create_multi_year_table(df = owner_burden_data, rgc_name = center_name(), data_yrs = as.character(census_years), dec = 1, center_type = center_type))
    
    }  
      
    # Tab layout
    output$ahousingtab <- renderUI({
      
      if (center_type == rgc_title) {
      
        tagList(
        
          # Total Housing Units
          hr(),
          strong(tags$div(class="chart_title","Total Housing Units")),
          fluidRow(column(6,br(), br(), br(), br(), dataTableOutput(ns("hu_table"))),
                  column(6,echarts4rOutput(ns("hu_chart")))),
          tags$div(class="chart_source","Source: Office of Financial Managment SAEP Program & PSRC Parcelization"),
          hr(style = "border-top: 1px solid #000000;"),
        
          # Net Housing Unit Change
          strong(tags$div(class="chart_title","New Net Housing Units")),
          fluidRow(column(6,br(), br(), br(), br(), dataTableOutput(ns("hu_change_table"))),
                  column(6,echarts4rOutput(ns("hu_change_chart")))),
          tags$div(class="chart_source","Source: Office of Financial Managment SAEP Program & PSRC Parcelization"),
          hr(style = "border-top: 1px solid #000000;"),
        
          # Housing Tenure
          strong(tags$div(class="chart_title","Housing Tenure")),
          fluidRow(column(12,echarts4rOutput(ns("tenure_chart")))),
          br(),
          fluidRow(column(12, dataTableOutput(ns("tenure_table")))),
          br(),
          tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B25003"),
          hr(style = "border-top: 1px solid #000000;"),
        
          # Housing Unit Type
          strong(tags$div(class="chart_title","Housing Unit Type")),
          fluidRow(column(12,echarts4rOutput(ns("type_chart")))),
          br(),
          fluidRow(column(12, dataTableOutput(ns("type_table")))),
          br(),
          tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B25024"),
          hr(style = "border-top: 1px solid #000000;"),
        
          # Renter Cost Burden
          strong(tags$div(class="chart_title","Cost Burden: Renters")),
          fluidRow(column(12,echarts4rOutput(ns("renter_burden_chart")))),
          br(),
          fluidRow(column(12, dataTableOutput(ns("renter_burden_table")))),
          br(),
          tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B25070"),
          hr(style = "border-top: 1px solid #000000;"),
        
          # Owner Cost Burden
          strong(tags$div(class="chart_title","Cost Burden: Owners")),
          fluidRow(column(12,echarts4rOutput(ns("owner_burden_chart")))),
          br(),
          fluidRow(column(12, dataTableOutput(ns("owner_burden_table")))),
          br(),
          tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B25091"),
          hr(style = "border-top: 1px solid #000000;")
        
        )
        
      } else {
        
        tagList(
          
          # Total Housing Units
          hr(),
          strong(tags$div(class="chart_title","Total Housing Units")),
          fluidRow(column(6,br(), br(), br(), br(), dataTableOutput(ns("hu_table"))),
                   column(6,echarts4rOutput(ns("hu_chart")))),
          tags$div(class="chart_source","Source: Office of Financial Managment SAEP Program & PSRC Parcelization"),
          hr(style = "border-top: 1px solid #000000;"),
          
          # Net Housing Unit Change
          strong(tags$div(class="chart_title","New Net Housing Units")),
          fluidRow(column(6,br(), br(), br(), br(), dataTableOutput(ns("hu_change_table"))),
                   column(6,echarts4rOutput(ns("hu_change_chart")))),
          tags$div(class="chart_source","Source: Office of Financial Managment SAEP Program & PSRC Parcelization"),
          hr(style = "border-top: 1px solid #000000;")
        
        )
        
      }
      
    })
    
  }) # end moduleServer
  
}
