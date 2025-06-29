# Demographic tabPanel

demographics_ui <- function(id) {
  ns <- NS(id)
  
  tagList( 
    uiOutput(ns('ademographicstab'))
  )
  
}

demographics_server <- function(id, center_name, center_type) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    # Tables and Charts
    output$pop_chart <- renderEcharts4r({
      
      echart_column_chart(df = pop_hh_hu_data %>% filter(geography_type == center_type, geography == center_name() & grouping == "Population" & year %in% pop_hsg_yrs),
                          x = "year", y = "estimate", tog = "grouping", title = "Total Population",
                          dec = 0, esttype = "number", color = "oranges")
      
    })
    
    output$pop_hsg_chart <- renderEcharts4r({
      
      echart_column_chart(df = pop_hh_hu_data %>% filter(geography_type == center_type, geography == center_name() & grouping %in% c("Population", "Housing Units") & year %in% pop_hsg_yrs),
                          x = "year", y = "estimate", tog = "grouping", title = "Total",
                          dec = 0, esttype = "number", color = "jewel")
      
    })
    
    output$pop_table <- DT::renderDataTable({create_single_group_table(df = pop_hh_hu_data, rgc_name = center_name(), data_yrs = pop_hsg_yrs, dec = 0, group = "Population", center_type = center_type)})
    
    output$pop_hsg_table <- DT::renderDataTable({create_multi_year_table_all(df = pop_hh_hu_data %>% filter(geography_type == center_type, geography == center_name() & grouping %in% c("Population", "Housing Units")), 
                                                                         data_yrs = pop_hsg_yrs, dec = 0, rgc_name = center_name(), center_type = center_type)})
    
    output$age_chart <- renderEcharts4r({
      
      echart_multi_column_chart(df = age_data %>% filter(year %in% census_years & geography_type %in% c(center_type, "County") & geography %in% c(center_name(), "Region", "All Centers", "All RGCs", "All MICs") & grouping != "Total"),
                                x = "grouping", y = "share", fill="geography", tog = "year", 
                                dec = 0, esttype = "percent", color = "jewel")
      
    })
    
    output$age_table <- DT::renderDataTable({create_multi_year_table(df = age_data, rgc_name = center_name(), data_yrs = as.character(census_years), dec = 1, center_type = center_type)})
    
    output$race_chart <- renderEcharts4r({
      
      echart_multi_bar_chart(df = race_data %>% 
                               filter(year %in% census_years & geography_type %in% c(center_type, "County") & geography %in% c(center_name(), "Region", "All Centers", "All RGCs", "All MICs") & grouping != "Total") %>%
                               arrange(desc(grouping)),
                             x = "grouping", y = "share", fill="geography", tog = "year",
                             dec = 0, esttype = "percent", color = "jewel")
      
      
    })
    
    output$race_table <- DT::renderDataTable({create_multi_year_table(df = race_data, rgc_name = center_name(), data_yrs = as.character(census_years), dec = 1, center_type = center_type)})
    
    output$income_chart <- renderEcharts4r({
      
      echart_multi_column_chart(df = income_data %>% filter(year %in% census_years & geography_type %in% c(center_type, "County") & geography %in% c(center_name(), "Region", "All Centers", "All RGCs", "All MICs") & grouping != "Total"),
                                x = "grouping", y = "share", fill="geography", tog = "year", 
                                dec = 0, esttype = "percent", color = "jewel")
    })
    
    output$income_table <- DT::renderDataTable({create_multi_year_table(df = income_data, rgc_name = center_name(), data_yrs = as.character(census_years), dec = 1, center_type = center_type)})
    
    output$education_chart <- renderEcharts4r({
      
      echart_multi_bar_chart(df = education_data %>% filter(year %in% census_years & geography_type %in% c(center_type, "County") & geography %in% c(center_name(), "Region", "All Centers", "All RGCs", "All MICs") & grouping != "Total"),
                             x = "grouping", y = "share", fill="geography", tog = "year", 
                             dec = 0, esttype = "percent", color = "jewel")
      
    })
    
    output$education_table <- DT::renderDataTable({create_multi_year_table(df = education_data, rgc_name = center_name(), data_yrs = as.character(census_years), dec = 1, center_type = center_type)})
    
    # Tab layout
    output$ademographicstab <- renderUI({
      
      if (center_type == rgc_title) {
      
        tagList(
        
          # Total Population
          hr(),
          strong(tags$div(class="chart_title","Total Population")),
          fluidRow(column(6,br(), br(), br(), br(), dataTableOutput(ns("pop_table"))),
                  column(6,echarts4rOutput(ns("pop_chart")))),
          tags$div(class="chart_source","Source: Office of Financial Managment SAEP Program & PSRC Parcelization"),
          hr(style = "border-top: 1px solid #000000;"),    
        
          # Age Group
          strong(tags$div(class="chart_title","Total Population by Age Group")),
          fluidRow(column(12, echarts4rOutput(ns("age_chart")))),
          br(),
          fluidRow(column(12, dataTableOutput(ns("age_table")))),
          br(),
          tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B01001"),
          hr(style = "border-top: 1px solid #000000;"),
        
          # Race & Hispanic Origin
          strong(tags$div(class="chart_title","Total Population by Race & Ethnicity")),
          fluidRow(column(12,echarts4rOutput(ns("race_chart"), height=500))),
          br(),
          fluidRow(column(12, dataTableOutput(ns("race_table")))),
          br(),
          tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B03002"),
          hr(style = "border-top: 1px solid #000000;"),
        
          # Household Income
          strong(tags$div(class="chart_title","Total Household Income")),
          fluidRow(column(12,echarts4rOutput(ns("income_chart"), height=500))),
          br(),
          fluidRow(column(12, dataTableOutput(ns("income_table")))),
          br(),
          tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B19001"),
          hr(style = "border-top: 1px solid #000000;"),
        
          # Educational Attainment
          strong(tags$div(class="chart_title","Educational Attainment")),
          fluidRow(column(12,echarts4rOutput(ns("education_chart"), height=500))),
          br(),
          fluidRow(column(12, dataTableOutput(ns("education_table")))),
          br(),
          tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B15002"),
          hr(style = "border-top: 1px solid #000000;")
        
      )
        
      } else {
        
        tagList(
          
          # Total Population
          hr(),
          strong(tags$div(class="chart_title","Total Population")),
          fluidRow(column(12,echarts4rOutput(ns("pop_hsg_chart")))),
          fluidRow(column(12,dataTableOutput(ns("pop_hsg_table")))),
          tags$div(class="chart_source","Source: Office of Financial Managment SAEP Program & PSRC Parcelization"),
          hr(style = "border-top: 1px solid #000000;")
          
        )
        
        
        
      }
      
    })
    
  }) # end moduleServer
  
}
