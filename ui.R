shinyUI(
  
  navbarPage(
    
    id = "PSRC-Centers-Monitoring",
    tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),
    title = tags$a(div(tags$img(src='psrc-logo.png',
                             style="margin-top: -30px; padding-left: 40px;",
                             height = "80")
                             ), href="https://www.psrc.org", target="_blank"),
             tags$head(
               tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:25px !important; 
                            padding-bottom:0 !important;
                            height: 75px;
                            }
                           .navbar {min-height:25px !important;}'))
             ),
    
             windowTitle = "PSRC Centers Monitoring", 
             theme = "styles.css",
             position = "fixed-top",
             
    tabPanel(title=HTML("Regional Growth Centers"),
             value="RGC",
             banner_ui('rgcBanner'),
             
             fluidRow(column(4, style='padding-left:50px; padding-right:50px;',
                             leftpanel_ui('rgcleftpanel')),
                      
                      column(8,
                             selectInput("RGC","Select Regional Growth Center:",rgc_names),
                             fluidRow(column(6, leafletOutput("rgc_map")),
                                      column(6,  strong("Description:"),
                                             br(),
                                             textOutput("RGCDescription"),
                                             br(),
                                             strong("Designation Year: "), 
                                             textOutput("RGCDesignationYear"),
                                             br(),
                                             strong("Center Type: "),
                                             textOutput("RGCType"))),
                             br(),
                             tabsetPanel(type = "tabs",
                                         tabPanel("Demographics", 
                                                  h1("Total Population"),
                                                  hr(),
                                                  #strong(tags$div(class="chart_title","Total Population")),
                                                  fluidRow(column(12,echarts4rOutput("rgc_population_chart"))),
                                                  tags$div(class="chart_source","Source: Office of Financial Managment SAEP Program & PSRC Parcelization"),
                                                  hr(),
                                                  h1("Population by Age"),
                                                  strong(tags$div(class="chart_title","Total Population by Age Group")),
                                                  fluidRow(column(12,echarts4rOutput("rgc_age_chart"))),
                                                  tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B01001"),
                                                  hr(),
                                                  h1("Population by Race & Ethnicity"),
                                                  strong(tags$div(class="chart_title","Total Population by Race & Ethnicity")),
                                                  fluidRow(column(12,echarts4rOutput("rgc_race_chart", height=500))),
                                                  tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B03002"),
                                                  hr(),
                                                  
                                                  ), # end of TabPanel for RGC Population
                                         tabPanel("Jobs", "Test"),
                                         tabPanel("Other", "Test")
                                         )
                      ),
             ) # End of Main Panel Fluid Row for RGC Tab 
    ), # end Tabpanel for RGC
             
    
    tabPanel(title=HTML("Manufacturing & Industrial Centers"),
             value="MIC",
             banner_ui('micBanner'),
             
             fluidRow(column(4, style='padding-left:50px; padding-right:50px;',
                             leftpanel_ui('micleftpanel')),
                      
                      column(8,
                             selectInput("MIC","Select MIC:",mic_names),
                             "Map of Center",
                             br(),br(),br(),
                             tabsetPanel(type = "tabs",
                                         tabPanel("People", 
                                                  h1("Total Population"),
                                                  hr(),
                                                  fluidRow(column(12,echarts4rOutput("mic_population_chart"))),
                                                  tags$div(class="chart_source","Source: Office of Financial Managment SAEP Program & PSRC Parcelization"),
                                                  hr(),
                                                  
                                         ), # end of TabPanel for RGC Population
                                         tabPanel("Jobs", "Test"),
                                         tabPanel("Other", "Test")
                             )
                      ),
             ) # End of Main Panel Fluid Row for MIC Tab 
    ), # end Tabpanel for MIC

                      
    tags$footer(footer_ui('psrcfooter'))
    
             ) # End of NavBar Page
  ) # End of Shiny App
