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
             
             fluidRow(
               # Side Panel for Regional Growth Centers
               column(4, style='padding-left:10px; padding-right:0px;', leftpanel_ui('rgcleftpanel')),
               
               # Main Panel for Regional Growth Centers
               column(8, 
                      style='padding-left:25px; padding-right:25px;',
                      
                      # Center Selection
                      fluidRow(
                        column(6, selectInput("RGC","Select Center:",rgc_names, selected = random_rgc)),
                        column(6, br(), downloadLink('downloadData', label = "Download Center Data in Excel Format"))
                        ), 
                      
                      # Center Summary Data
                      fluidRow(column(6, leafletOutput("rgc_map")),
                               column(6, strong("Summary Statistics"),
                                      br(),
                                      dataTableOutput("rgc_summary_table"),
                                      br(),
                                      tags$div(class="chart_source","* Employment data is suppressed")
                                      )),
                      br(),
                      fluidRow(column(12, strong("Description:"),
                                      br(),
                                      textOutput("RGCDescription"))),
                      br(),
                      
                      # Summary Charts and Tables by Tabs for Demographics, Jobs, Housing, Transportation and Urban Form
                      tabsetPanel(type = "tabs",
                                  tabPanel("Demographics", demographics_ui('rgcDemographics')),
                                  tabPanel("Jobs", jobs_ui('rgcEmployment')),
                                  tabPanel("Housing", housing_ui('rgcHousing')),
                                  tabPanel("Transportation", transportation_ui('rgcTransportation')),
                                  
                                  tabPanel("Urban Form", 
                                           
                                                  hr(),
                                                  
                                                  strong(tags$div(class="chart_title","Zoning Map")),
                                                  fluidRow(column(7,imageOutput("lu_map")),
                                                           column(5, div(img(src="legend.png", width = "75%", style = "padding-left: 0px;")))),
                                                  
                                                  br(),
                                                  fluidRow(column(12,dataTableOutput("urban_form_table"))),
                                                  hr(style = "border-top: 1px solid #000000;")
                                                  
                                        ) # end of TabPanel for RGC Urban Form
                                        
                              ) # end of RGC Tabsets
                      ), # End of RGC Main Panel
             ) # End of Main Panel Fluid Row for RGC Tab 
    ), # end Tabpanel for RGC
             
    
    tabPanel(title=HTML("Manufacturing & Industrial Centers"),
             value="MIC",
             banner_ui('micBanner'),
             
             fluidRow(column(4, style='padding-left:10px; padding-right:0px;',
                             leftpanel_ui('micleftpanel')),
                      
                      column(8, style='padding-left:25px; padding-right:25px;',
                             
                             
                             fluidRow(
                               column(6, selectInput("MIC","Select Center:",mic_names, selected = random_mic)),
                               column(6, br(), downloadLink('downloadmicData', label = "Download Center Data in Excel Format"))
                             ),
                             
                             # Section on page for Map and Summary Table
                             fluidRow(column(6, leafletOutput("mic_map")),
                                      column(6, strong("Summary Statistics"),
                                             br(),
                                             dataTableOutput("mic_summary_table"),
                                             br(),
                                             tags$div(class="chart_source","* Employment data is suppressed")
                                      )),
                             br(),
                             
                             # Section on page for Text Description
                             fluidRow(column(12, strong("Description:"),
                                             br(),
                                             textOutput("MICDescription"))),
                             br(),
                             tabsetPanel(type = "tabs",
                                         
                                         tabPanel("Demographics", demographics_ui('micDemographics')),
                                         tabPanel("Jobs", jobs_ui('micEmployment')),
                                         tabPanel("Housing", housing_ui('micHousing')),
                                         tabPanel("Transportation", transportation_ui('micTransportation')),
                                         
                                         tabPanel("Urban Form", 
                                                  
                                                  
                                                  "test"
                                                  
                                         ) # end of TabPanel for MIC Urban Form
                                         
                             ) # end of MIC Tabsets
                      ),
             ) # End of Main Panel Fluid Row for MIC Tab 
    ), # end Tabpanel for MIC

                      
    tags$footer(footer_ui('psrcfooter'))
    
             ) # End of NavBar Page
  ) # End of Shiny App
