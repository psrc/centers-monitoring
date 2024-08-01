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
    
             windowTitle = "PSRC Centers Dashboard", 
             theme = "styles.css",
             position = "fixed-top",
             
    tabPanel(title=HTML("Regional Growth Centers"),
             value="RGC",
             banner_ui('rgcBanner'),
             
             fluidRow(
               # Side Panel for Regional Growth Centers
               column(4, style='padding-left:10px; padding-right:0px;', leftpanel_ui('rgcleftpanel')),
               
               # Main Panel for Regional Growth Centers
               column(8, style='padding-left:25px; padding-right:25px;',
                      
                      # Center Selection
                      wellPanel(
                        fluidRow(
                        column(6, selectInput("RGC","Select Center:",rgc_names, selected = random_rgc),
                               bsPopover(id="RGC", title="Center Selection", content="Pick from the selection of Centers", placement = "left")),
                        column(6, br(), downloadLink('downloadData', label = "Download Center Data in Excel Format"))
                        )), 
                      
                      # Center Summary Data
                      overview_ui('rgcOverview'),
                      
                      # Summary Charts and Tables by Tabs for Demographics, Jobs, Housing, Transportation and Urban Form
                      tabsetPanel(type = "tabs",
                                  tabPanel("Demographics", demographics_ui('rgcDemographics')),
                                  tabPanel("Jobs", jobs_ui('rgcEmployment')),
                                  tabPanel("Housing", housing_ui('rgcHousing')),
                                  tabPanel("Transportation", transportation_ui('rgcTransportation')),
                                  tabPanel("Urban Form", form_ui('rgcForm')),
                                  tabPanel(icon("info-circle"), source_ui('rgcSource'))
                                  ) # end of RGC Tabsets
                      ), # End of RGC Main Panel
               ) # End of Main Panel Fluid Row for RGC Tab 
             ), # end Tabpanel for RGC
             
    tabPanel(title=HTML("Manufacturing/Industrial Centers"),
             value="MIC",
             banner_ui('micBanner'),
             
             fluidRow(
               
               # Side Panel for Manufacturing and Industrial Centers
               column(4, style='padding-left:10px; padding-right:0px;', leftpanel_ui('micleftpanel')),
               
               # Main Panel for Manufacturing and Industrial Centers
               column(8, style='padding-left:25px; padding-right:25px;',
                      
                      # Center Selection
                      wellPanel(
                        fluidRow(
                        column(6, selectInput("MIC","Select Center:",mic_names, selected = random_mic),
                               bsPopover(id="MIC", title="Center Selection", content="Pick from the selection of Centers", placement = "left")),
                        column(6, br(), downloadLink('downloadmicData', label = "Download Center Data in Excel Format"))
                        )),
                      
                      # Center Summary Data
                      overview_ui('micOverview'),
                      
                      # Summary Charts and Tables by Tabs for Demographics, Jobs, Housing, Transportation and Urban Form
                      tabsetPanel(type = "tabs",
                                  tabPanel("Jobs", jobs_ui('micEmployment')),
                                  tabPanel("Population and Housing", demographics_ui('micDemographics')),
                                  tabPanel("Transportation", transportation_ui('micTransportation')),
                                  tabPanel("Urban Form", form_ui('micForm')),
                                  tabPanel(icon("info-circle"), source_ui('micSource'))
                                  ) # end of Manufacturing and Industrial Centers tabsetPanel
                      ), # End of Main Panel Column for Manufacturing and Industrial Centers
               ) # End of Manufacturing and Industrial Centers fluidRow
             ), # end of Manufacturing and Industrial Centers tabPanel

    tags$footer(footer_ui('psrcfooter'))
    
    ) # End of navbarPage
  ) # End of shinyUI
