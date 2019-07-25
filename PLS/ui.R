######################################################################
## R Shiny App to visualize and analyze data from the 2014
## Public Libraries Survey, featuring:
## Exploratory data analysis
## Machine learning with supervised and unsupervised learning models
## and Geoplotting.
## 
## Kristin Calvert - Summer 2019
######################################################################

library(shiny)
library(shinydashboard)


ui <-dashboardPage(skin = "red",
        #Dashboard title
        dashboardHeader(title = "Public Libraries Survey, 2014", titleWidth = 300),
        
        #Dashboard sidebar menu
        dashboardSidebar(
            sidebarMenu(
                menuItem("Information", tabName = "info", icon = icon("info-circle")),
                menuItem("Data", tabName = "data", icon = icon("table")),
                menuItem("Unsupervised Model", tabName = "pca", icon = icon("hubspot")),
                menuItem("Supervised Models", tabName = "glm", icon = icon("chart-line")),
                menuItem("Map", tabName = "map", icon = icon("globe"))
            )
        ),
        dashboardBody(
            tabItems(
                #First Tab content
                tabItem(tabName = "info",
                    fluidRow(
                        column(6,
                               h2("What is this App?"),
                               box(width = 12, background = "light-blue",
                                   h4("Text"))),
                        column(6, 
                               h2("How do I use this App?"),
                               box(width = 14, background = "light-blue",
                                    h4("Text")))
                    ),
                    fluidRow(
                        column(12, 
                               h2("About the dataset"),
                               a(href="https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey", 
                                 "Description taken from IMLS page about the Public Libraries Survey"),
                               br(),
                               box(width = 12, background = "light-blue",
                                   h4("The Public Libraries Survey (PLS) examines when, where, and how library services are changing to 
                                      meet the needs of the public. These data, supplied annually by public libraries across the country, 
                                      provide information that policymakers and practitioners can use to make informed decisions about 
                                      the support and strategic management of libraries."),
                                   h4("Purpose: The survey provides statistics on the status of public libraries in the United States."),
                                   
                                   h4("Coverage: The data are collected from approximately 9,000 public libraries with approximately 
                                      17,000 individual public library outlets (main libraries, branches, and bookmobiles) in the 50 states, 
                                      the District of Columbia, and outlying territories."),
                                   
                                   h4("Content: Data includes information about library visits, circulation, size of collections, public
                                      service hours, staffing, electronic resources, operating revenues and expenditures and number of 
                                      service outlets."),
                                   
                                   h4("Frequency: Collected annually since 1988. (Data files are available since 1992.)"),
                                   
                                   h4("Methods: At the state level, PLS is administered by Data Coordinators, appointed by the chief officer
                                      of the state library agency from each state or outlying area. State Data Coordinators collect the 
                                      requested data from local public libraries and report these data to us via a web-based reporting system."),
                                   
                                   h4("Use: PLS data are useful to researchers, journalists, the public, local practitioners, and policymakers
                                      at the federal, state, and local levels, and are used for planning, evaluation, and policy making. Download
                                      the datasets in multiple formats below, or use our online Library Search & Compare tool to find a library
                                      and browse the latest available data. Browse over 25 years’ worth of research publications about the Public
                                      Libraries Survey (PLS).")
                                )
                        )
                    ),
                    fluidRow(
                        column(6,
                               box(width = 12,
                                    h4("Dataset Downloaded From"),
                                    a(href ="https://www.kaggle.com/imls/public-libraries", "Kaggle, 2014 Public Libraries Survey"),
                                    h4("Data definitions from the Institute of Museum and Library Services"),
                                    a(href ="https://www.imls.gov/sites/default/files/fy2014_pls_data_file_documentation.pdf","Documentation")
                                )
                               ),
                        column(6,
                            box(width = 12,
                                h4("Kristin Calvert"),
                                "North Carolina State University",
                                br(),
                                "ST558 - Data Science for Statisticians",
                                br(),
                                "Summer 2019",
                                br(),
                                "Project 3"))
                        )
                ), #end tabitem info
                
                #second tab content
                #data exploration
                tabItem(tabName ="data",
                    tabsetPanel(
                        #View data set
                        tabPanel("All State Data",
                                column(9,
                                    fluidRow(
                                        uiOutput("all_data_title"))),
                                column(3,
                                    fluidRow(
                                        br(),
                                        downloadButton("all_data_download"),
                                        br()
                                        )),
                                column(12,
                                       fluidRow(
                                        dataTableOutput("all_state_data")
                                        ))
                        ), #end tabPanel alldata
                        #Calculate summary statistics
                        tabPanel("Summary Statistics", 
                            fluidRow(
                               #User selects variable sets
                               column(7,
                                      selectInput("grouped_columns",h4("Select a Grouping of Variables"), 
                                           choices = list("Staffing","Finances","Collections"), multiple = FALSE)),
                                                                          
                                #Save Button to download csv of data table
                                column(5, 
                                       br(),
                                       br(),
                                       downloadButton("download_DT", "Download"))
                            ),
                             fluidRow(
                                box(width = 12,
                                    #Dynamic UI title
                                    uiOutput("DT_title"),
                                    #Data table of summary statistics
                                    dataTableOutput("summary_data_table")
                                )
                            )
                        ), #end tabPanel summary stats
                        
                        tabPanel("Scatter Plot",
                                 fluidRow(
                                        #User selects x and y variables
                                         column(7,
                                                selectInput("xaxis",h4("X-axis variable"),
                                                            choices = list("Circulation Transactions" = "Circulation.Transactions", 
                                                                           "Library Visits" = "Library.Visits", 
                                                                           "Number of Books" = "Print.Collection"), multiple = FALSE),
                                                selectInput("yaxis",h4("Y-axis variable"),
                                                            choices = list("Total Staff Expenditures" = "Total.Staff.Expenditures", 
                                                                           "Library Users" = "Service.Population", 
                                                                           "Number of Libraries" = "Total.Libraries"), multiple = FALSE),
                                                checkboxInput("sp_color", label = "Add color for region?", value = FALSE)
                                         ),
                                        #Save button to download scatter plot
                                        column(5,
                                               br(),
                                               br(),
                                               downloadButton("download_SP", "Download Plot")
                                               )
                                ), 
                                fluidRow(
                                        box(width = 12,
                                            #Scatter plot with user-inputted variables
                                            plotOutput("scatter_plot", click = "plot_click"),
                                            verbatimTextOutput("xy"))
                                ) 
                        ), #end tabPanel scatter plot
                        
                        tabPanel("Bar Chart",
                                fluidRow(
                                        #user selects region
                                        column(7,
                                               selectInput("region", h4("Select a US Region"),
                                                           choices = list("New England (CT ME MA NH RI VT)",
                                                                          "Mid East (DE DC MD NH NY PA)",
                                                                          "Great Lakes (IL IN MI OH WI)",
                                                                          "Plains (IA KS MN MO NE ND SD)",
                                                                          "Southeast (AL AR FL GA KY L MS NC SC TN VA WV)",
                                                                          "Southwest (AZ NM OK TX)",
                                                                          "Rocky Mountains (CO ID MT UT WY)",
                                                                          "Far West (AK CA HI NV OR WA)"), multiple = FALSE)
                                                ),
                                        #Save button to download bar chart
                                         column(5,
                                                br(),
                                                br(),
                                               downloadButton("download_bar", "Download Chart")
                                               )
                                        ),
                                 fluidRow(
                                         box(width = 12,
                                             #bar chart with user-selected region
                                             plotlyOutput("bar_chart"))
                                        )
                                 ) #end tabItem barchart 
                            ) #end tabsetPanel
                        ), #end tabItem "data"
                
                #third tab for PCA
                tabItem(tabName = "pca",
                         fluidRow(
                                 #User selects PCs
                                 column(9,
                                        fluidRow(
                                                includeHTML("PCA.html")
                                                ),
                                        fluidRow(
                                                column(6,
                                                        numericInput("first_PC", h4("Select 1st Principal Components for Biplot"), 
                                                                value = 1, min = 1, max = 5)),
                                                column(6,
                                                        numericInput("second_PC", h4("Select 2nd Principal Components for Biplot"), 
                                                                value = 2, min = 1, max = 5))
                                                ),
                                        fluidRow(
                                                column(12,
                                                       checkboxInput("scale_pca", label = "Scale variables?"))
                                                )
                                        ),
                                 #Save Button to download biplot or table
                                 column(3, 
                                        downloadButton("download_biplot", "Download Plot"),
                                        br(),
                                        br(),
                                        downloadButton("download_PCs", "Download Table")
                                        )
                                ),
                        fluidRow(
                                #biplot
                                column(6,
                                       box(width = 12,
                                           plotOutput("biplot"))),
                                
                                #PC table of loadings
                                column(6,
                                       box(width = 12,
                                           htmlOutput("PCA_data")))
                                )
                ), #end tabItem pca

                        tabItem("glm",
                           tabsetPanel(
                                   tabPanel("Simple Linear Regression",
                                        fluidRow(
                                           column(5,
                                                selectInput("linear_vars", label = h4("Select predictor variable"),
                                                            choices = list("Print Collection" = "Print.Collection", 
                                                                  "Digital Collection" = "Digital.Collection", 
                                                                  "Audio Collection" = "Audio.Collection",
                                                                  "Hours Open" = "Hours.Open", 
                                                                  "Registered Users" = "Registered.Users",
                                                                  "Total Libraries" = "Total.Libraries"),
                                                    selected = 1, multiple = FALSE),
                                                selectInput("linear_resp", label = h4("Select response variable"),
                                                            choices = list("Library Visits" = "Library.Visits",
                                                                   "Salaries",
                                                                   "Circulation Transactions"  = "Circulation.Transactions")),
                                                checkboxInput("pi", label = "Include prediction interval?"),
                                                downloadButton("slr_plot_download", "Download Plot")),
                                            column(7,
                                                br(),
                                                sliderInput("pred_slr_value", label ="Enter a value for selected predictor",
                                                            min = 1, max = 100000000, value = 500000, step = 10000),
                                                uiOutput("predict_slr_title"),
                                                box(width = 8,
                                                   textOutput("pred_response")
                                                   ),
                                                br(),
                                                br(),
                                                br(),
                                                br())
                                                ),
                                        fluidRow(
                                                box(width = 11,
                                                    plotOutput("slr_plot"))
                                                )
                                   ) #end tabPanel
                              ) #end tabsetPanel
                        ) #end tabItem glm
            ) #end tabItems
        ) #end dashboardBody
    ) #end dashboardPage