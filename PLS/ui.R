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
                                   h4("This application demonstrates and applies the learning outcomes for the Data Science for Statisticians 
                                      course at North Carolina State University."),
                                   h4("The major topics covered are:"),
                                   tags$ol(
                                           tags$li("Coding in R"),
                                           tags$li("Reading and summarizing data"),
                                           tags$li("Statistical/Machine learning"),
                                           tags$li("Visualizing and deploying data")
                                   ),
                                   h4("This applet provides a data exploration page with numeric and graphical summaries,
                                      one unsupervised learning model (principal components analysis (PCA)), two supervised learning
                                      models (simple linear regression and simple regression trees), and a map."),
                                   h4("Each section provides the user some opportunity to choose parameters or set properties
                                      of the algorithm and to save the data or plot."))),
                        column(6, 
                               h2("How do I use this App?"),
                               box(width = 14, background = "light-blue",
                                    h4("The tabs on the sidebar allow for navigation between models or data sections. Within each of these
                                       tabs, there are one or more tabs across the top of the app."),
                                   h4("Data tab:"),
                                   tags$ol(
                                           tags$li("Browse the entire data set"),
                                           tags$li("View summary data for the group of variables you choose"),
                                           tags$li("View a scatter plot of two variables you choose, an option to add color by a grouping,
                                                   and the ability to click on the graph"),
                                           tags$li("View a bar chart of operating costs by US region")
                                   ),
                                   h4("Unsupervised Model tab:"),
                                      tags$ol(
                                              tags$li("View the biplot and principal components you choose from the PCA")
                                      ),
                                   h4("Supervised Models tab:"),
                                   tags$ol(
                                           tags$li("Perform a simple linear regression with the variables you choose, and then
                                                   run your own prediction"),
                                           tags$li("Create a regression tree for the variable of your choosing and the maximum depth
                                                   for the model, and then run your own prediction")
                                   ),
                                   h4("Map tab:"),
                                   tags$ol(
                                           tags$li("View and interact with a map of all public libraries in the United States")
                                   )
                               ) #end box
                        )#end column
                    ), #end fluidRow
                    fluidRow(
                        column(12, 
                               h3("About the dataset"),
                               a(href="https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey", 
                                 "Description taken from IMLS page about the Public Libraries Survey"),
                               br(),
                               box(width = 12, 
                                   p("The Public Libraries Survey (PLS) examines when, where, and how library services are changing to 
                                      meet the needs of the public. These data, supplied annually by public libraries across the country, 
                                      provide information that policymakers and practitioners can use to make informed decisions about 
                                      the support and strategic management of libraries."),
                                   p("Purpose: The survey provides statistics on the status of public libraries in the United States."),
                                   
                                   p("Coverage: The data are collected from approximately 9,000 public libraries with approximately 
                                      17,000 individual public library outlets (main libraries, branches, and bookmobiles) in the 50 states, 
                                      the District of Columbia, and outlying territories."),
                                   
                                   p("Content: Data includes information about library visits, circulation, size of collections, public
                                      service hours, staffing, electronic resources, operating revenues and expenditures and number of 
                                      service outlets."),
                                   
                                   p("Frequency: Collected annually since 1988. (Data files are available since 1992.)"),
                                   
                                   p("Methods: At the state level, PLS is administered by Data Coordinators, appointed by the chief officer
                                      of the state library agency from each state or outlying area. State Data Coordinators collect the 
                                      requested data from local public libraries and report these data to us via a web-based reporting system."),
                                   
                                   p("Use: PLS data are useful to researchers, journalists, the public, local practitioners, and policymakers
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
                                    p("License:   CC0: Public Domain"),
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
                        ) #end fluidRow
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
                                        downloadButton("all_data_download", "Download Table"),
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
                                       downloadButton("download_DT", "Download Table"))
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
                                                                   "Salaries" = "Salaries",
                                                                   "Circulation Transactions"  = "Circulation.Transactions")),
                                                checkboxInput("pi", label = "Include prediction interval?"),
                                                downloadButton("slr_plot_download", "Download Plot")),
                                            column(7,
                                                h4("Run your prediction:"),
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
                                   ), #end tabPanel


                      tabPanel("Regression Tree",
                             fluidRow(
                               uiOutput("tree_title"),
                               column(4,
                                      h4("Build your Tree"),
                                      selectInput("Resp", label = "Choose a response variable:",
                                                  choices = list("Service Population" = "Service.Population",
                                                                 "State Population" = "State.Population",
                                                                 "Central Libraries" = "Central.Libraries",
                                                                 "Branch Libraries" = "Branch.Libraries",
                                                                 "Bookmobiles" = "Bookmobiles",
                                                                 "MLS Librarians" = "MLS.Librarians",
                                                                 "Librarians" = "Librarians",
                                                                 "Employees" = "Employees",
                                                                 "Total Staff" = "Total.Staff",
                                                                 "Local Government Operating Revenue" = "Local.Government.Operating.Revenue",
                                                                 "State Government Operating Revenue" = "State.Government.Operating.Revenue",
                                                                 "Federal Government Operating Revenue" = "Federal.Government.Operating.Revenue",
                                                                 "Other Operating Revenue" = "Other.Operating.Revenue",
                                                                 "Total Operating Revenue" = "Total.Operating.Revenue",
                                                                 "Salaries" = "Salaries",
                                                                 "Benefits" = "Benefits",
                                                                 "Total Staff Expenditures" = "Total.Staff.Expenditures",
                                                                 "Print Collection Expenditures" = "Print.Collection.Expenditures",
                                                                 "Digital Collection Expenditures" = "Digital.Collection.Expenditures",
                                                                 "Other Collection Expenditures" = "Other.Collection.Expenditures",
                                                                 "Total Collection Expenditures" = "Total.Collection.Expenditures",
                                                                 "Other Operating Expenditures" = "Other.Operating.Expenditures",
                                                                 "Total Operating Expenditures" = "Total.Operating.Expenditures",
                                                                 "Local Government Capital Revenue" = "Local.Government.Capital.Revenue",
                                                                 "State Government Capital Revenue" = "State.Government.Capital.Revenue",
                                                                 "Federal Government Capital Revenue" = "Federal.Government.Capital.Revenue",
                                                                 "Other Capital Revenue" = "Other.Capital.Revenue",
                                                                 "Total Capital Revenue" = "Total.Capital.Revenue",
                                                                 "Total Capital Expenditures" = "Total.Capital.Expenditures",
                                                                 "Digital Collection" = "Digital.Collection",
                                                                 "Audio Collection" = "Audio.Collection",
                                                                 "Downloadable Audio" = "Downloadable.Audio",
                                                                 "Physical Video" = "Physical.Video",
                                                                 "Downloadable Video" = "Downloadable.Video",
                                                                 "Local Cooperative Agreements" = "Local.Cooperative.Agreements",
                                                                 "State Licensed Databases" = "State.Licensed.Databases",
                                                                 "Total Licensed Databases" = "Total.Licensed.Databases",
                                                                 "Print Subscriptions" = "Print.Subscriptions",
                                                                 "Reference Transactions" = "Reference.Transactions",
                                                                 "Registered Users" = "Registered.Users",
                                                                 "Circulation Transactions" = "Circulation.Transactions",
                                                                 "Interlibrary Loans Provided" = "Interlibrary.Loans.Provided",
                                                                 "Interlibrary Loans Received" = "Interlibrary.Loans.Received",
                                                                 "Children’s Programs" = "Children’s.Programs",
                                                                 "Young Adult Programs" = "Young.Adult.Programs",
                                                                 "Library Program Audience" = "Library.Program.Audience",
                                                                 "Children’s Program Audience" = "Children’s.Program.Audience",
                                                                 "Young Adult Program Audience" = "Young.Adult.Program.Audience",
                                                                 "Public Internet Computers" = "Public.Internet.Computers",
                                                                 "Internet Computer Use" = "Internet.Computer.Use",
                                                                 "Wireless Internet Sessions" = "Wireless.Internet.Sessions"
                                                                 )),
                                      numericInput("seed", label = "Set a seed", min=1, max=999, value = 250),
                                      numericInput("ntrees", label = "Select number of trees",
                                                   min = 1, max = 4, value = 1)
                                      ), #end column
                               column(4,
                                      h4("Run your prediction:"),
                                      numericInput("tree_var_1", label = "Set Value for Library Programs", 
                                                   min=1, value = 10000),
                                      numericInput("tree_var_2", label ="Set Value for Print Collection", 
                                                   min = 1, value = 2000000),
                                      numericInput("tree_Var_3", label = "Set Value for Hours Open", 
                                                   min = 1, value = 100000)
                               ),
                               column(4,
                                      br(),
                                      br(),
                                      numericInput("tree_Var_4", label="Set Value for Total Libraries", 
                                                   min = 1, value = 50),
                                      numericInput("tree_Var_5", label = "Set Value for Library Visits", 
                                                   min = 1, value = 3000000)
                                     ) #end column
                                  ), #end fluidRow
                             fluidRow(
                                     column(4,
                                                withMathJax("$$MSE = \\frac{1}{n}\\sum_{i=1}^n (Y_i-\\hat{Y}_i)^2$$"),
                                                uiOutput("tree_rmse")),
                                     column(4,
                                                h4("Prediction Output"),
                                                box(width = 12,
                                                    uiOutput("predicted_tree"))
                                     ),
                                     column(4,
                                            br(),
                                            br(),
                                            p(strong("To save the decision tree, right click plot and select 'save image as...'")))
                             ), #end fluidRow
                              fluidRow(
                                box(width = 12,
                                    plotOutput("tree_plot"))
                              ) #end fluidRow
                             ) #end tabPanel 
                           ) #end tabsetPanel
                        ), #end tabItem glm
            tabItem("map",
                    fluidPage(
                      fluidRow(
                        box(width=12,
                            h4("This map plots all public libraries that reported data for the 2014 PLS."),
                            h4("Click a circle to view the library name and the county population."))
                      ),
                      leafletOutput("lib_map")
                    )
                    ) #end tabItem map
            ) #end tabItems
        ) #end dashboardBody
    ) #end dashboardPage