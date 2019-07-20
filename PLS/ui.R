#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
                               a(href="https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey", "Description taken from IMLS page about the Public Libraries Survey"),
                               br(),
                               box(width = 12, background = "light-blue",
                                   h4("The Public Libraries Survey (PLS) examines when, where, and how library services are changing to meet the needs of the public. These data, supplied annually by public libraries across the country, provide information that policymakers and practitioners can use to make informed decisions about the support and strategic management of libraries."),
                                   h4("Purpose: The survey provides statistics on the status of public libraries in the United States."),
                                   
                                   h4("Coverage: The data are collected from approximately 9,000 public libraries with approximately 17,000 individual public library outlets (main libraries, branches, and bookmobiles) in the 50 states, the District of Columbia, and outlying territories."),
                                   
                                   h4("Content: Data includes information about library visits, circulation, size of collections, public service hours, staffing, electronic resources, operating revenues and expenditures and number of service outlets. Learn more about PLS data element definitions."),
                                   
                                   h4("Frequency: Collected annually since 1988. (Data files are available since 1992.)"),
                                   
                                   h4("Methods: At the state level, PLS is administered by Data Coordinators, appointed by the chief officer of the state library agency from each state or outlying area. State Data Coordinators collect the requested data from local public libraries and report these data to us via a web-based reporting system."),
                                   
                                   h4("Use: PLS data are useful to researchers, journalists, the public, local practitioners, and policymakers at the federal, state, and local levels, and are used for planning, evaluation, and policy making. Download the datasets in multiple formats below, or use our online Library Search & Compare tool to find a library and browse the latest available data. Browse over 25 years’ worth of research publications about the Public Libraries Survey (PLS).")
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
                                "Project 3"
                            )
                        )
                    )
                )
            )
        )
    )