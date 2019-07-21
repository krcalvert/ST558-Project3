#
#

library(shiny)
library(tidyverse)
library(knitr)
library(DT)
library(tree)
library(leaflet)
library(plotly)

# Data ingest
# Load csv files and format the columns

#read in data
data_by_library <- read_csv("libraries.csv")

data_by_state <- read_csv("states.csv")

#Fix column names
colnames(data_by_state) <- make.names(colnames(data_by_state))
colnames(data_by_library) <- make.names(colnames(data_by_library))

#convert columns to factors
data_by_state$State <- as.factor(data_by_state$State)
data_by_state$Region.Code <- as.factor(data_by_state$Region.Code)

#create a new column for total number of libraries
data_by_state$Total.Libraries <- data_by_state$Central.Libraries+data_by_state$Branch.Libraries


#Shiny server dynamic code
shinyServer(function(input, output, session) {
    
    ###Datatable
    
    # Create title for datatable
    output$DT_title <- renderUI({
        group_name <- input$grouped_columns
        h3(paste("State-Level Summary Statistics for", group_name, "Variables"))
    })
    
    #Function to create a summary table based on grouped columns
    subset_data <- reactive({
        subset <- select(data_by_state, 
                   switch (input$grouped_columns,
                    "Staffing" = c("MLS.Librarians", "Librarians", "Employees", "Total.Staff"),
                    "Finances" = c("Total.Staff.Expenditures", "Total.Operating.Expenditures", "Print.Collection.Expenditures",
                                   "Digital.Collection.Expenditures", "Other.Collection.Expenditures", "Total.Collection.Expenditures"),
                    "Collections" =     Collections <- c("Print.Collection", "Digital.Collection", "Audio.Collection", "Downloadable.Audio", "Physical.Video", "Downloadable.Video", "State.Licensed.Databases", "Total.Licensed.Databases", "Print.Subscriptions")
                    )
                )
        round(apply(subset, 2, summary), 2)
    })

    # Create data table based on user input of groupings
    output$summary_data_table <- DT::renderDataTable({

        subset_data()

    })

    # Downloadable csv of data table
    output$download_DT <- downloadHandler(
        filename = function(){
            paste("Summary of ", input$grouped_columns, ".csv", sep = "")
            },
        content = function(file){
            write.csv(subset_data(), file, row.names = FALSE)
        }
    )

    ####
    #Scatter Plot

    # Create scatter plot based on user input
    
    sp <- reactive({
        if(input$sp_color){
            ggplot(data_by_state, aes_string(x = input$xaxis, y = input$yaxis)) + geom_point(aes(color = Region.Code)) +
                scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)
        } else {
            ggplot(data_by_state, aes_string(x = input$xaxis, y = input$yaxis)) + geom_point() +
                scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)
        }
    })
    output$scatter_plot <- renderPlot({
        sp()
    })

    # Click action for scatter plot
    output$xy <- renderText({
        paste0("x = ", input$plot_click$x, "\ny = ", input$plot_click$y)
    })
    
    # Downloadable png of plot
    output$download_SP <- downloadHandler(
        filename = function(){
            paste(input$xaxis, input$yaxis, ".png", sep = "")
        },
        content = function(file){
           ggsave(file, sp())
        }
    )
})

