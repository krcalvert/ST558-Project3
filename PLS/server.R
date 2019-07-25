#
#


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

#principal components analysis (PCA) model
PCs <- prcomp(select(data_by_state, Service.Population, Total.Libraries, Total.Staff, Local.Government.Operating.Revenue, 
                     Total.Operating.Revenue, Total.Staff.Expenditures, Print.Collection.Expenditures, Total.Collection.Expenditures, 
                     Total.Operating.Expenditures, Print.Collection, Audio.Collection, Physical.Video, Print.Subscriptions, 
                     Hours.Open, Library.Visits, Circulation.Transactions, Library.Programs, Library.Programs, 
                     Public.Internet.Computers, Internet.Computer.Use), center = TRUE, scale = TRUE)

#subset data for regression tree
  #select numeric variables only and exclude variables with zero variance
state_data_limited <- data_by_state %>% dplyr::select(-Submission.Year, -State.Code, -Region.Code) %>% dplyr::select_if(is.numeric) 


#subset of data for map
map_data <- data_by_library %>% dplyr::select(Library.Name, State, Zip.Code, latitude = Latitude, longitude = Longitude, County.Population)


#Shiny server dynamic code
shinyServer(function(input, output, session) {
    
    ###Datatable
    # Create title for full datatable
    output$all_data_title <- renderUI({
        h3("Aggregated Public Library Data by State")
    })
    
    # Create data table for all_state_date
    output$all_state_data <- DT::renderDataTable(
        data_by_state,
        options = list(scrollX = TRUE)
    )
    
    # Download button for all_State_data
    output$all_data_download <- downloadHandler(
        filename = ("PLS State Data.csv"),
        content = function(file){
            write.csv(data_by_state, file, row.names = FALSE)
        }
    )
    
    ###Summary Stats
    
    # Create title for summary datatable
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
    
    #### Bar chart
    
    # SUbset data by region
    subset_region_plot <- reactive({
        region <- switch(input$region, 
                         "New England (CT ME MA NH RI VT)" = 1, 
                         "Mid East (DE DC MD NH NY PA)" = 2, 
                         "Great Lakes (IL IN MI OH WI)" = 3, 
                         "Plains (IA KS MN MO NE ND SD)" = 4, 
                         "Southeast (AL AR FL GA KY L MS NC SC TN VA WV)" = 5, 
                         "Southwest (AZ NM OK TX)" = 6,
                         "Rocky Mountains (CO ID MT UT WY)" = 7,
                         "Far West (AK CA HI NV OR WA)" = 8) 
        subset_by_region <- data_by_state %>% filter(Region.Code == region)
        
        ggplot(data = subset_by_region, aes(x = State, y = Total.Operating.Expenditures/State.Population)) + 
            geom_bar(stat = "Identity", fill = "red") + 
            ggtitle(paste("Per Capita Public Library Operating Costs for ", input$region, ".", sep = "")) +
            ylab("Dollars Spent per Capita")
    })
    
    output$bar_chart <- renderPlotly({
        subset_region_plot()
    })
    
    # Download png of chart
    output$download_bar <- downloadHandler(
        filename = function(){
            paste(input$region, ".png", sep = "")
        },
        content = function(file){
            ggsave(file, subset_region_plot())
        }
    )

        
    #### PCA
    
    #Create Biplot
    ##Must add user input to view PCs
    plot_biplot <- reactive({
        PC1 <- input$first_PC
        PC2 <- input$second_PC
        validate(
            need(PC1 != PC2, "Please select distinct principal components.")
        )
        if(input$scale_pca){
            autoplot(PCs, x = PC1, y = PC2, scale = 0, loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3)
        }else{
            autoplot(PCs, x = PC1, y = PC2, loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3)
        }
    })
    
    output$biplot <- renderPlot({
        plot_biplot()
    })
    
    #Dynamic table of PCs and loadings
    pc_table <- reactive({
        PC1 <- input$first_PC
        PC2 <- input$second_PC
        kable(PCs$rotation[,c(PC1,PC2)]) %>%
                kable_styling("striped", full_width = F)
    })
    
    #output html table
    output$PCA_data <- renderText({
        pc_table()
    })
    
    # Downloadable png of biplot
    output$download_biplot <- downloadHandler(
        filename = function(){
            paste(input$first_PC, "_", input$second_PC, "_biplot.png", sep = "")
        },
        content = function(file){
            ggsave(file, plot_biplot())
        }
    )

    # Downloadable csv of PCA table
    output$download_PCs <- downloadHandler(
        filename = function(){
            paste("PC", input$first_PC, "_PC", input$second_PC, ".csv", sep = "")
        },
        content = function(file){
            PC1 <- input$first_PC
            PC2 <- input$second_PC
            write.csv(PCs$rotation[,c(PC1,PC2)], file, row.names = FALSE)
            }
        )
    
    ### Supervised Models
    
    ### Simple Linear Regression
    
    slr <- reactive({
        pred <- as.matrix(data_by_state[input$linear_vars])
        resp <- as.matrix(data_by_state[input$linear_resp])
        newdf <- data.frame(pred,resp)
        fit <- lm(resp ~ pred)
        
        newdf <- ciTools::add_pi(newdf, fit, names = c("lower", "upper"))

        if(input$pi){
            gp <- ggplot(newdf, aes_string(x = input$linear_vars, y = input$linear_resp)) +
                geom_point() +
                geom_smooth(method = "lm", fill = "Blue") +
                geom_ribbon(aes_string(ymin = newdf$lower, ymax = newdf$upper), alpha = 0.3, fill = "Red") +
                xlab(deparse(input$linear_vars)) + 
                ylab(deparse(input$linear_resp)) +
                scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma) +
                ggtitle("Scatter plot with 95% PI & 95% CI")
        } else {
            gp <- ggplot(newdf, aes_string(x = input$linear_vars, y = input$linear_resp)) +
                geom_point() +
                geom_smooth(method = "lm", fill = "Blue") +
                xlab(deparse(input$linear_vars)) +
                ylab(deparse(input$linear_resp)) +
                scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma) +
                ggtitle("Scatter plot with 95% CI")
        }
        print(gp)
    })
    
    output$slr_plot <- renderPlot({
        slr()
    })

    # Downloadable png of slr plot
    output$slr_plot_download <- downloadHandler(
        filename = function(){
            paste(input$linear_resp, "_", input$linear_vars, "_plot.png", sep = "")
        },
        content = function(file){
            ggsave(file, slr())
        }
    )
    # SLR prediction title
    output$predict_slr_title <- renderUI({
        p(strong("Predicted value for response variable"))
    })
    
    #change slider max
    observe({
        val <- max(data_by_state[,input$linear_vars])
        updateSliderInput(session, "pred_slr_value", value = val/2, max = val, step = val/100)
    })
    
    # Calculate prediction for SLR
    output$pred_response <- renderText({
        pred <- unlist(data_by_state[,input$linear_vars])
        resp <- unlist(data_by_state[,input$linear_resp])
        fit <- lm(resp ~ pred, data = data_by_state)
        new <- data.frame(pred = input$pred_slr_value)
        y <- predict(fit, newdata = new)
        paste0(deparse(input$linear_resp), " = ", scales::comma(round(y,2)))
    })
    
    ### Regression Tree
    tree_model <- reactive({
      #user selects seed
      set.seed(input$seed)
      
      #subset date into test and training sets
      train <- sample(1:nrow(state_data_limited), size = nrow(state_data_limited)*.8)
      test <- dplyr::setdiff(1:nrow(state_data_limited), train)
      
      state_data_train <- state_data_limited[train,]
      state_data_test <- state_data_limited[test,]
      
      #train model
      train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      
      #user selects number of response variable and the number of trees
      
      grid <- expand.grid(maxdepth = input$ntrees)
      
      tree_fit <- train(input$Resp ~ ., data = state_data_train,
                        method = "rpart2",
                        trControl = train_control,
                        tuneGrid = grid)
      fancyRpartPlot(tree_fit$finalModel)

    })
    
    output$tree_plot -> renderPlot({
      tree_model()
    })
    
    ###leaflet map
    output$lib_map <- renderLeaflet({
        leaflet(data = map_data) %>% 
        addTiles %>% 
        setView(zoom = 7, lng = -78.6821, lat = 35.7847) %>% 
        addCircleMarkers(clusterOptions = markerClusterOptions(),
                         radius = ~ifelse(County.Population > 500000, 15, 10), 
                         color = "green", stroke = FALSE, fillOpacity = .5, 
                         popup = paste("<h4>", map_data$Library.Name,"</h4>", "<br>", "County Pop. = ", map_data$County.Population))
    })
})

