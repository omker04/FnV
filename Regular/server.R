server = shinyServer(function(input, output, session){
  #shinyjs::(id = "RestOfPage")
  options(shiny.maxRequestSize = 5*1024^3) #setting maximum filesize to 5gb
  
  output$title <- renderUI({
    fluidRow(
      column(width = 11, valueBox(value = "F&V output for Lightbulbs", subtitle = "", color = "blue", width = "100%")),
      column(width = 1, infoBox(title = NULL, href='http://hazr10070725k4.cloud.wal-mart.com:3851/index1.html', color = "blue", icon = icon("home"), width = 10, fill = FALSE))
    )
  })
  {
  observeEvent(input$simulationNbr,{
    dataStoreGUPCs <- apply(get(paste0("data_store_", store_nbr))[attribute], 1, function(x) paste(x, collapse = " || "))
    #updateSelectInput(session, inputId = "GUPCs", choices = as.vector(dataStoreGUPCs), selected = integer(0))
    R2 <- R_sqr_train[rank(-R_sqr_train) %>% order]
    index <- which(R_sqr_train == R2[as.numeric(input$simulationNbr)])
    output$RsqrTrain <- renderValueBox(valueBox(value = strong(round(R2[as.numeric(input$simulationNbr)],4)), subtitle = h4(strong("R Square in Training Set")), width = "100%"))
    output$RsqrTest <- renderValueBox(valueBox(value = strong(round(R_sqr_test[index],4)), subtitle = h4(strong("R Square in Test Set")), width = "100%"))
    })
  
  output$trainDemShare <- renderPlot({
    R2 <- R_sqr_train[rank(-R_sqr_train) %>% order]
    index <- which(R_sqr_train == R2[as.numeric(input$simulationNbr)])
    ggplot() +
      geom_point(aes(x = par_set, y = par_set_est_rep[[index]])) +
      geom_abline(aes(intercept = 0, slope = 1), 
                  color ="red") +
      xlab("Initial Values") +
      ylab("Estimated Values") +
      ggtitle("Demand Share Parameters")
    })
  output$trainSubsParam <- renderPlot({
    R2 <- R_sqr_train[rank(-R_sqr_train) %>% order]
    index <- which(R_sqr_train == R2[as.numeric(input$simulationNbr)])
    ggplot() +
      geom_point(aes(x = par_set_sub, y = par_set_sub_est_rep[[index]])) +
      geom_abline(aes(intercept = 0, slope = 1), 
                  color ="red") +
      xlab("Initial Values") +
      ylab("Estimated Values") +
      ggtitle("Substitution Parameters")
    })
  
  
  output$trainScatterPlot <- renderPlot({
    # source('gg_subtitle.R')
    R2 <- R_sqr_train[rank(-R_sqr_train) %>% order]
    index <- which(R_sqr_train == R2[as.numeric(input$simulationNbr)])
    gg <- ggplot(data_for_graph_rep[[index]]) +
      geom_point(aes(x = actual_unit_sales_share, 
                     y = forcasted_unit_sales_share),
                 col = "black", size = 0.5) + 
      geom_abline(aes(intercept = 0, slope = 1), 
                  color ="red") +  theme_bw() +
      theme(panel.grid.minor = element_line(colour="gray95")) +
      xlab("Actual") +
      ylab("Estimated") +
      geom_smooth(span = 0.25) +
      ggtitle("Sales Share of GUPCs - Lightbulbs (wk-year 04-2015 to 28-2015)") +
      theme(plot.title = element_text(hjust=0, size=16))
    return(ggplot_with_subtitle(gg, paste("R-Square", " = ", round(R_sqr_train[index], 2)), bottom_margin = 10, lineheight=0.5))
    })
  
  output$trainLinePlot <- renderPlot({
    R2 <- R_sqr_train[rank(-R_sqr_train) %>% order]
    index <- which(R_sqr_train == R2[as.numeric(input$simulationNbr)])
    data_for_graph <- data_for_graph_rep[[index]]
    data_line <- data_for_graph[,c(2,3)] 
    colnames(data_line) <- colnames(data_for_graph[,c(2,4)])
    data_line <- rbind(data_line, data_for_graph[,c(2,4)])
    colnames(data_line) <- c("GUPC", "Sales.Share")
    data_line$label <- c(rep("Estimated", nrow(data_for_graph)), rep("Actual", nrow(data_for_graph)))
    data_line$GUPC_number <- c(seq(1 : nrow(data_for_graph)), seq(1 : nrow(data_for_graph)))
    
    # source('gg_subtitle.R')
    gg_line = ggplot(data_line) +
      geom_line(aes(x = GUPC_number, 
                    y = Sales.Share, group = label, colour = label))  +
      scale_color_manual("Sales Share\n",labels = c("Actual", "Estimated"), values = c("dark green", "red")) +
      theme_bw() +
      theme(panel.grid.minor = element_line(colour="gray95")) +
      xlab("GUPC") +
      ylab("Sales Share") +
      geom_smooth(span = 0.25) +
      ggtitle("Sales Share of GUPCs - Lightbulbs (wk-year 04-2015 to 28-2015)") +
      theme(plot.title = element_text(hjust=0, size=16))
    return(ggplot_with_subtitle(gg_line, paste("R-Square", " = ", round(R_sqr_train[index], 2)), bottom_margin = 10, lineheight=0.5))
    })
  
  
  output$testScatterPlot <- renderPlot({
    R2 <- R_sqr_train[rank(-R_sqr_train) %>% order]
    index <- which(R_sqr_train == R2[as.numeric(input$simulationNbr)])
    data_for_the_graph_test <- data_for_graph_test[[index]]
    # source('gg_subtitle.R')
    gg_test <- ggplot(data_for_the_graph_test) +
      geom_point(aes(x = actual_unit_sales_share, 
                     y = forcasted_unit_sales_share),
                 col = "black", size = 0.5) + 
      geom_abline(aes(intercept = 0, slope = 1), 
                  color ="red") +  theme_bw() +
      theme(panel.grid.minor = element_line(colour="gray95")) +
      xlab("Actual") +
      ylab("Estimated") +
      geom_smooth(span = 0.25) +
      ggtitle("Sales Share of GUPCs - Lightbulbs (wk-year 30-2015 to 02-2016)") +
      theme(plot.title = element_text(hjust=0, size=16))
    return(ggplot_with_subtitle(gg_test, paste("R-Square", " = ", round(R_sqr_test[index], 2)), bottom_margin = 10, lineheight = 0.5))
    })
  output$testLinePlot <- renderPlot({
    R2 <- R_sqr_train[rank(-R_sqr_train) %>% order]
    index <- which(R_sqr_train == R2[as.numeric(input$simulationNbr)])
    data_for_the_graph_test <- data_for_graph_test[[index]]
    data_line_test <- data_for_the_graph_test[,c(1,2)] 
    colnames(data_for_the_graph_test[,c(1,3)]) <- colnames(data_for_the_graph_test[,c(1,2)])
    colnames(data_line_test) <- colnames(data_for_the_graph_test[,c(1,3)])
    data_line_test <- rbind(data_line_test, data_for_the_graph_test[,c(1,3)])
    colnames(data_line_test) <- c("GUPC", "Sales.Share")
    data_line_test$label <- c(rep("Estimated", nrow(data_for_the_graph_test)), rep("Actual", nrow(data_for_the_graph_test)))
    data_line_test$GUPC_number <- c(seq(1 : nrow(data_for_the_graph_test)), seq(1 : nrow(data_for_the_graph_test)))
    
    # source('gg_subtitle.R')
    gg_line_test = ggplot(data_line_test) +
      geom_line(aes(x = GUPC_number, 
                    y = Sales.Share, group = label, colour = label))  +
      scale_color_manual("Sales Share\n",labels = c("Actual", "Estimated"), values = c("dark green", "red")) +
      theme_bw() +
      theme(panel.grid.minor = element_line(colour="gray95")) +
      xlab("GUPC") +
      ylab("Sales Share") +
      geom_smooth(span = 0.25) +
      ggtitle(h1("Sales Share of GUPCs - Lightbulbs (wk-year 30-2015 to 02-2016)")) +
      theme(plot.title = element_text(hjust=0, size=16))
    return(ggplot_with_subtitle(gg_line_test, paste("R-Square", " = ", round(R_sqr_test[index], 2)), bottom_margin = 10, lineheight = 0.5))
    })
  }
  deleteItem <- reactiveValues(item = NULL, fn = NULL, n = 0)
  
  observeEvent(input$salesTransfer1DblClick,{
    deleteItem$n <- deleteItem$n + 1
    deleteItem$item <- input$salesTransfer1DblClick
  })
  
  defaultValues <- reactiveValues(slider = 0.9, fns = "All Functions")
  output$absPanel <- renderUI({
    if(is.null(deleteItem$item)){
      absolutePanel(box(collapsible = TRUE, width = "100%", title = "Info Panel", status = "primary",
        div(id = "alert1", valueBox(value = h5(""), subtitle = h6("Quick Tip : Double Click on a bar to delete the corresponding UPC"), width = 14, color = "yellow")),
        #div(id = "alert2", valueBox(value = h5(""), subtitle = h6("Quick Tip : Brush any area on the plot and Click to zoom-in"), width = 14, color = "yellow")),
        #hidden(div(id = "alert3", valueBox(value = h5(""), subtitle = h6("Quick Tip : Click anywhere in the plot to zoom-out"), width = 14, color = "yellow"))),
        selectInput("functions", label = "Select a Function for a Targeted View", choices = c("All Functions", df_s$Function %>% unique), selected = defaultValues$fns)),
        hidden(div(id = "slider", sliderInput("cutoff1", label = "Enter the Substitution Cutoff", min = 0, max = 1, step = 0.01, value = defaultValues$slider, width = "100%"))),
        fixed = TRUE, bottom = 100, width = "22%")
    }else{
      # if(is.na(deleteItem$item)){
      #   absolutePanel(box(collapsible = TRUE, width = "100%", title = "Info Panel", status = "primary",
      #     div(id = "alert1", valueBox(value = h5(""), subtitle = h6("Quick Tip : Double Click on a bar to delete the corresponding UPC"), width = 14, color = "yellow")),
      #     div(id = "alert2", valueBox(value = h5(""), subtitle = h6("Quick Tip : Brush any area on the plot and Click to zoom-in"), width = 14, color = "yellow")),
      #     hidden(div(id = "alert3", valueBox(value = h5(""), subtitle = h6("Quick Tip : Click anywhere in the plot to zoom-out"), width = 14, color = "yellow"))),
      #     selectInput("functions", label = "Select a Function for a Targeted View", choices = c("All Functions", df_s$Function %>% unique), selected = input$functions)),
      #     fixed = TRUE, bottom = 100, width = "22%")
      # }else{
        absolutePanel(box(collapsible = TRUE, width = "100%", title = "Info Panel", status = "primary",
          div(id = "alert1", valueBox(value = h5(""), subtitle = h6("Quick Tip : Double Click on a bar to delete the corresponding UPC"), width = 14, color = "yellow")),
          #div(id = "alert2", valueBox(value = h5(""), subtitle = h6("Quick Tip : Brush any area on the plot and Click to zoom-in"), width = 14, color = "yellow")),
          #hidden(div(id = "alert3", valueBox(value = h5(""), subtitle = h6("Quick Tip : Click anywhere in the plot to zoom-out"), width = 14, color = "yellow"))),
          selectInput("functions", label = "Select a Function for a Targeted View", choices = c("All Functions", df_s$Function %>% unique), selected = defaultValues$fns),
          sliderInput("cutoff1", label = "Enter the Substitution Cutoff", min = 0, max = 1, step = 0.01, value = defaultValues$slider, width = "100%"),
          valueBoxOutput(outputId = "deleteDetails", width = "100%"),
          fluidRow(
            column(width = 6, valueBoxOutput(outputId = "salesTransfer", width = "100%")),
            column(width = 6, valueBoxOutput(outputId = "walkoff", width = "100%"))
          )),
          fixed = TRUE, bottom = 20, width = "22%")
      # }
    }
  })
  
  observeEvent(input$cutoff1, {defaultValues$slider <- input$cutoff1})
  observeEvent(input$functions,{
    defaultValues$fns <- input$functions
    if(deleteItem$n > 0){
      deleteItem$n <- 0
      deleteItem$item <- NULL
    }
  })
  
  
  getPlot <- reactive({
    R2 <- R_sqr_train[rank(-R_sqr_train) %>% order]
    index <- which(R_sqr_train == R2[as.numeric(input$simulationNbr)])
    plot <- UPCwiseSalesFLow(deletedUPC = fetch_upc(deleteItem$item, input$functions), Function = input$functions, 
                             dem_par = final_demand_par[[index]], subs_par = final_subs_par[[index]], cutoff = as.numeric(input$cutoff1))
    return(plot)
  })
  
  # getTable <- reactive({
  #   show(id = "n")
  #   print(input$cutoff1)
  #   print(input$cutoff1 %>% is.numeric())
  #   R2 <- R_sqr_train[rank(-R_sqr_train) %>% order]
  #   index <- which(R_sqr_train == R2[as.numeric(input$simulationNbr)])
  #   #n <- input$n
  #   #if(is.numeric(input$cutoff1)){
  #   table <- delete(Function = input$functions, cutoff = input$cutoff1, dem_par = final_demand_par[[index]], subs_par = final_subs_par[[index]])
  #   return(table)
  # })
  # 
  # table <- reactiveValues(t1 = NULL, t2 = NULL)
  # observe({
  #   if(input$functions == "All Functions" || is.null(input$functions)){
  #     hide(id = "n")
  #   }else{
  #     observeEvent(input$functions,{
  #       show(id = "n")
  #       # output$deleteReco <- renderTable({
  #         # progress <- shiny::Progress$new(session, min=0, max=1)
  #         # on.exit(progress$close())
  #         # 
  #         # progress$set(message = 'Obtaining Delete Recommendations',
  #         #              detail = 'This process may take a while...')
  #         withProgress(value = 0.1, message = 'Obtaining Delete Recommendations', detail = 'Getting pre-delete sale information', {
  #           Sys.sleep(0.2)
  #           incProgress(amount = 0.15, detail = 'Geting items with low dollar sale for possible delete')
  #           Sys.sleep(0.5)
  #           incProgress(amount = 0.45, detail = 'Getting walkoff and dollar-loss information for the selected items')
  #           n <- input$n
  #           table$t1 = getTable()[[1]]
  #           table$t2 = getTable()[[2]]
  #           incProgress(amount = 1)
  #         })
  #     })
  #   }
  # })
  # 
  
  output$salesTransfer1 <- renderPlot({
    # source('demandSharePlot.R')
    # R2 <- R_sqr_train[rank(-R_sqr_train) %>% order]
    # index <- which(R_sqr_train == R2[as.numeric(input$simulationNbr)])
    if(deleteItem$n == 0){
      if(input$functions != "All Functions"){
        return(getPlot() + ggtitle("Showing pre-delete sale scenario of in-store UPCs") #+ geom_hline(yintercept = table$t2 %>% as.numeric()) 
               + coord_flip())
      }else{
        return(getPlot() + ggtitle("Showing pre-delete sale scenario of in-store UPCs") + coord_flip())
      }
    }else{
      if(is.null(deleteItem$item)){
        if(input$functions != "All Functions"){
          return(getPlot() + ggtitle("Showing pre-delete sale scenario of in-store UPCs") #+ geom_hline(yintercept = table$t2 %>% as.numeric())
                 + coord_flip())
        }else{
          return(getPlot() + ggtitle("Showing pre-delete sale scenario of in-store UPCs") + coord_flip())
        }
      }else{
        if(input$functions != "All Functions"){
          return(getPlot()[[1]] + ggtitle("Showing post-delete sale scenario of in-store UPCs") #+ geom_hline(yintercept = table$t2 %>% as.numeric()) 
                 + coord_flip())
        }else{
          return(getPlot()[[1]] + ggtitle("Showing post-delete sale scenario of in-store UPCs") + coord_flip())
        }
      }
    }
    # print(deleteItem$item)
    # plotSales(deletedGUPC = input$GUPCs, dem_par = final_demand_par[[index]], subs_par = final_subs_par[[index]], cutoff = as.numeric(input$cutoff1))
  })
  
  output$deleteDetails <- renderValueBox({
    if(!is.null(deleteItem$item)){
      defn <- getPlot()[[2]]
    }else{
      defn <- NULL
    }
    delete <- paste("Deleted Item ::", fetch_upc(deleteItem$item, input$functions))
    details <- paste("DollarSales of Deleted UPC ::", paste0("$", defn[1]))#"--- Sales Transferred ::", paste0(defn[2],"%"),"--- Walkoff Rate ::", paste0(defn[3],"%"))
    valueBox(value = h5(delete), subtitle = h5(strong(details)), width = "100%")
  })
  
  output$salesTransfer <- renderValueBox({
    if(!is.null(deleteItem$item)){
      defn <- getPlot()[[2]]
    }else{
      defn <- NULL
    }
    valueBox(value = paste0(defn[2],"%"), subtitle = "Sales Transferred", color = "green", icon = icon("exchange"))
  })
  
  output$walkoff <- renderValueBox({
    if(!is.null(deleteItem$item)){
      defn <- getPlot()[[2]]
    }else{
      defn <- NULL
    }
    if(defn[3] > 10)
      return(valueBox(value = paste0(defn[3],"%"), subtitle = "Walkoff Rate", color = "red", icon = icon("close"), width = "150%"))
    if(defn[3] <= 10)
      return(valueBox(value = paste0(defn[3],"%"), subtitle = "Walkoff Rate", color = "green", icon = icon("check"), width = "150%"))
  })
  
  
  getTable <- reactive({
    show(id = "n")
    print(input$cutoff1)
    print(input$cutoff1 %>% is.numeric())
    R2 <- R_sqr_train[rank(-R_sqr_train) %>% order]
    index <- which(R_sqr_train == R2[as.numeric(input$simulationNbr)])
    #n <- input$n
    #if(is.numeric(input$cutoff1)){
    table <- delete(Function = input$functions, cutoff = input$cutoff1, dem_par = final_demand_par[[index]], subs_par = final_subs_par[[index]])
    return(table)
  })
  
  observe({
    if(input$functions == "All Functions" || is.null(input$functions)){
      hide(id = "n")
    }else{
      observeEvent(input$functions,{
        show(id = "n")
        # print(input$cutoff1)
        # print(input$cutoff1 %>% is.numeric())
        # R2 <- R_sqr_train[rank(-R_sqr_train) %>% order]
        # index <- which(R_sqr_train == R2[as.numeric(input$simulationNbr)])
        # if(is.numeric(input$cutoff1)){
        #   table <- delete(Function = input$functions, cutoff = input$cutoff1, dem_par = final_demand_par[[index]], subs_par = final_subs_par[[index]])
        # }else{
        #   table <- delete(Function = input$functions, cutoff = input$cutoff1, dem_par = final_demand_par[[index]], subs_par = final_subs_par[[index]])
        # }
        output$deleteReco <- renderTable({
          # progress <- shiny::Progress$new(session, min=0, max=1)
          # on.exit(progress$close())
          #
          # progress$set(message = 'Obtaining Delete Recommendations',
          #              detail = 'This process may take a while...')
          withProgress(value = 0.1, message = 'Obtaining Delete Recommendations', detail = 'Getting pre-delete sale information', {
            Sys.sleep(0.2)
            incProgress(amount = 0.15, detail = 'Geting items with low dollar sale for possible delete')
            Sys.sleep(0.5)
            incProgress(amount = 0.4, detail = 'Getting walkoff and dollar-loss information for the selected items')
            n <- input$n
            table <- reactiveValues(t1 = getTable()[[1]])
            incProgress(amount = 1)
          })
          n <- input$n
          if(as.numeric(n) < nrow(table$t1)){
            return(table$t1[1:as.numeric(n),])
          }else{
            return(table$t1)
          }
        }, rownames = FALSE, width = "100%")
      })
    }
  })
  
  # # observe({
  # #   if(input$functions == "All Functions" || is.null(input$functions)){
  # #     hide(id = "n")
  # #   }else{
  #     observeEvent(input$functions,{
  #       show(id = "n")
  # #       # print(input$cutoff1)
  #       # print(input$cutoff1 %>% is.numeric())
  #       # R2 <- R_sqr_train[rank(-R_sqr_train) %>% order]
  #       # index <- which(R_sqr_train == R2[as.numeric(input$simulationNbr)])
  #       # if(is.numeric(input$cutoff1)){
  #       #   table <- delete(Function = input$functions, cutoff = input$cutoff1, dem_par = final_demand_par[[index]], subs_par = final_subs_par[[index]])
  #       # }else{
  #       #   table <- delete(Function = input$functions, cutoff = input$cutoff1, dem_par = final_demand_par[[index]], subs_par = final_subs_par[[index]])
  #       # }
  #       output$deleteReco <- renderTable({
  #         # progress <- shiny::Progress$new(session, min=0, max=1)
  #         # on.exit(progress$close())
  # 
  #         # progress$set(message = 'Obtaining Delete Recommendations',
  #         #              detail = 'This process may take a while...')
  #         # withProgress(value = 0.1, message = 'Obtaining Delete Recommendations', detail = 'Getting pre-delete sale information', {
  #         #   Sys.sleep(0.2)
  #         #   incProgress(amount = 0.15, detail = 'Geting items with low dollar sale for possible delete')
  #         #   Sys.sleep(0.5)
  #         #   incProgress(amount = 0.4, detail = 'Getting walkoff and dollar-loss information for the selected items')
  #         #   n <- input$n
  #         #   table <- reactiveValues(t1 = getTable()[[1]])
  #         #   incProgress(amount = 1)
  #         # })
  #         n <- input$n
  #         if(as.numeric(n) < nrow(table$t1)){
  #           return(table$t1[1:as.numeric(n),])
  #         }else{
  #           return(table$t1)
  #         }
  #       }, rownames = FALSE, width = "100%")
  #     })
  # #   }
  # # })
  
})