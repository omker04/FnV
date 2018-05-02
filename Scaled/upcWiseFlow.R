train$GUPC <- apply(train[attribute], 1, function(x) paste(x, collapse = " || "))
train_group_by_all$GUPC <- apply(train_group_by_all[attribute], 1, function(x) paste(x, collapse = " || "))
upc_GUPC <- inner_join(train[,c(1,3,4,ncol(train))], train_group_by_all[,c(5, 6, ncol(train_group_by_all))], by = c("GUPC"="GUPC")) %>% arrange(GUPC)
upc_GUPC_distn <- upc_GUPC %>% group_by(GUPC) %>% summarise(count=n()) %>% data.frame()
multi_upc_GUPC <- upc_GUPC_distn[upc_GUPC_distn$count > 1, ]
multi_upc_GUPC <- inner_join(data.frame(GUPC=multi_upc_GUPC[,1]), upc_GUPC, by = c("GUPC" = "GUPC"))
upc_GUPC$unit_share <- upc_GUPC$total_unit_qty/upc_GUPC$sum_unit_sales
upc_GUPC$sales_share <- upc_GUPC$total_retail_price/upc_GUPC$sum_dollar_sales

dataStore <- get(paste0("data_store_", store_nbr))[attribute]
compStore <- comp_train_group_by_all[attribute]
dataStoreGUPCs <- apply(dataStore, 1, function(x) paste(x, collapse = " || "))

UPCwiseSalesFLow <- function(deletedUPCdesc, Function, dem_par, subs_par, cutoff, rangeX=NULL, rangeY=NULL){
  
  if(is.null(deletedUPCdesc) || is.na(deletedUPCdesc)){
    preDeletionSales <- train %>% subset(., select = c("upc_nbr", "GUPC", "total_retail_price")) %>% as.data.frame()
    preDeletionSales$UPC <- apply(preDeletionSales[c("GUPC", "upc_nbr")], 1, function(x) paste(x, collapse = " -- "))
    temp <- arrange(preDeletionSales, total_retail_price)
    temp$cumsum <- cumsum(temp$total_retail_price)
    saleCutoff <- max(temp$cumsum) * 0.25
    if(Function != "All Functions"){
      preDeletionSales <- preDeletionSales[grep(Function, preDeletionSales$UPC),]
      temp <- arrange(preDeletionSales, total_retail_price)
      temp$cumsum <- cumsum(temp$total_retail_price)
      saleCutoff <- max(temp$cumsum) * 0.2
    }
    preDeletionSales$UPC <- as.factor(preDeletionSales$UPC)
    preDeletionSales$Key <- "A"
    preDeletionSales$Key <- as.factor(preDeletionSales$Key)
    plot <- ggplot(data = preDeletionSales, aes(x = UPC, y = total_retail_price, fill = factor(Key))) + geom_bar(stat = "identity", show.legend = FALSE) + coord_flip() + 
      #ggtitle("Double-Click on the Bar of the UPC that you would want to delete and study further") + 
      scale_fill_manual(values = c("steelblue4")) + scale_y_continuous(expand = c(0, 100))
    return(plot)
  }else{
    deletedUPC <- strsplit(deletedUPCdesc, split = " -- ") %>% unlist() %>% tail(1) %>% as.numeric()
    #deletedFunction <- strsplit(deletedUPCdesc, split = " || ", fixed = TRUE) %>% unlist() %>% head(2) %>% tail(1)
    # upc_GUPC <- inner_join(train[,c(1,3,4,ncol(train))], train_group_by_all[,c(5, 6, ncol(train_group_by_all))], by = c("GUPC"="GUPC")) %>% arrange(GUPC)
    # upc_GUPC_distn <- upc_GUPC %>% group_by(GUPC) %>% summarise(count=n()) %>% data.frame()
    # multi_upc_GUPC <- upc_GUPC_distn[upc_GUPC_distn$count > 1, ]
    # multi_upc_GUPC <- inner_join(data.frame(GUPC=multi_upc_GUPC[,1]), upc_GUPC, by = c("GUPC" = "GUPC"))
    # upc_GUPC$unit_share <- upc_GUPC$total_unit_qty/upc_GUPC$sum_unit_sales
    # upc_GUPC$sales_share <- upc_GUPC$total_retail_price/upc_GUPC$sum_dollar_sales
    # 
    # dataStore <- get(paste0("data_store_", store_nbr))[attribute]
    # compStore <- comp_train_group_by_all[attribute]
    # dataStoreGUPCs <- apply(dataStore, 1, function(x) paste(x, collapse = " || "))
    
    deletedGUPC <- upc_GUPC[which(upc_GUPC$upc_nbr == deletedUPC),]$GUPC
    deletedGUPCindex <- which(dataStoreGUPCs %in% deletedGUPC)
    compStoreDeleted <- rbind(compStore, dataStore[deletedGUPCindex,])
    dataStoreDeleted <- dataStore[-deletedGUPCindex,]
    
    forecastedPiPostDeletion <- forecast(dem_par = dem_par, subs_par = subs_par, data_store = dataStoreDeleted, data_comp = compStoreDeleted)[[5]]
    Pi4DeletedGUPC <- forecastedPiPostDeletion[nrow(forecastedPiPostDeletion),]
    
    if(deletedGUPC %in% multi_upc_GUPC$GUPC %>% unique()){
      n <- upc_GUPC_distn[which(upc_GUPC_distn$GUPC == deletedGUPC),]$count
      adjustedPi <- c(Pi4DeletedGUPC, rep(1, n-1))
      adjustedPi <- adjustedPi/sum(adjustedPi)
      dummy <- adjustedPi[1:(dataStoreGUPCs %>% length() - deletedGUPCindex %>% length())]
      cutoffPi <- dummy %>% quantile(.,cutoff)
      adjustedPi[adjustedPi < cutoffPi] <- 0
      
      sameGUPCtransfer <- adjustedPi %>% tail(n-1) %>% sum()
      Pi4DeletedGUPC <- adjustedPi[1:(dataStoreGUPCs %>% length() - 1)]
    }else{
      Pi4DeletedGUPC <- Pi4DeletedGUPC/sum(Pi4DeletedGUPC)
      cutoffPi <- Pi4DeletedGUPC[Pi4DeletedGUPC > 0] %>% quantile(.,cutoff)
      Pi4DeletedGUPC[Pi4DeletedGUPC < cutoffPi] <- 0
      sameGUPCtransfer <- 0
    }
    
    preDeletionSales <- train_group_by_all %>% subset(., select = c(attribute, "sum_dollar_sales")) %>% as.data.frame(.)
    preDeletionSales$GUPC <- apply(preDeletionSales[attribute], 1, function(x) paste(x, collapse = " || "))
    preDeletionSales$Key <- "A"
    deletedUPCsales <- upc_GUPC[which(upc_GUPC$upc_nbr == deletedUPC),]$total_retail_price
    postDeletionSales <- preDeletionSales[-deletedGUPCindex,]
    postDeletionSales$sum_dollar_sales <- Pi4DeletedGUPC * deletedUPCsales
    postDeletionSales$Key <- "B"
    dataForGraph <<- rbind(preDeletionSales[-deletedGUPCindex, ], postDeletionSales)
    
    flowedInto <- dataForGraph[which(dataForGraph$Key == "B" & dataForGraph$sum_dollar_sales > 0), 6:5]
    upc_GUPC_flow <- left_join(upc_GUPC, flowedInto, by = c("GUPC"="GUPC"))
    
    if(!sameGUPCtransfer == 0){
      #deletedUPCsales <- upc_GUPC[which(upc_GUPC$upc_nbr == deletedUPC),]$total_retail_price
      sameGUPC_upc <- upc_GUPC[which(upc_GUPC$GUPC == deletedGUPC), 1]
      sameGUPC_upc1 <- sameGUPC_upc[!sameGUPC_upc %in% deletedUPC]
      upc_GUPC_flow[which(upc_GUPC_flow$upc_nbr %in% sameGUPC_upc1),]$sum_dollar_sales.y <- deletedUPCsales*sameGUPCtransfer
      upc_GUPC_flow[which(upc_GUPC_flow$upc_nbr %in% sameGUPC_upc1),]$sales_share <- upc_GUPC_flow[which(upc_GUPC_flow$upc_nbr %in% sameGUPC_upc1),]$sales_share / upc_GUPC_flow[which(upc_GUPC_flow$upc_nbr %in% sameGUPC_upc1),]$sales_share %>% sum()
    }
    upc_GUPC_flow$sum_dollar_sales.y <- upc_GUPC_flow$sum_dollar_sales.y * upc_GUPC_flow$sales_share
    upc_GUPC_flow <- upc_GUPC_flow %>% as.data.frame() %>% replace_na(., list(sum_dollar_sales.y = 0))
    colnames(upc_GUPC_flow)[c(3,9)] <- rep("dollar sales", 2)
    
    plotDF <- rbind(upc_GUPC_flow[c(1,3)] %>% mutate(., Key = "A"), upc_GUPC_flow[c(1,9)] %>% mutate(., Key = "B"))
    plotDF$`dollar sales`[which(plotDF$upc_nbr==deletedUPC)] <- 0
    dfTemp <- upc_GUPC_flow[c(1,9)] %>% mutate(., Key = "C")
    dfTemp$`dollar sales` <- 0
    dfTemp$`dollar sales`[which(dfTemp$upc_nbr==deletedUPC)] <- deletedUPCsales
    plotDF <- rbind(plotDF, dfTemp)
    plotDF <- inner_join(plotDF, train %>% subset(., select = c("upc_nbr", "GUPC")), by = c("upc_nbr"="upc_nbr"))
    plotDF$UPC <- apply(plotDF[c("GUPC", "upc_nbr")], 1, function(x) paste(x, collapse = " -- "))
    temp <- plotDF[which(plotDF$Key == "A"),]
    temp[which(temp$`dollar sales` == 0), "dollar sales"] <- deletedUPCsales
    temp <- arrange(temp, `dollar sales`)
    temp$cumsum <- cumsum(temp$`dollar sales`)
    saleCutoff <- max(temp$`dollar sales`) * 0.25
    if(Function != "All Functions"){
      plotDF <- plotDF[grep(Function, plotDF$UPC),]
      temp <- plotDF[which(plotDF$Key == "A"),]
      temp[which(temp$`dollar sales` == 0), "dollar sales"] <- deletedUPCsales
      temp <- arrange(temp, `dollar sales`)
      temp$cumsum <- cumsum(temp$`dollar sales`)
      saleCutoff <- max(temp$`dollar sales`) * 0.2
    }
    plotDF$UPC <- as.factor(plotDF$UPC)
    plotDF$Key <- as.factor(plotDF$Key)
    whichB <- which(plotDF$Key=='B' & plotDF$`dollar sales` != 0)
    plotDF$Label <- NA
    plotDF[whichB,]$Label <- paste0(round(plotDF$`dollar sales`[whichB]*100/deletedUPCsales, 2), "%")
    salesTrans <- plotDF$`dollar sales`[which(plotDF$Key == "B")] %>% sum()
    
    #plotDFselected <- plotDFselected[grep(deletedFunction, plotDF$UPC),]
    ggSales <- ggplot(data = plotDF, aes(x = UPC, y = `dollar sales`, fill = factor(Key, levels = c("A", "B", "C")), label = Label)) +
      geom_bar(stat = "identity", show.legend = FALSE) + scale_fill_manual(values = c("steelblue4", "mediumaquamarine", "indianred1")) + 
      scale_y_continuous(expand = c(0, 100))
    
    if(Function != "All Functions")
      ggSales <- ggSales + geom_label(show.legend = FALSE, hjust = 0, vjust = 0.5, na.rm = TRUE)
    
    # if(is.null(rangeX) & is.null(rangeY)){
    #   ggSales <- ggSales + ggtitle(label = "Brush and Double-Click the area you want to zoom")
    # }else{
    #   ggSales <- ggSales + ggtitle(label = "Double-Click anywhere to zoom-out")
    # }
    
    sales <- round(deletedUPCsales, 2)
    trans <- round((salesTrans/deletedUPCsales)*100, 2)
    walkoff <- round(100-(salesTrans/deletedUPCsales)*100, 2)
    # return(ggplot_with_subtitle(ggSales, label = paste("DollarSales of Deleted UPC ::", paste0("$", round(deletedUPCsales, 2)),
    #                                             "--- Sales Transferred ::", paste0(round((salesTrans/deletedUPCsales)*100, 2),"%"),
    #                                             "--- Walkoff Rate ::", paste0(round(100-(salesTrans/deletedUPCsales)*100, 2),"%")),
    #                      bottom_margin = 10, lineheight = 0.5, fontsize = 12))
    return(list(ggSales, c(sales, trans, walkoff)))
  }
}



