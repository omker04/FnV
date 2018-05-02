delete <- function(Function, cutoff = 0.9, dem_par, subs_par){
  if(!is.numeric(cutoff))
    cutoff <- 0.9
  if(Function == "All Functions"){
    return(NULL)
  }else{
    preDeletionSales <- train %>% subset(., select = c("upc_nbr", "GUPC", "total_retail_price")) %>% as.data.frame()
    preDeletionSales$UPC <- apply(preDeletionSales[c("GUPC", "upc_nbr")], 1, function(x) paste(x, collapse = " -- "))
    preDeletionSales <- preDeletionSales[grep(Function, preDeletionSales$UPC),]
    temp <- arrange(preDeletionSales, total_retail_price)
    temp$cumsum <- cumsum(temp$total_retail_price)
    saleCutoff <- max(temp$cumsum) * 0.2
    lowSaleUPCs <- temp$UPC[temp$cumsum <= saleCutoff]
    lowSaleUPCmaxSale <- temp$total_retail_price[temp$cumsum <= saleCutoff] %>% max()
    
    print(paste("max sale", lowSaleUPCmaxSale))
    print(paste("length selected UPC", lowSaleUPCs %>% length()))
    
    getWalkoff <- function(deletedUPCdesc){
      deletedUPC <- strsplit(deletedUPCdesc, split = " -- ") %>% unlist() %>% tail(1) %>% as.numeric()
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
      
      walkoff <- (1 - (upc_GUPC_flow[,9] %>% sum()/deletedUPCsales))*100
      return(c(walkoff, deletedUPCsales))
    }
    
    
    lowSaleUPCwalkoff <- sapply(lowSaleUPCs, function(x) getWalkoff(x))
    rownames(lowSaleUPCwalkoff) <- c("walkoff.pct", "dollar.sale")
    lowSaleUPCwalkoff <- round(lowSaleUPCwalkoff %>% t() %>% data.frame(), 2)
    
    lowSaleUPCwalkoff$dollar.loss <- round(lowSaleUPCwalkoff$walkoff.pct/100 * lowSaleUPCwalkoff$dollar.sale, 2)
    lowSaleUPCwalkoff$UPC <- lowSaleUPCs
    lowSaleUPCwalkoff <- arrange(lowSaleUPCwalkoff, dollar.loss, dollar.sale)
    
    
    return(list(lowSaleUPCwalkoff[,c("UPC", "dollar.sale", "walkoff.pct", "dollar.loss")], lowSaleUPCmaxSale))
  }
}