source('gg_subtitle.R')

plotSalesShare <- function(deletedGUPC, dem_par, subs_par){
  #source('gg_subtitle.R')
  dataStore <- get(paste0("data_store_", store_nbr))[attribute]
  compStore <- comp_train_group_by_all[attribute]
  dataStoreGUPCs <- apply(dataStore, 1, function(x) paste(x, collapse = " || "))
  deletedGUPCindex <- which(dataStoreGUPCs %in% deletedGUPC)
  compStoreDeleted <- rbind(compStore, dataStore[deletedGUPCindex,])
  dataStoreDeleted <- dataStore[-deletedGUPCindex,]
  
  forecastedProbPreDeletion <- forecast(dem_par = dem_par, subs_par = subs_par, data_store = dataStore, data_comp = compStore)[[1]]
  forecastedProbPreDeletion$FjA <- forecastedProbPreDeletion$prob_of_buying_GUPC_given_assortment/sum(forecastedProbPreDeletion$prob_of_buying_GUPC_given_assortment)
  forecastedProbPostDeletion <- forecast(dem_par = dem_par, subs_par = subs_par, data_store = dataStoreDeleted, data_comp = compStoreDeleted)[[1]]
  forecastedProbPostDeletion$FjA <- forecastedProbPostDeletion$prob_of_buying_GUPC_given_assortment/sum(forecastedProbPostDeletion$prob_of_buying_GUPC_given_assortment)
  effectOfDeletion <- inner_join(forecastedProbPreDeletion[,-1], forecastedProbPostDeletion[,-1], by = c("GUPC" = "GUPC"))
  colnames(effectOfDeletion) <- gsub(".x", ".Pre", colnames(effectOfDeletion))
  colnames(effectOfDeletion) <- gsub(".y", ".Post", colnames(effectOfDeletion))
  effectOfDeletion$diff <- effectOfDeletion$FjA.Post - effectOfDeletion$FjA.Pre
  effectOfDeletion$sign <- effectOfDeletion$diff/abs(effectOfDeletion$diff)
  effectOfDeletion$diff <- abs(effectOfDeletion$diff)
  shareTransferred <- round((sum(effectOfDeletion$diff * effectOfDeletion$sign)/forecastedProbPreDeletion[deletedGUPCindex,]$FjA)*100, 3)
  
  dataShare <- data.frame("GUPC" = effectOfDeletion$GUPC, "SalesShare" = min(effectOfDeletion$forcasted_unit_sales_share.Pre, effectOfDeletion$forcasted_unit_sales_share.Post), "Key" = "A")
  for (i in 1:nrow(dataShare))
    dataShare$SalesShare[i] <-min(effectOfDeletion$forcasted_unit_sales_share.Pre[i], effectOfDeletion$forcasted_unit_sales_share.Post[i]) 
  dataDiff <- data.frame("GUPC" = effectOfDeletion$GUPC, "SalesShare" = effectOfDeletion$diff, "Key" = effectOfDeletion$sign)
  for(i in 1:nrow(dataDiff))
    dataDiff$Key[i] <- ifelse(dataDiff$Key[i] == 1, "B", "C")
  
  dataPlot <<- rbind(dataShare, dataDiff)
  dataPlot$GUPC <<- as.factor(dataPlot$GUPC)
  dataPlot$Key <<- as.factor(dataPlot$Key)
  
  ggShare <- ggplot(data = dataPlot, aes(x = GUPC, y = SalesShare, fill = factor(Key, levels = c("A", "B", "C")))) + 
    geom_bar(stat = "identity", show.legend = FALSE) + coord_flip() + scale_fill_manual(values = c("black", "green3", "red"))
  return(ggplot_with_subtitle(ggShare, label = paste("Deleted GUPC ::", deletedGUPC, "--- Initial Demand Share ::", round(forecastedProbPreDeletion[deletedGUPCindex,]$FjA,4), "--- Demand Share Transferred ::", paste0(shareTransferred,"%")), bottom_margin = 10, lineheight=0.5, fontsize = 12))
  #return(ggShareSubtitle)
}

# plotSales <- function(deletedGUPC, dem_par, subs_par, cutoff){
#   #source('gg_subtitle.R')
#   dataStore <- get(paste0("data_store_", store_nbr))[attribute]
#   compStore <- comp_train_group_by_all[attribute]
#   dataStoreGUPCs <- apply(dataStore, 1, function(x) paste(x, collapse = " || "))
#   deletedGUPCindex <- which(dataStoreGUPCs %in% deletedGUPC)
#   compStoreDeleted <- rbind(compStore, dataStore[deletedGUPCindex,])
#   dataStoreDeleted <- dataStore[-deletedGUPCindex,]
#   
#   forecastedPiPostDeletion <- forecast(dem_par = dem_par, subs_par = subs_par, data_store = dataStoreDeleted, data_comp = compStoreDeleted)[[5]]
#   Pi4DeletedGUPC <- forecastedPiPostDeletion[nrow(forecastedPiPostDeletion),]
#   Pi4DeletedGUPC <- Pi4DeletedGUPC/sum(Pi4DeletedGUPC)
#   cutoffPi <- Pi4DeletedGUPC[Pi4DeletedGUPC > 0] %>% quantile(.,cutoff)
#   Pi4DeletedGUPC[Pi4DeletedGUPC < cutoffPi] <- 0
#   
#   preDeletionSales <- train_group_by_all %>% subset(., select = c(attribute, "sum_dollar_sales")) %>% as.data.frame(.)
#   preDeletionSales$GUPC <- apply(preDeletionSales[attribute], 1, function(x) paste(x, collapse = " || "))
#   preDeletionSales$Key <- "A"
#   deletedGUPCsales <- preDeletionSales[deletedGUPCindex, "sum_dollar_sales"] %>% as.numeric()
#   postDeletionSales <- preDeletionSales[-deletedGUPCindex,]
#   postDeletionSales$sum_dollar_sales <- Pi4DeletedGUPC * deletedGUPCsales
#   postDeletionSales$Key <- "B"
#   dataForGraph <<- rbind(preDeletionSales[-deletedGUPCindex, ], postDeletionSales)
#   
#   ggSales <- ggplot(data = dataForGraph, aes(x = GUPC, y = sum_dollar_sales, fill = factor(Key, levels = c("A", "B")))) +
#     geom_bar(stat = "identity", show.legend = FALSE) + coord_flip() + scale_fill_manual(values = c("black", "green3"))
#   return(ggplot_with_subtitle(ggSales, label = paste("Deleted GUPC ::", deletedGUPC, "--- DollarSales of Deleted GUPC ::", paste0("$", round(deletedGUPCsales, 2)), "--- DollarSales Transferred ::", paste0(round(sum(Pi4DeletedGUPC)*100, 2),"%")), bottom_margin = 10, lineheight=0.5, fontsize = 9.5))
# }


fetch_upc <- function(pos_input, Function)
{
  if(is.null(pos_input) || is.na(pos_input)){
    return(NULL)
  }else{
    df <- train
    df$UPC <- apply(df[c("GUPC", "upc_nbr")], 1, function(x) paste(x, collapse = " -- "))
    if(Function == "All Functions"){
      df$UPC <- as.factor(df$UPC)
      upc_vec <- levels(df$UPC)
      max_del <- unique(df$UPC) %>% length()
      
      pos_top <- pos_input$domain$top 
      pos_bottom <- pos_input$domain$bottom
      pos_interval <- (pos_top - pos_bottom) / (max_del)
      input_val <- pos_input$y
      idx <- sapply(1 : (max_del), function(x) ifelse(input_val >  (pos_interval * (x - 1) + pos_bottom) & input_val <=  (pos_interval * (x) + pos_bottom), TRUE, FALSE))
      idn <- c(1 : (max_del))[idx]
      return(ifelse(idn > max_del || length(idn) == 0, "No Delete Scenario", upc_vec[idn]))
    }else{
      df <- df[grep(Function, df$UPC),]
      df$UPC <- as.factor(df$UPC)
      upc_vec <- levels(df$UPC)
      max_del <- unique(df$UPC) %>% length()
      
      pos_top <- pos_input$domain$top 
      pos_bottom <- pos_input$domain$bottom
      pos_interval <- (pos_top - pos_bottom) / (max_del)
      input_val <- pos_input$y
      idx <- sapply(1 : (max_del), function(x) ifelse(input_val >  (pos_interval * (x - 1) + pos_bottom) & input_val <=  (pos_interval * (x) + pos_bottom), TRUE, FALSE))
      idn <- c(1 : (max_del))[idx]
      return(ifelse(idn > max_del || length(idn) == 0, "No Delete Scenario", upc_vec[idn]))
    }
  }
}

