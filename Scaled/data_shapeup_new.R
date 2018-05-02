store_upc_attribute_data$price <- gsub("\\$", "", store_upc_attribute_data$price) %>% as.numeric()

store_upc_attribute_data <- split(store_upc_attribute_data, store_upc_attribute_data$subcat)
store_upc_attribute_data <- lapply(store_upc_attribute_data, function(x) {
  x <- mutate(x, PriceBucket = "VeryHigh")
  for(i in 1:nrow(x)){
    x$PriceBucket[i] <- ifelse(x$price[i] < as.numeric(quantile(x$price, 0.75)), yes =  "High", no = x$PriceBucket[i])
    x$PriceBucket[i] <- ifelse(x$price[i] < as.numeric(quantile(x$price, 0.50)), yes = "Medium", no = x$PriceBucket[i])
    x$PriceBucket[i] <- ifelse(x$price[i] < as.numeric(quantile(x$price, 0.25)), yes = "Low", no = x$PriceBucket[i])
  }
  return(x)
})

store_upc_attribute_data <- do.call(rbind, store_upc_attribute_data)
rownames(store_upc_attribute_data) <- NULL

store_data_3135 <- store_upc_attribute_data[which(store_upc_attribute_data$store_ind == 1),]
#store_data_3135 <- inner_join(store_data_3135, lightbulbs_primary_attribute, by = c("upc_nbr"="Rollup.ID"))

store_data_3135_compliment <- store_upc_attribute_data[which(store_upc_attribute_data$store_ind == 0),]
#store_data_3135_compliment <- inner_join(store_data_3135_compliment, lightbulbs_primary_attribute, by = c("upc_nbr"="Rollup.ID"))

#national_train <- inner_join(store_data, lightbulbs_primary_attribute, by = c("upc_nbr"="Rollup.ID"))
national_train <- store_upc_attribute_data

store_data_3135$fineline <- tolower(store_data_3135$fineline)
store_data_3135_compliment$fineline <- tolower(store_data_3135_compliment$fineline)
national_train$fineline <- tolower(national_train$fineline)