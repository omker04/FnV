source("setup_packages.R")
# store_nbr = 3135


lightbulbs_attribute <- as.data.frame(read.csv("Lightbulbs ATTRIBUTES 082916_final.csv", header = TRUE, stringsAsFactors = FALSE))
lightbulbs_attribute <- lightbulbs_attribute[complete.cases(lightbulbs_attribute),]
primary_attributes <- c("Technology..Subcat.", "Function", "Fineline", "Brand", "Price")

lightbulbs_primary_attribute <- unique(lightbulbs_attribute[,c("Rollup.ID", primary_attributes)])
lightbulbs_primary_attribute$Price <- gsub("\\$", "", lightbulbs_primary_attribute$Price) %>% as.numeric()

lightbulbs_subcat_list <- split(lightbulbs_primary_attribute, lightbulbs_primary_attribute$Technology..Subcat.)
lightbulbs_subcat_list <- lapply(lightbulbs_subcat_list, function(x) {
  x <- mutate(x, PriceBucket = "VeryHigh")
  for(i in 1:nrow(x)){
    x$PriceBucket[i] <- ifelse(x$Price[i] < as.numeric(quantile(x$Price, 0.75)), yes =  "High", no = x$PriceBucket[i])
    x$PriceBucket[i] <- ifelse(x$Price[i] < as.numeric(quantile(x$Price, 0.50)), yes = "Medium", no = x$PriceBucket[i])
    x$PriceBucket[i] <- ifelse(x$Price[i] < as.numeric(quantile(x$Price, 0.25)), yes = "Low", no = x$PriceBucket[i])
  }
  return(x)
})
lightbulbs_primary_attribute <- do.call(rbind, lightbulbs_subcat_list)
rownames(lightbulbs_primary_attribute) <- NULL

store_nbr <- prev.ID
# store_nbr <- unique(lightbulbs_train$ID)

store_data_3135 <- lightbulbs_train[which(lightbulbs_train$store_ind == 1),]
store_data_3135 <- inner_join(store_data_3135, lightbulbs_primary_attribute, by = c("upc_nbr" = "Rollup.ID"))

#store_data_3135_compliment <- anti_join(lightbulbs_train, store_data_3135, by = c("store_nbr"="store_nbr"))
store_data_3135_compliment <- lightbulbs_train[which(lightbulbs_train$store_ind == 0),]
store_data_3135_compliment <- inner_join(store_data_3135_compliment, lightbulbs_primary_attribute, by = c("upc_nbr" = "Rollup.ID"))
national_train <- inner_join(lightbulbs_train, lightbulbs_primary_attribute, by = c("upc_nbr" = "Rollup.ID"))

store_data_3135$Fineline <- tolower(store_data_3135$Fineline)
store_data_3135_compliment$Fineline <- tolower(store_data_3135_compliment$Fineline)
national_train$Fineline <- tolower(national_train$Fineline)


# store_data_3135_test <- lightbulbs_test[which(lightbulbs_test$store_nbr == 3135),]
# store_data_3135_test <- inner_join(store_data_3135_test, lightbulbs_primary_attribute, by = c("upc_nbr" = "Rollup.ID"))
# store_data_3135_compliment_test <- anti_join(lightbulbs_test, store_data_3135_test, by = c("store_nbr"="store_nbr"))
# store_data_3135_compliment_test <- inner_join(store_data_3135_compliment_test, lightbulbs_primary_attribute, by = c("upc_nbr" = "Rollup.ID"))
# national_test <- inner_join(lightbulbs_test, lightbulbs_primary_attribute, by = c("upc_nbr" = "Rollup.ID"))
# 
# store_data_3135_test$Fineline <- tolower(store_data_3135_test$Fineline)
# store_data_3135_compliment_test$Fineline <- tolower(store_data_3135_compliment_test$Fineline)
# national_test$Fineline <- tolower(national_test$Fineline)
