source("setup_packages.R")
#store_nbr = 100

train_data_fname <- 'lightbulbs_train.csv'
test_data_fname <- 'lightbulbs_test.csv'
attributeData_fname <- 'Lightbulbs ATTRIBUTES 082916_final.csv'

lightbulbs_train <- data.frame(fread(train_data_fname, integer64 = "numeric"))
lightbulbs_test <- data.frame(fread(test_data_fname, integer64 = "numeric"))


colnames(lightbulbs_train) <- unlist(lapply(strsplit(colnames(lightbulbs_train), split = "\\."), function(x) x[2]))
colnames(lightbulbs_test) <- unlist(lapply(strsplit(colnames(lightbulbs_test), split = "\\."), function(x) x[2]))


lightbulbs_train <- lightbulbs_train[which(lightbulbs_train$total_unit_qty > 0 & lightbulbs_train$total_retail_price > 0),]
lightbulbs_test <- lightbulbs_test[which(lightbulbs_test$total_unit_qty > 0 & lightbulbs_test$total_retail_price > 0),]

lightbulbs_attribute <- data.frame(fread(attributeData_fname, integer64 = "numeric")) 
lightbulbs_attribute <- lightbulbs_attribute[complete.cases(lightbulbs_attribute),]

#primary_attributes <- c("TechnologySubcat", "Function", "Fineline", "Brand", "Price")

allAttributes <- lightbulbs_attribute %>% colnames()

if(prod(primary_attributes %in% allAttributes) != 1)
  stop('primary_attributes do not match with colnames in attributeData_fname')
if(!is.null(noSubsAttribute) %in% allAttributes)
  stop('noSubsAttribute do not match with colnames in attributeData_fname')


lightbulbs_primary_attribute <- unique(lightbulbs_attribute[,c("RollupID", primary_attributes)])
lightbulbs_primary_attribute$Price <- gsub("\\$", "", lightbulbs_primary_attribute$Price) %>% as.numeric()

lightbulbs_subcat_list <- split(lightbulbs_primary_attribute, lightbulbs_primary_attribute$TechnologySubcat)
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

store_data_3135 <- lightbulbs_train[which(lightbulbs_train$store_nbr == store_nbr),]
store_data_3135 <- inner_join(store_data_3135, lightbulbs_primary_attribute, by = c("upc_nbr" = "RollupID"))
store_data_3135_compliment <- anti_join(lightbulbs_train, store_data_3135, by = c("store_nbr"="store_nbr"))
store_data_3135_compliment <- inner_join(store_data_3135_compliment, lightbulbs_primary_attribute, by = c("upc_nbr" = "RollupID"))
national_train <- inner_join(lightbulbs_train, lightbulbs_primary_attribute, by = c("upc_nbr" = "RollupID"))

store_data_3135$Fineline <- tolower(store_data_3135$Fineline)
store_data_3135_compliment$Fineline <- tolower(store_data_3135_compliment$Fineline)
national_train$Fineline <- tolower(national_train$Fineline)


store_data_3135_test <- lightbulbs_test[which(lightbulbs_test$store_nbr == store_nbr),]
store_data_3135_test <- inner_join(store_data_3135_test, lightbulbs_primary_attribute, by = c("upc_nbr" = "RollupID"))
store_data_3135_compliment_test <- anti_join(lightbulbs_test, store_data_3135_test, by = c("store_nbr"="store_nbr"))
store_data_3135_compliment_test <- inner_join(store_data_3135_compliment_test, lightbulbs_primary_attribute, by = c("upc_nbr" = "RollupID"))
national_test <- inner_join(lightbulbs_test, lightbulbs_primary_attribute, by = c("upc_nbr" = "RollupID"))

store_data_3135_test$Fineline <- tolower(store_data_3135_test$Fineline)
store_data_3135_compliment_test$Fineline <- tolower(store_data_3135_compliment_test$Fineline)
national_test$Fineline <- tolower(national_test$Fineline)
