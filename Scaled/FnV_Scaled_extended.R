#!/usr/bin/Rscript
splitLine<-function(line) {
  feature.vector <- unlist(strsplit(line,'\t'))
  return (feature.vector)
}

connection <- file("stdin", open = "r")

flag <- TRUE

while( length(line <- readLines(connection, n = 1, warn = FALSE)) > 0){

  split.data <- splitLine(line)
  upc_nbr <- split.data[1]
  store <- split.data[2]
  total_unit_qty <- split.data[3]
  total_retail_price <- split.data[4]
  store_ind <- split.data[5]
  ID <- split.data[6]

  if(flag){
    prev.ID <<- ID
    v.upc_nbr <- vector()
    v.store <- vector()
    v.total_unit_qty <- vector()
    v.total_retail_price <- vector()
    v.store_ind <- vector()
    flag <- FALSE
  }

  if(prev.ID == ID){

    v.upc_nbr <- c(v.upc_nbr, upc_nbr)
    v.store <- c(v.store, store)
    v.total_unit_qty <- c(v.total_unit_qty, total_unit_qty)
    v.total_retail_price <- c(v.total_retail_price, total_retail_price)
    v.store_ind <- c(v.store_ind, store_ind)

  }else{
    library(dplyr)
    lightbulbs_train <- data.frame(
      upc_nbr = v.upc_nbr,
      store_nbr = v.store,
      total_unit_qty = v.total_unit_qty,
      total_retail_price = v.total_retail_price,
      store_ind = v.store_ind
    )
    
    lightbulbs_train$upc_nbr <- as.numeric(lightbulbs_train$upc_nbr)
    lightbulbs_train$store_nbr <- as.numeric(lightbulbs_train$store_nbr)
    lightbulbs_train$total_unit_qty <- as.numeric(lightbulbs_train$total_unit_qty)
    lightbulbs_train$total_retail_price <- as.numeric(lightbulbs_train$total_retail_price)
    lightbulbs_train$store_ind <- as.numeric(lightbulbs_train$store_ind)
    
    
    lightbulbs_train <- data.frame(
      upc_nbr = as.numeric(v.upc_nbr),
      store_nbr = as.numeric(v.store),
      total_unit_qty = as.numeric(v.total_unit_qty),
      total_retail_price = as.numeric(v.total_retail_price),
      store_ind = as.numeric(v.store_ind)
    )
    
    ######################################################################################
    ######################################################################################
    
    source('setup_packages.R')
    
    lightbulbs_attribute <- data.frame(fread("Lightbulbs ATTRIBUTES 082916_final.csv", integer64 = "numeric"))
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
    
    store_nbr <- as.numeric(prev.ID)
    
    store_data_3135 <- lightbulbs_train[which(lightbulbs_train$store_ind == 1),]
    store_data_3135 <- inner_join(store_data_3135, lightbulbs_primary_attribute, by = c("upc_nbr" = "Rollup.ID"))
    
    store_data_3135_compliment <- lightbulbs_train[which(lightbulbs_train$store_ind == 0),]
    store_data_3135_compliment <- inner_join(store_data_3135_compliment, lightbulbs_primary_attribute, by = c("upc_nbr" = "Rollup.ID"))
    national_train <- inner_join(lightbulbs_train, lightbulbs_primary_attribute, by = c("upc_nbr" = "Rollup.ID"))
    
    store_data_3135$Fineline <- tolower(store_data_3135$Fineline)
    store_data_3135_compliment$Fineline <- tolower(store_data_3135_compliment$Fineline)
    national_train$Fineline <- tolower(national_train$Fineline)
    
    print("data shapeup done")
    
    ######################################################################################
    ######################################################################################
    
    #source('data_preprocessing.R')
    
    options(scipen = 999)
    
    # set of attributes taken in consideration
    attribute <- c(primary_attributes[1:4], "PriceBucket")[-3]
    noSubsAttribute <- "Function"
    
    # Transforming data from UPC to GUPC and calculating the sales of those
    gupc_wise_sales <- function(df)
    {
      return(
        df %>%
          subset(., select = colnames(df)[grepl(paste(c(attribute, "total_retail_price", "total_unit_qty"), collapse = "|"), colnames(df))]) %>%
          dplyr::group_by_(., .dots = lapply(colnames(df)[grepl(paste(attribute, collapse = "|"), colnames(df))], as.symbol)) %>% 
          dplyr::summarise(sum_dollar_sales = sum(total_retail_price), sum_unit_sales = sum(total_unit_qty)) %>% 
          dplyr::mutate(., dollar_sales_share = sum_dollar_sales / sum(df$total_retail_price)) %>% 
          dplyr::mutate(., unit_sales_share = sum_unit_sales / sum(df$total_unit_qty)) %>%
          as.data.frame(.)
      )
    }
    
    ## 1.Training Set
    train <- store_data_3135
    
    ## naming the attribute columns properly
    for(i in 1 : length(attribute))
    {
      colnames(train)[str_detect(colnames(train), attribute[i])] <- attribute[i]
    }
    
    ## Training set GUPC level rollups
    train_group_by_all <- gupc_wise_sales(train)
    
    ## complement set of store 100
    comp_train <- store_data_3135_compliment
    ## naming the attribute columns properly
    for(i in 1 : length(attribute))
    {
      colnames(comp_train)[str_detect(colnames(comp_train), attribute[i])] <- attribute[i]
    }
    
    ## renaming columns to maintain similarity with train data for store 100
    colnames(comp_train)[colnames(comp_train) %in% "total_dollar_sales"] <- "total_retail_price"
    colnames(comp_train)[colnames(comp_train) %in% "total_unit_sales"] <- "total_unit_qty"
    
    ## GUPC level rollup
    comp_train_group_by_all <- gupc_wise_sales(comp_train)
    
    # Remove the GUPC that is common in train data and comp_train data, from comp_train data
    concat_levels <- function(df)
    {
      return(apply(df[, attribute], 1, function(x) paste(trimws(x), collapse = ",")))
    }
    GUPC_to_be_removed_from_comp_train <- train_group_by_all %>% concat_levels()
    comp_train_group_by_all$concat_attr <- comp_train_group_by_all %>% concat_levels()
    comp_train_group_by_all <- comp_train_group_by_all[! comp_train_group_by_all$concat_attr %in% GUPC_to_be_removed_from_comp_train, ]
    comp_train_group_by_all$concat_attr <- NULL
    rownames(comp_train_group_by_all) <- NULL
    
    share_var <- function(varname, share)
    {
      train_summ <- national_train %>% 
        subset(., select = c(varname, "total_retail_price", "total_unit_qty")) %>%
        dplyr::group_by_(varname) %>%
        dplyr::summarise(sum_dollar_sales = sum(total_retail_price), sum_unit_sales = sum(total_unit_qty)) %>%
        dplyr::mutate(dollar_sales_share = sum_dollar_sales / sum(sum_dollar_sales),
                      unit_sales_share = sum_unit_sales / sum(sum_unit_sales)) %>%
        as.data.frame(.)
      train_temp <- subset(train_summ, select = colnames(train_summ)[grepl(paste(c(varname, paste0(share, "_sales_share")), collapse = "|"), colnames(train_summ))])
      colnames(train_temp) <- c("attribute_level", "unit_share")
      return(train_temp)
    }
    
    #Apply function to get data required
    temp <- vector("list", length(attribute))
    for(i in 1 : length(attribute))
    {
      temp[[i]] <- share_var(attribute[i], share = "unit")
    }
    
    attribute_level_sales_share <- do.call("rbind", temp)
    
    # store's attribute level wise total unit sales data
    assign(paste0("data_store_", store_nbr) , train_group_by_all %>% 
             subset(., select = c(attribute, "sum_unit_sales")) %>%
             as.data.frame(.))
    
    
    # To fetch all possible levels of all attributes in order for the  store
    df_s <- subset(get(paste0("data_store_", store_nbr)), 
                   select = colnames(get(paste0("data_store_", store_nbr)))[grepl(paste(attribute, collapse = "|"), colnames(get(paste0("data_store_", store_nbr))))])
    df_c <- subset(comp_train_group_by_all, select = colnames(comp_train_group_by_all)[grepl(paste(attribute, collapse = "|"), colnames(comp_train_group_by_all))])
    
    all_attrib_univ <- rbind(df_s, df_c) %>% group_by_(., .dots = lapply(attribute, as.symbol)) %>%  as.matrix(.) %>% as.vector(.) %>% unique(.) %>% as.data.frame(.)
    all_attrib_univ$level_order <- 1:nrow(all_attrib_univ) 
    colnames(all_attrib_univ)[1] <- "attribute_level"
    
    # function to find out unique number of levels of an attribute from two different data
    unique_no_level <- function(data1, data2, attrib)
    {
      d_1 <- select_(data1, attrib)
      d_2 <- select_(data2, attrib)
      return(rbind(d_1, d_2) %>% unique(.) %>% nrow(.))
    }
    
    # number of levels of different attributes
    p <- c()
    for(i in 1 : length(attribute))
    {
      p[i] <- paste0("data_store_", store_nbr) %>% get(.) %>% unique_no_level(., comp_train_group_by_all, attribute[i])
    }
    
    # variables to distinguish attribute levels coming from different
    pcum1  <-  cumsum(c(1,  p[-length(p)]))
    pcum2  <-  cumsum(p)
    
    ## to find the initial values of the parameters to be supplied in the MLE est algo
    initial_values <- merge(all_attrib_univ, attribute_level_sales_share, by = "attribute_level", all.x = TRUE)
    initial_values <- initial_values[order(initial_values$level_order), ]
    
    # Reading and chaninging "others" level of Transitional Probabilities
    trans_prob_list <- vector("list", length(attribute))
    
    for(i in 1 : length(attribute))
    {
      trans_prob_list[[i]] <- expand.grid(initial_values$attribute_level[pcum1[i] : pcum2[i]],
                                          initial_values$attribute_level[pcum1[i] : pcum2[i]])
      colnames(trans_prob_list[[i]]) <- c("item_from", "item_to")
      trans_prob_list[[i]]$corrected_T_prob <- runif(nrow(trans_prob_list[[i]]), 0, 1)
      temp_lev_ind <- which(trans_prob_list[[i]]$item_to == trans_prob_list[[i]]$item_from)
      trans_prob_list[[i]]$corrected_T_prob[temp_lev_ind] <- 1
    }
    
    if(!is.null(noSubsAttribute)){
      if(is.na(match(noSubsAttribute, attribute)) %>% Reduce("+",.) > 0){
        stop("Attribute without no substitution is not considered. Change value of noSubsAttribute.")
      }else{
        matchCases <- match(noSubsAttribute, attribute)
        for(i in matchCases){
          temp_lev_ind <- which(trans_prob_list[[i]]$item_to == trans_prob_list[[i]]$item_from)
          trans_prob_list[[i]]$corrected_T_prob[-temp_lev_ind] <- 0
        }
      }
    }
    
    # For the transition probabilities for the particular data set
    trans_prob_req <- function(data_store, comp_train)
    {
      trans_prob <- vector("list", length(attribute))
      for(i in 1 : length(attribute))
      {
        trans_prob[[i]] <- trans_prob_list[[i]][trans_prob_list[[i]]$item_from %in% comp_train[, attribute[i]]
                                                & trans_prob_list[[i]]$item_to %in% data_store[, attribute[i]],  c(1 : 3)]
        row.names(trans_prob[[i]]) <- NULL
      }
      return(trans_prob)
    }
    
    attrib_set_subs_pre <- trans_prob_req(paste0("data_store_", store_nbr) %>% get(), comp_train_group_by_all) %>% do.call(rbind, .)
    
    # no of different level of substitution parameters
    
    # psub <- c()
    # psub <- trans_prob_req(paste0("data_store_", store_nbr) %>% get(), comp_train_group_by_all) %>% sapply(., nrow)
    # 
    # psub_cum1 <- cumsum(c(1,  psub[-length(psub)]))
    # psub_cum2 <- cumsum(psub)
    
    # Data Driven Upto this point! 
    ######################################################################################
    ######################################################################################
    
    attrib_set_subs_pre$store_nbr <- store_nbr
    write.table(attrib_set_subs_pre, quote=FALSE, sep = "\t", row.names=FALSE, col.names=FALSE)
    
    prev.ID <- ID
    v.upc_nbr <- upc_nbr
    v.store <- store
    v.total_unit_qty <- total_unit_qty
    v.total_retail_price <- total_retail_price
    v.store_ind <- store_ind
    }
  
}



lightbulbs_train <- data.frame(
  upc_nbr = v.upc_nbr,
  store_nbr = v.store,
  total_unit_qty = v.total_unit_qty,
  total_retail_price = v.total_retail_price,
  store_ind = v.store_ind
)

lightbulbs_train$upc_nbr <- as.numeric(lightbulbs_train$upc_nbr)
lightbulbs_train$store_nbr <- as.numeric(lightbulbs_train$store_nbr)
lightbulbs_train$total_unit_qty <- as.numeric(lightbulbs_train$total_unit_qty)
lightbulbs_train$total_retail_price <- as.numeric(lightbulbs_train$total_retail_price)
lightbulbs_train$store_ind <- as.numeric(lightbulbs_train$store_ind)


lightbulbs_train <- data.frame(
      upc_nbr = as.numeric(v.upc_nbr),
      store_nbr = as.numeric(v.store),
      total_unit_qty = as.numeric(v.total_unit_qty),
      total_retail_price = as.numeric(v.total_retail_price),
      store_ind = as.numeric(v.store_ind)
    )

######################################################################################
######################################################################################

source('setup_packages.R')

lightbulbs_attribute <- data.frame(fread("Lightbulbs ATTRIBUTES 082916_final.csv", integer64 = "numeric"))
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

store_nbr <- as.numeric(prev.ID)

store_data_3135 <- lightbulbs_train[which(lightbulbs_train$store_ind == 1),]
store_data_3135 <- inner_join(store_data_3135, lightbulbs_primary_attribute, by = c("upc_nbr" = "Rollup.ID"))

store_data_3135_compliment <- lightbulbs_train[which(lightbulbs_train$store_ind == 0),]
store_data_3135_compliment <- inner_join(store_data_3135_compliment, lightbulbs_primary_attribute, by = c("upc_nbr" = "Rollup.ID"))
national_train <- inner_join(lightbulbs_train, lightbulbs_primary_attribute, by = c("upc_nbr" = "Rollup.ID"))

store_data_3135$Fineline <- tolower(store_data_3135$Fineline)
store_data_3135_compliment$Fineline <- tolower(store_data_3135_compliment$Fineline)
national_train$Fineline <- tolower(national_train$Fineline)

print("data shapeup done")

######################################################################################
######################################################################################

#source('data_preprocessing.R')

options(scipen = 999)

# set of attributes taken in consideration
attribute <- c(primary_attributes[1:4], "PriceBucket")[-3]
noSubsAttribute <- "Function"

# Transforming data from UPC to GUPC and calculating the sales of those
gupc_wise_sales <- function(df)
{
  return(
    df %>%
      subset(., select = colnames(df)[grepl(paste(c(attribute, "total_retail_price", "total_unit_qty"), collapse = "|"), colnames(df))]) %>%
      dplyr::group_by_(., .dots = lapply(colnames(df)[grepl(paste(attribute, collapse = "|"), colnames(df))], as.symbol)) %>% 
      dplyr::summarise(sum_dollar_sales = sum(total_retail_price), sum_unit_sales = sum(total_unit_qty)) %>% 
      dplyr::mutate(., dollar_sales_share = sum_dollar_sales / sum(df$total_retail_price)) %>% 
      dplyr::mutate(., unit_sales_share = sum_unit_sales / sum(df$total_unit_qty)) %>%
      as.data.frame(.)
  )
}

## 1.Training Set
train <- store_data_3135

## naming the attribute columns properly
for(i in 1 : length(attribute))
{
  colnames(train)[str_detect(colnames(train), attribute[i])] <- attribute[i]
}

## Training set GUPC level rollups
train_group_by_all <- gupc_wise_sales(train)

## complement set of store 100
comp_train <- store_data_3135_compliment
## naming the attribute columns properly
for(i in 1 : length(attribute))
{
  colnames(comp_train)[str_detect(colnames(comp_train), attribute[i])] <- attribute[i]
}

## renaming columns to maintain similarity with train data for store 100
colnames(comp_train)[colnames(comp_train) %in% "total_dollar_sales"] <- "total_retail_price"
colnames(comp_train)[colnames(comp_train) %in% "total_unit_sales"] <- "total_unit_qty"

## GUPC level rollup
comp_train_group_by_all <- gupc_wise_sales(comp_train)

# Remove the GUPC that is common in train data and comp_train data, from comp_train data
concat_levels <- function(df)
{
  return(apply(df[, attribute], 1, function(x) paste(trimws(x), collapse = ",")))
}
GUPC_to_be_removed_from_comp_train <- train_group_by_all %>% concat_levels()
comp_train_group_by_all$concat_attr <- comp_train_group_by_all %>% concat_levels()
comp_train_group_by_all <- comp_train_group_by_all[! comp_train_group_by_all$concat_attr %in% GUPC_to_be_removed_from_comp_train, ]
comp_train_group_by_all$concat_attr <- NULL
rownames(comp_train_group_by_all) <- NULL

share_var <- function(varname, share)
{
  train_summ <- national_train %>% 
    subset(., select = c(varname, "total_retail_price", "total_unit_qty")) %>%
    dplyr::group_by_(varname) %>%
    dplyr::summarise(sum_dollar_sales = sum(total_retail_price), sum_unit_sales = sum(total_unit_qty)) %>%
    dplyr::mutate(dollar_sales_share = sum_dollar_sales / sum(sum_dollar_sales),
                  unit_sales_share = sum_unit_sales / sum(sum_unit_sales)) %>%
    as.data.frame(.)
  train_temp <- subset(train_summ, select = colnames(train_summ)[grepl(paste(c(varname, paste0(share, "_sales_share")), collapse = "|"), colnames(train_summ))])
  colnames(train_temp) <- c("attribute_level", "unit_share")
  return(train_temp)
}

#Apply function to get data required
temp <- vector("list", length(attribute))
for(i in 1 : length(attribute))
{
  temp[[i]] <- share_var(attribute[i], share = "unit")
}

attribute_level_sales_share <- do.call("rbind", temp)

# store's attribute level wise total unit sales data
assign(paste0("data_store_", store_nbr) , train_group_by_all %>% 
         subset(., select = c(attribute, "sum_unit_sales")) %>%
         as.data.frame(.))


# To fetch all possible levels of all attributes in order for the  store
df_s <- subset(get(paste0("data_store_", store_nbr)), 
               select = colnames(get(paste0("data_store_", store_nbr)))[grepl(paste(attribute, collapse = "|"), colnames(get(paste0("data_store_", store_nbr))))])
df_c <- subset(comp_train_group_by_all, select = colnames(comp_train_group_by_all)[grepl(paste(attribute, collapse = "|"), colnames(comp_train_group_by_all))])

all_attrib_univ <- rbind(df_s, df_c) %>% group_by_(., .dots = lapply(attribute, as.symbol)) %>%  as.matrix(.) %>% as.vector(.) %>% unique(.) %>% as.data.frame(.)
all_attrib_univ$level_order <- 1:nrow(all_attrib_univ) 
colnames(all_attrib_univ)[1] <- "attribute_level"

# function to find out unique number of levels of an attribute from two different data
unique_no_level <- function(data1, data2, attrib)
{
  d_1 <- select_(data1, attrib)
  d_2 <- select_(data2, attrib)
  return(rbind(d_1, d_2) %>% unique(.) %>% nrow(.))
}

# number of levels of different attributes
p <- c()
for(i in 1 : length(attribute))
{
  p[i] <- paste0("data_store_", store_nbr) %>% get(.) %>% unique_no_level(., comp_train_group_by_all, attribute[i])
}

# variables to distinguish attribute levels coming from different
pcum1  <-  cumsum(c(1,  p[-length(p)]))
pcum2  <-  cumsum(p)

## to find the initial values of the parameters to be supplied in the MLE est algo
initial_values <- merge(all_attrib_univ, attribute_level_sales_share, by = "attribute_level", all.x = TRUE)
initial_values <- initial_values[order(initial_values$level_order), ]

# Reading and chaninging "others" level of Transitional Probabilities
trans_prob_list <- vector("list", length(attribute))

for(i in 1 : length(attribute))
{
  trans_prob_list[[i]] <- expand.grid(initial_values$attribute_level[pcum1[i] : pcum2[i]],
                                      initial_values$attribute_level[pcum1[i] : pcum2[i]])
  colnames(trans_prob_list[[i]]) <- c("item_from", "item_to")
  trans_prob_list[[i]]$corrected_T_prob <- runif(nrow(trans_prob_list[[i]]), 0, 1)
  temp_lev_ind <- which(trans_prob_list[[i]]$item_to == trans_prob_list[[i]]$item_from)
  trans_prob_list[[i]]$corrected_T_prob[temp_lev_ind] <- 1
}

if(!is.null(noSubsAttribute)){
  if(is.na(match(noSubsAttribute, attribute)) %>% Reduce("+",.) > 0){
    stop("Attribute without no substitution is not considered. Change value of noSubsAttribute.")
  }else{
    matchCases <- match(noSubsAttribute, attribute)
    for(i in matchCases){
      temp_lev_ind <- which(trans_prob_list[[i]]$item_to == trans_prob_list[[i]]$item_from)
      trans_prob_list[[i]]$corrected_T_prob[-temp_lev_ind] <- 0
    }
  }
}

# For the transition probabilities for the particular data set
trans_prob_req <- function(data_store, comp_train)
{
  trans_prob <- vector("list", length(attribute))
  for(i in 1 : length(attribute))
  {
    trans_prob[[i]] <- trans_prob_list[[i]][trans_prob_list[[i]]$item_from %in% comp_train[, attribute[i]]
                                            & trans_prob_list[[i]]$item_to %in% data_store[, attribute[i]],  c(1 : 3)]
    row.names(trans_prob[[i]]) <- NULL
  }
  return(trans_prob)
}

attrib_set_subs_pre <- trans_prob_req(paste0("data_store_", store_nbr) %>% get(), comp_train_group_by_all) %>% do.call(rbind, .)

# no of different level of substitution parameters

# psub <- c()
# psub <- trans_prob_req(paste0("data_store_", store_nbr) %>% get(), comp_train_group_by_all) %>% sapply(., nrow)
# 
# psub_cum1 <- cumsum(c(1,  psub[-length(psub)]))
# psub_cum2 <- cumsum(psub)

# Data Driven Upto this point! 
######################################################################################
######################################################################################

attrib_set_subs_pre$store_nbr <- store_nbr
write.table(attrib_set_subs_pre, quote=FALSE, sep = "\t", row.names=FALSE, col.names=FALSE)

close(connection)
