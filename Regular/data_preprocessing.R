# source('setup_packages.R')
# source('data_shapeup.R')
options(scipen = 999)

# set of attributes taken in consideration
attribute <- primary_attributes
if('price' %in% tolower(attribute)){
  attribute <- attribute[-which(tolower(attribute) == 'price')]
}
attribute <- c(attribute, 'PriceBucket')


#noSubsAttribute <- "Function"
# store number

# Datasets Required
# training_data <- "train_100.csv"  # filename of data for store 100
# complement_to_training_data <- "complement_train_100.csv"  # file name of data for complement of store 100
# 
# # sales_share_file <- "Attribute_level_wise_sales_share_20160425.csv" # file name of data for Unit share of attribute levels contributing to top 95% sales
# 
# # files for data of transition probabilities
# for(i in 1 : length(attribute))
# {
#   assign(paste0(attribute[i],"_trans"), paste0("loyalty_revised_cdt_full_tran_prob_alg_911_", attribute[i], "_corrected.csv"))
# }
# 
# # Renaming the levels : "others". 
# rename_level_others <- function(df)
# {
#   for(i in 1 : length(attribute))
#   {
#     df[, str_detect(colnames(df), attribute[i])] <- 
#       apply(df[,str_detect(colnames(df), attribute[i])] %>% as.data.frame,
#             2, function(x) ifelse(x == "others", paste0("other_", attribute[i]), x))
#   }
#   return(df)
# }

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
# train <- read.csv(training_data,  as.is = T)
train <- store_data_3135

## naming the attribute columns properly
for(i in 1 : length(attribute))
{
  colnames(train)[str_detect(colnames(train), attribute[i])] <- attribute[i]
}

## changing the "others" levels of train data
# train <- rename_level_others(train)

## Training set GUPC level rollups
train_group_by_all <- gupc_wise_sales(train)

## complement set of store 100
# comp_train <- read.csv(complement_to_training_data, as.is =T)
comp_train <- store_data_3135_compliment
## naming the attribute columns properly
for(i in 1 : length(attribute))
{
  colnames(comp_train)[str_detect(colnames(comp_train), attribute[i])] <- attribute[i]
}

## renaming columns to maintain similarity with train data for store 100
colnames(comp_train)[colnames(comp_train) %in% "total_dollar_sales"] <- "total_retail_price"
colnames(comp_train)[colnames(comp_train) %in% "total_unit_sales"] <- "total_unit_qty"

# # changing the "others" levels of comp_train data
# comp_train <- rename_level_others(comp_train)
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


# # Reading the sales share of attribute levels
# attribute_level_sales_share <- read.csv(sales_share_file, as.is = TRUE)
# attribute_level_sales_share <- attribute_level_sales_share[order(attribute_level_sales_share$Field, attribute_level_sales_share$attribute_level), ]
# attribute_level_sales_share <- attribute_level_sales_share[, c("attribute_level", "Sales_share")]
# colnames(attribute_level_sales_share) <- c("attribute_level", "unit_share")
 
# # function to change the "others" levels in transition probability data 
# transition <- function(data, attrib_name)
# {
#   dat <- select(data, item_from, item_to, corrected_T_prob)
#   dat[, c(1:2)] <- apply(dat[, 1:2], 2, function(x) ifelse(x == "others", paste0("other_", attrib_name), x))
#   return(dat)
# }
# 
# # Reading and chaninging "others" level of Transitional Probabilities 
# trans_prob_list <- vector("list", length(attribute))
# for( i in 1 : length(attribute))
# {
#   trans_prob_list[[i]] <- paste0(attribute[i], "_trans") %>% get(.) %>% read.csv(., as.is = TRUE) %>% transition(., attribute[i])
# }


# store's attribute level wise total unit sales data
assign(paste0("data_store_", store_nbr) , train_group_by_all %>% 
         subset(., select = c(attribute, "sum_unit_sales")) %>%
         as.data.frame(.))


# To fetch all possible levels of all attributes in order for the  store
df_s <- subset(get(paste0("data_store_", store_nbr)), 
               select = colnames(get(paste0("data_store_", store_nbr)))[grepl(paste(attribute, collapse = "|"), colnames(get(paste0("data_store_", store_nbr))))])
df_c <- subset(comp_train_group_by_all, select = colnames(comp_train_group_by_all)[grepl(paste(attribute, collapse = "|"), colnames(comp_train_group_by_all))])
all_attrib_univ <- rbind(df_s, df_c) %>% group_by_(., .dots = lapply(attribute, as.symbol)) %>%  as.matrix(.) %>% as.vector(.) %>% unique(.) %>% as.data.frame(.) %>% 
  mutate(level_order = seq(1, length(.), 1)) 
colnames(all_attrib_univ)[1] <- "attribute_level"

# to find the initial values of the parameters to be supplied in the MLE est algo
# initial_values <- merge(all_attrib_univ, attribute_level_sales_share, by = "attribute_level")
# initial_values <- initial_values[order(initial_values$level_order), ]

# # For the transition probabilities for the particular data set
# trans_prob_req <- function(data_store, comp_train)
# {
#   trans_prob <- vector("list", length(attribute))
#   for(i in 1 : length(attribute))
#   {
#     trans_prob[[i]] <- trans_prob_list[[i]][trans_prob_list[[i]]$item_from %in% comp_train[, attribute[i]]
#                                                  & trans_prob_list[[i]]$item_to %in% data_store[, attribute[i]],  c(1 : 3)]
#     row.names(trans_prob[[i]]) <- NULL
#   }
#   return(trans_prob)
# }
# 
# attrib_set_subs_pre <- trans_prob_req(paste0("data_store_", store_nbr) %>% get(), comp_train_group_by_all) %>% do.call(rbind, .)


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

## for(i in 1 : length(attribute))
## {
##   initial_values$unit_share[pcum1[i] : pcum2[i]] <- runif(p[i], 0, 1)
##   initial_values$unit_share[pcum1[i] : pcum2[i]] <- initial_values$unit_share[pcum1[i] : pcum2[i]]/sum(initial_values$unit_share[pcum1[i] : pcum2[i]])
## }


# files for data of transition probabilities
# for(i in 1 : length(attribute))
# {
#   assign(paste0(attribute[i],"_trans"), paste0("loyalty_revised_cdt_full_tran_prob_alg_911_", attribute[i], "_corrected.csv"))
# }

# function to change the "others" levels in transition probability data
# transition <- function(data, attrib_name)
# {
#   dat <- select(data, item_from, item_to, corrected_T_prob)
#   dat[, c(1:2)] <- apply(dat[, 1:2], 2, function(x) ifelse(x == "others", paste0("other_", attrib_name), x))
#   return(dat)
# }

# Reading and chaninging "others" level of Transitional Probabilities
trans_prob_list <- vector("list", length(attribute))
# for( i in 1 : length(attribute))
# {
#   trans_prob_list[[i]] <- paste0(attribute[i], "_trans") %>% get(.) %>% read.csv(., as.is = TRUE) %>% transition(., attribute[i])
# }

for(i in 1 : length(attribute))
{
  trans_prob_list[[i]] <- expand.grid(initial_values$attribute_level[pcum1[i] : pcum2[i]],
                                      initial_values$attribute_level[pcum1[i] : pcum2[i]])
  colnames(trans_prob_list[[i]]) <- c("item_from", "item_to")
  # if(i == which(attribute == "flavor"))
  # {
  #   # trans_prob_list[[i]] <- paste0("flavor", "_trans") %>% get(.) %>% read.csv(., as.is = TRUE) %>% transition(., "flavor")
  #   trans_prob_list[[i]]$corrected_T_prob <- 0
  # } else {
    # trans_prob_list[[i]] <- expand.grid(initial_values$attribute_level[pcum1[i] : pcum2[i]],
    #                                     initial_values$attribute_level[pcum1[i] : pcum2[i]])
    # colnames(trans_prob_list[[i]]) <- c("item_from", "item_to")
    trans_prob_list[[i]]$corrected_T_prob <- runif(nrow(trans_prob_list[[i]]), 0, 1)
  # }
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


# if("Fineline" %in% attribute){
#   i <- which(attribute == "Fineline")
#   temp_lev_ind <- which(trans_prob_list[[i]]$item_to == trans_prob_list[[i]]$item_from)
#   trans_prob_list[[i]]$corrected_T_prob[-temp_lev_ind] <- 0
# }
# 
# if("Technology..Subcat." %in% attribute){
#   i <- which(attribute == "Technology..Subcat.")
#   temp_lev_ind <- which(trans_prob_list[[i]]$item_to == trans_prob_list[[i]]$item_from)
#   trans_prob_list[[i]]$corrected_T_prob[-temp_lev_ind] <- 0
# }


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
psub <- c()
psub <- trans_prob_req(paste0("data_store_", store_nbr) %>% get(), comp_train_group_by_all) %>% sapply(., nrow)

psub_cum1 <- cumsum(c(1,  psub[-length(psub)]))
psub_cum2 <- cumsum(psub)

# Data Driven Upto this point! 
