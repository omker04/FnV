testing_data <- store_data_3135_test  # filename of data for store 100
complement_to_testing_data <- store_data_3135_compliment_test  # file name of data for complement of store 100


# 1.testing Set
#test <- read.csv(testing_data,  as.is = T)
test <- testing_data
# naming the attribute columns properly
for(i in 1 : length(attribute))
{
  colnames(test)[str_detect(colnames(test), attribute[i])] <- attribute[i]
}

# changing the "others" levels of test data
#test <- rename_level_others(test)
# testing set GUPC level rollups
test_group_by_all <- gupc_wise_sales(test)
test_group_by_all_national <- gupc_wise_sales(national_test)
## complement set of store 100
#comp_test <- read.csv(complement_to_testing_data, as.is =T)
comp_test <- complement_to_testing_data
# naming the attribute columns properly
for(i in 1 : length(attribute))
{
  colnames(comp_test)[str_detect(colnames(comp_test), attribute[i])] <- attribute[i]
}

# renaming columns to maintain similarity with test data for store 100
colnames(comp_test)[colnames(comp_test) %in% "total_dollar_sales"] <- "total_retail_price"
colnames(comp_test)[colnames(comp_test) %in% "total_unit_sales"] <- "total_unit_qty"

# changing the "others" levels of comp_test data
# comp_test <- rename_level_others(comp_test)
# GUPC level rollup
comp_test_group_by_all <- gupc_wise_sales(comp_test)

new_levels_to_others <- function(df)
{
  for( i in 1 : length(attribute))
  {
    assign(paste0("temp_not_match_", attribute[i]), df %>% select_(., attribute[i]) %>% as.matrix(.) %>% as.vector(.) %>% unique(.))
    assign(paste0("not_match_", attribute[i]), get(paste0("temp_not_match_", attribute[i]))[! get(paste0("temp_not_match_", attribute[i])) %in% all_attrib_univ$attribute_level])
    if(length(get(paste0("not_match_", attribute[i]))) > 0 )
    {
      df[df[, attribute[i]] %in% get(paste0("not_match_", attribute[i])) , attribute[i]] <- paste0("other_",attribute[i])
    }
    rm(list= c(paste0("not_match_", attribute[i]), paste0("temp_not_match_", attribute[i]))) 
  }
  return(df)
}

test_group_by_all <- new_levels_to_others(test_group_by_all)
comp_test_group_by_all <- new_levels_to_others(comp_test_group_by_all)


GUPC_to_be_removed_from_comp_test <- test_group_by_all %>% concat_levels()
comp_test_group_by_all$concat_attr <- comp_test_group_by_all %>% concat_levels()
comp_test_group_by_all <- comp_test_group_by_all[! comp_test_group_by_all$concat_attr %in% GUPC_to_be_removed_from_comp_test, ]
comp_test_group_by_all$concat_attr <- NULL



# store's attribute level wise total unit sales data
assign(paste0("test_data_store_", store_nbr) , test_group_by_all %>% 
         subset(., select = c(attribute, "sum_unit_sales")) %>%
         as.data.frame(.))

####################################
# test_attrib_set_subs_pre <- trans_prob_req(paste0("test_data_store_", store_nbr) %>% get(), comp_test_group_by_all) %>% do.call(rbind, .)
# test_attrib_set_subs_est_pre <- merge(test_attrib_set_subs_pre, final_subs_par, by = c("item_from", "item_to"), all.x = TRUE)
# test_attrib_set_subs_est_pre$corrected_T_prob.y <- NULL
# print(test_attrib_set_subs_est_pre[is.na(test_attrib_set_subs_est_pre$final_estimate), c(1,2)])
# test_attrib_set_subs_est_pre$final_estimate[is.na(test_attrib_set_subs_est_pre$final_estimate)] <-
#   test_attrib_set_subs_est_pre$corrected_T_prob.x[is.na(test_attrib_set_subs_est_pre$final_estimate)]
# final_subs_par_test <- test_attrib_set_subs_est_pre
# final_subs_par_test$corrected_T_prob.x <- NULL

####################################
final_demand_par_test <- final_demand_par
final_subs_par_test <- final_subs_par

final_forc_test <- vector("list", number_of_simulations)
data_for_graph_test <- vector("list", number_of_simulations)
R_sqr_test <- vector("list", number_of_simulations)
for(i in 1:number_of_simulations){
  final_forc_test[[i]] <- forecast(data_store = get(paste0("test_data_store_", store_nbr)),
                            data_comp = comp_test_group_by_all,
                            dem_par = final_demand_par_test[[i]],
                            subs_par = final_subs_par_test[[i]])[[1]]
  data_for_graph_test[[i]] <- cbind(final_forc_test[[i]][,-1], actual_unit_sales_share = test_group_by_all$unit_sales_share)
  R_sqr_test[[i]] <- (cor(data_for_graph_test[[i]][,-1])^2)[1,2]
}
####################################

R_sqr_test <- R_sqr_test %>% do.call(c, .)
