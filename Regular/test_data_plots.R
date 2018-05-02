# source('optimization_run.R')
# source('train_data_plots.R')
source('setup_packages.R')

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
comp_test <- rename_level_others(comp_test)
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
####################################
final_forc_test <- forecast(data_store = get(paste0("test_data_store_", store_nbr)),
                            data_comp = comp_test_group_by_all,
                            dem_par = final_demand_par_test,
                            subs_par = final_subs_par_test)
####################################

data_for_graph_test <- cbind(final_forc_test[, -1], actual_unit_sales_share = test_group_by_all$unit_sales_share)
head(data_for_graph_test)
# r -square
R_sqr_test <- (cor(data_for_graph_test[, -1])^2)[1,2]
print(R_sqr_test)


source('gg_subtitle.R')
gg_test <- ggplot(data_for_graph_test) +
  geom_point(aes(x = actual_unit_sales_share, 
                 y = forcasted_unit_sales_share),
             col = "black", size = 0.5) + 
  geom_abline(aes(intercept = 0, slope = 1), 
              color ="red") +  theme_bw() +
  theme(panel.grid.minor = element_line(colour="gray95")) +
  xlab("Actual") +
  ylab("Estimated") +
  geom_smooth(span = 0.25) +
  ggtitle("Sales Share of GUPCs - Yogurt (wk-year 25-2015 to 47-2015)") +
  theme(plot.title = element_text(hjust=0, size=16))
ggplot_with_subtitle(gg_test, paste("R-Square", " = ", round(R_sqr_test, 2)), bottom_margin = 10, lineheight = 0.5)
 

data_line_test <- data_for_graph_test[,c(1,2)] 
colnames(data_for_graph_test[,c(1,3)]) <- colnames(data_for_graph_test[,c(1,2)])
colnames(data_line_test) <- colnames(data_for_graph_test[,c(1,3)])
data_line_test <- rbind(data_line_test, data_for_graph_test[,c(1,3)])
colnames(data_line_test) <- c("GUPC", "Sales.Share")
data_line_test$label <- c(rep("Estimated", nrow(data_for_graph_test)), rep("Actual", nrow(data_for_graph_test)))
data_line_test$GUPC_number <- c(seq(1 : nrow(data_for_graph_test)), seq(1 : nrow(data_for_graph_test)))

gg_line_test = ggplot(data_line_test) +
  geom_line(aes(x = GUPC_number, 
                y = Sales.Share, group = label, colour = label))  +
  scale_color_manual("Sales Share\n",labels = c("Actual", "Estimated"), values = c("dark green", "red")) +
  theme_bw() +
  theme(panel.grid.minor = element_line(colour="gray95")) +
  xlab("GUPC") +
  ylab("Sales Share") +
  geom_smooth(span = 0.25) +
  ggtitle("Sales Share of GUPCs - Yogurt (Test Data : wk-year 25-2015 to 47-2015)") +
  theme(plot.title = element_text(hjust=0, size=12))
ggplot_with_subtitle(gg_line_test, paste("R-Square", " = ", round(R_sqr_test, 2)), bottom_margin = 10, lineheight = 0.5)



# Attribute level sales share comparison
data_req_test <- get(paste0("test_data_store_", store_nbr))[, attribute]
data_req_test$forcasted_unit_sales_share <- final_forc_test$forcasted_unit_sales_share

result_attribute_level_test <- lapply(attribute,
                                 function(x) attribute_level_estimated_unit_sales_share(data_req_test, x)) %>% Reduce('rbind', .)

data_req_test_1 <- test_group_by_all_national[, c(attribute, "unit_sales_share")]
colnames(data_req_test_1)[5] <- "forcasted_unit_sales_share"
initial_values_test <-  lapply(attribute,
                               function(x) attribute_level_estimated_unit_sales_share(data_req_test_1, x)) %>% Reduce('rbind', .)
colnames(initial_values_test)[2] <- "actual_unit_sales_share"
initial_values_test <- inner_join(initial_values_test, initial_values[,1:2], by = c("attribute_level"="attribute_level"))



result_attribute_level_test <- merge(result_attribute_level_test, initial_values_test, by = "attribute_level", all.y = TRUE)
result_attribute_level_test <- result_attribute_level_test[order(result_attribute_level_test$level_order), ]
result_attribute_level_test$level_order <- NULL
colnames(result_attribute_level_test) <- c("attribute_level", "forcasted_unit_sales_share", "actual_unit_sales_share")
result_attribute_level_test

result_attribute_level_test$attribute <- c(rep(attribute, p))
gg1_test <- ggplot(result_attribute_level_test) +
  geom_point(aes(x = actual_unit_sales_share, 
                 y = forcasted_unit_sales_share, color = attribute, size = attribute, shape = attribute)
  ) + 
  scale_size_manual(values = c(1.5, 1.7, 2, 3)) + 
  scale_shape_manual(values = c(17:20)) +
  geom_abline(aes(intercept = 0, slope = 1), 
              color ="black", size = 0.4) +  
  theme_bw() +
  theme(panel.grid.minor = element_line(colour="gray95")) +
  xlab("Actual") +
  ylab("Estimated") +
  geom_smooth(span = 0.25) +
  ggtitle("Sales Share of Attributes - Yogurt (wk-year 25-2015 to 47-2015)") +
  theme(plot.title = element_text(hjust=0, size=16))
ggplot_with_subtitle(gg1_test, paste("R2", round(cor(result_attribute_level_test[, -c(1,4)], use = "pairwise.complete.obs")[1, 2], 4), sep = " = "), bottom_margin = 10, lineheight=0.5)






