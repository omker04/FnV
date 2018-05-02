# source('setup_packages.R')
# source('data_preprocessing.R')
# 

# Table subs_probs_order_pre_attributeNAME
for(i in 1 : length(attribute))
{
  assign(paste0("subs_probs_order_pre_", attribute[i]), 
         expand.grid(paste0("data_store_", store_nbr) %>% get() %>% dplyr::select_(., attribute[i]) %>% as.matrix() %>% as.vector(), 
                     comp_train_group_by_all %>% dplyr::select_(., attribute[i]) %>% as.matrix() %>% as.vector()
         ) %>% 
           rev() %>% 
           apply(., 1 , function(x) paste(x[1], x[2], sep = " - "))
  )
}

# Table subs_probs_order_attributeNAME
for(i in 1 : length(attribute))
{
  temp_order_table <- get(paste0("subs_probs_order_pre_", attribute[i])) %>% unique() %>% as.data.frame()
  colnames(temp_order_table)[1] <- "level"
  temp_order_table$order <- seq(1 : nrow(temp_order_table))
  assign(paste0("subs_probs_order_", attribute[i]), temp_order_table)
  rm(list = "temp_order_table")
}

# Table subs_probs_order_count_attributeNAME
for(i in 1 : length(attribute))
{
  temp_count_table <- get(paste0("subs_probs_order_pre_", attribute[i])) %>% table() %>% as.data.frame()
  colnames(temp_count_table)[1] <- "level"
  assign(paste0("subs_probs_order_count_", attribute[i]), temp_count_table)
  rm(list = "temp_count_table")
}

# Table subs_probs_order_count_final_attributeNAME
for(i in 1 : length(attribute))
{
  temp_order_count_final <- merge(get(paste0("subs_probs_order_count_", attribute[i])), 
                                  get(paste0("subs_probs_order_", attribute[i])), by = "level") 
  temp_order_count_final <- temp_order_count_final[order(temp_order_count_final$order), ]
  assign(paste0("subs_probs_order_count_final_", attribute[i]), temp_order_count_final)
  rm(list = "temp_order_count_final")
}

# function to order individual attribute level
order_indiv_attrib_lvl <- function(data1, data2)
{
  temp_data = data2
  temp_data$level <- apply(temp_data, 1, function(x) paste(x[1], x[2], sep = " - "))
  temp_data_1 <- merge(temp_data, data1, by = "level")
  temp_data_1 <- temp_data_1[order(temp_data_1$order), ]
  temp_data_final <- temp_data_1[, c("item_from", "item_to", "corrected_T_prob")]
  return(temp_data_final)
}

attrib_set_subs <- lapply(seq(1 : length(attribute)), 
                         function(x)
                           order_indiv_attrib_lvl(data1 = get(paste0("subs_probs_order_count_final_", attribute[x])),
                                                  data2 = attrib_set_subs_pre[psub_cum1[x] : psub_cum2[x], ]
                           )) %>% do.call(rbind, .)
row.names(attrib_set_subs) <- NULL

attrib_set_subs_rep <- lapply(seq(1 : length(attribute)), 
                              function(x) get(paste0("subs_probs_order_count_final_", attribute[x])) %>% 
                                dplyr::select(Freq)
                              ) %>% do.call(rbind, .) %>% as.matrix() %>% as.vector()

for(i in 1 : length(attribute))
{
  assign(paste0("gupc_cs_sub_order_", attribute[i]),
         lapply(get(paste0("subs_probs_order_", attribute[i]))[, "level"], function(x) which(get(paste0("subs_probs_order_pre_", attribute[i]))==x)) %>% Reduce('c', .)
         )
}

