
# log Likelihood
loglikelihood <- function(tot_par_set)
{
  data_store = get(paste0("data_store_", store_nbr))
  comp_train = comp_train_group_by_all
  param_set <- tot_par_set[seq(1, length(par_set), 1)]
  param_set_sub <- tot_par_set[seq(length(par_set) + 1, length(tot_par_set), 1)] %>%
    same_level_sub_addition(., num = 20, same_level_sub) 
  
  # if("Fineline" %in% attribute)
  #   param_set_sub <- param_set_sub %>% same_level_sub_addition(., num = -750, whichZero)
  # 
  # if("Technology..Subcat." %in% attribute)
  #   param_set_sub <- param_set_sub %>% same_level_sub_addition(., num = -750, whichZero)
  
  if(exists("whichZero"))
    param_set_sub <- param_set_sub %>% same_level_sub_addition(., num = -750, whichZero)
  
  param_set_sub <- param_set_sub %>% 
    reparam_sub() %>% 
    round(., 5)
  
  gupc_cs_sub_mat_all <- vector("list", length(attribute))
  for(i in 1 : length(attribute))
  {
    temp_data_sub <- data.frame(value = rep(param_set_sub[psub_cum1[i] : psub_cum2[i]], attrib_set_subs_rep[psub_cum1[i] : psub_cum2[i]]),
                                order = get(paste0("gupc_cs_sub_order_", attribute[i]))
    )
    assign(paste0("param_set_sub_", attribute[i]),
           temp_data_sub$value[order(temp_data_sub$order)]
    )
    rm(ls = "temp_data_sub")
    gupc_cs_sub_mat_all[[i]] <- matrix(get(paste0("param_set_sub_", attribute[i])),
                                  nrow = nrow(comp_train_group_by_all),
                                  ncol = nrow(data_store),
                                  byrow = TRUE
                                  )
  }
  
  fst_prt_ml  <- ml_parts(reparam_old(param_set), data1 = data_store, data2 = all_attrib_univ)
  # Building the second part of the expression for MLE
  subs_part <- ml_parts(reparam_old(param_set), 
                        data1 = comp_train, 
                        data2 = all_attrib_univ) %>% 
    as.matrix(.) #substitution part
  
  subs_prob_gupc <- Reduce("*", gupc_cs_sub_mat_all)
  
  scnd_prt_ml <- sapply(seq(1 : nrow(data_store)), function(y){
    return((subs_prob_gupc[, y] * subs_part) %>% sum())
  })
  
  #likelihood contribution for each upc.
  final_res <- fst_prt_ml + scnd_prt_ml  
  
  # maximum likelihood function ( two parts kept seperately )
  logl1  <- sum(log(final_res) * data_store[,  "sum_unit_sales"])
  logl2  <- log(sum(final_res)) * sum(data_store[, "sum_unit_sales"])

  # Loglikelihood
  logl  <-  logl1 - logl2
  
  # # Loglikelihood to avoid Inf value in function call while optimization routine
  # logl <- sum(log(final_res/sum(final_res)) * data_store[,  "sum_unit_sales"])
  # 
  return(-logl)
}

