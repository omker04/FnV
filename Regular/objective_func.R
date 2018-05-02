# source('setup_packages.R')
# source('data_preprocessing.R')
# source('subs_param_order.R')
# source('subs_param_selection.R')
# source('parameter_shortlist.R')
# source('funcs_to_calculate_logL.R')

# log Likelihood
loglikelihood <- function(tot_par_set)
{
  data_store = get(paste0("data_store_", store_nbr))
  comp_train = comp_train_group_by_all
  param_set <- tot_par_set[seq(1, length(par_set), 1)]
  param_set_sub <- tot_par_set[seq(length(par_set) + 1, length(tot_par_set), 1)] %>%
    same_level_sub_addition(., num1 = 20, num2 = -20) %>% round(., 5)
  
  fst_prt_ml  <- ml_parts(reparam_old(param_set), data1 = data_store, data2 = all_attrib_univ)
  # Building the second part of the expression for MLE
  subs_part <- ml_parts(reparam_old(param_set), 
                        data1 = comp_train, 
                        data2 = all_attrib_univ
                        ) %>% 
    as.matrix(.) #substitution part

  
  s1 <- Sys.time()
  subs_prob_gupc <- sapply(seq(1 : nrow(data_store)), function(y){
    sapply(seq(1 : length(attribute)),
           function(x){
             ml_parts_sub(param_sub = reparam_sub(param_set_sub), 
                          k = y, 
                          data1 = comp_train, 
                          data2 = attrib_set_subs, 
                          attribute[x], 
                          data_store = data_store)
             }
    ) %>% apply(., 1, prod)
  })
  s2 <- Sys.time() 
  print(s2- s1)
  
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
  return(-logl)
}

