# options(error=recover)
# options(warning.expression=quote(recover()))
options(warn = -1)
number_of_simulations <- 5

# source('setup_packages.R')
# source('data_shapeup.R')
# source('data_preprocessing.R')
# source('subs_param_order.R')
# source('funcs_to_calculate_logL.R')
# source('objective_func_modified.R')


# initial parameters to be passed
toRemove <- same_level_sub
if(exists("whichZero"))
  toRemove <- c(toRemove, whichZero)
# if("Fineline" %in% attribute)
#   toRemove <- c(toRemove, whichZero)
# if("Technology..Subcat." %in% attribute)
#   toRemove <- c(toRemove, whichZero)

ini_set <- c(trans_par_set(par_set), trans_par_set_sub(par_set_sub[-c(toRemove)]))

# value of objective function at the initial value
x1 <- Sys.time()
loglikelihood(ini_set)
Sys.time() - x1



forecast <- function(dem_par, subs_par, data_store, data_comp)
{
  forc_frst_prt_ml_train <- ml_parts(dem_par$final_estimate, data_store, dem_par) # final_demand_par
  forc_subs_part_train <- ml_parts(dem_par$final_estimate,
                                   data1 = data_comp,
                                   data2 = dem_par
  ) %>% as.matrix(.)  # final_demand_par
  forc_subs_prob_gupc <- sapply(seq(1 : nrow(data_store)), function(y){
    sapply(seq(1 : length(attribute)),
           function(x){
             ml_parts_sub(param_sub = subs_par$final_estimate,
                          k = y,
                          data1 = data_comp,
                          data2 = subs_par, # final_subs_par
                          attribute[x],
                          data_store = data_store)
           }
    ) %>% apply(., 1, prod)
  })
  forc_scnd_prt_ml_train <- sapply(seq(1 : nrow(data_store)), function(y){
    return((forc_subs_prob_gupc[, y] * forc_subs_part_train) %>% sum())
  })
  
  final_forc_train = (forc_frst_prt_ml_train + forc_scnd_prt_ml_train) %>% as.data.frame(.)
  colnames(final_forc_train) <- "prob_of_buying_GUPC_given_assortment"
  final_forc_train$GUPC <- apply(data_store[, attribute], 1, function(x) paste(x, collapse = " || "))
  # final_forc_train <- final_forc_train[, c(2,1)]
  final_forc_train$forcasted_unit_sales_share = (final_forc_train$prob_of_buying_GUPC_given_assortment)/sum(final_forc_train$prob_of_buying_GUPC_given_assortment)
  return(list(final_forc_train, forc_frst_prt_ml_train, forc_scnd_prt_ml_train, forc_subs_part_train, forc_subs_prob_gupc))
}

# number_of_simulations <- 5

ini_set_rep <- vector("list", number_of_simulations)
result_solnp <- vector("list", number_of_simulations)
par_set_est_rep <- vector("list", number_of_simulations)
par_set_sub_est_rep <- vector("list", number_of_simulations)
final_forc_train_rep <- vector("list", number_of_simulations)
data_for_graph_rep <- vector("list", number_of_simulations)
R_sqr_rep <- vector("list", number_of_simulations)
s1 <- vector("list", number_of_simulations)
s2 <- vector("list", number_of_simulations)
par_sub_set_random_input <- vector("list", number_of_simulations)
par_random_input <- vector("list", number_of_simulations)
final_demand_par <- vector("list", number_of_simulations)
final_subs_par <- vector("list", number_of_simulations)

library(foreach)
library(doParallel)

registerDoParallel(cores = 7)

optimization_output <- foreach(i = 1 : number_of_simulations) %dopar% {
  # par_sub_set_random_input[[i]] <- runif(length(par_set_sub[-c(same_level_sub, whichZero)]), 0, 1)
  par_sub_set_random_input[[i]] <- runif(length(par_set_sub[-toRemove]), 0, 1)
  # par_random_input <- c()
  for(j in 1 : length(attribute))
  {
    par_random_input[[i]][pcum1[j] : pcum2[j]] <- runif(p[j], 0, 1)
    par_random_input[[i]][pcum1[j] : pcum2[j]] <- par_random_input[[i]][pcum1[j] : pcum2[j]]/sum(par_random_input[[i]][pcum1[j] : pcum2[j]])
  }
  # trans_par_set(par_set)
  # initial parameters to be passed
  ini_set_rep[[i]] <- c(trans_par_set(par_random_input[[i]]), trans_par_set_sub(par_sub_set_random_input[[i]]))
  print(loglikelihood(ini_set_rep[[i]]))
  
  s1[[i]] <- Sys.time()
  result_solnp[[i]] <- solnp(pars = ini_set_rep[[i]], #log odds transformation
                             fun = loglikelihood,
                             control = list(
                               # inner.iter = 2,
                               # outer.iter = 5,
                               delta = 1e-4
                               # tol = 1e-11
                             )
  )
  s2[[i]] <- Sys.time()
  
  # Estimated Demand Parameters
  par_set_est_rep[[i]] <- result_solnp[[i]]$pars[seq(1, length(par_set), 1)] %>% reparam_old()
  # Estimated Substitution Parameters
  par_set_sub_est_rep[[i]] <- result_solnp[[i]]$pars[seq(length(par_set) + 1, length(result_solnp[[i]]$pars), 1)]  %>% reparam_sub() %>% 
    same_level_sub_addition(., num = 100, toRemove)
  par_set_sub_est_rep[[i]][same_level_sub] <- 1
  if(exists("whichZero"))
    par_set_sub_est_rep[[i]][whichZero] <- 0
  
  final_demand_par[[i]] <- initial_values[, -2]
  row.names(final_demand_par[[i]]) <- NULL
  final_demand_par[[i]]$final_estimate <- par_set_est_rep[[i]]
  
  final_subs_par[[i]] <- attrib_set_subs
  row.names(final_subs_par[[i]]) <- NULL
  final_subs_par[[i]]$final_estimate <- par_set_sub_est_rep[[i]] %>% round(., 10)
  
  final_forc_train_rep[[i]] <- forecast(data_store = get(paste0("data_store_", store_nbr)),
                                        data_comp = comp_train_group_by_all,
                                        dem_par = final_demand_par[[i]],
                                        subs_par = final_subs_par[[i]]
  )[[1]]
  
  # Actual sales share of GUPCs
  actual_unit_sales_share <- get(paste0("data_store_", store_nbr))[, "sum_unit_sales"]/sum(get(paste0("data_store_", store_nbr))[, "sum_unit_sales"])
  data_for_graph_rep[[i]] <- cbind(final_forc_train_rep[[i]], actual_unit_sales_share)
  
  R_sqr_rep[[i]] <- (cor(data_for_graph_rep[[i]]$actual_unit_sales_share, data_for_graph_rep[[i]]$forcasted_unit_sales_share))^2
  
  list(par_random_input[[i]],
       par_sub_set_random_input[[i]],
       par_set_est_rep[[i]],
       par_set_sub_est_rep[[i]],
       R_sqr_rep[[i]],
       s1[[i]],
       s2[[i]],
       result_solnp[[i]],
       final_demand_par[[i]],
       final_subs_par[[i]],
       ini_set_rep[[i]],
       final_forc_train_rep[[i]],
       data_for_graph_rep[[i]]
  )
}

par_random_input <- sapply(1 : number_of_simulations, function(x) list(optimization_output[[x]][[1]]))
par_sub_set_random_input <- sapply(1 : number_of_simulations, function(x) list(optimization_output[[x]][[2]]))
par_set_est_rep <- sapply(1 : number_of_simulations, function(x) list(optimization_output[[x]][[3]]))
par_set_sub_est_rep <- sapply(1 : number_of_simulations, function(x) list(optimization_output[[x]][[4]]))
R_sqr_rep <- sapply(1 : number_of_simulations, function(x) list(optimization_output[[x]][[5]]))
s1 <- sapply(1 : number_of_simulations, function(x) list(optimization_output[[x]][[6]]))
s2 <- sapply(1 : number_of_simulations, function(x) list(optimization_output[[x]][[7]]))
result_solnp <- sapply(1 : number_of_simulations, function(x) list(optimization_output[[x]][[8]]))
final_demand_par <- sapply(1 : number_of_simulations, function(x) list(optimization_output[[x]][[9]]))
final_subs_par <- sapply(1 : number_of_simulations, function(x) list(optimization_output[[x]][[10]]))
ini_set_rep <- sapply(1 : number_of_simulations, function(x) list(optimization_output[[x]][[11]]))
final_forc_train_rep <- sapply(1 : number_of_simulations, function(x) list(optimization_output[[x]][[12]]))
data_for_graph_rep <- sapply(1 : number_of_simulations, function(x) list(optimization_output[[x]][[13]]))

R_sqr_train <- R_sqr_rep %>% do.call(c, .)
maxR <- which.max(R_sqr_train)
maxDemandPar <- final_demand_par[[maxR]]
maxSubsPar <- final_subs_par[[maxR]]
maxDemandPar$item_to <- "demand par"
maxDemandPar <- maxDemandPar[,c(1,4,2,3)]
colnames(maxDemandPar) <- colnames(maxSubsPar)
allParameters <- rbind(maxDemandPar, maxSubsPar)
allParameters$store_nbr <- store_nbr

# source('data_for_graph_test.R')
# source('demandSharePlot.R')
# source('gg_subtitle.R')


