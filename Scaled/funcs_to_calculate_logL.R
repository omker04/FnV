# source('setup_packages.R')
# source('data_preprocessing.R')
# source('subs_param_order.R')

par_set  <-  initial_values$unit_share
par_set_sub <- attrib_set_subs$corrected_T_prob

# important functions to Calculate likelihood values
####################################################

# finds out the positions of the level of each attribute contributiing to minimum share
base_level  <-  function(param,  attr)
{
  k_temp  <-  which(attribute == attr)
  return(which.min(param[pcum1[k_temp] : pcum2[k_temp]]))
}

# finds out the positions same as before but wrt the par_set vector
k_set  <-  c()
for(i in 1:length(attribute))
{
  k_set[i]  <-  base_level(par_set,  attribute[i]) + pcum1[i] - 1
}

# Transformation of parameter set to log-odd
# initial parameter values after this transformation goes into optimization, as initial starting point
trans_par_set  <-  function(param)
{
  par_trans  <-  c()
  for (i in 1:length(attribute))
  {
    par_trans[pcum1[i]:pcum2[i]]  <- log(param[pcum1[i]:pcum2[i]] / param[k_set[i]])
  }
  return(par_trans)
}

trans_par_set_sub <- function(param_sub)
{
  par_sub_trans <- log(param_sub/(1 - param_sub))
  return(par_sub_trans)
}

# the reparametriziting functions
reparam_old  <-  function(param)
{
  tot  <-  c()
  re_param  <-  c()
  for (i in 1:length(attribute))
  {
    if(max(exp(param[pcum1[i] : pcum2[i]])) == Inf){
      param[pcum1[i] : pcum2[i]] <- param[pcum1[i] : pcum2[i]]/max(param[pcum1[i] : pcum2[i]])
    }
    tot[i]  <-  sum(exp(param[pcum1[i] : pcum2[i]]))
    re_param[pcum1[i] : pcum2[i]]  <- exp(param[pcum1[i] : pcum2[i]]) / tot[i]
    # re_param[k_set[i]] <- 1/tot[i]
  }
  return(re_param)
}

reparam_sub <- function(param_sub)
{
  sub_temp <- c()
  sub_temp <- 1/(1 + exp(-param_sub))
  return(sub_temp)
}

# function to calculate product of attribute level shares for GUPCs
ml_parts  <-  function(param,  data1,  data2)
{
  z  <-  array(0,  nrow(data1))
  for (i in 1:nrow(data1))
  {
    tem1  <-  c()
    for (k in 1:length(attribute))
    {
      tem1[k]  <- param[which(as.character(data2$attribute_level) == as.character(data1[,  attribute[k]][i]))]
    }
    z[i]  <-  prod(tem1)
    tem1  <-  NULL
  }
  return(z)
}

ml_parts_sub <- function(param_sub, k, data1, data2, attr, data_store)
{
  x = data_store[k, attr]
  data_comp <- data1[, attr]
  z  <-  array(0,  length(data_comp))
  for (i in 1 : length(data_comp))
  {
    z[i]  <- param_sub[which(as.character(data2$item_from) == as.character(data_comp[i]) & as.character(data2$item_to) == as.character(x))]
  }
  return(z)
}

same_level_sub <- which(attrib_set_subs$item_to == attrib_set_subs$item_from)
same_level_sub_addition <- function(param, num, toReplace)
{
  param_added <- array(num, length(par_set_sub))
  temp_pos <- c(1 : length(par_set_sub))[!c(1 : length(par_set_sub)) %in% toReplace]
  param_added[temp_pos] <- param
  return(param_added)
}

# if("Fineline" %in% attribute){
#   i <- which(attribute == "Fineline")
#   whichZero <- which(attrib_set_subs$corrected_T_prob == 0)
# }
# 
# if("Technology..Subcat." %in% attribute){
#   i <- which(attribute == "Technology..Subcat.")
#     whichZero <- which(attrib_set_subs$corrected_T_prob == 0)
# }


if(!is.null(noSubsAttribute)){
  if(is.na(match(noSubsAttribute, attribute)) %>% Reduce("+",.) > 0){
    stop("Attribute without no substitution is not considered. Change value of noSubsAttribute.")
  }else{
    whichZero <- which(attrib_set_subs$corrected_T_prob == 0)
  }
}
