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
    prev.ID <- ID
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
    lightbulbs_train <- data.frame(
      upc_nbr = as.numeric(v.upc_nbr),
      store_nbr = as.numeric(v.store),
      total_unit_qty = as.numeric(v.total_unit_qty),
      total_retail_price = as.numeric(v.total_retail_price),
      store_ind = as.numeric(v.store_ind)
    )
    
    source('data_shapeup.R')
    source('data_preprocessing.R')
    
    attrib_set_subs_pre$store_nbr <- prev.ID
    apply(attrib_set_subs_pre, 1, function(x){cat(x[1], '\t', x[2], '\t', x[3], '\t', x[4], '\n')})
    write.table(attrib_set_subs_pre, quote=FALSE, sep = "\t", row.names=FALSE, col.names=FALSE)
    
    # data.final <- data.frame(wm_dollars = as.numeric(dollars.v),
    #                          promo_dollars = as.numeric(promo.dollars.v),
    #                          avg_price = as.numeric(avg.price.v))
    # 
    # lm.model <- lm(wm_dollars ~ promo_dollars + avg_price, data = data.final)
    # coeff <- lm.model$coefficients
    # cat(prev.category, '\t', prev.subcategory, '\t', coeff[1],'\t', coeff[2], '\t', coeff[3], '\n')
    # 
    # 
    # prev.subcategory <- subcategory
    # dollars.v <- wm_dollars
    # promo.dollars.v <- wm_any_promo_dollars
    # avg.price.v <- wm_avg_price
    
    
    
    prev.ID <- ID
    v.upc_nbr <- upc_nbr
    v.store <- store
    v.total_unit_qty <- total_unit_qty
    v.total_retail_price <- total_retail_price
    v.store_ind <- store_ind
    }
  
}



lightbulbs_train <- data.frame(
  upc_nbr = as.numeric(v.upc_nbr),
  store_nbr = as.numeric(v.store),
  total_unit_qty = as.numeric(v.total_unit_qty),
  total_retail_price = as.numeric(v.total_retail_price),
  store_ind = as.numeric(v.store_ind)
)

source('data_shapeup.R')
source('data_preprocessing.R')

attrib_set_subs_pre$store_nbr <- store
write.table(attrib_set_subs_pre, quote=FALSE, sep = "\t", row.names=FALSE, col.names=FALSE)

# apply(attrib_set_subs_pre, 1, function(x){cat(x[1], '\t', x[2], '\t', x[3], '\t', x[4], '\n')})
# data.final <- data.frame(wm_dollars = as.numeric(dollars.v),
#                          promo_dollars = as.numeric(promo.dollars.v),
#                          avg_price = as.numeric(avg.price.v))
# 
# lm.model <- lm(wm_dollars ~ promo_dollars + avg_price, data = data.final)
# coeff <- lm.model$coefficients
# cat(prev.category, '\t', prev.subcategory, '\t', coeff[1],'\t', coeff[2], '\t', coeff[3],'\n')

close(connection)
