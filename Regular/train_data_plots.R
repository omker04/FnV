# source('optimization_run.R')
source('setup_packages.R')

# time required for optimization part to run
print(tym_to_run_optimization)

# print(par_set_est) # Demand Share Parameters
# print(par_set_sub_est) # Substitution Parameters

# Demand Share Parameters Plot
ggplot() +
  geom_point(aes(x = par_set, y = par_set_est)) +
  geom_abline(aes(intercept = 0, slope = 1), 
              color ="red") +
  xlab("Initial Values") +
  ylab("Estimated Values") +
  ggtitle("Demand Share Parameters")

# # Substitution Parameters Plot
ggplot() +
  geom_point(aes(x = par_set_sub, y = par_set_sub_est)) +
  geom_abline(aes(intercept = 0, slope = 1), 
              color ="red") +
  xlab("Initial Values") +
  ylab("Estimated Values") +
  ggtitle("Substitution Parameters")

final_demand_par <- initial_values[, -2]
row.names(final_demand_par) <- NULL
final_demand_par$final_estimate <- par_set_est

final_subs_par <- attrib_set_subs
row.names(final_subs_par) <- NULL
final_subs_par$final_estimate <- par_set_sub_est %>% round(., 10)

# Constraint check | wheter each levels summing upto 1 or not
constraint_check <- sapply(seq(1:length(attribute)), function(x) apply(final_demand_par[pcum1[x]:pcum2[x],-1], 2, sum))
colnames(constraint_check) <- attribute
print(constraint_check)

#Diagnostics
# converged or not ( 0 for convergence )
result_solnp$convergence
# Final 
result_solnp$value

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
  return(final_forc_train)
}

final_forc_train <- forecast(data_store = get(paste0("data_store_",store_nbr)),
                             data_comp = comp_train_group_by_all,
                             dem_par = final_demand_par,
                             subs_par = final_subs_par
                             )

# Actual sales share of GUPCs
actual_unit_sales_share <- get(paste0("data_store_",store_nbr))[, "sum_unit_sales"]/sum(get(paste0("data_store_",store_nbr))[, "sum_unit_sales"])
data_for_graph <- cbind(final_forc_train, actual_unit_sales_share)

R_sqr <- (cor(data_for_graph$actual_unit_sales_share, data_for_graph$forcasted_unit_sales_share))^2


# library(ggvis)
# # data_for_graph$GUPC <- seq(1 : nrow(data_for_graph))
# data_for_graph %>%
#   ggvis(x = ~actual_unit_sales_share,
#         y = ~forcasted_unit_sales_share) %>%
#   layer_points(size := 7) %>%
#   layer_paths(~x,
#               ~y,
#               stroke:="red",
#               data=data.frame(y=seq(0, 0.045, 1/100),
#                               x=seq(0, 0.045, 1/100)
#               )
#   ) %>%
#   add_axis("x", title = "Actual", offset = 0, title_offset = 30) %>%
#   add_axis("y", title = "Estimated", offset = 0, title_offset = 50) %>%
#   add_axis("x", orient = "top", ticks = 0, title = "Sales Share of GUPCs - Yogurt (wk-year 49-2014 to 23-2015)",
#            properties = axis_props(
#              axis = list(stroke = "white"),
#              labels = list(fontSize = 10))) %>%
#   add_axis("x", orient = "bottom", ticks = 0, title_offset = 50, title = paste("R2", " = ", round(R_sqr, 2)),
#            properties = axis_props(
#              axis = list(stroke = "white"),
#              labels = list(fontSize = 10)))

source('gg_subtitle.R')
gg <- ggplot(data_for_graph) +
  geom_point(aes(x = actual_unit_sales_share, 
                 y = forcasted_unit_sales_share),
             col = "black", size = 0.5) + 
  geom_abline(aes(intercept = 0, slope = 1), 
              color ="red") +  theme_bw() +
  theme(panel.grid.minor = element_line(colour="gray95")) +
  xlab("Actual") +
  ylab("Estimated") +
  geom_smooth(span = 0.25) +
  ggtitle("Sales Share of GUPCs - Yogurt (wk-year 49-2014 to 23-2015)") +
  theme(plot.title = element_text(hjust=0, size=16))
ggplot_with_subtitle(gg, paste("R-Square", " = ", round(R_sqr, 2)), bottom_margin = 10, lineheight=0.5)


data_line <- data_for_graph[,c(2,3)] 
colnames(data_line) <- colnames(data_for_graph[,c(2,4)])
data_line <- rbind(data_line, data_for_graph[,c(2,4)])
colnames(data_line) <- c("GUPC", "Sales.Share")
data_line$label <- c(rep("Estimated", nrow(data_for_graph)), rep("Actual", nrow(data_for_graph)))
data_line$GUPC_number <- c(seq(1 : nrow(data_for_graph)), seq(1 : nrow(data_for_graph)))

gg_line = ggplot(data_line) +
  geom_line(aes(x = GUPC_number, 
                y = Sales.Share, group = label, colour = label))  +
  scale_color_manual("Sales Share\n",labels = c("Actual", "Estimated"), values = c("dark green", "red")) +
  theme_bw() +
  theme(panel.grid.minor = element_line(colour="gray95")) +
  xlab("GUPC") +
  ylab("Sales Share") +
  geom_smooth(span = 0.25) +
  ggtitle("Sales Share of GUPCs - Yogurt (Train Data : wk-year 49-2014 to 23-2015)") +
  theme(plot.title = element_text(hjust=0, size=12))
ggplot_with_subtitle(gg_line, paste("R-Square", " = ", round(R_sqr, 2)), bottom_margin = 10, lineheight=0.5)


#data_req <- national_train[,attribute]
data_req <- get(paste0("data_store_", store_nbr))[, attribute]
data_req$forcasted_unit_sales_share <- final_forc_train$forcasted_unit_sales_share
# Attribute level sales share comparison
attribute_level_estimated_unit_sales_share <- function(df, attrib)
{
  data_temp <- df[, c(attrib, "forcasted_unit_sales_share")]
  ret_df <- data_temp %>% group_by_(attrib) %>% summarise(forcasted_unit_sales_share = sum(forcasted_unit_sales_share)) %>% as.data.frame()
  colnames(ret_df)[1] <- "attribute_level"
  return(ret_df)
}  

result_attribute_level <- lapply(attribute,
                                         function(x) attribute_level_estimated_unit_sales_share(data_req, x)) %>% Reduce('rbind', .)

result_attribute_level <- merge(result_attribute_level, initial_values, by = "attribute_level", all.y = TRUE)
result_attribute_level <- result_attribute_level[order(result_attribute_level$level_order), ]
result_attribute_level$level_order <- NULL
colnames(result_attribute_level) <- c("attribute_level", "forcasted_unit_sales_share", "actual_unit_sales_share")
result_attribute_level

result_attribute_level$attribute <- c(rep(attribute, p))
gg1 <- ggplot(result_attribute_level) +
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
  ggtitle("Sales Share of Attributes - Yogurt (wk-year 49-2014 to 23-2015)") +
  theme(plot.title = element_text(hjust=0, size=16))
ggplot_with_subtitle(gg1, paste("R2", round(cor(result_attribute_level[, -c(1,4)], use = "pairwise.complete.obs")[1, 2], 4), sep = " = "), bottom_margin = 10, lineheight=0.5)


