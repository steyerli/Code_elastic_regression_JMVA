library(ggplot2)
library(elasdics)
library(gridExtra)
library(grid)

colours <- c("blue", "red")

################################################################################

prep_model_data <- function(sim_number = 1, k = 1, x_select = c(-1, 0, 1), model_types = 1:4){
  x <- -5:5/5
  regression_models <- readRDS(paste0("../simulation_data/regression_models_", sim_number))
  mse_estimates <- readRDS(paste0("../simulation_data/mse_estimates_", sim_number))
  #################################
  is_error <- sapply(mse_estimates, function(x) class(x) == "try-error")
  mse_data <- lapply(mse_estimates[!is_error], function(mse_estimates){
    data.frame(mse = mse_estimates[[2]], "fit_level" = c("srv", "srv", "curve", "curve"),
               "model_type" = c("elastic", "pre-align", "elastic", "pre-align"))
  })
  mse_data <- do.call("rbind", mse_data)
  
  #################################
  observed_data <- lapply(1:length(x), function(i){
    data.frame(regression_models[[k]][[1]][[i]], x = x[i])
  })
  observed_data <- do.call("rbind", observed_data)
  observed_data <- observed_data[observed_data$x %in% x_select,]
  
  #################################
  predicted_curves <- list("quotient_reg" = predict_srv_model(regression_models[[k]][[2]], x),
                           "pre_align" = predict_srv_model(regression_models[[k]][[3]], x),
                           "elastic_curve" = predict_model(regression_models[[k]][[4]], x),
                           "pre_align_curve" = predict_model(regression_models[[k]][[5]], x))
  
  model_data <- lapply(1:length(x), function(i){
    model_data_1 <- data.frame(predicted_curves[[1]][[i]], x = x[i], "fit_level" = "srv",
                               "model_type" = "elastic")
    names(model_data_1)[1:2] <- names(observed_data)[1:2]
    model_data_2 <- data.frame(predicted_curves[[2]][[i]], x = x[i], "fit_level" = "srv",
                               "model_type" = "pre-align")
    names(model_data_2)[1:2] <- names(observed_data)[1:2]
    model_data_3 <- data.frame(predicted_curves[[3]][[i]], x = x[i], "fit_level" = "curve",
                               "model_type" = "elastic")
    names(model_data_3)[1:2] <- names(observed_data)[1:2]
    model_data_4 <- data.frame(predicted_curves[[4]][[i]], x = x[i], "fit_level" = "curve",
                               "model_type" = "pre-align")
    names(model_data_4)[1:2] <- names(observed_data)[1:2]
    
    do.call("rbind", list(model_data_1, model_data_2, model_data_3, model_data_4)[model_types])
  })
  model_data <- do.call("rbind", model_data)
  model_data <- model_data[model_data$x %in% x_select,]
  
  return(list(mse_data = mse_data, model_data = model_data, observed_data = observed_data))
}


sim_numbers <- c(1,4,6,9,10)
k_numbers <- c(2,2,10,3,3)

data_list <- lapply(1:length(sim_numbers), function(i) prep_model_data(sim_number = sim_numbers[i], 
                                                                         k = k_numbers[i]))
data_list <- lapply(1:length(data_list), function(i){
  lapply(data_list[[i]], function(data){
    data$simulation <- paste("simulation", i)
    data$x <- paste("x =", data$x)
    data
  })
})

model_data <- lapply(data_list, '[[', 2)
model_data <- do.call("rbind", model_data)
observed_data <- lapply(data_list, '[[', 3)
observed_data <- do.call("rbind", observed_data)
mse_data <- lapply(data_list, '[[', 1)
mse_data <- do.call("rbind", mse_data)

g_data <- ggplot(aes(x = y1, y = y2, group = as.factor(x), alpha = "data"), data = observed_data) + 
  geom_path(size = 0.5) + geom_point(size = 0.6) +
  facet_grid(x ~ simulation) + 
  coord_fixed() + 
  scale_x_continuous(breaks = c(-4, -2, 0, 2, 4)) +
  scale_alpha_manual("", labels = "simulated data", values = 1,
                     guide = guide_legend(override.aes = list(size = 0.4))) +
  xlab(expression(y[1])) + ylab(expression(y[2]))

g_data <- g_data + geom_path(aes(group = interaction(fit_level, model_type), 
                                 color = interaction(fit_level, model_type), 
                                 linetype = interaction(fit_level, model_type)), alpha = 0.7, 
                             size = 0.7, data = model_data)   +
  scale_color_manual(name = "model", values = rep(colours, 2),
                     labels = c("elastic,\n curve-fit", "elastic,\n srv-fit", 
                                "pre-align,\n curve-fit", "pre-align,\n srv-fit")) +
  scale_linetype_manual(name = "model", values = rep(1:2, each = 2),
                        guide = guide_legend(override.aes = list(size = 0.5)),
                        labels = c("elastic,\n curve-fit", "elastic,\n srv-fit", 
                                   "pre-align,\n curve-fit", "pre-align,\n srv-fit")) +
  theme(legend.position="bottom")
g_data

# change points size in legend
grid.ls(grid.force())
grid.gedit("key-1-3-2.2-4-2-4", size = unit(0.25, "npc"))
g_data <- grid.grab()

g_mse <- ggplot(data = mse_data, aes(x = model_type, y = mse, color = fit_level, linetype = model_type)) + 
  geom_boxplot(size = 0.4, outlier.size = 0.4, fatten = 1.5) + scale_color_manual(name = "fit level", values = colours) + 
  facet_wrap(. ~ simulation, scales = "free", ncol = length(data_list)) + theme(legend.position="none", axis.title.x=element_blank())

dummy_mse_data <- mse_data[mse_data$simulation %in% paste("simulation", 4:5),]
dummy_mse_data <- dummy_mse_data[c(which.min(dummy_mse_data$mse), which.max(dummy_mse_data$mse)), 1:3]
dummy_mse_data <- cbind(dummy_mse_data, simulation = rep(paste("simulation", 4:5), 2))
g_mse <- g_mse + geom_blank(data = dummy_mse_data)

pdf("../../Figures/simulation_results.pdf", height = 6.5, width = 9)
grid.arrange(g_data, g_mse, nrow = 2, heights = c(0.8, 0.2))
dev.off()

