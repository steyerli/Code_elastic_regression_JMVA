library(elasdics)
library(ggplot2)
library(ggpubr)
source("fit_frechet_regression.R")
source("fit_elastic_curve.R")

HUblue <- colorRampPalette(c(rgb(1,1,1), rgb(0, 55/255, 108/255)))(8)
HUred <- colorRampPalette(c(rgb(1,1,1), rgb(138/255, 15/255, 20/255)))(8)
HUpurple <- colorRampPalette(c(HUred[8],HUblue[8]))(8)
HUsand <- colorRampPalette(c(rgb(1,1,1), rgb(210/255, 192/255, 103/255)))(8)

colours <- c(HUblue[8],  HUsand[8], HUred[8], HUpurple[4])
################################################################################

create_one_plot <- function(regression_models, x_select = c(-1, -0.6, 0, 0.4, 1),
                            model_select = as.list(1:5), reverse = FALSE){
  x <- -5:5/5
  x_data <- data.frame("x" = x)
  
  observed_data <- lapply(1:length(x), function(i){
    cbind(regression_models$sample_data[[i]], "x" = x[i])
  })
  observed_data <- do.call("rbind", observed_data)
  observed_data <- observed_data[observed_data$x %in% x_select,]
  observed_data <- do.call("rbind", lapply(1:length(model_select), function(i){
    data.frame(observed_data, "model_select" = as.factor(i))
  }))
  observed_data$x = factor(paste("x =", observed_data$x), 
                           levels = unique(paste("x =", observed_data$x)))
  
  predicted_curves <- list("quotient_reg" = predict(regression_models[[3]][[1]], x_data),
                           "pre_align" = predict(regression_models[[3]][[2]], x_data),
                           "elastic_curve" = predict_elastic_curve(regression_models[[3]][[3]], x),
                           "pre_align_curve" = predict_elastic_curve(regression_models[[3]][[4]], x),
                           "frechet" = predict_frechet_model(regression_models[[3]][[5]]))
model <- c("quotient\n regression", "pre align,\n srv fit", "iterate align,\n curve fit",
             "pre align,\n curve fit", "Fréchet\n regression")
  
  model_data <- lapply(1:length(model_select), function(k){
    model_data_list <- lapply(model_select[[k]], function(j){
      model_data <- lapply(1:length(x), function(i){
        data <- data.frame(predicted_curves[[j]][[i]], x = x[i], "model" = model[j],
                           "model_select" = k)
        names(data)[1:2] <- names(observed_data)[1:2]
        data
      })
      do.call("rbind", model_data)
    })
    do.call("rbind", model_data_list)
  })
  
  model_data <- do.call("rbind", model_data)
  model_data$model_select <- factor(model_data$model_select)
  model_data$model <- factor(model_data$model, levels = model)
  model_data <- model_data[model_data$x %in% x_select,]
  model_data$x = factor(paste("x =", model_data$x), 
                        levels = unique(paste("x =", model_data$x)))
  
  formula <- ifelse(reverse, "x ~ model_select", "model_select ~ x")
  
  g_data <- ggplot(aes(x = y1, y = y2, group = as.factor(x), alpha = "data"), data = observed_data) + 
    geom_path(linewidth = 0.6, color = "darkgrey") + geom_point(size = 0.8, color = "darkgrey") +
    facet_grid(formula) + 
    coord_fixed() + 
    scale_x_continuous(breaks = c(-3, 0, 3)) +
    scale_y_continuous(breaks = c(-2, 0, 2)) +
    scale_alpha_manual("", labels = "simulated\n data", values = 1,
                       guide = guide_legend(override.aes = list(linewidth = 0.6, size = 1))) +
    xlab(expression(y[1])) + ylab(expression(y[2])) + 
    theme(legend.position = "right", legend.key.size = unit(1, "cm"),
          legend.key = element_rect(color = NA, fill = NA), legend.title.align = 0.5,
          panel.background = element_rect(fill = 'white', colour = 'white'))
  
  if(reverse){
    g_data <- g_data + theme(strip.text.x = element_blank())
  } else {
    g_data <- g_data + theme(strip.text.y = element_blank())
  }
  
  g_data + geom_path(aes(group = model, color = model), alpha = 0.9, 
                     linewidth = 0.5, data = model_data) +
    scale_color_manual(name = "model", values = c(HUred[8], HUblue[8], "#CC6677", "#44AA99", "black")) +
    theme(plot.margin = unit(c(0,0,0,0), "cm"))
}
#################################
# create plots for simulation section
g_1 <- create_one_plot(readRDS("simulation_data/regression_models_1_101"),
                       x_select = c(-1, 0, 1), model_select = list(1:2, 3:5),
                       reverse = TRUE) +
                        theme(plot.margin=unit(c(0,0,-2,0), "cm"))

g_2 <- create_one_plot(readRDS("simulation_data/regression_models_5_102"),
                       x_select = c(-1, 0, 1), model_select = list(1:2, 3:5),
                       reverse = TRUE) + ylab("") +
                        theme(plot.margin=unit(c(0,0,-2, 0), "cm"), 
                              axis.text.y=element_blank(),
                              axis.ticks.y=element_blank())
  
g_3 <- create_one_plot(readRDS("simulation_data/regression_models_11_102"),
                       x_select = c(-1, 0, 1), model_select = list(1:2, 3:5),
                       reverse = TRUE) + ylab("") +
                        theme(plot.margin=unit(c(0,0,-2,0), "cm"), 
                              axis.text.y=element_blank(),
                              axis.ticks.y=element_blank())

ggarrange(g_1, g_2, g_3,
          ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom",
          align = "hv") %>% 
  ggexport(filename = "../../../Figures/simulated_models.pdf", width = 12, height = 5)
#################################
x <- -5:5/5
# create plots for appendix
pdf("../../../Figures/simulation_1.pdf", width = 10, height = 10)
create_one_plot(readRDS("simulation_data/regression_models_1_101"),
                x_select = x, reverse = TRUE)
dev.off()
##############
pdf("../../../Figures/simulation_5.pdf", width = 10, height = 10)
create_one_plot(readRDS("simulation_data/regression_models_5_102"),
                x_select = x, reverse = TRUE)
dev.off()
##############
pdf("../../../Figures/simulation_11.pdf", width = 10, height = 10)
create_one_plot(readRDS("simulation_data/regression_models_11_102"),
                x_select = x, reverse = TRUE)
dev.off()
#################################################################################
library(xtable)
file_list <- list.files("simulation_data/")
id_data <- data.frame(t(sapply(file_list, function(file){
  c("model_id" = as.numeric(strsplit(file, "_")[[1]][3]), 
    "seed" = as.numeric(strsplit(file, "_")[[1]][4]))
})))

mse_run_time_data <- sapply(file_list, function(file){
  sim_id <- as.numeric(strsplit(file, "_")[[1]][3])
  seed <- as.numeric(strsplit(file, "_")[[1]][4])
  regression_models <- readRDS(paste0("simulation_data/", file))
  if(is.list(regression_models$mse_estimates)){
    is_error <- sapply(regression_models$mse_estimates, function(x){
      class(x) == "try-error"
    })
    estimates <- regression_models$mse_estimates[!is_error]
    regression_models$mse_estimates <- do.call("cbind", estimates)
  }
  mse_estimates <- rowMeans(regression_models$mse_estimates)
  names(mse_estimates) <- c("quotient reg model", "pre-align, srv-fit",
                            "iterate align, curve-fit", "pre-align, curve-fit", "frechet")
  run_times <- sapply(regression_models[[3]], function(model) attributes(model)$run_time[3])
  names(run_times) <- c("quotient reg model", "pre-align, srv-fit",
                        "iterate align, curve-fit", "pre-align, curve-fit", "frechet")
  c("simulation" = sim_id, "seed" = seed, mse_estimates, run_times)
})

mse_run_time_data <- as.data.frame(t(mse_run_time_data))

sim_data <- data.frame(t(sapply(1:12, function(i){
  colMeans(mse_run_time_data[mse_run_time_data$simulation == i, - 2])
})))
sim_data <- cbind("sim" = sim_data[,1],
                  "sd" = c(0.4, 0.8, 0.4, 0.8, 0.2, 0.4, 0.2, 0.4, 0.1, 0.2, 0.1, 0.2),
                  "m" = rep(rep(c("[15, 20]", "[30, 40]"), each = 2), 3),
                  sim_data[,-1])

xtable_sim_data <- xtable(sim_data, digits = c(1, 0, 1, 0, rep(2, 5), rep(0,5)))
print(xtable_sim_data, include.rownames=FALSE)
