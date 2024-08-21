library(elasdics)
library(ggplot2)
library(gridExtra)

colours <- c("blue", "red")

plot_regression_models <- function(regression_models, mse_estimates, k = 1){
  x <- -5:5/5
  x_data <- data.frame("x" = x)
  #################################
  is_error <- sapply(mse_estimates, function(x) class(x) == "try-error")
  mse_data <- lapply(mse_estimates[!is_error], function(x){
    data.frame(mse = x[[2]], "fit_level" = c("srv", "srv", "curve", "curve", "srv"),
               "model_type" = c("elastic", "pre_align", "elastic", "pre_align", "frechet"))
  })
  mse_data <- do.call("rbind", mse_data)

  #################################
  observed_data <- lapply(1:length(x), function(i){
    data.frame(regression_models[[k]][[1]][[i]], x = x[i])
  })
  observed_data <- do.call("rbind", observed_data)

  #################################
  predicted_curves <- list("quotient_reg" = predict(regression_models[[k]][[2]], x_data),
                           "pre_align" = predict(regression_models[[k]][[3]], x_data),
                           "elastic_curve" = predict_elastic_curve(regression_models[[k]][[4]], x),
                           "pre_align_curve" = predict_elastic_curve(regression_models[[k]][[5]], x),
                           "frechet" = predict_frechet_model(regression_models[[k]][[6]]))


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
    model_data_5 <- data.frame(predicted_curves[[5]][[i]], x = x[i], "fit_level" = "srv",
                               "model_type" = "frechet")
    names(model_data_5)[1:2] <- names(observed_data)[1:2]

    rbind(model_data_1, model_data_2, model_data_3, model_data_4, model_data_5)
  })
  model_data <- do.call("rbind", model_data)

  x_select <- c(-1, -0.6, 0, 0.4, 1)

  bgnd <- theme_get()$panel.background$fill
  g_data <- ggplot(aes(x = y1, y = y2, group = as.factor(x)), data = observed_data[observed_data$x %in% x_select,]) +
    geom_path(size = 1) + geom_point(size = 1.8, fill = "black", shape = 21, color = bgnd)
  g_data <- g_data + geom_path(data = model_data[model_data$x %in% x_select,],
                               aes(group = fit_level, color = fit_level, alpha = 1), size = 1.3) +
    facet_grid(x ~ model_type) + theme(legend.position="none") +
    scale_color_manual(name = "fit level", values = colours) + coord_fixed()

  g_mse <- ggplot(data = mse_data, aes(x = model_type, y = mse, fill = fit_level)) + geom_boxplot(alpha = 0.7) +
    scale_fill_manual(name = "fit level", values = colours)

  grid.arrange(g_data, g_mse, ncol = 2, nrow = 2, widths = c(0.6, 0.4),  layout_matrix = cbind(c(1, 1),
                                                                                               c(2, NA)))
}
