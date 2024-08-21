library(ggplot2)
library(manipulate)
library(elasdics)
source("draw_regression_data.R")
source("fit_frechet_regression.R")

x <- -5:5/5
x_data <- data.frame("x" = x)

################################################################################
fish_data <- draw_regression_data(type = 1, x = x, sd = 0, m_range = NULL)
manipulate({plot(fish_data[[i]], type = "b", asp = 1, xlim = c(-5, 5), lwd = 3)}, 
           i = slider(1,11))

frechet_model <- fit_frechet_regression(x_new = x, fish_data, x = x, knots = seq(0,1, by = 0.1))
elastic_reg_model <- fit_elastic_regression(data_curves ~ x, data_curves = fish_data,
                                             x_data = x_data, knots = seq(0,1, by = 0.1))

regression_data_1 <- list("data_curves" = fish_data, 
                          "elastic_reg" = predict(elastic_reg_model),
                          "frechet_model" = predict_frechet_model(frechet_model))

manipulate({plot(regression_data_1[[1]][[i]], type = "b", asp = 1, xlim = c(-5,5), lwd = 3)
  lines(regression_data_1[[2]][[i]], col = "red", lwd = 3)
  lines(regression_data_1[[3]][[i]], col = "purple", lwd = 3)
}, 
i = slider(1,11))
################################################################################

fish_data <- draw_regression_data(type = 1, x = x, sd = 0.3, m_range = NULL)
frechet_model <- fit_frechet_regression(x_new = x, fish_data, x = x, knots = seq(0,1, by = 0.1))
quotient_reg_model <- fit_elastic_regression(data_curves ~ x, data_curves = fish_data,
                                             x_data = x_data, knots = seq(0,1, by = 0.1))

regression_data_2 <- list("data_curves" = fish_data, 
                          "quotient_reg" = predict_srv_model(quotient_reg_model, x),
                          "frechet_model" = predict_frechet_model(frechet_model))

manipulate({plot(regression_data_2[[1]][[i]], type = "b", asp = 1, xlim = c(-5,5), lwd = 3)
  lines(regression_data_2[[2]][[i]], col = "red", lwd = 3)
  lines(regression_data_2[[3]][[i]], col = "purple", lwd = 3)
}, 
i = slider(1,11))
################################################################################
fish_data <- draw_regression_data(type = 1, x = x, sd = 0.8, m_range = 30:50)
frechet_model <- fit_frechet_regression(x_new = x, fish_data, x = x, knots = seq(0,1, by = 0.1))
quotient_reg_model <- fit_quotient_regression(fish_data, x, knots = seq(0,1, by = 0.1))

regression_data_3 <- list("data_curves" = fish_data, 
                          "quotient_reg" = predict_srv_model(quotient_reg_model, x),
                          "frechet_model" = predict_frechet_model(frechet_model))
############
frechet_model <- fit_frechet_regression(x_new = x, fish_data, x = x, knots = seq(0,1, by = 0.02), type = "discrete")
quotient_reg_model <- fit_quotient_regression(fish_data, x, knots = seq(0,1, by = 0.02), type = "discrete")

regression_data_4 <- list("data_curves" = fish_data, 
                          "quotient_reg" = predict_srv_model(quotient_reg_model, x),
                          "frechet_model" = predict_frechet_model(frechet_model))
################################################################################
curve_data <- draw_regression_data(type = 3, x = x, sd = 1, m_range = 10:11)
frechet_model <- fit_frechet_regression(x_new = x, curve_data, x = x, knots = seq(0,1, by = 0.02), 
                                        type = "polygon")
quotient_reg_model <- fit_elastic_regression(data_curves ~ x, data_curves = curve_data,
                                             x_data = x_data, knots = seq(0,1, by = 0.02), type = "discrete")

regression_data_5 <- list("data_curves" = curve_data, 
                          "quotient_reg" = predict_srv_model(quotient_reg_model, x),
                          "frechet_model" = predict_frechet_model(frechet_model))
manipulate({plot(regression_data_5[[1]][[i]], type = "b", asp = 1, xlim = c(-5,5), lwd = 3)
  lines(regression_data_5[[2]][[i]], col = "red", lwd = 3)
  lines(regression_data_5[[3]][[i]], col = "purple", lwd = 3)
}, 
i = slider(1,11))
################################################################################
curve_data <- draw_regression_data(type = 2, x = x, sd = 0.3, m_range = 40:50)
frechet_model <- fit_frechet_regression(x_new = x, curve_data, x = x, knots = seq(0,1, by = 0.02), type = "discrete")
quotient_reg_model <- fit_quotient_regression(curve_data, x, knots = seq(0,1, by = 0.02), type = "discrete")

regression_data_6 <- list("data_curves" = curve_data, 
                          "quotient_reg" = predict_srv_model(quotient_reg_model, x),
                          "frechet_model" = predict_frechet_model(frechet_model))

################################################################################
curve_data <- draw_regression_data(type = 3, x =x, sd_2 = 0.05, m_range = 20:21)
frechet_model <- fit_frechet_regression(x_new = x, curve_data, x = x, knots = seq(0,1, by = 0.02), type = "discrete")
quotient_reg_model <- fit_quotient_regression(curve_data, x, knots = seq(0,1, by = 0.02), type = "discrete")

regression_data_7 <- list("data_curves" = curve_data, 
                          "quotient_reg" = predict_srv_model(quotient_reg_model, x),
                          "frechet_model" = predict_frechet_model(frechet_model))


################################################################################
curve_data <- draw_regression_data(type = 4, sd = 0.3, m_range = 25:35)
manipulate({plot(curve_data[[i]], type = "b", asp = 1, xlim = c(-3, 3), lwd = 3)}, 
           i = slider(1,11))

frechet_model <- fit_frechet_regression(x_new = x, curve_data, x = x, knots = seq(0,1, by = 0.02), type = "discrete")
quotient_reg_model <- fit_quotient_regression(curve_data, x, knots = seq(0,1, by = 0.02), type = "discrete")

regression_data_8 <- list("data_curves" = curve_data, 
                          "quotient_reg" = predict_srv_model(quotient_reg_model, x),
                          "frechet_model" = predict_frechet_model(frechet_model))
################################################################################
frechet_reg_data <- readRDS("simulation_data/frechet_reg_data")
frechet_reg_data[[8]] <- regression_data_8

#frechet_reg_data <- list(
#  regression_data_1, regression_data_2, regression_data_3, regression_data_4,
#  regression_data_5, regression_data_6, regression_data_7, regression_data_8
#)

saveRDS(frechet_reg_data, file = "simulation_data/frechet_reg_data")
