library(ggplot2)
library(manipulate)
source("draw_regression_data.R")
source("../fit_quotient_regression.R")
source("../fit_elastic_curve_regression.R")
source("../fit_frechet_regression.R")

x <- -5:5/5
################################################################################
fish_data <- draw_regression_data(type = 1, x = x, sd = 0.4, m_range = 15:20, t_grid = seq(0,1, length = 25))
manipulate({plot(fish_data[[i]], type = "b", asp = 1, xlim = c(-5, 5), lwd = 3)}, 
           i = slider(1,11))

quotient_reg_model <- fit_quotient_regression(fish_data, x, knots = seq(0,1, by = 0.1))
pre_align_model <- fit_quotient_regression(fish_data, x, knots = seq(0,1, by = 0.1), max_iter = 0)
elastic_curve_model <- fit_elastic_curve_regression(fish_data, x, knots = seq(0,1, by = 0.1), eps = 10^-5)
pre_align_curve_model <- fit_elastic_curve_regression(fish_data, x, knots = seq(0,1, by = 0.1), max_iter = 0)
#frechet_model <- fit_frechet_regression(x_new = x, fish_data, x = x, knots = seq(0,1, by = 0.02), type = "discrete")

predicted_curves <- list("quotient_reg" = predict_srv_model(quotient_reg_model, x),
                         "pre_align" = predict_srv_model(pre_align_model, x),
                         "elastic_curve" = predict_model(elastic_curve_model, x),
                         "pre_align_curve" = predict_model(pre_align_curve_model, x)
                         #"frechet_model" = predict_frechet_model(frechet_model)
                         )

manipulate({plot(fish_data[[i]], type = "b", asp = 1, xlim = c(-5,5), lwd = 3)
  lines(predicted_curves[[1]][[i]], col = "red", lwd = 3)
  lines(predicted_curves[[2]][[i]], col = "orange", lwd = 3)
  lines(predicted_curves[[3]][[i]], col = "blue", lwd = 3)
  lines(predicted_curves[[4]][[i]], col = "green", lwd = 3)
  #lines(predicted_curves[[5]][[i]], col = "purple", lwd = 3)
}, 
i = slider(1,11))
################################################################################
set.seed(123)
fish_data <- draw_regression_data(type = 1, x = x, sd_2 = 0.1, m_range = NULL)
manipulate({plot(fish_data[[i]], type = "b", asp = 1, xlim = c(-5, 5), lwd = 3)}, 
           i = slider(1,11))

pre_align_curve_model <- fit_elastic_curve_regression(fish_data, x, knots = seq(0,1, by = 0.1), max_iter = 0)
predicted_curves <- predict_model(pre_align_curve_model, x)
                        

manipulate({plot(fish_data[[i]], type = "b", asp = 1, xlim = c(-5,5), lwd = 3)
  lines(predicted_curves[[i]], col = "green", lwd = 3)
}, 
i = slider(1,11))
################################################################################
fish_data <- draw_regression_data(type = 2, x = x, sd = 0.2)
manipulate({plot(fish_data[[i]], type = "b", asp = 1, xlim = c(-5, 5), lwd = 3)}, 
i = slider(1,11))

quotient_reg_model <- fit_quotient_regression(fish_data, x, knots = seq(0,1, by = 0.01), type = "discrete")
pre_align_model <- fit_quotient_regression(fish_data, x, knots = seq(0,1, by = 0.01), type = "discrete", max_iter = 1)
elastic_curve_model <- fit_elastic_curve_regression(fish_data, x, knots = seq(0,1, by = 0.01), type = "discrete")
pre_align_curve_model <- fit_elastic_curve_regression(fish_data, x, knots = seq(0,1, by = 0.01), 
                                                      type = "discrete", max_iter = 1)

predicted_curves <- list("quotient_reg" = predict_srv_model(quotient_reg_model, x),
                         "pre_align" = predict_srv_model(pre_align_model, x),
                         "elastic_curve" = predict_model(elastic_curve_model, x),
                         "pre_align_curve" = predict_model(pre_align_curve_model, x))

manipulate({plot(fish_data[[i]], type = "b", asp = 1, xlim = c(-3,3), lwd = 3)
  lines(predicted_curves[[1]][[i]], col = "red", lwd = 3)
  lines(predicted_curves[[2]][[i]], col = "orange", lwd = 3)
  lines(predicted_curves[[3]][[i]], col = "blue", lwd = 3)
  lines(predicted_curves[[4]][[i]], col = "green", lwd = 3)
}, 
i = slider(1,11))

################################################################################
curve_data <- draw_regression_data(type = 13, sd_2 = 0.1, m_range = 15:20, t_grid = seq(0,1, length = 20))
manipulate({plot(curve_data[[i]], type = "b", asp = 1, xlim = c(-5, 5), lwd = 3)}, 
           i = slider(1,11))

# fit all four models
quotient_reg_model <- fit_quotient_regression(curve_data, x, knots = seq(0,1, by = 0.1))
elastic_curve_model <- fit_elastic_curve_regression(curve_data, x, knots = seq(0,1, by = 0.1))
pre_align_model <- fit_quotient_regression(curve_data, x, knots = seq(0,1, by = 0.1), max_iter = 0)
pre_align_curve_model <- fit_elastic_curve_regression(curve_data, x, knots = seq(0,1, by = 0.1), max_iter = 0)

predicted_curves <- list("quotient_reg" = predict_srv_model(quotient_reg_model, x),
                         "pre_align" = predict_srv_model(pre_align_model, x),
                         "elastic_curve" = predict_model(elastic_curve_model, x),
                         "pre_align_curve" = predict_model(pre_align_curve_model, x))

manipulate({plot(curve_data[[i]], type = "b", asp = 1, xlim = c(-3,3), lwd = 3, ylim = c(-3, 3))
  lines(predicted_curves[[1]][[i]], col = "red", lwd = 3)
  lines(predicted_curves[[2]][[i]], col = "orange", lwd = 3)
  lines(predicted_curves[[3]][[i]], col = "blue", lwd = 3)
  lines(predicted_curves[[4]][[i]], col = "green", lwd = 3)
}, 
i = slider(1,11))

################################################################################
curve_data <- draw_regression_data(type = 4, sd = 0.4, m_range = 20:25, t_grid = seq(0,1, length = 25))
manipulate({plot(curve_data[[i]], type = "b", asp = 1, xlim = c(-3, 3), lwd = 3)}, 
           i = slider(1,11))

# fit all four models
quotient_reg_model <- fit_quotient_regression(curve_data, x, knots = seq(0,1, by = 0.02), type = "discrete")
elastic_curve_model <- fit_elastic_curve_regression(curve_data, x, knots = seq(0,1, by = 0.02), type = "discrete")
pre_align_model <- fit_quotient_regression(curve_data, x, knots = seq(0,1, by = 0.02), type = "discrete", max_iter = 1)
pre_align_curve_model <- fit_elastic_curve_regression(curve_data, x, knots = seq(0,1, by = 0.02), type = "discrete", max_iter = 1)

predicted_curves <- list("quotient_reg" = predict_srv_model(quotient_reg_model, x),
                         "pre_align" = predict_srv_model(pre_align_model, x),
                         "elastic_curve" = predict_model(elastic_curve_model, x),
                         "pre_align_curve" = predict_model(pre_align_curve_model, x))

manipulate({plot(curve_data[[i]], type = "b", asp = 1, xlim = c(-3,3), lwd = 3)
  lines(predicted_curves[[1]][[i]], col = "red", lwd = 3)
  lines(predicted_curves[[2]][[i]], col = "orange", lwd = 3)
  lines(predicted_curves[[3]][[i]], col = "blue", lwd = 3)
  lines(predicted_curves[[4]][[i]], col = "green", lwd = 3)
}, 
i = slider(1,11))
