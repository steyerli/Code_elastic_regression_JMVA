library(ggplot2)
library(manipulate)
library(elasdics)
source("draw_regression_data.R")
source("fit_elastic_curve.R")
source("fit_frechet_regression.R")

x <- -5:5/5
x_data <- data.frame("x" = x)
################################################################################
fish_data <- draw_regression_data(type = 1, x = x, sd = 0.5, m_range = 15:20)
manipulate({plot(fish_data[[i]], type = "b", asp = 1, xlim = c(-5, 5), lwd = 3)},
           i = slider(1,11))

quotient_reg_model <- fit_elastic_regression(data_curves ~ x, data_curves = fish_data,
                                             x_data = x_data, knots = seq(0,1, by = 0.1),
                                             pre_align = TRUE)
pre_align_model <- fit_elastic_regression(data_curves ~ x, data_curves = fish_data,
                                          x_data = x_data, knots = seq(0,1, by = 0.1), max_iter = 0,
                                          pre_align = TRUE)
elastic_curve_model <- fit_elastic_curve(fish_data, x, knots = seq(0,1, by = 0.1),
                                         pre_align = TRUE)
pre_align_curve_model <- fit_elastic_curve(fish_data, x, knots = seq(0,1, by = 0.1), max_iter = 0,
                                           pre_align = TRUE)
frechet_model <- fit_frechet_regression(x_new = x, fish_data, x = x, knots = seq(0,1, by = 0.1),
                                        type = "smooth", pre_align = TRUE)

predicted_curves <- list("quotient_reg" = predict(quotient_reg_model, x_data),
                         "pre_align" = predict(pre_align_model, x_data),
                         "elastic_curve" = predict_elastic_curve(elastic_curve_model, x),
                         "pre_align_curve" = predict_elastic_curve(pre_align_curve_model, x),
                         "frechet_model" = predict_frechet_model(frechet_model)
                         )

manipulate({plot(fish_data[[i]], type = "b", asp = 1, xlim = c(-5,5), lwd = 3)
  lines(predicted_curves[[1]][[i]], col = "red", lwd = 3)
  lines(predicted_curves[[2]][[i]], col = "orange", lwd = 3)
  lines(predicted_curves[[3]][[i]], col = "blue", lwd = 3)
  lines(predicted_curves[[4]][[i]], col = "green", lwd = 3)
  lines(predicted_curves[[5]][[i]], col = "purple", lwd = 3)
},
i = slider(1,11))
################################################################################
curve_data <- draw_regression_data(type = 2, sd = 0.2, m_range = 30:40)
manipulate({plot(curve_data[[i]], type = "b", asp = 1, xlim = c(-6, 6), lwd = 3)},
           i = slider(1,11))

# fit all four models
quotient_reg_model <- fit_elastic_regression(data_curves ~ x, data_curves = curve_data,
                                             x_data = x_data, knots = seq(0,1, by = 0.02),
                                             type = "polygon", eps = 0.001)
pre_align_model <- fit_elastic_regression(data_curves ~ x, data_curves = curve_data,
                                          x_data = x_data, knots = seq(0,1, by = 0.02),
                                          max_iter = 0, type = "polygon", pre_align = TRUE)
elastic_curve_model <- fit_elastic_curve(curve_data, x, knots = seq(0,1, by = 0.02),
                                         type = "polygon")
pre_align_curve_model <- fit_elastic_curve(curve_data, x, knots = seq(0,1, by = 0.02),
                                           max_iter = 0, type = "smooth", pre_align = TRUE)
frechet_model <- fit_frechet_regression(x_new = x, curve_data, x = x, knots = seq(0,1, by = 0.02),
                                        type = "polygon", eps = 0.001)

predicted_curves <- list("quotient_reg" = predict(quotient_reg_model, x_data),
                         "pre_align" = predict(pre_align_model, x_data),
                         "elastic_curve" = predict_elastic_curve(elastic_curve_model, x),
                         "pre_align_curve" = predict_elastic_curve(pre_align_curve_model, x),
                         "frechet_model" = predict_frechet_model(frechet_model)
)

manipulate({plot(curve_data[[i]], type = "b", asp = 1, ylim = c(-3,3), xlim = c(-5,5), lwd = 3)
  lines(predicted_curves[[1]][[i]], col = "red", lwd = 3)
  lines(predicted_curves[[2]][[i]], col = "orange", lwd = 3)
  lines(predicted_curves[[3]][[i]], col = "blue", lwd = 3)
  lines(predicted_curves[[4]][[i]], col = "green", lwd = 3)
  lines(predicted_curves[[5]][[i]], col = "purple", lwd = 3)
},
i = slider(1,11))
################################################################################
curve_data <- draw_regression_data(type = 3, sd_2 = 0, m_range = 15:20)
manipulate({plot(curve_data[[i]], type = "b", asp = 1, xlim = c(-4, 4), lwd = 3)},
           i = slider(1,11))

# fit all five models
quotient_reg_model <- fit_elastic_regression(data_curves ~ x, data_curves = curve_data,
                                             x_data = x_data, knots = seq(0,1, by = 0.05),
                                             type = "smooth", eps = 0.001, closed = TRUE,
                                             pre_align = TRUE)
pre_align_model <- fit_elastic_regression(data_curves ~ x, data_curves = curve_data,
                                          x_data = x_data, knots = seq(0,1, by = 0.05),
                                          max_iter = 0, type = "smooth", pre_align = TRUE,
                                          closed = TRUE)
elastic_curve_model <- fit_elastic_curve(curve_data, x, knots = seq(0,1, by = 0.05),
                                         type = "smooth", closed = TRUE, pre_align = TRUE)
pre_align_curve_model <- fit_elastic_curve(curve_data, x, knots = seq(0,1, by = 0.05),
                                           max_iter = 0, type = "smooth", pre_align = TRUE,
                                           closed = TRUE)
frechet_model <- fit_frechet_regression(x_new = x, curve_data, x = x, knots = seq(0,1, by = 0.05),
                                        type = "smooth", eps = 0.001, closed = TRUE,
                                        pre_align = TRUE)

predicted_curves <- list("quotient_reg" = predict(quotient_reg_model, x_data),
                         "pre_align" = predict(pre_align_model, x_data),
                         "elastic_curve" = predict_elastic_curve(elastic_curve_model, x),
                         "pre_align_curve" = predict_elastic_curve(pre_align_curve_model, x),
                         "frechet_model" = predict_frechet_model(frechet_model)
)

manipulate({plot(curve_data[[i]], type = "b", asp = 1, lwd = 3)
  lines(predicted_curves[[1]][[i]], col = "red", lwd = 3)
  lines(predicted_curves[[2]][[i]], col = "orange", lwd = 3)
  lines(predicted_curves[[3]][[i]], col = "blue", lwd = 3)
  lines(predicted_curves[[4]][[i]], col = "green", lwd = 3)
  lines(predicted_curves[[5]][[i]], col = "purple", lwd = 3)
},
i = slider(1,11))
