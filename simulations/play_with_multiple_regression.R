library(manipulate)
source("draw_regression_data.R")
source("../fit_quotient_regression.R")

################################################################################
x_data <- data.frame(x1 = -5:5/5, x2 = sample(c(0,1), size = 11, replace = TRUE))
fish_data <- draw_regression_data(type = 1, x = x_data[,1], sd = 0.4, m_range = 15:20, t_grid = seq(0,1, length = 25))
manipulate({plot(fish_data[[i]], type = "b", asp = 1, xlim = c(-5, 5), lwd = 3)}, 
           i = slider(1,11))

quotient_reg_model <- fit_quotient_regression(data_curves ~ x1*x2, data_curves = fish_data, x_data = x_data, 
                                              knots = seq(0,1, by = 0.1))

x_new <- x_data
x_new[,2] <- 1
predicted_curves <- predict_srv_model(quotient_reg_model, x_data)
predicted_curves_new <- predict_srv_model(quotient_reg_model, x_new)

manipulate({plot(fish_data[[i]], type = "b", asp = 1, xlim = c(-5,5), lwd = 3)
  lines(predicted_curves[[i]], col = "red", lwd = 3)
  lines(predicted_curves_new[[i]], col = "blue", lwd = 3)
}, 
i = slider(1,11))
