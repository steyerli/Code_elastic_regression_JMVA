source("simulate_regression_models.R")
################################################################################
# do the simulation, takes very long!!!!
# Do NOT RUN!

simulate_models(n = 100, sd = 0.4, data_type = 1, m_range = 15:20,
                model_number = 1, pre_align = TRUE)
simulate_models(n = 100, sd = 0.8, data_type = 1, m_range = 15:20,
                model_number = 2, pre_align = TRUE)
simulate_models(n = 100, sd = 0.4, data_type = 1, m_range = 30:40,
                model_number = 3, pre_align = TRUE)
simulate_models(n = 100, sd = 0.8, data_type = 1, m_range = 30:40,
                model_number = 4, pre_align = TRUE)
simulate_models(n = 100, sd = 0.2, data_type = 2, m_range = 15:20, type = "polygon",
                model_number = 5, pre_align = FALSE, knots = seq(0,1, by = 0.02))
simulate_models(n = 100, sd = 0.4, data_type = 2, m_range = 15:20, type = "polygon",
                model_number = 6, pre_align = FALSE, knots = seq(0,1, by = 0.02))
simulate_models(n = 100, sd = 0.2, data_type = 2, m_range = 30:40, type = "polygon",
                model_number = 7, pre_align = FALSE, knots = seq(0,1, by = 0.02))
simulate_models(n = 100, sd = 0.4, data_type = 2, m_range = 30:40, type = "polygon",
                model_number = 8, pre_align = FALSE, knots = seq(0,1, by = 0.02))
simulate_models(n = 100, sd_2 = 0.1, data_type = 3, m_range = 15:20, type = "smooth",
                model_number = 9, pre_align = FALSE, knots = seq(0,1, by = 0.05))
simulate_models(n = 100, sd_2 = 0.2, data_type = 3, m_range = 15:20, type = "smooth",
                model_number = 10, pre_align = FALSE, knots = seq(0,1, by = 0.05))
simulate_models(n = 100, sd_2 = 0.1, data_type = 3, m_range = 30:40, type = "smooth",
                model_number = 11, pre_align = FALSE, knots = seq(0,1, by = 0.05))
simulate_models(n = 100, sd_2 = 0.2, data_type = 3, m_range = 30:40, type = "smooth",
                model_number = 12, pre_align = FALSE, knots = seq(0,1, by = 0.05))

################################################################################
source("../plot_regression_models.R")
source("fit_elastic_curve.R")
source("fit_frechet_regression.R")

regression_models <- readRDS("simulation_data/regression_models_1")
mse_estimates <- readRDS("simulation_data/mse_estimates_1")
plot_regression_models(regression_models, mse_estimates, k = 1)

regression_models <- readRDS("simulation_data/regression_models_5")
mse_estimates <- readRDS("simulation_data/mse_estimates_5")
plot_regression_models(regression_models, mse_estimates, k = 1)

regression_models <- readRDS("simulation_data/regression_models_9")
mse_estimates <- readRDS("simulation_data/mse_estimates_9")
plot_regression_models(regression_models, mse_estimates, k = 3)
