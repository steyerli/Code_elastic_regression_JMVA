source("simulate_regression_models.R")
################################################################################
# do the simulation, takes very long!!!!
# Do NOT RUN!

simulate_models(n = 30, sd = 0.4, m_range = 15:20, t_grid = seq(0,1, length = 25))

simulate_models(n = 30, sd = 0.4, m_range = 15:20, t_grid = seq(0,1, length = 25), 
                type = "discrete", knots = seq(0,1, 0.01), model_number = 2)

simulate_models(n = 30, sd = 0.2, m_range = 20:30, model_number = 3, data_type = 4)

simulate_models(n = 30, sd = 0.3, m_range = 25:35, model_number = 4, data_type = 4,
                type = "discrete", knots = seq(0,1, 0.01))

simulate_models(n = 30, sd = 0.3, m_range = 25:35, model_number = 5, data_type = 14,
                type = "discrete", knots = seq(0,1, 0.01))

simulate_models(n = 30, sd_2 = 0.1,  m_range = 15:20, t_grid = seq(0,1, length = 20), 
                model_number = 6, data_type = 13)

simulate_models(n = 30, sd_2 = 0.01, m_range = NULL, t_grid = seq(0,1, length = 40), 
                model_number = 7, data_type = 13)

simulate_models(n = 30, sd_2 = 0.01, m_range = NULL, t_grid = seq(0,1, length = 40), 
                model_number = 8, data_type = 13,
                type = "discrete", knots = seq(0,1, 0.02))

simulate_models(n = 30,  sd_2 = 0.1, m_range = NULL, t_grid = seq(0,1, length = 40), 
                model_number = 9, data_type = 1,
                type = "discrete", knots = seq(0,1, by = 0.02))

simulate_models(n = 30,  sd_2 = 0.1, m_range = NULL, t_grid = seq(0,1, length = 40), 
                model_number = 10, data_type = 1)
################################################################################
source("plot_regression_models.R")

regression_models <- readRDS("../simulation_data/regression_models_1")
mse_estimates <- readRDS("../simulation_data/mse_estimates_1")
plot_regression_models(regression_models, mse_estimates, k = 1)

regression_models <- readRDS("../simulation_data/regression_models_2")
mse_estimates <- readRDS("../simulation_data/mse_estimates_2")
plot_regression_models(regression_models, mse_estimates, k = 1)

regression_models <- readRDS("../simulation_data/regression_models_3")
mse_estimates <- readRDS("../simulation_data/mse_estimates_3")
plot_regression_models(regression_models, mse_estimates, k = 1)

regression_models <- readRDS("../simulation_data/regression_models_4")
mse_estimates <- readRDS("../simulation_data/mse_estimates_4")
plot_regression_models(regression_models, mse_estimates, k = 1)

regression_models <- readRDS("../simulation_data/regression_models_5")
mse_estimates <- readRDS("../simulation_data/mse_estimates_5")
plot_regression_models(regression_models, mse_estimates, k = 1)

regression_models <- readRDS("../simulation_data/regression_models_6")
mse_estimates <- readRDS("../simulation_data/mse_estimates_6")
plot_regression_models(regression_models, mse_estimates, k = 1)

regression_models <- readRDS("../simulation_data/regression_models_7")
mse_estimates <- readRDS("../simulation_data/mse_estimates_7")
plot_regression_models(regression_models, mse_estimates, k = 1)

regression_models <- readRDS("../simulation_data/regression_models_8")
mse_estimates <- readRDS("../simulation_data/mse_estimates_8")
plot_regression_models(regression_models, mse_estimates, k = 1)

regression_models <- readRDS("../simulation_data/regression_models_9")
mse_estimates <- readRDS("../simulation_data/mse_estimates_9")
plot_regression_models(regression_models, mse_estimates, k = 2)

regression_models <- readRDS("../simulation_data/regression_models_10")
mse_estimates <- readRDS("../simulation_data/mse_estimates_10")
plot_regression_models(regression_models, mse_estimates, k = 1)
