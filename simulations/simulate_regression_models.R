library(parallel)
source("draw_regression_data.R")
source("../fit_quotient_regression.R")
source("../fit_elastic_curve_regression.R")
source("../fit_frechet_regression.R")

################################################################################
# sample data for different regression settings
################################################################################
simulate_models <- function(n, x = seq(-1,1,0.2), data_type = 1, sd = 0, sd_2 = 0, m_range = NULL, 
                            t_grid = seq(0,1, 0.02), type = "smooth", knots = seq(0,1, by = 0.1), 
                            n_reps = 5, model_number = 1){
  #parallel computations
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)
  clusterExport(cl, c("draw_regression_data", "get_discrete_end_curves", "sample_sparse_data", "x",
                      "fit_quotient_regression", "fit_elastic_curve_regression", "get_model_data",
                      "make_curve_design", "make_design", "get_curve_model_data", "predict_srv_model",
                      "predict_model", "fit_frechet_regression", "predict_frechet_model", 
                      "get_frechet_model_data", "make_frechet_design", "x", "sd", "m_range", 
                      "model_number", "get_mse_estimates", "data_type", "knots", "t_grid"), envir=environment())
  clusterEvalQ(cl, library(elasdics))
  
  regression_models <- parLapply(cl, 1:n, function(i){
    seed <- 100 + i
    set.seed(seed)
    sample_data <- draw_regression_data(type = data_type, x = x, sd = sd, sd_2 = sd_2, m_range = m_range,
                                        t_grid = t_grid)
    attr(sample_data, "seed") <- seed
    attr(sample_data, "x") <- x
    attr(sample_data, "sd") <- sd
    attr(sample_data, "m_range") <- m_range
    
    quotient_reg_model <- try(fit_quotient_regression(sample_data, x, knots = knots, type = type))
    pre_align_model <- try(fit_quotient_regression(sample_data, x, knots = knots, type = type, max_iter = 0))
    elastic_curve_model <- try(fit_elastic_curve_regression(sample_data, x, knots = knots, type = type))
    pre_align_curve_model <- try(fit_elastic_curve_regression(sample_data, x, knots = knots, type = type, max_iter = 0))
    
    list(sample_data, quotient_reg_model, pre_align_model, elastic_curve_model, pre_align_curve_model)
  })
  saveRDS(regression_models, file = paste0("../simulation_data/regression_models_", model_number))
  
  mse_estimates <- sapply(1:n_reps, function(i){
    parLapply(cl, regression_models, function(models){
      sample_data <- draw_regression_data(type = data_type, x = x, sd = sd, sd_2 = sd_2, m_range = m_range,
                                          t_grid = t_grid)
      try(get_mse_estimates(models, sample_data))
    })
  })
  
  saveRDS(mse_estimates, file = paste0("../simulation_data/mse_estimates_", model_number))
  #stop parallel computing
  stopCluster(cl)
}

################################################################################
get_mse_estimates <- function(regression_model, sample_data){
  t_grid <- seq(0,1,0.01)
  model_curve_1 <- predict_srv_model(regression_model[[2]], x, t_grid = t_grid)
  model_curve_2 <- predict_srv_model(regression_model[[3]], x, t_grid = t_grid)
  model_curve_3 <- predict_model(regression_model[[4]], x, t_grid = t_grid)
  model_curve_4 <- predict_model(regression_model[[5]], x, t_grid = t_grid)
  
  list(sample_data,
       c(mean(sapply(1:length(x), function(i) align_curves(model_curve_1[[i]], sample_data[[i]])$elastic_dist)^2),
         mean(sapply(1:length(x), function(i) align_curves(model_curve_2[[i]], sample_data[[i]])$elastic_dist)^2),
         mean(sapply(1:length(x), function(i) align_curves(model_curve_3[[i]], sample_data[[i]])$elastic_dist)^2),
         mean(sapply(1:length(x), function(i) align_curves(model_curve_4[[i]], sample_data[[i]])$elastic_dist)^2)))
}
