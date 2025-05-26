library(parallel)
source("draw_regression_data.R")
source("fit_elastic_curve.R")
source("fit_frechet_regression.R")

################################################################################
# sample data for different regression settings
################################################################################
simulate_models <- function(n, x = seq(-1,1,0.2), data_type = 1, sd = 0, sd_2 = 0, m_range = NULL,
                            type = "smooth", knots = seq(0,1, by = 0.1),
                            n_reps = 5, model_number = 1, pre_align = FALSE){
  closed <- data_type == 3
  #parallel computations
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)
  clusterExport(cl, c("draw_regression_data", "get_discrete_end_curves", "sample_sparse_data", "x",
                      "fit_elastic_curve", "make_curve_design", "get_curve_model_data",
                      "predict_elastic_curve", "fit_frechet_regression", "predict_frechet_model",
                      "get_frechet_model_data", "make_frechet_design", "x", "sd", "m_range",
                      "model_number", "get_mse_estimates", "data_type", "knots",
                      "pre_align", "closed"), envir=environment())
  clusterEvalQ(cl, library(elasdics))

  parLapply(cl, 1:n, function(i){
    seed <- 100 + i
    set.seed(seed)
    sample_data <- draw_regression_data(type = data_type, x = x, sd = sd, sd_2 = sd_2, m_range = m_range)
    attr(sample_data, "seed") <- seed
    attr(sample_data, "x") <- x
    x_data <- data.frame("x" = x)
    attr(sample_data, "sd") <- sd
    attr(sample_data, "m_range") <- m_range

    run_time <- system.time(quotient_reg_model <- try(fit_elastic_regression(data_curves ~ x, data_curves = sample_data,
                                                     x_data = x_data, knots = knots, type = type,
                                                     pre_align = pre_align, closed = closed)))
    attr(quotient_reg_model, "run_time") <- run_time
    run_time <- system.time(pre_align_model <- try(fit_elastic_regression(data_curves ~ x, data_curves = sample_data, type = type,
                                                  x_data = x_data, pre_align = TRUE, max_iter = 0,
                                                  knots = knots, closed = closed)))
    attr(pre_align_model, "run_time") <- run_time 
    run_time <- system.time(elastic_curve_model <- try(fit_elastic_curve(sample_data, x, knots = knots, type = type,
                                                 pre_align = pre_align, closed = closed)))
    attr(elastic_curve_model, "run_time") <- run_time 
    run_time <- system.time(pre_align_curve_model <- try(fit_elastic_curve(sample_data, x, knots = knots, type = type,
                                                   max_iter = 0, pre_align = TRUE, closed = closed)))
    attr(pre_align_curve_model, "run_time") <- run_time 
    run_time <- system.time(frechet_model <- try(fit_frechet_regression(x_new = x, sample_data, x = x, knots = knots,
                                                type = type, pre_align = pre_align, closed = closed)))
    attr(frechet_model, "run_time") <- run_time 

    regression_models <- list(quotient_reg_model, pre_align_model, elastic_curve_model, pre_align_curve_model,
         frechet_model)
    
    mse_estimates <- sapply(1:n_reps, function(i){
      print(i)
      test_data <- draw_regression_data(type = data_type, x = x, sd = sd,
                                        sd_2 = sd_2, m_range = m_range)
      try(get_mse_estimates(regression_models, test_data, x, closed = closed))
    })
    
    regression_models <- list("sample_data" = sample_data, "mse_estimates" = mse_estimates,
                              "regression_models" = regression_models)
    
    saveRDS(regression_models, file = paste0("simulation_data/regression_models_", model_number, "_", seed))
  })
  
  #stop parallel computing
  stopCluster(cl)
}

################################################################################
get_mse_estimates <- function(regression_model, sample_data, x, closed){
  x_data = data.frame("x" = x)
  t_grid <- seq(0,1,0.01)
  model_curve_1 <- predict(regression_model[[1]], x_data, t_grid = t_grid)
  model_curve_2 <- predict(regression_model[[2]], x_data, t_grid = t_grid)
  model_curve_3 <- predict_elastic_curve(regression_model[[3]], x, t_grid = t_grid)
  model_curve_4 <- predict_elastic_curve(regression_model[[4]], x, t_grid = t_grid)
  model_curve_5 <- predict_frechet_model(regression_model[[5]], t_grid = t_grid)
  
  c(mean(sapply(1:length(x), function(i) align_curves(model_curve_1[[i]], sample_data[[i]])$elastic_dist)^2),
    mean(sapply(1:length(x), function(i) align_curves(model_curve_2[[i]], sample_data[[i]])$elastic_dist)^2),
    mean(sapply(1:length(x), function(i) align_curves(model_curve_3[[i]], sample_data[[i]])$elastic_dist)^2),
    mean(sapply(1:length(x), function(i) align_curves(model_curve_4[[i]], sample_data[[i]])$elastic_dist)^2),
    mean(sapply(1:length(x), function(i) align_curves(model_curve_5[[i]], sample_data[[i]])$elastic_dist)^2))
}
