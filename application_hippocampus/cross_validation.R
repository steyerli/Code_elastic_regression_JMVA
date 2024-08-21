library(parallel)
library(caret)

data_curves_left <- readRDS("data_curves_left.RDS")
data_curves_right <- readRDS("data_curves_right.RDS")
meta_data <- read.csv("meta_data_1.csv")
meta_data$Group <- factor(meta_data$Group)
################################################################################
#  compute elastic distance to prediction
get_dist_to_prediction <- function(reg_model, data_curve, x_data){
  srv_data <- get_srv_from_points(data_curve)
  
  x_data <- x_data[, all.vars(reg_model$formula[[3]]), drop = FALSE]
  x_data_old <- reg_model$x_data[, all.vars(reg_model$formula[[3]]), drop = FALSE]
  # create x model matrix
  if(ncol(x_data_old) == 0){
    x_design <- matrix(1)
  } else {
    x_design <- model.matrix(reg_model$formula, cbind("data_curves" = 1, rbind(x_data, x_data_old)))
    x_design <- x_design[1:nrow(x_data),,drop = FALSE]
  }
  pred_coefs <- Reduce("+", lapply(1:length(x_design), function(j){
    x_design[j]*reg_model$coefs_list[[j]]
  }))
  srv_curve <- function(t){
    t(elasdics:::make_reg_design(x_design_mat = NULL, t = t, type = "smooth",
                                 knots = reg_model$knots, closed = reg_model$closed) %*% pred_coefs)
  }
  t_grid <- seq(0,1,0.005)
  pred_curve <- predict(reg_model, newdata = x_data, t_grid = t_grid)
  initial_t <- align_curves(data.frame("t" = t_grid, pred_curve), 
                            data_curve)$data_curve2_aligned$t_optim 
  
  optimal_t <- elasdics:::find_optimal_t(srv_curve = srv_curve, s = c(srv_data[,1],1),
                                         q = t(srv_data[,-1]), eps = 0.01, initial_t = initial_t)
  attributes(optimal_t)$dist
}

################################################################################
# prediction error for full model
###############################################
my_seed_list <- 1230:1239
sapply(my_seed_list, function(my_seed){
  print(my_seed)
  set.seed(my_seed)
  folds_ids <- createFolds(1:length(data_curves_left), k = 10)
  
  #parallel computations
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)
  clusterExport(cl, c("data_curves_left", "data_curves_right", "meta_data", "folds_ids",
                      "my_seed", "get_dist_to_prediction"), envir=environment())
  clusterEvalQ(cl, library(elasdics))
  
  parLapply(cl, 1:length(folds_ids), function(i){
    is_training <- !(1:length(data_curves_left) %in% folds_ids[[i]])
    
    reg_model_left <- try(fit_elastic_regression(data_curves ~ Age + Group + Sex, 
                                                 data_curves = data_curves_left[is_training], 
                                                 x_data = meta_data[is_training,], knots = seq(0,1,0.05)))
    reg_model_right <- try(fit_elastic_regression(data_curves ~ Age + Group + Sex, 
                                                  data_curves = data_curves_right[is_training], 
                                                  x_data = meta_data[is_training,], knots = seq(0,1,0.05)))
    dists <- sapply(folds_ids[[i]], function(idx){
      c(get_dist_to_prediction(reg_model_left, data_curves_left[[idx]], meta_data[idx,]),
        get_dist_to_prediction(reg_model_right, data_curves_right[[idx]], meta_data[idx,]))
    })
    
    reg_model <- list(reg_model_left, reg_model_right, "dists" = dists, test_idx = folds_ids[[i]])
    
    saveRDS(reg_model, file = paste0("cv_models/reg_model_full_", my_seed, "_", i))
  })
  
  stopCluster(cl)
  
  ################################################################################
  # prediction error for model without sex effect
  ###############################################
  set.seed(my_seed)
  folds_ids <- createFolds(1:length(data_curves_left), k = 10)
  
  #parallel computations
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)
  clusterExport(cl, c("data_curves_left", "data_curves_right", "meta_data", "folds_ids",
                      "my_seed", "get_dist_to_prediction"), envir=environment())
  clusterEvalQ(cl, library(elasdics))
  
  parLapply(cl, 1:length(folds_ids), function(i){
    is_training <- !(1:length(data_curves_left) %in% folds_ids[[i]])
    
    reg_model_left <- try(fit_elastic_regression(data_curves ~ Age + Group, 
                                                 data_curves = data_curves_left[is_training], 
                                                 x_data = meta_data[is_training,], knots = seq(0,1,0.05)))
    reg_model_right <- try(fit_elastic_regression(data_curves ~ Age + Group, 
                                                  data_curves = data_curves_right[is_training], 
                                                  x_data = meta_data[is_training,], knots = seq(0,1,0.05)))
    
    dists <- sapply(folds_ids[[i]], function(idx){
      c(get_dist_to_prediction(reg_model_left, data_curves_left[[idx]], meta_data[idx,]),
        get_dist_to_prediction(reg_model_right, data_curves_right[[idx]], meta_data[idx,]))
    })
    
    reg_model <- list(reg_model_left, reg_model_right, "dists" = dists, test_idx = folds_ids[[i]])
    
    saveRDS(reg_model, file = paste0("cv_models/reg_model_no_sex_", my_seed, "_", i))
  })
  
  stopCluster(cl)
  
  ################################################################################
  # prediction error for model without group effect
  ###############################################
  set.seed(my_seed)
  folds_ids <- createFolds(1:length(data_curves_left), k = 10)
  
  #parallel computations
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)
  clusterExport(cl, c("data_curves_left", "data_curves_right", "meta_data", "folds_ids",
                      "my_seed", "get_dist_to_prediction"), envir=environment())
  clusterEvalQ(cl, library(elasdics))
  
  parLapply(cl, 1:length(folds_ids), function(i){
    is_training <- !(1:length(data_curves_left) %in% folds_ids[[i]])
    
    reg_model_left <- try(fit_elastic_regression(data_curves ~ Age + Sex, 
                                                 data_curves = data_curves_left[is_training], 
                                                 x_data = meta_data[is_training,], knots = seq(0,1,0.05)))
    reg_model_right <- try(fit_elastic_regression(data_curves ~ Age + Sex, 
                                                  data_curves = data_curves_right[is_training], 
                                                  x_data = meta_data[is_training,], knots = seq(0,1,0.05)))
    dists <- sapply(folds_ids[[i]], function(idx){
      c(get_dist_to_prediction(reg_model_left, data_curves_left[[idx]], meta_data[idx,]),
        get_dist_to_prediction(reg_model_right, data_curves_right[[idx]], meta_data[idx,]))
    })
    
    reg_model <- list(reg_model_left, reg_model_right, "dists" = dists, test_idx = folds_ids[[i]])
    
    saveRDS(reg_model, file = paste0("cv_models/reg_model_no_group_", my_seed, "_", i))
  })
  
  stopCluster(cl)
  
  ################################################################################
  # prediction error for model without age effect
  ###############################################  
  set.seed(my_seed)
  folds_ids <- createFolds(1:length(data_curves_left), k = 10)
  
  #parallel computations
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)
  clusterExport(cl, c("data_curves_left", "data_curves_right", "meta_data", "folds_ids",
                      "my_seed", "get_dist_to_prediction"), envir=environment())
  clusterEvalQ(cl, library(elasdics))
  
  parLapply(cl, 1:length(folds_ids), function(i){
    is_training <- !(1:length(data_curves_left) %in% folds_ids[[i]])
    
    reg_model_left <- try(fit_elastic_regression(data_curves ~ Group + Sex, 
                                                 data_curves = data_curves_left[is_training], 
                                                 x_data = meta_data[is_training,], knots = seq(0,1,0.05)))
    reg_model_right <- try(fit_elastic_regression(data_curves ~ Group + Sex, 
                                                  data_curves = data_curves_right[is_training], 
                                                  x_data = meta_data[is_training,], knots = seq(0,1,0.05)))
    dists <- sapply(folds_ids[[i]], function(idx){
      c(get_dist_to_prediction(reg_model_left, data_curves_left[[idx]], meta_data[idx,]),
        get_dist_to_prediction(reg_model_right, data_curves_right[[idx]], meta_data[idx,]))
    })
    
    reg_model <- list(reg_model_left, reg_model_right, "dists" = dists, test_idx = folds_ids[[i]])
    
    saveRDS(reg_model, file = paste0("cv_models/reg_model_no_age_", my_seed, "_", i))
  })
  
  stopCluster(cl)
})
