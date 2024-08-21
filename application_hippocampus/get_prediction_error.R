library(elasdics)
library(parallel)

data_curves_left <- readRDS("data_curves_left.RDS")
data_curves_right <- readRDS("data_curves_right.RDS")
meta_data <- read.csv("meta_data_1.csv")
meta_data$Group <- factor(meta_data$Group)
################################################################################
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
get_mse <- function(name){
  file_list <- list.files(paste0("bootstrap_models", name, "/"))
  #parallel computations
  n_cores <- detectCores() - 1
  cl <- makeCluster(n_cores)
  clusterExport(cl, c("data_curves_left", "data_curves_right", "meta_data", "file_list",
                      "get_dist_to_prediction", "name"), envir=environment())
  clusterEvalQ(cl, library(elasdics))
  
  parLapply(cl, file_list, function(file) {
    model_id <- as.numeric(strsplit(file, "_")[[1]][3])
    set.seed(model_id)
    idx_train <-
      sample(1:length(data_curves_left),
             length(data_curves_left),
             replace = TRUE)
    idx_test <- setdiff(1:length(data_curves_left), idx_train)
    reg_model <- readRDS(paste0("bootstrap_models", name, "/", file))
    dists_left <- sapply(idx_test, function(idx) {
      get_dist_to_prediction(reg_model[[1]], data_curve = data_curves_left[[idx]],
                             meta_data[idx, ])
    })
    dists_right <- sapply(idx_test, function(idx) {
      get_dist_to_prediction(reg_model[[2]], data_curve = data_curves_right[[idx]],
                             meta_data[idx, ])
    })
    mse <- dists_left^2 + dists_right^2
    saveRDS(mse, file = paste0("bootstrap_mse", name, "/mse_", model_id))
  })
  stopCluster(cl)
}

get_mse("")
get_mse("_no_age")
get_mse("_no_group")
get_mse("_no_sex")





