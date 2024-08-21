library(parallel)

data_curves_left <- readRDS("data_curves_left.RDS")
data_curves_right <- readRDS("data_curves_right.RDS")
meta_data <- read.csv("meta_data_1.csv")
meta_data$Group <- factor(meta_data$Group)
################################################################################

#parallel computations
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
clusterExport(cl, c("data_curves_left", "data_curves_right", "meta_data"), envir=environment())
clusterEvalQ(cl, library(elasdics))

n <- 1000
parLapply(cl, 1:n, function(i){
  my_seed <- round(runif(1,1,10^8))
  set.seed(my_seed)
  idx_sample <- sample(1:length(data_curves_left), length(data_curves_left), replace = TRUE)
  
  reg_model_left <- try(fit_elastic_regression(data_curves ~ Age + Group + Sex, data_curves = data_curves_left[idx_sample], 
                                               x_data = meta_data[idx_sample,], knots = seq(0,1,0.05)))
  reg_model_right <- try(fit_elastic_regression(data_curves ~ Age + Group + Sex, data_curves = data_curves_right[idx_sample], 
                                                x_data = meta_data[idx_sample,], knots = seq(0,1,0.05)))
  reg_model <- list(reg_model_left, reg_model_right)
  saveRDS(reg_model, file = paste0("bootstrap_models/reg_model_", my_seed))
})

stopCluster(cl)

################################################################################
file_list <- list.files("bootstrap_models/")
file_list_done <- list.files("bootstrap_models_no_sex//")
model_ids <- sapply(file_list, function(file){
  as.numeric(strsplit(file, "_")[[1]][3])
})
model_ids_done <- sapply(file_list_done, function(file){
  as.numeric(strsplit(file, "_")[[1]][3])
})
model_ids_new <- setdiff(model_ids, model_ids_done)

#parallel computations
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
clusterExport(cl, c("data_curves_left", "data_curves_right", "meta_data",
                    "model_ids_new"), envir=environment())
clusterEvalQ(cl, library(elasdics))


parLapply(cl, 1:length(model_ids_new), function(i){
  set.seed(model_ids_new[i])
  idx_sample <- sample(1:length(data_curves_left), length(data_curves_left), replace = TRUE)
  
  reg_model_left <- try(fit_elastic_regression(data_curves ~ Age + Group, data_curves = data_curves_left[idx_sample], 
                                               x_data = meta_data[idx_sample,], knots = seq(0,1,0.05)))
  reg_model_right <- try(fit_elastic_regression(data_curves ~ Age + Group, data_curves = data_curves_right[idx_sample], 
                                                x_data = meta_data[idx_sample,], knots = seq(0,1,0.05)))
  reg_model <- list(reg_model_left, reg_model_right)
  saveRDS(reg_model, file = paste0("bootstrap_models_no_sex/reg_model_", model_ids_new[i]))
})

stopCluster(cl)
################################################################################
################################################################################
file_list <- list.files("bootstrap_models/")
file_list_done <- list.files("bootstrap_models_no_age//")
model_ids <- sapply(file_list, function(file){
  as.numeric(strsplit(file, "_")[[1]][3])
})
model_ids_done <- sapply(file_list_done, function(file){
  as.numeric(strsplit(file, "_")[[1]][3])
})
model_ids_new <- setdiff(model_ids, model_ids_done)

#parallel computations
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
clusterExport(cl, c("data_curves_left", "data_curves_right", "meta_data",
                    "model_ids_new"), envir=environment())
clusterEvalQ(cl, library(elasdics))


parLapply(cl, 1:length(model_ids_new), function(i){
  set.seed(model_ids_new[i])
  idx_sample <- sample(1:length(data_curves_left), length(data_curves_left), replace = TRUE)
  
  reg_model_left <- try(fit_elastic_regression(data_curves ~ Group + Sex, data_curves = data_curves_left[idx_sample], 
                                               x_data = meta_data[idx_sample,], knots = seq(0,1,0.05)))
  reg_model_right <- try(fit_elastic_regression(data_curves ~ Group + Sex, data_curves = data_curves_right[idx_sample], 
                                                x_data = meta_data[idx_sample,], knots = seq(0,1,0.05)))
  reg_model <- list(reg_model_left, reg_model_right)
  saveRDS(reg_model, file = paste0("bootstrap_models_no_age/reg_model_", model_ids_new[i]))
})

stopCluster(cl)
################################################################################
################################################################################
file_list <- list.files("bootstrap_models/")
file_list_done <- list.files("bootstrap_models_no_group//")
model_ids <- sapply(file_list, function(file){
  as.numeric(strsplit(file, "_")[[1]][3])
})
model_ids_done <- sapply(file_list_done, function(file){
  as.numeric(strsplit(file, "_")[[1]][3])
})
model_ids_new <- setdiff(model_ids, model_ids_done)

#parallel computations
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
clusterExport(cl, c("data_curves_left", "data_curves_right", "meta_data",
                    "model_ids_new"), envir=environment())
clusterEvalQ(cl, library(elasdics))


parLapply(cl, 1:length(model_ids_new), function(i){
  set.seed(model_ids_new[i])
  idx_sample <- sample(1:length(data_curves_left), length(data_curves_left), replace = TRUE)
  
  reg_model_left <- try(fit_elastic_regression(data_curves ~ Age + Sex, data_curves = data_curves_left[idx_sample], 
                                               x_data = meta_data[idx_sample,], knots = seq(0,1,0.05)))
  reg_model_right <- try(fit_elastic_regression(data_curves ~ Age + Sex, data_curves = data_curves_right[idx_sample], 
                                                x_data = meta_data[idx_sample,], knots = seq(0,1,0.05)))
  reg_model <- list(reg_model_left, reg_model_right)
  saveRDS(reg_model, file = paste0("bootstrap_models_no_group/reg_model_", model_ids_new[i]))
})

stopCluster(cl)