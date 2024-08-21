library(parallel)
library(caret)

data_curves_left <- readRDS("data_curves_left.RDS")
data_curves_right <- readRDS("data_curves_right.RDS")
meta_data <- read.csv("meta_data_1.csv")
meta_data$Group <- factor(meta_data$Group)
################################################################################
# permutation sampling
###############################################
#parallel computations
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
clusterExport(cl, c("data_curves_left", "data_curves_right", "meta_data"), envir=environment())
clusterEvalQ(cl, library(elasdics))

n <- 100
parLapply(cl, 1:n, function(i){
  my_seed <- round(runif(1,1,10^8))
  set.seed(my_seed)
  idx_sample <- sample(1:length(data_curves_left), length(data_curves_left), replace = FALSE)
  
  reg_model_left <- try(fit_elastic_regression(data_curves ~ Age + Group + Sex, data_curves = data_curves_left, 
                                               x_data = meta_data[idx_sample,], knots = seq(0,1,0.05)))
  reg_model_left_0 <- try(fit_elastic_regression(data_curves ~ 1, data_curves = data_curves_left, 
                                               x_data = meta_data[idx_sample,], knots = seq(0,1,0.05)))
  reg_model_right <- try(fit_elastic_regression(data_curves ~ Age + Group + Sex, data_curves = data_curves_right, 
                                                x_data = meta_data[idx_sample,], knots = seq(0,1,0.05)))
  reg_model_right_0 <- try(fit_elastic_regression(data_curves ~ 1, data_curves = data_curves_right, 
                                                x_data = meta_data[idx_sample,], knots = seq(0,1,0.05)))
  permutated_model <- list(reg_model_left, reg_model_left_0, reg_model_right, reg_model_right_0)
  saveRDS(permutated_model, file = paste0("permutation_models/permutated_model_", my_seed))
})

stopCluster(cl)