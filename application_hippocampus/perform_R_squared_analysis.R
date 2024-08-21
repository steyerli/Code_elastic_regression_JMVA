library(elasdics)
library(xtable)
################################################################################
# compute adjusted R_squared for all models
R_squared_adj <- function(reg_model_name){
  if(reg_model_name == "full"){
    names <- c("reg_model_left.RDS", "reg_model_right.RDS")
  } else {
    names <- c(paste0("reg_model_left_", reg_model_name, ".RDS"), 
               paste0("reg_model_right_", reg_model_name, ".RDS"))
  }
  model_left <- readRDS(paste0("reg_models/" , names[1]))
  model_right <- readRDS(paste0("reg_models/" , names[2]))

  n <- length(model_left$data_curves)
  d <- length(model_left$coefs_list) - 1
  mse_model <- in_sample_mse(model_left) + in_sample_mse(model_right)
  mse_model_0 <- in_sample_mse(readRDS("reg_models/reg_model_left_intercept.RDS")) +
    in_sample_mse(readRDS("reg_models/reg_model_right_intercept.RDS"))
  1 - (n - 1)/(n - d -1)*mse_model/mse_model_0
}

in_sample_mse <- function(reg_model){
  mean(sapply(reg_model$data_curves, function(curve) attributes(curve)$dist_to_prediction)^2)
}
##############################
R_squared_adj_table <- data.frame("full" = R_squared_adj("full"),
                                  "no Age" = R_squared_adj("no_age"),
                                  "no Group" = R_squared_adj("no_group"),
                                  "no Sex" = R_squared_adj("no_sex"),
                                  "only Group" = R_squared_adj("group"),
                                  "only Sex" = R_squared_adj("sex"),
                                  "only Age" = R_squared_adj("age"))
xtable(R_squared_adj_table, digits = 3)
################################################################################
# permutation test using R_squared as a test statistic

reg_model_left <- readRDS("reg_models/reg_model_left.RDS")
reg_model_left_intercept <- readRDS("reg_models/reg_model_left_intercept.RDS")
reg_model_right <- readRDS("reg_models/reg_model_right.RDS")
reg_model_right_intercept <- readRDS("reg_models/reg_model_right_intercept.RDS")

file_list <- list.files("permutation_models/")

R_squared_sampled <- sapply(file_list, function(file){
  reg_model <- readRDS(paste0("permutation_models/", file))
  dists <- sapply(reg_model, function(x){
    sapply(x$data_curves, function(curve){
      attributes(curve)$dist_to_prediction
    })
  })
  1 - mean(dists[,1]^2 + dists[,3]^2)/mean(dists[,2]^2 + dists[,4]^2)
})

boxplot(R_squared_sampled)

R_squared_model <- 1 - (in_sample_mse(reg_model_left) + in_sample_mse(reg_model_right))/
  (in_sample_mse(reg_model_left_intercept) + in_sample_mse(reg_model_right_intercept))

boxplot(c(R_squared_sampled, R_squared_model))
points(R_squared_model, col = "red", pch = 22)

t.test(R_squared_sampled, mu = R_squared_model, alternative = "less")

