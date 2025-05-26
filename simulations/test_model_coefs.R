library(elasdics)
library(parallel)

################################################################################
# create model equation
curve <- function(x_1, x_2, t){
  rbind(2*t*cos(6*t) - x_1*t , x_2*t*sin(6*t))
}
x_data <- data.frame(expand.grid(x_1 = -1:1, x_2 = -1:1))
t_grid <- seq(0,1,0.1)
data_curves <- apply(x_data, 1, function(x){
  curve <- data.frame(t(curve((x[1] + 1), (x[2] + 2), t_grid)))
  names(curve) <- c("y1", "y2")
  curve
})
reg_model_0 <- fit_elastic_regression(data_curves ~ x_1 + x_2,
                                    data_curves = data_curves, x_data = x_data)
reg_model_0$coefs_list$beta_2[3:6,] <- 0*reg_model_0$coefs_list$beta_2[3:6,]
################################################################################
draw_reg_data <- function(n = 30, reg_model_0){
  newdata = data.frame(x_1 = runif(n,-1,1), x_2 = runif(n, -1, 1),
                       x_3 = runif(n, -1, 1))
  data_curves <-lapply(1:n, function(i){
    delta <- abs(rnorm(sample(10:15, 1), mean = 1, sd = 0.5))
    t_grid <- cumsum(delta)/sum(delta)
    predict(reg_model_0, newdata = newdata[i, 1:2], t_grid)[[1]]
  })
  list("data_curves" = data_curves, 
       "x_data" = newdata)
}
################################################################################
get_coefs_boot <- function(reg_data, N = 100, alpha = 0.05, knots = seq(0,1,0.2)){
  n <- length(reg_data$data_curves)
  n_knots <- length(knots)
  coefs_boot <- lapply(1:N, function(i){
    idx <- sample(1:n, n, replace = TRUE)
    try(fit_elastic_regression(data_curves ~ x_1 + x_2 + x_3,
                               data_curves = reg_data$data_curves[idx],
                               x_data = reg_data$x_data[idx,], knots = knots)$coefs_list)
  })
  coefs_boot <- coefs_boot[!sapply(coefs_boot, function(x) class(x) == "try-error")]
  betas <- lapply(1:3, function(i){
    t(sapply(coefs_boot, function(coefs) coefs[[i + 1]]))
  })
  
  T_s <- sapply(betas, function(beta){
    sapply(1:n_knots, function(i){
      colMeans(beta[,c(i,i+n_knots)])%*%solve(cov(beta[,c(i, i+n_knots)]))%*%
        colMeans(beta[,c(i, i + n_knots)])
    })
  })
  colnames(T_s) <- names(coefs_boot[[1]])[2:4]
  keep_H0 <- apply(T_s < qchisq(1-alpha/n_knots, 2), 2, all)
  list("beta_vectors" = betas, "test_statistics" =T_s, 
       "keep_H0" = keep_H0, "critical_value" = qchisq(1-alpha/n_knots, 2))
}
################################################################################
# find out which simulations are missing
n_data <- 60
N_boot <- 500
file_list_done <- list.files("bootstrap_sim_data/", 
                             pattern = paste0("bootstrap_sim_", N_boot, "_",n_data))
idx_done <- sapply(file_list_done, function(file) as.numeric(strsplit(file, "_")[[1]][5]))
idx_new <- setdiff(1001:2000, idx_done)

#parallel computation
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
clusterEvalQ(cl, library(elasdics))
clusterExport(cl, c("get_coefs_boot", "draw_reg_data", "n_data",
                    "reg_model_0", "N_boot"), envir=environment())

parLapply(cl, idx_new, function(seed){
  set.seed(seed)
  bootstrap_sim_data <- try(get_coefs_boot(draw_reg_data(n_data, reg_model_0), 
                                           N = N_boot))
  saveRDS(bootstrap_sim_data, 
          file = paste0("bootstrap_sim_data/bootstrap_sim_", N_boot, "_",
                        n_data, "_", seed))
})
#stop parallel computing
stopCluster(cl)
################################################################################
#estimate true model coefficients
reg_data <- draw_reg_data(n = 500, reg_model_0)
reg_model <- fit_elastic_regression(data_curves ~ x_1 + x_2 + x_3,
                                    data_curves = reg_data$data_curves,
                                    x_data = reg_data$x_data)
saveRDS(reg_model, file = "true_model.RDS")
################################################################################
################################################################################
# create model equation with more coefs
curve <- function(x_1, x_2, t){
  rbind(2*t*cos(6*t) - x_1*t , x_2*t*sin(6*t))
}
x_data <- data.frame(expand.grid(x_1 = -1:1, x_2 = -1:1))
t_grid <- seq(0,1,0.1)
data_curves <- apply(x_data, 1, function(x){
  curve <- data.frame(t(curve((x[1] + 1), (x[2] + 2), t_grid)))
  names(curve) <- c("y1", "y2")
  curve
})
reg_model_02 <- fit_elastic_regression(data_curves ~ x_1 + x_2, data_curves = data_curves,
                                       x_data = x_data, knots = seq(0,1,0.1))
reg_model_02$coefs_list$beta_2[1:8,] <- 0*reg_model_02$coefs_list$beta_2[1:8,]
################################################################################
# find out which simulations are missing
n_data <- 60
N_boot <- 1000
file_list_done <- list.files("bootstrap_sim_data2/", 
                             pattern = paste0("bootstrap_sim_", N_boot, "_",n_data))
idx_done <- sapply(file_list_done, function(file) as.numeric(strsplit(file, "_")[[1]][5]))
idx_new <- setdiff(1101:1200, idx_done)

#parallel computation
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)
clusterEvalQ(cl, library(elasdics))
clusterExport(cl, c("get_coefs_boot", "draw_reg_data", "n_data",
                    "reg_model_02", "N_boot"), envir=environment())

parLapply(cl, idx_new, function(seed){
  set.seed(seed)
  bootstrap_sim_data <- try(get_coefs_boot(draw_reg_data(n_data, reg_model_02), 
                                           N = N_boot, knots = seq(0,1,0.1)))
  saveRDS(bootstrap_sim_data, 
          file = paste0("bootstrap_sim_data2/bootstrap_sim_", N_boot, "_",
                        n_data, "_", seed))
})
#stop parallel computing
stopCluster(cl)
################################################################################
#estimate true model coefficients
reg_data <- draw_reg_data(n = 1000, reg_model_02)
reg_model <- fit_elastic_regression(data_curves ~ x_1 + x_2 + x_3,
                                    data_curves = reg_data$data_curves,
                                    x_data = reg_data$x_data,
                                    knots = seq(0,1,0.1))
saveRDS(reg_model, file = "true_model2.RDS")
