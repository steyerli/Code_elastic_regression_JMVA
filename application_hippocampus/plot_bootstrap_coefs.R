library(elasdics)
library(ggplot2)
library(grid)
library(gridExtra)
file_list <- list.files("bootstrap_models/")
coefs_names <- c("Age", "Group", "Sex")

left_beta_coefs <- lapply(file_list, function(file){
  reg_model <- readRDS(paste0("bootstrap_models/", file))
  reg_model[[1]]$coefs_list
})
right_beta_coefs <- lapply(file_list, function(file){
  reg_model <- readRDS(paste0("bootstrap_models/", file))
  reg_model[[2]]$coefs_list
})

########################################
coef_data_list <- lapply(1:3, function(i){
  coef_matrix <- sapply(left_beta_coefs, function(coef_list){
    coef_list[[i + 1]]
    })
  coef_list <- lapply(1:nrow(coef_matrix), function(j){
    coef_data <- data.frame("data" = coef_matrix[j,], "coef" = factor((j - 1) %% 21 + 1),
                            "effect" = coefs_names[i])
    coef_data$coordinate <- factor(ifelse(j <= 21, "y1", "y2"))
    coef_data
  })
  do.call("rbind", coef_list)
})
coef_data_left <- do.call("rbind", coef_data_list)
levels(coef_data_left$coordinate) <- c(expression(y[1]), expression(y[2]))


g_boxplots <- lapply(coefs_names, function(group_name){ 
  ggplot(data = coef_data_left[coef_data_left$effect == group_name,], aes(x = coef, y =data)) + geom_boxplot(lwd=0.2, outlier.size = 0.2) + 
    facet_grid(coordinate~., labeller = label_parsed) + geom_hline(yintercept = 0, color = "red") + ylab("") + 
    ggtitle(group_name) + theme(plot.title = element_text(hjust = 0.5)) + 
    scale_x_discrete(breaks = seq(1, 21, by = 2))
})

pdf("../../Figures/bootstrap_coefs_left.pdf", width = 10, height = 3.5)
grid.arrange(grobs = g_boxplots, ncol = 3) 
dev.off()
#############################################
coef_data_list <- lapply(1:3, function(i){
  coef_matrix <- sapply(right_beta_coefs, function(coef_list){
    coef_list[[i + 1]]
  })
  coef_list <- lapply(1:nrow(coef_matrix), function(j){
    coef_data <- data.frame("data" = coef_matrix[j,], "coef" = factor((j - 1) %% 21 + 1),
                            "effect" = coefs_names[i])
    coef_data$coordinate <- factor(ifelse(j <= 21, "y1", "y2"))
    coef_data
  })
  do.call("rbind", coef_list)
})
coef_data_right <- do.call("rbind", coef_data_list)
levels(coef_data_right$coordinate) <- c(expression(y[1]), expression(y[2]))


g_boxplots <- lapply(coefs_names, function(group_name){ 
  ggplot(data = coef_data_right[coef_data_right$effect == group_name,], aes(x = coef, y =data)) + geom_boxplot(lwd=0.2, outlier.size = 0.2) + 
    facet_grid(coordinate~., labeller = label_parsed) + geom_hline(yintercept = 0, color = "red") + ylab("") + 
    ggtitle(group_name) + theme(plot.title = element_text(hjust = 0.5)) + 
    scale_x_discrete(breaks = seq(1, 21, by = 2))
})

pdf("../../Figures/bootstrap_coefs_right.pdf", width = 10, height = 3.5)
grid.arrange(grobs = g_boxplots, ncol = 3) 
dev.off()
################################################################################
# is 0 outside of 95 percent confidence region?

left_beta_coefs[[1]]

beta1s <- lapply(1:length(left_beta_coefs), function(i){
  as.vector(rbind(left_beta_coefs[[i]]$beta_1, right_beta_coefs[[i]]$beta_1))
})
beta1s <- do.call("rbind", beta1s)

n_knots <- ncol(beta1s)/2
q_s <- sapply(1:n_knots, function(i){
    beta_i_centered <- t(t(beta1s[,c(i,i+n_knots)]) - colMeans(beta1s[,c(i,i+n_knots)]))
    cov_beta_i <- cov(beta1s[,c(i, i+n_knots)])
    t_obs <- apply(beta_i_centered, 1, function(beta_i){
      beta_i%*%solve(cov_beta_i)%*%beta_i
    })
    sort(t_obs)[round(length(t_obs)*(1-0.05/n_knots))]
  })

T_s <- sapply(1:n_knots, function(i){
  beta_i <- beta1s[,c(i,i+n_knots)]
  colMeans(beta_i)%*%solve(cov(beta_i))%*%colMeans(beta_i)
})
q_s > T_s

qchisq(1-0.05/42, 2)

T_s <- sapply(betas, function(beta){
  sapply(1:n_knots, function(i){
    colMeans(beta[,c(i,i+n_knots)])%*%solve(cov(beta[,c(i, i+n_knots)]))%*%
      colMeans(beta[,c(i, i + n_knots)])
  })
})