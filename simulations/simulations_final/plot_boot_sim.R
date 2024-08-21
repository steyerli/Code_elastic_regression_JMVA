library(elasdics)
library(ggplot2)
library(xtable)
library(ggpubr)
HUblue <- colorRampPalette(c(rgb(1,1,1), rgb(0, 55/255, 108/255)))(8)
HUred <- colorRampPalette(c(rgb(1,1,1), rgb(138/255, 15/255, 20/255)))(8)
################################################################################
########################################
file_list <- list.files("bootstrap_sim_data/")
decisions_data <- sapply(file_list, function(file){
  n_data_N_boot <- as.numeric(strsplit(file, "_")[[1]][3:4])
  bootstrap_data <- readRDS(paste0("bootstrap_sim_data/", file))
  if(class(bootstrap_data) == "try-error"){
    keep_H0 <-rep(NA, 3)
  } else {
    q <- sapply(bootstrap_data$beta_vectors, function(beta){
      n_knots <- ncol(beta)/2
      sapply(1:n_knots, function(i){
        beta_i_centered <- t(t(beta[,c(i,i+n_knots)]) - colMeans(beta[,c(i,i+n_knots)]))
        cov_beta_i <- cov(beta[,c(i, i+n_knots)])
        t_obs <- apply(beta_i_centered, 1, function(beta_i){
          beta_i%*%solve(cov_beta_i)%*%beta_i
        })
        sort(t_obs)[round(length(t_obs)*(1-0.05/n_knots))]
      })
    })
    keep_H0 <- apply(bootstrap_data$test_statistics <= q, 2, all)
  }
  names(keep_H0) <- c("beta1", "beta2", "beta3")
  c("n_data" = n_data_N_boot[2], "N_boot" = n_data_N_boot[1], keep_H0)
})
decisions_data <- data.frame(t(decisions_data))

P_reject_H0_data <- lapply(c(10,30,60), function(n_data){
  t(sapply(c(100, 500, 1000), function(N_boot){
    which_idx <- decisions_data$n_data == n_data & decisions_data$N_boot == N_boot
    c("n_data" = n_data, "N_boot" = N_boot, 
      1 - apply(decisions_data[which_idx, 3:5], 2, mean, na.rm = TRUE))
  }))
})
P_reject_H0_data <- do.call("rbind", P_reject_H0_data)
print(xtable(P_reject_H0_data, digits = c(0,0,0,2,2,2)), include.rownames=FALSE)
##################################
#global test
# decisions_data_global <- sapply(file_list, function(file){
#   n_data_N_boot <- as.numeric(strsplit(file, "_")[[1]][3:4])
#   bootstrap_data <- readRDS(paste0("bootstrap_sim_data/", file))
#   if(class(bootstrap_data) == "try-error"){
#     keep_H0 <-rep(NA, 3)
#   } else {
#     keep_H0 <- sapply(bootstrap_data$beta_vectors, function(beta){
#       beta_i_centered <- t(t(beta) - colMeans(beta))
#       t_obs <- apply(beta_i_centered, 1, function(beta_i){
#         beta_i%*%solve(cov(beta))%*%beta_i
#         })
#       q <- sort(t_obs)[round(length(t_obs)*(1-0.05))]
#       colMeans(beta)%*%solve(cov(beta))%*%colMeans(beta) <= q
#     })
#   }
#   names(keep_H0) <- c("beta1", "beta2", "beta3")
#   c("n_data" = n_data_N_boot[2], "N_boot" = n_data_N_boot[1], keep_H0)
# })
# decisions_data_global <- data.frame(t(decisions_data_global))
# P_reject_H0_data <- lapply(c(10,30,60), function(n_data){
#   t(sapply(c(100, 500, 1000), function(N_boot){
#     which_idx <- decisions_data_global$n_data == n_data & decisions_data_global$N_boot == N_boot
#     c("n_data" = n_data, "N_boot" = N_boot, 
#       1 - apply(decisions_data_global[which_idx, 3:5], 2, mean, na.rm = TRUE))
#   }))
# })
# P_reject_H0_data <- do.call("rbind", P_reject_H0_data)
# print(xtable(P_reject_H0_data, digits = c(0,0,0,2,2,2)), include.rownames=FALSE)
##########################################################################################
##########################################################################################
true_model <- readRDS("true_model.RDS")
x_data_1 <- data.frame("x_1" = -50:50/50, "x_2" = 0, "x_3" = 0)
predicted_curves <- predict(true_model, newdata = x_data_1)
predicted_curve_data_1 <- lapply(1:nrow(x_data_1), function(i){
  data.frame(predicted_curves[[i]], "x_1" = x_data_1$x_1[i]) 
})
predicted_curve_data_1 <- do.call("rbind", predicted_curve_data_1)

g_effect_1 <- ggplot(data = predicted_curve_data_1, aes(y = y2, x = y1, group = x_1, color = x_1)) + 
  geom_path() + xlab(expression(y[1])) + ylab(expression(y[2])) +
  scale_color_gradientn(colors = c(HUblue[2], HUblue[4], HUblue[8], HUred[3], HUred[8])) +
  guides(color = guide_colourbar(title=expression(x[1] ~ phantom(0)))) + 
  ggtitle(expression(Effect~of~x[1])) + coord_fixed() + xlim(-1.1, 2) + ylim(-1, 1.2) +
  theme(legend.position = "bottom", legend.key.height = unit(0.1, "cm"),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        plot.margin=unit(c(0,0,0,0), "cm"))

################################################################################
file_list <- list.files("bootstrap_sim_data/", pattern = "bootstrap_sim_1000_60",
                        full.names = TRUE)
true_beta1 <- true_model$coefs_list[[2]]
true_beta1_data <- data.frame("beta1_vectors" = as.vector(true_beta1), 
                              "coef" = c(1:6, 1:6), 
                              "coordinate" = rep(c("y1", "y2"), each = 6))
true_beta1_data$coef <- factor(true_beta1_data$coef, levels = 6:1)
true_beta1_data$coordinate <- factor(true_beta1_data$coordinate, 
                                     levels = c("y1", "y2"),
                                     labels = c(expression(y[1]), expression(y[2])))

beta1_vector_list <- lapply(file_list, function(file){
  bootstrap_data <- readRDS(file)
  if(class(bootstrap_data) == "try-error"){
    beta1_vectors <- rep(NA, ncol(bootstrap_data$beta_vectors[[1]]))
  } else {
    beta1_vectors <- colMeans(bootstrap_data$beta_vectors[[1]])
  }
  data.frame(beta1_vectors, "coef" = c(1:6, 1:6), 
             "coordinate" = rep(c("y1", "y2"), each = 6))
})
beta1_data <- do.call("rbind", beta1_vector_list)
beta1_data$coef <- factor(beta1_data$coef, levels = 6:1)
beta1_data$coordinate <- factor(beta1_data$coordinate, levels = c("y1", "y2"),
                                labels = c(expression(y[1]), expression(y[2])))

g_boxplots_1 <- ggplot(data = beta1_data, aes(y = coef, x = beta1_vectors)) + 
  geom_boxplot(lwd=0.2, outlier.size = 0.2) + 
  facet_grid(. ~ coordinate, labeller = label_parsed) + 
  geom_vline(xintercept = 0, color = "red") + 
  xlab("coefficient value") + ylab("coefficient") + 
  ggtitle(expression(B-spline~coefficients~of~beta[1])) +
  geom_point(data = true_beta1_data, fill = HUblue[6], shape=21, size = 1) +
  scale_x_continuous(breaks=seq(-0.8,0.8, 0.8))

###########
keep_H0_beta1_data <- lapply(file_list, function(file){
  bootstrap_data <- readRDS(file)
  beta <- bootstrap_data$beta_vectors[[1]]
  if(class(bootstrap_data) == "try-error"){
    keep_H0_beta1 <- rep(NA, 6)
  } else {
    n_knots <- ncol(beta)/2
    q <-  sapply(1:n_knots, function(i){
      beta_i_centered <- t(t(beta[,c(i,i+n_knots)]) - colMeans(beta[,c(i,i+n_knots)]))
      cov_beta_i <- cov(beta[,c(i, i+n_knots)])
      t_obs <- apply(beta_i_centered, 1, function(beta_i){
        beta_i%*%solve(cov_beta_i)%*%beta_i
      })
      sort(t_obs)[round(length(t_obs)*(1-0.05/n_knots))]
    })
    keep_H0_beta1 <- bootstrap_data$test_statistics[,1] <= q
  }
  data.frame("keep_H0_beta1" = keep_H0_beta1, "coef" = 1:6)
})
keep_H0_beta1_data <- do.call("rbind", keep_H0_beta1_data)
reject_HO_beta1_data <- data.frame(t(sapply(1:6, function(i){
  c("coef" = i, "P" = 1 - mean(keep_H0_beta1_data[keep_H0_beta1_data$coef == i,1]))
})))
reject_HO_beta1_data$coef <- factor(reject_HO_beta1_data$coef, levels = 6:1)
reject_HO_beta1_data$combined <- rep("Rejection probability", 6)

g_probs_1 <- ggplot(data = reject_HO_beta1_data, aes(x = P, y = coef, group=1)) + 
  geom_path() + geom_point() + ylab("") + xlab("") +
  facet_grid(. ~ combined) + ylab("coefficient") + ggtitle(bquote(phantom(beta[1]))) +
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0,1)) 

################################################################################
x_data_2 <- data.frame("x_1" = 0, "x_2" = -50:50/50, "x_3" = 0)
predicted_curves_2 <- predict(true_model, newdata = x_data_2)
predicted_curve_data_2 <- lapply(1:nrow(x_data_2), function(i){
  data.frame(predicted_curves_2[[i]], "x_2" = x_data$x_2[i]) 
})
predicted_curve_data_2 <- do.call("rbind", predicted_curve_data_2)

g_effect_2 <- ggplot(data = predicted_curve_data_2, aes(y = y2, x = y1, group = x_2, color = x_2)) + 
  geom_path() + xlab(expression(y[1])) + ylab(expression(y[2])) +
  scale_color_gradientn(colors = c(HUblue[2], HUblue[4], HUblue[8], HUred[3], HUred[8])) +
  guides(color = guide_colourbar(title=expression(x[2] ~ phantom(0)))) + 
  ggtitle(expression(Effect~of~x[2])) + coord_fixed() + xlim(-1.1, 2) + ylim(-1, 1.2) +
  theme(legend.position = "bottom", legend.key.height = unit(0.1, "cm"),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        plot.margin=unit(c(0,0,0,0), "cm"))

################################################################################
file_list <- list.files("bootstrap_sim_data/", pattern = "bootstrap_sim_1000_60",
                        full.names = TRUE)
true_beta2 <- true_model$coefs_list[[3]]
true_beta2_data <- data.frame("beta2_vectors" = as.vector(true_beta2), 
                              "coef" = c(1:6, 1:6), 
                              "coordinate" = rep(c("y1", "y2"), each = 6))
true_beta2_data$coef <- factor(true_beta2_data$coef, levels = 6:1)
true_beta2_data$coordinate <- factor(true_beta2_data$coordinate, 
                                     levels = c("y1", "y2"),
                                     labels = c(expression(y[1]), expression(y[2])))

beta2_vector_list <- lapply(file_list, function(file){
  bootstrap_data <- readRDS(file)
  if(class(bootstrap_data) == "try-error"){
    beta2_vectors <- rep(NA, ncol(bootstrap_data$beta_vectors[[2]]))
  } else {
    beta2_vectors <- colMeans(bootstrap_data$beta_vectors[[2]])
  }
  data.frame(beta2_vectors, "coef" = c(1:6, 1:6), 
        "coordinate" = rep(c("y1", "y2"), each = 6))
})
beta2_data <- do.call("rbind", beta2_vector_list)
beta2_data$coef <- factor(beta2_data$coef, levels = 6:1)
beta2_data$coordinate <- factor(beta2_data$coordinate, levels = c("y1", "y2"),
       labels = c(expression(y[1]), expression(y[2])))

g_boxplots_2 <- ggplot(data = beta2_data, aes(y = coef, x = beta2_vectors)) + 
  geom_boxplot(lwd=0.2, outlier.size = 0.2) + 
  facet_grid(. ~ coordinate, labeller = label_parsed) + 
  geom_vline(xintercept = 0, color = "red") + 
  xlab("coefficient value") +ylab("coefficient") + 
  ggtitle(expression(B-spline~coefficients~of~beta[2])) +
  geom_point(data = true_beta2_data, fill = HUblue[6], shape=21, size = 1) +
  scale_x_continuous(breaks=seq(-0.4,0.8, 0.4))

###########
keep_H0_beta2_data <- lapply(file_list, function(file){
  bootstrap_data <- readRDS(file)
  beta <- bootstrap_data$beta_vectors[[2]]
  if(class(bootstrap_data) == "try-error"){
    keep_H0_beta2 <- rep(NA, 6)
  } else {
    n_knots <- ncol(beta)/2
    q <-  sapply(1:n_knots, function(i){
      beta_i_centered <- t(t(beta[,c(i,i+n_knots)]) - colMeans(beta[,c(i,i+n_knots)]))
      cov_beta_i <- cov(beta[,c(i, i+n_knots)])
      t_obs <- apply(beta_i_centered, 1, function(beta_i){
        beta_i%*%solve(cov_beta_i)%*%beta_i
      })
      sort(t_obs)[round(length(t_obs)*(1-0.05/n_knots))]
    })
    keep_H0_beta2 <- bootstrap_data$test_statistics[,2] <= q
  }
  data.frame("keep_H0_beta2" = keep_H0_beta2, "coef" = 1:6)
})
keep_H0_beta2_data <- do.call("rbind", keep_H0_beta2_data)
reject_HO_beta2_data <- data.frame(t(sapply(1:6, function(i){
  c("coef" = i, "P" = 1 - mean(keep_H0_beta2_data[keep_H0_beta2_data$coef == i,1]))
  })))
reject_HO_beta2_data$coef <- factor(reject_HO_beta2_data$coef, levels = 6:1)
reject_HO_beta2_data$combined <- rep("Rejection probability", 6)

g_probs_2 <- ggplot(data = reject_HO_beta2_data, aes(x = P, y = coef, group=1)) + 
  geom_path() + geom_point() + ylab("") + xlab("") +
  facet_grid(. ~ combined) + ylab("coefficient") + ggtitle(bquote(phantom(beta[1]))) +
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0,1)) 
################################################################################
# combine plots
ggarrange(g_effect_1, g_boxplots_1, g_probs_1,
          g_effect_2, g_boxplots_2, g_probs_2,
          ncol = 3, nrow = 2, widths = c(2,2,1)) %>% 
  ggexport(filename = "../../../Figures/test_beta_coefs.pdf", width = 9, height = 5.5)

################################################################################
################################################################################
# more coefs plot
################################################################################
file_list <- list.files("bootstrap_sim_data2/", pattern = "bootstrap_sim_1000_60",
                        full.names = TRUE)
decisions_data <- sapply(file_list, function(file){
  bootstrap_data <- readRDS(file)
  if(class(bootstrap_data) == "try-error"){
    keep_H0 <-rep(NA, 3)
  } else {
    q <- sapply(bootstrap_data$beta_vectors, function(beta){
      n_knots <- ncol(beta)/2
      sapply(1:n_knots, function(i){
        beta_i_centered <- t(t(beta[,c(i,i+n_knots)]) - colMeans(beta[,c(i,i+n_knots)]))
        cov_beta_i <- cov(beta[,c(i, i+n_knots)])
        t_obs <- apply(beta_i_centered, 1, function(beta_i){
          beta_i%*%solve(cov_beta_i)%*%beta_i
        })
        sort(t_obs)[round(length(t_obs)*(1-0.05/n_knots))]
      })
    })
    keep_H0 <- apply(bootstrap_data$test_statistics <= q, 2, all)
  }
  names(keep_H0) <- c("beta1", "beta2", "beta3")
  keep_H0
})
1- rowMeans(decisions_data)
#############################
true_model2 <- readRDS("true_model2.RDS")
x_data <- data.frame("x_1" = 0, "x_2" = -50:50/50, "x_3" = 0)
predicted_curves <- predict(true_model2, newdata = x_data)
predicted_curve_data <- lapply(1:nrow(x_data), function(i){
  data.frame(predicted_curves[[i]], "x_2" = x_data$x_2[i]) 
})
predicted_curve_data <- do.call("rbind", predicted_curve_data)
names(predicted_curve_data) <- c("y1", "y2", "x_2")

g_effect <- ggplot(data = predicted_curve_data, aes(y = y2, x = y1, group = x_2, color = x_2)) + 
  geom_path() + xlab(expression(y[1])) + ylab(expression(y[2])) +
  scale_color_gradientn(colors = c(HUblue[2], HUblue[4], HUblue[8], HUred[3], HUred[8])) +
  guides(color = guide_colourbar(title=expression(x[2] ~ phantom(0)))) + 
  ggtitle(expression(Effect~of~x[2]~on~the~predicted~curve)) + coord_fixed() +
  theme(legend.position = "bottom", legend.key.height = unit(0.1, "cm"))
################################################################################
true_beta2 <- true_model2$coefs_list[[3]]
true_beta2_data <- data.frame("beta2_vectors" = as.vector(true_beta2), 
                              "coef" = c(1:11, 1:11), 
                              "coordinate" = rep(c("y1", "y2"), each = 11))
true_beta2_data$coef <- factor(true_beta2_data$coef, levels = 11:1)
true_beta2_data$coordinate <- factor(true_beta2_data$coordinate, 
                                     levels = c("y1", "y2"),
                                     labels = c(expression(y[1]), expression(y[2])))

beta2_vector_list <- lapply(file_list, function(file){
  bootstrap_data <- readRDS(file)
  if(class(bootstrap_data) == "try-error"){
    beta2_vectors <- rep(NA, ncol(bootstrap_data$beta_vectors[[2]]))
  } else {
    beta2_vectors <- colMeans(bootstrap_data$beta_vectors[[2]])
  }
  data.frame(beta2_vectors, "coef" = c(1:11, 1:11), 
             "coordinate" = rep(c("y1", "y2"), each = 11))
})
beta2_data <- do.call("rbind", beta2_vector_list)
beta2_data$coef <- factor(beta2_data$coef, levels = 11:1)
beta2_data$coordinate <- factor(beta2_data$coordinate, levels = c("y1", "y2"),
                                labels = c(expression(y[1]), expression(y[2])))

g_boxplots <- ggplot(data = beta2_data, aes(y = coef, x = beta2_vectors)) + 
  geom_boxplot(lwd=0.2, outlier.size = 0.2) + 
  facet_grid(. ~ coordinate, labeller = label_parsed) + 
  geom_vline(xintercept = 0, color = "red") + 
  xlab("coefficient value") + ylab("B-spline coefficient") +
  geom_point(data = true_beta2_data, fill = HUblue[6], shape=21, size = 1) +
  scale_x_continuous(breaks=seq(-0.4,0.4, 0.4))
#################################
keep_H0_beta2_data <- lapply(file_list, function(file){
  bootstrap_data <- readRDS(file)
  if(class(bootstrap_data) == "try-error"){
    keep_H0_beta2 <- rep(NA, 11)
  } else {
    n_knots <- ncol(beta)/2
    q <-  sapply(1:n_knots, function(i){
      beta_i_centered <- t(t(beta[,c(i,i+n_knots)]) - colMeans(beta[,c(i,i+n_knots)]))
      cov_beta_i <- cov(beta[,c(i, i+n_knots)])
      t_obs <- apply(beta_i_centered, 1, function(beta_i){
        beta_i%*%solve(cov_beta_i)%*%beta_i
      })
      sort(t_obs)[round(length(t_obs)*(1-0.05/n_knots))]
    })
    keep_H0_beta2 <- bootstrap_data$test_statistics[,2] <= q
  }
  data.frame("keep_H0_beta2" = keep_H0_beta2, "coef" = 1:11)
})
keep_H0_beta2_data <- do.call("rbind", keep_H0_beta2_data)
reject_HO_beta_2_data <- data.frame(t(sapply(1:11, function(i){
  c("coef" = i, "P" = 1 - mean(keep_H0_beta2_data[keep_H0_beta2_data$coef == i,1]))
})))
reject_HO_beta_2_data$coef <- factor(reject_HO_beta_2_data$coef, levels = 11:1)
reject_HO_beta_2_data$combined <- rep("Rejection probability", 11)

g_probs <- ggplot(data = reject_HO_beta_2_data, aes(x = P, y = coef, group=1)) + 
  geom_path() + geom_point() + ylab("") + xlab("") +
  facet_grid(. ~ combined) + ylab("B-spline coefficient") + 
  scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(0,1)) 

################################################################################
# combine plots
ggarrange(g_effect, g_boxplots, g_probs, ncol = 3, nrow = 1, widths = c(3,2,1.2)) %>% 
  ggexport(filename = "../../../Figures/test_beta2_more_coefs.pdf", width = 9, height = 4)
