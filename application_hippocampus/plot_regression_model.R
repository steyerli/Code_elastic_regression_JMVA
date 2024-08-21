library(ggplot2)
library(elasdics)
library(grid)
library(gridExtra)

HUblue <- colorRampPalette(c(rgb(1,1,1), rgb(0, 55/255, 108/255)))(8)
HUred <- colorRampPalette(c(rgb(1,1,1), rgb(138/255, 15/255, 20/255)))(8)
HUpurple <- colorRampPalette(c(HUred[6],HUblue[6]))(8)
HUsand <- colorRampPalette(c(rgb(1,1,1), rgb(210/255, 192/255, 103/255)))(8)

colours <- c(HUblue[8],  HUsand[8], HUred[8], HUpurple[4])

reg_model_left<- readRDS("reg_models/reg_model_left.RDS")
reg_model_right<- readRDS("reg_models/reg_model_right.RDS")
file_list <- list.files("bootstrap_models/")
bootstrap_pred_regions <- readRDS("bootstrap_pred_regions.RDS")
################################################################################
Age_mean <- round(mean(reg_model_left$x_data$Age))
Age_range <- min(reg_model_left$x_data$Age):max(reg_model_left$x_data$Age)

newdata <- data.frame(Age = Age_range,
                      Group = factor(rep("CN", length(Age_range)),
                                     levels = c("AD", "CN")),
                      Sex = factor(rep("F", length(Age_range)),
                                     levels = c("M", "F")))

pred_model_left <- predict(reg_model_left, newdata = newdata)
pred_model_right <- predict(reg_model_right, newdata = newdata)
################

pred_data_left <- lapply(1:length(pred_model_left), function(i){
  names(pred_model_left[[i]]) <- c("y1", "y2")
  pred_model_left[[i]]$id <- i
  pred_model_left[[i]]$side <- "left"
  pred_model_left[[i]]$Age <- newdata$Age[i]
  pred_model_left[[i]]
})
pred_data_right <- lapply(1:length(pred_model_right), function(i){
  names(pred_model_right[[i]]) <- c("y1", "y2")
  pred_model_right[[i]]$id <- i
  pred_model_right[[i]]$side <- "right"
  pred_model_right[[i]]$Age <- newdata$Age[i]
  pred_model_right[[i]]
})
pred_data_list <- c(pred_data_left, pred_data_right)
plot_data_pred_curves <- do.call("rbind", pred_data_list)

g_age_effect <- ggplot(aes(x = y1, y = y2, group = id, color = Age), data = plot_data_pred_curves) +
  geom_path(linewidth = 0.6, alpha = 0.8) + coord_fixed() + facet_grid(~side) +
  scale_color_gradientn(colors = c(HUblue[2], HUblue[4], HUblue[8], HUred[3], HUred[8])) +
  xlim(-10, 10) + ylim(-21,20) + xlab(expression(y[1])) + ylab(expression(y[2])) +
  theme(legend.position="top", panel.background = element_blank(), axis.line = element_line(colour = "grey"),
        legend.key.height = unit(0.1, "cm")) 
g_age_effect

# pdf("../../Figures/effect_age.pdf", width = 4, height = 3.5)
# g_age_effect
# dev.off()
################################################################################
newdata2 <-  data.frame(Age = c(Age_mean),
                        Group = factor(c("CN", "AD"), levels = c("AD", "CN")),
                        Sex = factor(rep("M", 2), levels = c("M", "F")))
pred_model_left2 <- predict(reg_model_left, newdata = newdata2)
pred_model_right2 <- predict(reg_model_right, newdata = newdata2)
################
pred_data_left2 <- lapply(1:length(pred_model_left2), function(i){
  pred_curve <- pred_model_left2[[i]]
  names(pred_curve) <- c("y1", "y2")
  pred_curve$id <- i
  pred_curve$side <- "left"
  pred_curve$Group <- newdata2$Group[i]
  pred_curve
})


pred_data_right2 <- lapply(1:length(pred_model_right2), function(i){
  pred_curve <- pred_model_right2[[i]]
  names(pred_curve) <- c("y1", "y2")
  pred_curve$id <- i
  pred_curve$side <- "right"
  pred_curve$Group <- newdata2$Group[i]
  pred_curve
})
plot_data_CN <- rbind(pred_data_left2[[1]], pred_data_right2[[1]])
plot_data_AD <- rbind(pred_data_left2[[2]], pred_data_right2[[2]])

boot_data <- bootstrap_pred_regions[bootstrap_pred_regions$Age == Age_mean &
                                       bootstrap_pred_regions$Sex == "M",]
g_group_effect <- ggplot(aes(x = y1, y = y2, group = interaction(id, Group), 
                             color = Group), data = boot_data) +
  geom_path(linewidth = 0.2, alpha = 0.4) + coord_fixed() + facet_grid(~side)  +
  scale_color_manual(values = c(HUred[4],HUblue[4])) + xlim(-10, 10) + ylim(-21,20) +
  xlab(expression(y[1])) + ylab(expression(y[2])) +
  geom_path(linewidth = 0.3, data = plot_data_AD, color = colours[3]) + 
  geom_path(linewidth = 0.3, data = plot_data_CN, color = colours[1]) +
  guides(color = guide_legend(override.aes = list(alpha = 0.8, 
                                                  color = c(HUred[8],HUblue[8]), 
                                                  linewidth = 0.6))) +
  theme(legend.position="top", panel.background = element_blank(), axis.line = element_line(colour = "grey")) 
g_group_effect  

# pdf("../../Figures/effect_Alzheimers.pdf", width = 4, height = 3.5)
# g_group_effect 
# dev.off()

################################################################################
newdata3 <-  data.frame(Age = c(Age_mean),
                        Group = factor(rep("CN", 2),
                                       levels = c("CN", "AD")),
                        Sex = factor(c("M", "F"), levels = c("M", "F")))
pred_model_left3 <- predict(reg_model_left, newdata = newdata3)
pred_model_right3 <- predict(reg_model_right, newdata = newdata3)
################
pred_data_left3 <- lapply(1:length(pred_model_left3), function(i){
  pred_curve <- pred_model_left3[[i]]
  names(pred_curve) <- c("y1", "y2")
  pred_curve$id <- i
  pred_curve$side <- "left"
  pred_curve$Group <- "CN"
  pred_curve$Sex <- newdata3$Sex[i]
  pred_curve
})
pred_data_right3 <- lapply(1:length(pred_model_right3), function(i){
  pred_curve <- pred_model_right3[[i]]
  names(pred_curve) <- c("y1", "y2")
  pred_curve$id <- i
  pred_curve$side <- "right"
  pred_curve$Group <- "CN"
  pred_curve$Sex <- newdata3$Sex[i]
  pred_curve
})
plot_data_M <- rbind(pred_data_left3[[1]], pred_data_right3[[1]])
plot_data_F <- rbind(pred_data_left3[[2]], pred_data_right3[[2]])

boot_data <- bootstrap_pred_regions[bootstrap_pred_regions$Age == Age_mean &
                                      bootstrap_pred_regions$Group == "CN",]
g_sex_effect <- ggplot(aes(x = y1, y = y2, group = interaction(id, Sex), 
                           color = Sex), data = boot_data) +
  geom_path(linewidth = 0.2, alpha = 0.4) + coord_fixed() + facet_grid(~side) +
  scale_color_manual(values = c(HUred[4],HUblue[4])) + xlim(-10, 10) + ylim(-21,20) +
  xlab(expression(y[1])) + ylab(expression(y[2])) +
  geom_path(linewidth = 0.3, data = plot_data_M, color = colours[1]) + 
  geom_path(linewidth = 0.3, data = plot_data_F, color = colours[3]) +
  guides(color = guide_legend(override.aes = list(alpha = 0.8, 
                                                  color = c(HUred[8],HUblue[8]),
                                                  linewidth = 0.6))) +
  theme(legend.position="top", panel.background = element_blank(), axis.line = element_line(colour = "grey")) 

g_sex_effect 

############################

#pdf("../../Figures/hippocampus_effects.pdf", width = 9, height = 3.5)
grid.arrange(g_age_effect, g_group_effect, g_sex_effect, ncol = 3) 
#dev.off()

################################################################################
# additional plots
################################################################################
boot_data_age <- bootstrap_pred_regions[bootstrap_pred_regions$Group == "CN" &
                                          bootstrap_pred_regions$Sex == "M" &
                                          bootstrap_pred_regions$Age != Age_mean,]
newdata4 <- unique(boot_data_age[,4:6])
pred_model_left4 <- predict(reg_model_left, newdata = newdata4)
pred_model_right4 <- predict(reg_model_right, newdata = newdata4)
boot_data_age$Age <- as.factor(boot_data_age$Age)
################
pred_data_left4 <- lapply(1:length(pred_model_left4), function(i){
  pred_curve <- pred_model_left4[[i]]
  names(pred_curve) <- c("y1", "y2")
  pred_curve$id <- i
  pred_curve$side <- "left"
  pred_curve$Age <- newdata4$Age[i]
  pred_curve
})
pred_data_right4 <- lapply(1:length(pred_model_right4), function(i){
  pred_curve <- pred_model_right4[[i]]
  names(pred_curve) <- c("y1", "y2")
  pred_curve$id <- i
  pred_curve$side <- "right"
  pred_curve$Age <- newdata4$Age[i]
  pred_curve
})
plot_data <- do.call("rbind", c(pred_data_left4, pred_data_right4))

g_age_effect_boot <- ggplot(aes(x = y1, y = y2, group = interaction(id, Age), 
                           color = Age), data = boot_data_age) +
  geom_path(linewidth = 0.1, alpha = 0.3) + coord_fixed() + facet_grid(~side) + 
  xlab(expression(y[1])) + ylab(expression(y[2])) +
  scale_color_manual(values = c(HUblue[4],HUred[4])) + xlim(-10, 10) + ylim(-21,20) +
  geom_path(linewidth = 0.3, data = plot_data[plot_data$Age == 66, ], color = colours[1]) + 
  geom_path(linewidth = 0.3, data = plot_data[plot_data$Age == 86, ], color = colours[3]) +
  guides(color = guide_legend(override.aes = list(alpha = 0.8, 
                                                  color = c(HUred[8],HUblue[8]),
                                                  linewidth = 0.6))) +
  theme(legend.position="top", panel.background = element_blank(), axis.line = element_line(colour = "grey")) 
g_age_effect_boot
################################################################################
# project group effect on age effect
beta_group_left <- reg_model_left$coefs_list[[3]]
beta_age_left <- reg_model_left$coefs_list[[2]]
beta_group_right <- reg_model_right$coefs_list[[3]]
beta_age_right <- reg_model_right$coefs_list[[2]]
beta_group <- rbind(beta_group_left, beta_group_right)
beta_age <- rbind(beta_age_left, beta_age_right)
####################
# geometric computations
scalar_proj = sum(beta_group*beta_age)/sum(beta_age^2)
# scalar projection of group effect on age effect
scalar_proj
angle_rad <- acos(sum(beta_group*beta_age)/sqrt(sum(beta_group^2)*sum(beta_age^2)))
angle_deg <- angle_rad*180/pi
# angle between group effect and age effect
angle_deg
###################
base_curve_left <- predict(reg_model_left, 
                           newdata = data.frame("Age" = Age_mean, 
                                                "Group" = "CN", 
                                                "Sex" = "M"))[[1]]
base_curve_left$side <- "left"
base_curve_right <- predict(reg_model_right, 
                           newdata = data.frame("Age" = Age_mean, 
                                                "Group" = "CN", 
                                                "Sex" = "M"))[[1]]
base_curve_right$side <- "right"
base_curve <- rbind(base_curve_left, base_curve_right)
base_curve$Group <- "CN"
alzheimer_curve_left <- predict(reg_model_left, 
                                newdata = data.frame("Age" = Age_mean, 
                                                     "Group" = "AD", 
                                                     "Sex" = "M"))[[1]]
alzheimer_curve_left$side <- "left"
alzheimer_curve_right <- predict(reg_model_right, 
                                newdata = data.frame("Age" = Age_mean, 
                                                     "Group" = "AD", 
                                                     "Sex" = "M"))[[1]]
alzheimer_curve_right$side <- "right"
alzheimer_curve <- rbind(alzheimer_curve_left, alzheimer_curve_right)
alzheimer_curve$Group <- "AD"
######################
reg_model_left_projected_group <- reg_model_left
reg_model_left_projected_group$coefs_list[[3]] <- scalar_proj*beta_age_left
projected_curve_left <- predict(reg_model_left_projected_group, 
                           newdata = data.frame("Age" = Age_mean, 
                                                "Group" = "AD", 
                                                "Sex" = "M"))[[1]]
projected_curve_left$side <- "left"

reg_model_right_projected_group <- reg_model_right
reg_model_right_projected_group$coefs_list[[3]] <- scalar_proj*beta_age_right
projected_curve_right <- predict(reg_model_right_projected_group, 
                                 newdata = data.frame("Age" = Age_mean, 
                                                      "Group" = "AD", 
                                                      "Sex" = "M"))[[1]]
projected_curve_right$side <- "right"
projected_curve <- rbind(projected_curve_left, projected_curve_right)
projected_curve$Group <- "AD projected\n on Age"

plot_data <- rbind(base_curve, alzheimer_curve, projected_curve)
label_data <- data.frame("label" = 1:4, "y1" = c(3.6, 9.8, 0, -8.5), 
                         "y2" = c(2.9, 9.5, 19.4, 9.5), "side" = "left")

g_projection <- ggplot(aes(x = V1, y = V2, group = Group, color = Group), 
                       data = plot_data) + 
  geom_path(linewidth = rep(c(0.6, 0.6, 0.6), each =202), 
            alpha = rep(c(0.5,1,1), each =202)) + coord_fixed() + facet_grid(~side) + 
  scale_color_manual(values = c(HUred[8],"black", HUblue[8])) +
  xlim(-10, 10) + ylim(-21,20) + xlab(expression(y[1])) + ylab(expression(y[2])) +
  theme(legend.position="top", panel.background = element_blank(), axis.line = element_line(colour = "grey")) +
  guides(color = guide_legend(override.aes = list(linewidth = c(0.6, 0.6, 0.6),
                                                  alpha = c(1,1,0.5)))) +
  geom_label(aes(x = y1, y = y2, label = label), data = label_data, inherit.aes = FALSE,
             size = 2.5, label.padding = unit(0.1, "lines"))
g_projection

#pdf("../../Figures/AD_projected_on_age.pdf", width = 3.6, height = 4)
#g_projection 
#dev.off()

################################################################################
# Out of bootstrap mse
file_list <- list.files("bootstrap_mse/")

pred_error <- sapply(file_list, function(file){
  c("mse_full" = mean(readRDS(paste0("bootstrap_mse/", file))),
    "mse_no_age" = mean(readRDS(paste0("bootstrap_mse_no_age/", file))),
    "mse_no_group" = mean(readRDS(paste0("bootstrap_mse_no_group/", file))),
    "mse_no_sex" = mean(readRDS(paste0("bootstrap_mse_no_sex/", file))))
})

mse_data <- as.data.frame(t(pred_error))

################################################################################
plot_data <- rbind(data.frame("mse_diff" = mse_data[,2] - mse_data[,1],
                              "model" = "no Age"),
                   data.frame("mse_diff" = mse_data[,3] - mse_data[,1],
                              "model" = "no Group"),
                   data.frame("mse_diff" = mse_data[,4] - mse_data[,1],
                              "model" = "no Sex"))
g_mse <- ggplot(aes(y = mse_diff, x = model), data = plot_data) + 
  geom_boxplot() + ylab("Difference of MSE to full model MSE") + 
  geom_hline(yintercept = 0, color = HUred[6]) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("")

g_mse

#pdf("../../Figures/MSE_comparison.pdf", width = 2, height = 4)
#g_mse
#dev.off()
#########################
mean(mse_data[,1])
# does age effect improve model?
mean(mse_data[,2])
mean(mse_data[,1] - mse_data[,2] <= 0)
# does group effect improve model?
mean(mse_data[,3])
mean(mse_data[,1] - mse_data[,3] <= 0)
# does sex effect improve model?
mean(mse_data[,4])
mean(mse_data[,1] - mse_data[,4] <= 0)
################################################################################

#pdf("../../Figures/hippocampus_effects_supplement.pdf", width = 9, height = 4)
grid.arrange(g_projection, g_age_effect_boot, g_mse, ncol = 3, widths = c(2,2,1)) 
#dev.off()


