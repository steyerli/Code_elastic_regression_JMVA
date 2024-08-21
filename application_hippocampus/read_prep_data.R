library(RNifti)
library(elasdics)
library(manipulate)
source("find_hippocampus_outline.R")

file_list <- list.files(pattern = ".nii", recursive = TRUE)
meta_data <- read.csv("Hippocampal_Mask_3_14_2022.csv")

################################################################################
file_id <- sapply(file_list, function(file){
  strings <- strsplit(file, split = "/")[[1]]
  strings[5]
})
names(file_id) <- NULL

#####
meta_data_1 <- meta_data[meta_data$Group %in% c("CN", "AD") & meta_data$Visit == 1,]
write.csv(meta_data_1, "meta_data_1.csv")
file_list_select <- file_id %in% meta_data_1$Image.Data.ID

masks <- lapply(file_list[file_list_select], readNifti)

################################
curves <- lapply(1:length(masks), function(i){
  print(i)
  find_hippocamous_outline(masks[[i]])})

manipulate({plot(curves[[k]][[1]], asp = 1, type = "l", pch = 16, lwd = 2)
            points(curves[[k]][[1]][1,])},
           k = slider(1, length(curves)))

################################################################################
# open mean computation to get a template for alignment
curves_left <- lapply(curves, '[[', 1)
knots <- seq(0,1,0.02)

mean_left_open <- compute_elastic_mean(curves_left, knots = knots, closed = FALSE, type = "polygon", eps = 0.01)
plot(mean_left_open)

mean_points <- get_points_from_srv(data.frame("t" = knots[-length(knots)], mean_left_open$coefs))
data_curves <- mean_left_open$data_curves

plot(mean_points, type = "b", asp = 1)
points(mean_points[24,], col = "red")
points(mean_points[28,], col = "orange")
gap <- knots[c(24,28)]

data_curves_left <- lapply(data_curves, function(data_curve){
  data_curve <- data_curve[,-1]
  names(data_curve)[1] <- "t"
  ends <- get_evals(data_curve, gap)
  data_curve <- rbind(ends[2,], data_curve[data_curve$t > gap[2], 2:3], 
                      data_curve[data_curve$t < gap[1], 2:3], ends[1,]) 
  data_curve <- elasdics:::remove_duplicate(data_curve, closed = FALSE)
  row.names(data_curve) <- NULL
  data_curve
})

mean_left <- compute_elastic_mean(data_curves_left, knots = knots, closed = FALSE, type = "polygon", eps = 0.01)
plot(mean_left)

data_left <- lapply(mean_left$data_curves, function(data_curve){
  min_diff <- min(diff(data_curve$t_optim))
  while(min_diff < 0.015){
    idx_min <- which.min(diff(data_curve$t_optim)) + 1
    data_curve <- data_curve[-idx_min,]
    min_diff <- min(diff(data_curve$t_optim))
  }
  data_curve[,3:4]
  
  # #remove co-linear points
  # diff_outline <- apply(outline, 2, diff)
  # co_lin <- sapply(1:(nrow(diff_outline) - 1), function(i){
  #   ind <- sum(diff_outline[i,]*diff_outline[i+1,])/sqrt(sum(diff_outline[i,]^2)*sum(diff_outline[i+1,]^2))
  #   abs(ind - 1) <= 10^-2
  # })
  # co_lin <- c(FALSE, co_lin, FALSE)
  # outline <- outline[!co_lin|is.na(co_lin),]
  # row.names(outline) <- NULL
  # outline
})

manipulate({plot(data_left[[k]], type = "b", asp = 1, pch = 16)
            lines(curves[[k]][[1]], col = "red")},
           k = slider(1, length(data_curves_left)))
################################################################################
curves_right <- lapply(curves, '[[', 2)
mean_right_open <- compute_elastic_mean(curves_right, knots = knots, closed = FALSE, type = "polygon", eps = 0.01)
plot(mean_right_open)

mean_points <- get_points_from_srv(data.frame("t" = knots[-length(knots)], mean_right_open$coefs))
data_curves <- mean_right_open$data_curves

plot(mean_points, type = "b", asp = 1)
points(mean_points[24,], col = "red")
points(mean_points[28,], col = "orange")
gap <- knots[c(24,28)]

data_curves_right <- lapply(data_curves, function(data_curve){
  data_curve <- data_curve[,-1]
  names(data_curve)[1] <- "t"
  ends <- get_evals(data_curve, gap)
  data_curve <- rbind(ends[2,], data_curve[data_curve$t > gap[2], 2:3], 
                      data_curve[data_curve$t < gap[1], 2:3], ends[1,]) 
  data_curve <- elasdics:::remove_duplicate(data_curve, closed = FALSE)
  row.names(data_curve) <- NULL
  data_curve
})

mean_right <- compute_elastic_mean(data_curves_right, knots = knots, closed = FALSE, type = "polygon", eps = 0.01)
plot(mean_right)

data_right <- lapply(mean_right$data_curves, function(data_curve){
  min_diff <- min(diff(data_curve$t_optim))
  while(min_diff < 0.015){
    idx_min <- which.min(diff(data_curve$t_optim)) + 1
    data_curve <- data_curve[-idx_min,]
    min_diff <- min(diff(data_curve$t_optim))
  }
  data_curve[,3:4]
})

manipulate(plot(data_right[[k]], type = "b", asp = 1, pch = 16),
           k = slider(1, length(data_curves_left)))
################################################################################
saveRDS(data_left, "data_curves_left.RDS")
saveRDS(data_right, "data_curves_right.RDS")
################
saveRDS(data_curves_left, "data_curves_left_full.RDS")
saveRDS(data_curves_right, "data_curves_right_full.RDS")
