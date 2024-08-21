library(elasdics)

file_list <- list.files("bootstrap_models/")
reg_model_left <- readRDS("reg_models/reg_model_left.RDS")
reg_model_right <- readRDS("reg_models/reg_model_right.RDS")
################################################################################
# DO NOT RUN!!!
#####################
Age_mean <- round(mean(reg_model_left$x_data$Age))
n <- 100
numbs <- (950 - n + 1):950
################

x_data <- rbind(data.frame("Age" = Age_mean, "Group" = c("CN", "AD"), "Sex" = "M"),
                data.frame("Age" = Age_mean, "Group" = "CN", "Sex" = "F"),
                data.frame("Age" = c(Age_mean - 10, Age_mean + 10), 
                                     "Group" = "CN", "Sex" = "M"))

idx_list <- lapply(1:nrow(x_data), function(i){
  dists <- sapply(file_list, function(file){
    print(do.call("paste", c(x_data[i,], file)))
    reg_model <- readRDS(paste0("bootstrap_models/", file))
    dist_left <- align_curves(predict(reg_model[[1]], x_data[i, ])[[1]], 
                              predict(reg_model_left, x_data[i, ])[[1]])$elastic_dist
    dist_right <- align_curves(predict(reg_model[[2]], x_data[i, ])[[1]], 
                               predict(reg_model_right, x_data[i, ])[[1]])$elastic_dist
    sqrt(dist_left^2 + dist_right^2)
  })
  
  idx <- order(dists)[numbs]
  list("x_data" = x_data[i, ], idx)
})

saveRDS(idx_list, file = "bootstrap_prediction_region_idx.RDS")
################################################################################
pred_regions_idx <- readRDS("bootstrap_prediction_region_idx.RDS")

predictions_bootstrap <- lapply(pred_regions_idx, function(region){
  preds <- lapply(1:length(file_list[region[[2]]]), function(i){
    reg_model <- readRDS(paste0("bootstrap_models/", file_list[region[[2]]][i]))
    curve_left <- predict(reg_model[[1]], newdata = region[[1]])[[1]]
    curve_left$side <- "left"
    curve_right <- predict(reg_model[[2]], newdata = region[[1]])[[1]]
    curve_right$side <- "right"
    curve <- rbind(curve_left, curve_right)
    names(curve) <- c("y1", "y2", "side")
    curve$Group <- region[[1]]$Group
    curve$Sex <- region[[1]]$Sex
    curve$Age <- region[[1]]$Age
    curve$id <- i
    curve
  })
  do.call("rbind", preds)
})

bootstrap_pred_regions <- do.call("rbind", predictions_bootstrap)
saveRDS(bootstrap_pred_regions, file = "bootstrap_pred_regions.RDS")
