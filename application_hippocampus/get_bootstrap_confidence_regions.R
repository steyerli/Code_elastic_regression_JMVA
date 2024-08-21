library(elasdics)

file_list <- list.files("bootstrap_models/")
reg_model_left <- readRDS("reg_models/reg_model_left.RDS")
reg_model_right <- readRDS("reg_models/reg_model_right.RDS")
numbs <- (950 - n + 1):950
################################################################################
# age coefs
coefs_age_left <- lapply(file_list, function(file){
  reg_model <- readRDS(paste0("bootstrap_models/", file))
  reg_model[[1]]$coefs_list[[2]]
})
coefs_age_right <- lapply(file_list, function(file){
  reg_model <- readRDS(paste0("bootstrap_models/", file))
  reg_model[[2]]$coefs_list[[2]]
})

dists_left <- sapply(coefs_age_left, 
                     function(coef) sum((coef - reg_model_left$coefs_list[[2]])^2))
dists_right <- sapply(coefs_age_right, 
                      function(coef) sum((coef - reg_model_right$coefs_list[[2]])^2))


coefs_age_boot <- list("left" = coefs_age_left[order(dists_left)][numbs],
                       "right" = coefs_age_right[order(dists_right)][numbs])
#############################
# group coefs
coefs_group_left <- lapply(file_list, function(file){
  reg_model <- readRDS(paste0("bootstrap_models/", file))
  reg_model[[1]]$coefs_list[[3]]
})
coefs_group_right <- lapply(file_list, function(file){
  reg_model <- readRDS(paste0("bootstrap_models/", file))
  reg_model[[2]]$coefs_list[[3]]
})

dists_left <- sapply(coefs_group_left, 
                     function(coef) sum((coef - reg_model_left$coefs_list[[3]])^2))
dists_right <- sapply(coefs_group_right, 
                      function(coef) sum((coef - reg_model_right$coefs_list[[3]])^2))


coefs_group_boot <- list("left" = coefs_group_left[order(dists_left)][numbs],
                       "right" = coefs_group_right[order(dists_right)][numbs])
#############################
# sex coefs
coefs_sex_left <- lapply(file_list, function(file){
  reg_model <- readRDS(paste0("bootstrap_models/", file))
  reg_model[[1]]$coefs_list[[4]]
})
coefs_sex_right <- lapply(file_list, function(file){
  reg_model <- readRDS(paste0("bootstrap_models/", file))
  reg_model[[2]]$coefs_list[[4]]
})

dists_left <- sapply(coefs_sex_left, 
                     function(coef) sum((coef - reg_model_left$coefs_list[[4]])^2))
dists_right <- sapply(coefs_sex_right, 
                      function(coef) sum((coef - reg_model_right$coefs_list[[4]])^2))


coefs_sex_boot <- list("left" = coefs_sex_left[order(dists_left)][numbs],
                         "right" = coefs_sex_right[order(dists_right)][numbs])
################################################################################
saveRDS(list("age" = coefs_age_boot, "group" = coefs_group_boot,
             "sex" = coefs_sex_boot), "bootstrap_coef_regions.RDS")
