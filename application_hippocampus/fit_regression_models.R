library(elasdics)

data_curves_left <- readRDS("data_curves_left.RDS")
data_curves_right <- readRDS("data_curves_right.RDS")
data_curves_left_full <- readRDS("data_curves_left_full.RDS")
data_curves_right_full <- readRDS("data_curves_right_full.RDS")
meta_data <- read.csv("meta_data_1.csv")
meta_data$Group <- factor(meta_data$Group, levels = c("CN", "AD"))
meta_data$Sex <- factor(meta_data$Sex, levels = c("M", "F"))
################################################################################
system.time(reg_model_left <- fit_elastic_regression(data_curves ~ Age + Group + Sex, data_curves = data_curves_left, 
                                         x_data = meta_data, knots = seq(0,1,0.05)))
saveRDS(reg_model_left, "reg_models/reg_model_left.RDS")

system.time(reg_model_right <- fit_elastic_regression(data_curves ~ Age + Group + Sex, data_curves = data_curves_right, x_data = meta_data,
                                        knots = seq(0,1,0.05)))
saveRDS(reg_model_right, "reg_models/reg_model_right.RDS")

################################################################################
# no group
system.time(reg_model_left_no_group <- fit_elastic_regression(data_curves ~ Age + Sex, data_curves = data_curves_left, 
                                                     x_data = meta_data, knots = seq(0,1,0.05)))
saveRDS(reg_model_left_no_group, "reg_models/reg_model_left_no_group.RDS")

system.time(reg_model_right_no_group <- fit_elastic_regression(data_curves ~ Age + Sex, data_curves = data_curves_right, x_data = meta_data,
                                                      knots = seq(0,1,0.05)))
saveRDS(reg_model_right_no_group, "reg_models/reg_model_right_no_group.RDS")

###########
# no sex
system.time(reg_model_left_no_sex <- fit_elastic_regression(data_curves ~ Age + Group, data_curves = data_curves_left, 
                                                              x_data = meta_data, knots = seq(0,1,0.05)))
saveRDS(reg_model_left_no_sex, "reg_models/reg_model_left_no_sex.RDS")

system.time(reg_model_right_no_sex <- fit_elastic_regression(data_curves ~ Age + Group, data_curves = data_curves_right, x_data = meta_data,
                                                               knots = seq(0,1,0.05)))
saveRDS(reg_model_right_no_sex, "reg_model_right_no_sex.RDS")

###########
# no age
system.time(reg_model_left_no_age <- fit_elastic_regression(data_curves ~ Sex + Group, data_curves = data_curves_left, 
                                                            x_data = meta_data, knots = seq(0,1,0.05)))
saveRDS(reg_model_left_no_age, "reg_model_left_no_age.RDS")

system.time(reg_model_right_no_age <- fit_elastic_regression(data_curves ~ Sex + Group, data_curves = data_curves_right, x_data = meta_data,
                                                             knots = seq(0,1,0.05)))
saveRDS(reg_model_right_no_age, "reg_model_right_no_age.RDS")

################################################################################
# only group
system.time(reg_model_left_group <- fit_elastic_regression(data_curves ~ Group, data_curves = data_curves_left, 
                                                              x_data = meta_data, knots = seq(0,1,0.05)))
saveRDS(reg_model_left_group, "reg_model_left_group.RDS")

system.time(reg_model_right_group <- fit_elastic_regression(data_curves ~ Group, data_curves = data_curves_right, x_data = meta_data,
                                                               knots = seq(0,1,0.05)))
saveRDS(reg_model_right_group, "reg_model_right_group.RDS")

###########
# only sex
system.time(reg_model_left_sex <- fit_elastic_regression(data_curves ~ Sex, data_curves = data_curves_left, 
                                                            x_data = meta_data, knots = seq(0,1,0.05)))
saveRDS(reg_model_left_sex, "reg_model_left_sex.RDS")

system.time(reg_model_right_sex <- fit_elastic_regression(data_curves ~ Sex, data_curves = data_curves_right, x_data = meta_data,
                                                             knots = seq(0,1,0.05)))
saveRDS(reg_model_right_sex, "reg_model_right_sex.RDS")

###########
# only age
system.time(reg_model_left_age <- fit_elastic_regression(data_curves ~ Age, data_curves = data_curves_left, 
                                                            x_data = meta_data, knots = seq(0,1,0.05)))
saveRDS(reg_model_left_age, "reg_model_left_age.RDS")

system.time(reg_model_right_age <- fit_elastic_regression(data_curves ~ Age, data_curves = data_curves_right, x_data = meta_data,
                                                             knots = seq(0,1,0.05)))
saveRDS(reg_model_right_age, "reg_model_right_age.RDS")


################################################################################
# only intercept model
system.time(reg_model_left_intercept <- fit_elastic_regression(data_curves ~ 1, data_curves = data_curves_left, 
                                                              x_data = meta_data, knots = seq(0,1,0.05)))
saveRDS(reg_model_left_intercept, "reg_model_left_intercept.RDS")

system.time(reg_model_right_intercept <- fit_elastic_regression(data_curves ~ 1, data_curves = data_curves_right, x_data = meta_data,
                                                               knots = seq(0,1,0.05)))
saveRDS(reg_model_right_intercept, "reg_model_right_intercept.RDS")

