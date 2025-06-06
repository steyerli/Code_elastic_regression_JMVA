library(elasdics)
draw_regression_data <- function(type = 1, sd = 0, sd_2 = 0,
                                 x = seq(-1,1,0.2), t_grid = seq(0,1, 0.02), m_range = NULL){
  stopifnot(type %in% 1:3)
  if(!is.null(m_range)) stopifnot(max(m_range) <= length(t_grid))
  
  end_curves <- get_discrete_end_curves(type)

  # linear model
  linear_model <- function(x){
    if(type %in% 1:2){
      get_points_from_srv(0.5*(1-x)*get_srv_from_points(end_curves[[1]]) +
                          0.5*(1 + x)*get_srv_from_points(end_curves[[2]]))
    } else {
      (0.5*(1-x)*end_curves[[1]] + 0.5*(1+x)*end_curves[[2]])[,-1]
    }

  }
  # draw data with error
  data_curves <- lapply(x, function(x){
    data_curve <- linear_model(x)
    srv_data_curve <- get_srv_from_points(get_evals(cbind("t" = get_arc_length_param(data_curve), data_curve),
                                                    t_grid = t_grid))
    #add gaussian error to srv-curve
    srv_data_curve[,-1] <- srv_data_curve[,-1] + rnorm(n = length(unlist(srv_data_curve[,-1])),mean = 0, sd = sd)
    data_curve <- get_points_from_srv(srv_data_curve)
    
    #select sparse sample
    if(!is.null(m_range)){
      m <- sample(m_range, size = 1)
      data_curve <- sample_sparse_data(m, data_curve)
    }
    
    #add gaussian error to curve
    data_curve <- data_curve + cumsum(rnorm(2*nrow(data_curve), sd = sd_2))
    names(data_curve) <- c("y1", "y2")
    
    #close curve
    if(type == 3) data_curve <- elasdics:::project_curve_on_closed(data_curve)

    #compute offset for data
    offset <- apply(get_evals(cbind("t" = elasdics:::get_arc_length_param(data_curve),
                                    data_curve)), 2, mean)
    data.frame(t(t(data_curve) - offset))
    })
  data_curves
}

sample_sparse_data <- function(m, data_curve){
  for(i in 1:(nrow(data_curve)-m)){
    diff_data <- apply(data_curve, 2,  diff)
    norm_diff_data <- sqrt(rowSums(diff_data^2))
    alpha_bending <- acos(round(rowSums(diff_data[-1,]*diff_data[-nrow(diff_data),])/
      (norm_diff_data[-1]*norm_diff_data[-length(norm_diff_data)]), 3))
    probs <- alpha_bending/pi
    idx <- sample(2:(nrow(data_curve) - 1), size = 1, prob = (1- probs)^4)
    data_curve <- data_curve[-idx,]
  }
  data_curve
}

get_discrete_end_curves <- function(type){
  t_grid <- seq(0,1, by = 0.01)
  if(type == 1){
    fish_1 <- data.frame("y1" = c(6, 4:0, 1:4, 4.5),
                         "y2" = c(1.5, 1.5, 0, -2, -1, 0, 1, 1.5, 0, -1.5, -1))
    fish_1_discrete <- cbind("t" = t_grid,
                             get_evals(compute_elastic_mean(list(fish_1),
                                                            knots = seq(0,1,length = 6)),
                                       t_grid = t_grid))

    fish_2 <- data.frame("y1" = 1.5*c(5:1, -1,  1:5),
                         "y2" = c(2, 0.5, 0, -0.5, -0.5, 0, 1, 1, 0, -0.5, -2))
    fish_2_discrete <- cbind("t" = t_grid,
                             get_evals(compute_elastic_mean(list(fish_2),
                                                            knots = seq(0,1,length = 6)),
                                       t_grid = t_grid))
    aligned_fish <- align_curves(fish_1_discrete, fish_2_discrete, eps = 0.001)
    fish_2_discrete_aligned <- subset(aligned_fish$data_curve2_aligned, select = c("t_optim","y1", "y2"))
    names(fish_2_discrete_aligned)[1] <- "t"
    fish_2_discrete_aligned <- cbind("t" = t_grid, get_evals(fish_2_discrete_aligned, t_grid = t_grid))
    return(list("end_curve_1" = fish_1_discrete, "end_curve_2" = fish_2_discrete_aligned))

  } else if (type == 2){
    t_grid <- seq(0,1, by = 0.02)
    fish_1 <- data.frame("y1" = c(5, 4:1, -1, 1:4, 5),
                         "y2" = c(2, 1.5, 0, -1, -1, 0, 2, 1.3, 0, -1.5, -2))

    fish_1_discrete <- cbind("t" = t_grid,
                             get_evals(compute_elastic_mean(list(fish_1),
                                                            knots = seq(0,1,length = 6)),
                                       t_grid = t_grid))

    fish_2 <- data.frame("y1" = 1.5*c(4, 3.5, 2:0, -2, 0:2, 3.5, 4),
                         "y2" = 1.5*c(0.8, 0.5, 0, -0.5, -0.5, 0, 1, 0.5, 0, -0.5, -1))


    fish_2_discrete <- cbind("t" = t_grid,
                             get_evals(compute_elastic_mean(list(fish_2),
                                                            knots = seq(0,1,length = 6)),
                                       t_grid = t_grid))

    aligned_fish <- align_curves(fish_1_discrete, fish_2_discrete, eps = 0.001)
    fish_1_points <- subset(fish_1_discrete, select = c("y1", "y2"))
    fish_1_points[25,] <- c(-1.2, 0.1)
    fish_1_points[c(24,26), 1] <- -1.6
    fish_1_points[27, 1] <- -2.2
    fish_1_points <- get_evals(data.frame("t" = elasdics:::get_arc_length_param(fish_1_points),
                                          fish_1_points), t_grid = t_grid)
    fish_2_points <- subset(aligned_fish$data_curve2_aligned, select = c("t_optim","y1", "y2"))
    names(fish_2_points)[1] <- "t"
    fish_2_points <- get_evals(fish_2_points, t_grid = t_grid)
    return(list("end_curve_1" = cbind("t" = t_grid, fish_1_points),
                "end_curve_2" = cbind("t" = t_grid, fish_2_points)))
  } else if(type == 3){
    t_grid <- seq(0,1, by = 0.02)
    curve_1 <- data.frame("y1" = c(3, 2.5, 4, 4, -1, -3, -3, -2, 0, 4, 4, 3),
                          "y2" = c(1, 0, -2, -2.5,  -1, -2, -1, 0, 1.5, 2, 1.8, 1))
    
    curve_1_discrete <- cbind("t" = t_grid, get_evals(compute_elastic_mean(list(curve_1),
                                                                           knots = seq(0,1,length = 8),
                                                                           closed = TRUE),
                                       t_grid = t_grid))
    
    curve_2 <- data.frame("y1" = c(6, 4, -1, -3, -3, 0, 2, 5, 6),
                          "y2" = c(0, -1, -1, 1, 3, 1, 1, 2.5, 0))
    
    curve_2_discrete <- cbind("t" = t_grid, get_evals(compute_elastic_mean(list(curve_2),
                                                                           knots = seq(0,1,length = 7),
                                                                           closed = TRUE),
                                                      t_grid = t_grid))
    
    return(list("end_curve_1" = cbind("t" = t_grid, curve_1_discrete),
                "end_curve_2" = cbind("t" = t_grid, curve_2_discrete)))
  }
}
