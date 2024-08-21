fit_elastic_curve_regression <- function(data_curves, x, knots = seq(0,1,0.2), type = "smooth", max_iter = 5, eps = 0.001){
  #remove duplicated points
  data_curves <- lapply(data_curves, elasdics:::remove_duplicate, closed = FALSE)
  srv_data_curves <- lapply(data_curves, get_srv_from_points)
  
  # compute initial time parametrisation
  mean <- compute_elastic_mean(data_curves, knots = seq(0,1,0.01), type = "polygon", eps = 0.001)
  data_curves_t <- lapply(mean$data_curves, function(curve){
    curve <- curve[,-1]
    names(curve)[1] <- "t"
    curve
  })
  
  coefs_list <- 0
  
  for(i in 1:max_iter){
    coefs_list_old <- coefs_list
    
    if(type == "discrete"){
      data_discrete <- lapply(data_curves_t, function(curve) unlist(get_evals(curve, knots)))
      coefs_discrete <- sapply(1:length(data_discrete[[1]]), function(i){
        response <- sapply(data_discrete, '[[', i)
        lm(response ~ x)$coefficients
      })
      coefs_list <- lapply(1:nrow(coefs_discrete), function(i){
        matrix(coefs_discrete[i,], ncol = ncol(data_curves[[1]]))
      })
      data_curves_t <- lapply(1:length(data_curves), function(i){
        x_i <- c(1, x[i])
        pred_curve <- Reduce("+", lapply(1:length(x_i), function(j) x_i[j]*coefs_list[[j]]))
        data_curve_t <- align_curves(data.frame("t" = knots, pred_curve), data_curves_t[[i]][,-1])$data_curve2_aligned[,-1]
        names(data_curve_t)[1] <- "t"
        data_curve_t
      })
    } else {
      #spline model fit
      model_data <- get_curve_model_data(data_curves_t, x)
      design_mat <- make_curve_design(model_data$x_long, model_data$t_long, knots, deg = 2)
      
      coefs_long <- apply(model_data[,-(1:2), drop = FALSE], 2, function(data_x_long){
        coef(lm(data_x_long ~ -1 + design_mat))
      })
      
      n_coefs <- nrow(coefs_long)/ncol(cbind(1,x))
      coefs_list <- lapply(1:ncol(cbind(1,x)), function(i){
        coefs <- coefs_long[n_coefs*(i - 1) + 1:n_coefs, ]
        colnames(coefs) <- NULL
        rownames(coefs) <- paste0("coef_", 1:n_coefs)
        coefs
      })
      names(coefs_list) <- paste0("beta_", 1:ncol(cbind(1,x)) - 1)
      
      #stop if coefficients don't change much anymore
      stop_crit <- sum((unlist(coefs_list) - unlist(coefs_list_old))^2)/sum(unlist(coefs_list)^2)
      if(stop_crit < eps | max_iter == 0){
        return(list("coefs_list" = coefs_list, "knots" = knots, "type" = type))
      }
      
      # update warping alignment
      for(i in 1:length(data_curves_t)){
        x_i <- c(1, x[i])
        pred_coefs <- Reduce("+", lapply(1:length(x_i), function(j) x_i[j]*coefs_list[[j]]))
        
        betafun_deriv  <- function(t){
          t(make_curve_design(x = NULL, t = t, knots = knots, deg = 2, derivs = 1) %*% pred_coefs)
        }
        pfun <- function(t){
          betafun_deriv(t)/sqrt(sqrt(sum(betafun_deriv(t)^2)))
        }
        optimal_t <- elasdics:::find_optimal_t(srv_curve = pfun, s = c(srv_data_curves[[i]]$t, 1), 
                                               q = t(srv_data_curves[[i]][,-1]), 
                                               initial_t <- data_curves_t[[i]]$t, eps = 0.01)
        data_curves_t[[i]]$t <- optimal_t
      }
    }
   
  }
  
  return(list("coefs_list" = coefs_list, "knots" = knots, "type" = type))
}

get_curve_model_data <- function(data_curves, x){
  t <- lapply(data_curves, '[[', "t")
  data <- lapply(data_curves, function(data) data[,-1])
  
  #convert in long format
  x_long <- do.call(c, lapply(1:length(x), function(i) rep(x[i], length(t[[i]]))))
  t_long <- do.call(c, t)
  data_long <- do.call(rbind, data)
  data.frame("x_long" = x_long, "t_long" = t_long, "data_long" = data_long)
}

# creating the design matrix
make_curve_design <- function(x, t, knots, deg = 2, derivs = 0) {
  spline_design_mat <- splines:::splineDesign(knots = c(rep(0, deg), knots, rep(1, deg)),
                                              x = t, outer.ok = TRUE, ord = deg + 1, derivs = derivs)
  if(is.null(x)) return(spline_design_mat)
  x_design_mat <- cbind(1,x)
  t(sapply(1:nrow(x_design_mat), function(i) kronecker(x_design_mat[i,], spline_design_mat[i,])))
}

predict_model <- function(model, x, t_grid = seq(0, 1, by = 0.01)){
  x_design <- cbind(1, matrix(x))
  lapply(1:nrow(x_design), function(k){
    x <- x_design[k,]
    pred <- Reduce("+", lapply(1:length(x), function(j) x[j]*model$coefs_list[[j]]))
    if(model$type != "discrete"){
      pred <- make_curve_design(x = NULL, t = t_grid, model$knots, deg = 2) %*% pred
    }
    data.frame(pred)
  })
}
