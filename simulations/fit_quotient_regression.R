library(elasdics)

fit_quotient_regression <- function(formula, data_curves, x_data, knots = seq(0,1,0.2), type = "smooth", max_iter = 10,
                                    eps = 0.001){
  formula <- as.formula(formula)
  if(formula[[2]] != "data_curves") stop("formula must be of form data_curves ~ ...") 
  
  # create x dataframe
  x_data <- data.frame(x_data)
  
  # create x model matrix
  x_model_matrix <- model.matrix(formula, cbind("data_curves" = 1, x_data))
  
  
  #remove duplicated points
  data_curves <- lapply(data_curves, elasdics:::remove_duplicate, closed = FALSE)
  srv_data <- lapply(data_curves, get_srv_from_points)
  
  # compute initial time parametrisation
  mean <- compute_elastic_mean(data_curves, knots = seq(0,1,0.01), type = "polygon", eps = 0.001)
  t_optims <- lapply(mean$data_curves, `[[`, "t_optim")
  data_curves_t <- lapply(mean$data_curves, function(data_curve){
    data_curve <- data_curve[,-1]
    names(data_curve)[1] <- "t"
    data_curve
  })
  
  coefs_list <- 0
  
  for(i in 1:max_iter){
    coefs_list_old <- coefs_list
    
    if(type == "discrete"){
      srv_data_discrete <- lapply(1:length(data_curves), function(i){
        curve_data <- get_evals(cbind("t" = t_optims[[i]], data_curves[[i]]), t_grid = knots)
        unlist(get_srv_from_points(cbind("t" = knots, curve_data))[, -1])
      })
      coefs_discrete <- sapply(1:length(srv_data_discrete[[1]]), function(i){
        response <- sapply(srv_data_discrete, '[[', i)
        lm(response ~ x)$coefficients
      })
      coefs_list <- lapply(1:nrow(coefs_discrete), function(i){
        matrix(coefs_discrete[i,], ncol = ncol(data_curves[[1]]))
      })
    } else {
      for(i in 1:length(data_curves_t)){
        idx <- c(TRUE, diff(t_optims[[i]]) != 0)
        data_curves_t[[i]] <- data.frame("t" = t_optims[[i]][idx], data_curves_t[[i]][idx, -1])
      }
      srv_data <- lapply(data_curves_t, get_srv_from_points)
      
      model_data <- get_model_data(srv_data, x_model_matrix)
      design_mat <- make_design(model_data[, 1:ncol(x_model_matrix)], model_data$m_long, knots, type)
      
      coefs_long <- apply(model_data[,-(1:(ncol(x_model_matrix) + 1)), drop = FALSE], 2, function(q_m_x_long){
        q_m_x_long[!is.finite(q_m_x_long)] <- NA
        coef(lm(q_m_x_long ~ -1 + design_mat))
      })
      
      n_coefs <- nrow(coefs_long)/ncol(x_model_matrix)
      coefs_list <- lapply(1:ncol(x_model_matrix), function(i){
        coefs <- coefs_long[n_coefs*(i - 1) + 1:n_coefs, ]
        colnames(coefs) <- NULL
        rownames(coefs) <- paste0("coef_", 1:n_coefs)
        coefs
      })
    }
        
    names(coefs_list) <- paste0("beta_", 1:ncol(x_model_matrix) - 1)
    
    #stop if coefficients don't change much anymore
    stop_crit <- sum((unlist(coefs_list) - unlist(coefs_list_old))^2)/sum(unlist(coefs_list)^2)
    if(stop_crit < eps | max_iter == 0){
      return(list("coefs_list" = coefs_list, "formula" = formula, "knots" = knots, "type" = type))
    }
    ############################################################################
    # update warping alignment
    t_optims <- lapply(1:length(data_curves), function(i){
      x_i <- x_model_matrix[i,]
      pred_coefs <- Reduce("+", lapply(1:length(x_i), function(j) x_i[j]*coefs_list[[j]]))
      
      if(type == "smooth"){
        pfun <- function(t){
          t(make_design(x = NULL, t = t, knots = knots, type = type) %*% pred_coefs)
        }
        elasdics:::find_optimal_t(srv_curve = pfun, s = c(srv_data[[i]][,1],1), 
                                  q = t(srv_data[[i]][,-1]), eps = 0.01)
      } else {
        elasdics:::find_optimal_t_discrete(r = knots, p = t(pred_coefs), s = c(srv_data[[i]][,1],1), 
                                           q = t(srv_data[[i]][,-1]), eps = 0.01)
      }
    })
  }
  
  return(list("coefs_list" = coefs_list, "formula" = formula, "knots" = knots, "type" = type))
}

get_model_data <- function(srv_data, x_model_data){
  m <- lapply(srv_data, function(srv_data){
    srv_data$t + 0.5*diff(c(srv_data$t, 1))
  })
  q_m <- lapply(srv_data, function(srv_data) srv_data[,-1])

  #convert in long format
  x_long <- do.call("rbind", lapply(1:nrow(x_model_data), function(i){
    sapply(x_model_data[i,], function(x) rep(x, length(m[[i]])))
    }))
  m_long <- do.call(c, m)
  q_m_long <- do.call(rbind, q_m)
  data.frame("x_long" = x_long, "m_long" = m_long, "q_m_long" = q_m_long)
}

# creating the design matrix
make_design <- function(x_design_mat, t, knots, type = "smooth") {
  deg <- ifelse(type == "smooth", 1, 0)
  spline_design_mat <- splines:::splineDesign(knots = c(rep(0, deg), knots, rep(1, deg)),
                                    x = t, outer.ok = TRUE, ord = deg + 1)
  if(is.null(x_design_mat)) return(spline_design_mat)
  x_design_mat <- as.matrix(x_design_mat)
  t(sapply(1:nrow(x_design_mat), function(i) kronecker(x_design_mat[i,], spline_design_mat[i,])))
}

predict_srv_model <- function(model, x_data, t_grid = seq(0,1, 0.01)){
  # create x dataframe
  x_data <- data.frame(x_data)
  
  # create x model matrix
  x_design <- model.matrix(model$formula, cbind("data_curves" = 1, x_data))
  
  lapply(1:nrow(x_design), function(k){
    x <- x_design[k,]
    pred_coefs <- Reduce("+", lapply(1:length(x), function(j) x[j]*model$coefs_list[[j]]))
    if(model$type == "smooth"){
      srv_mean_curve <- function(t){
        t(make_design(x = NULL, t = t, model$knots) %*% pred_coefs)
      }
      pred_data_curve <- as.data.frame(t(elasdics:::srvf_to_curve(t_grid, srv_mean_curve)))
    } else {
      pred_data_curve <- get_points_from_srv(data.frame("t" = model$knots[-length(model$knots)],
                                                        pred_coefs))
    }
    #compute translation
    offset <- apply(get_evals(cbind("t" = elasdics:::get_arc_length_param(pred_data_curve), 
                                    pred_data_curve)), 2, mean)
    data.frame(t(t(pred_data_curve) - offset))
  })
}
