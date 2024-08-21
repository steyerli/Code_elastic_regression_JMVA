fit_elastic_curve <- function(data_curves, x, knots = seq(0,1,0.2), type = "smooth", closed = FALSE,
                              max_iter = 10, eps = 0.001, pre_align = FALSE){
  #remove duplicated points
  data_curves <- lapply(data_curves, elasdics:::remove_duplicate, closed = closed)
  srv_data_curves <- lapply(data_curves, get_srv_from_points)

  if(pre_align){
    # compute initial time parametrisation
    mean <- compute_elastic_mean(data_curves, knots = seq(0,1,0.01), type = "polygon", eps = 0.001,
                                 closed = closed)
    data_curves_t <- lapply(mean$data_curves, function(curve){
      curve <- curve[,-1]
      names(curve)[1] <- "t"
      curve[nrow(curve), ]$t <- 1 + curve[1,]$t
      curve <- unique(curve[order(curve$t),])
      curve
    })
  } else {
    data_curves_t <- lapply(data_curves, function(curve){
      data.frame("t" = elasdics:::get_arc_length_param(curve), curve)
    })
  }

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
      model_data <- get_curve_model_data(data_curves_t, x, seq(0,1,0.01))
      design_mat <- make_curve_design(model_data$x_long, model_data$t_long, knots, deg = 2,
                                      closed = closed)

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
        return(list("coefs_list" = coefs_list, "knots" = knots, "type" = type, "closed" = closed))
      }

      # update warping alignment
      for(i in 1:length(data_curves_t)){
        x_i <- c(1, x[i])
        pred_coefs <- Reduce("+", lapply(1:length(x_i), function(j) x_i[j]*coefs_list[[j]]))

        betafun_deriv  <- function(t){
          t(make_curve_design(x = NULL, t = t, knots = knots, deg = 2, derivs = 1,
                              closed = closed) %*% pred_coefs)
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

  return(list("coefs_list" = coefs_list, "knots" = knots, "type" = type, "closed" = closed))
}

get_curve_model_data <- function(data_curves, x, t_grid){
  data <- lapply(data_curves, function(curve){
    get_evals(curve, t_grid = t_grid)
  })

  #convert in long format
  x_long <- do.call(c, lapply(1:length(x), function(i) rep(x[i], length(t_grid))))
  t_long <- rep(t_grid, length(data_curves))
  data_long <- do.call(rbind, data)
  data.frame("x_long" = x_long, "t_long" = t_long, "data_long" = data_long)
}

# creating the design matrix
make_curve_design <- function(x, t, knots, deg = 2, derivs = 0, closed = FALSE) {
  if(closed){
    spline_design_mat <- mgcv:::cSplineDes(x = t, knots = knots,
                                    ord = deg + 1, derivs = derivs)
  } else {
      spline_design_mat <- splines:::splineDesign(knots = c(rep(0, deg), knots, rep(1, deg)),
                                                  x = t, outer.ok = TRUE, ord = deg + 1,
                                                  derivs = derivs)
  }

  if(is.null(x)) return(spline_design_mat)
  x_design_mat <- cbind(1,x)
  t(sapply(1:nrow(x_design_mat), function(i) kronecker(x_design_mat[i,], spline_design_mat[i,])))
}

predict_elastic_curve <- function(model, x, t_grid = seq(0, 1, by = 0.01)){
  x_design <- cbind(1, matrix(x))
  lapply(1:nrow(x_design), function(k){
    x <- x_design[k,]
    pred <- Reduce("+", lapply(1:length(x), function(j) x[j]*model$coefs_list[[j]]))
    if(model$type != "discrete"){
      pred <- make_curve_design(x = NULL, t = t_grid, model$knots,
                                deg = 2, closed = model$closed) %*% pred
    }
    data.frame(pred)
  })
}
