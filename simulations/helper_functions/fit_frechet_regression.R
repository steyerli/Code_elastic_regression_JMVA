library(elasdics)

fit_frechet_regression <- function(x_new, data_curves, x, knots = seq(0,1,0.2), type = "smooth",
                                   closed = FALSE, eps = 0.01, max_iter = 10, pre_align = FALSE){
  stopifnot(all(sapply(data_curves, is.data.frame)))
  # remove duplicated points
  data_curves <- lapply(data_curves, elasdics:::remove_duplicate, closed = closed)
  if(sum(sapply(data_curves, function(curve){attributes(curve)$points_rm}) > 0)){
    warning("Duplicated points in data curves have been removed!")
  }
  data_curves <- lapply(data_curves, function(curve){
    attr(curve, "points_rm") <- NULL
    curve
  })
  # input checking given parametrisation t
  lapply(data_curves, function(data_curve){
    if("t" %in% names(data_curve)) elasdics:::check_param(data_curve, closed)
  })
  # input checking for closed curves
  if(closed){
    data_curves <- lapply(data_curves, function(data_curve){
      elasdics:::check_closed(data_curve)
    })
  }
  # parametrisation with respect to arc length if not given,
  # after this, parametrisation is always in the first column
  data_curves <- lapply(data_curves, function(data_curve){
    if(!("t" %in% colnames(data_curve))){
      data.frame("t" = elasdics:::get_arc_length_param(data_curve), data_curve)
    } else {
      param <- data_curve$t
      data_curve$t <- NULL
      data.frame("t" = param, data_curve)
    }
  })
  ##############################################################################

  #compute srv
  srv_data <- lapply(data_curves, elasdics:::get_srv_from_points)

  if(pre_align){
    #initial alignment as optimal alignment to the mean
    mean <- compute_elastic_mean(data_curves, knots = seq(0,1,0.01), max_iter = 100, type = "polygon",
                                 eps = 0.001, closed = closed)

    data_curves <- lapply(mean$data_curves, function(data_curve){
      if(data_curve$t_optim[nrow(data_curve)] == 0){
        data_curve$t_optim[nrow(data_curve)] <- 1
      }
      attr(data_curve, "dist_to_prediction") <- attributes(data_curve)$dist_to_mean
      attr(data_curve, "dist_to_mean") <- NULL
      data_curve
    })
  } else {
    data_curves <- lapply(data_curves, function(data_curve){
      data_curve <- data.frame("t" = data_curve$t, "t_optim" = data_curve$t, data_curve[,-1])
      attr(data_curve, "dist_to_prediction") <- Inf
      data_curve
    })
  }

  srv_data_initial <- lapply(1:length(srv_data), function(i){
    dat <- srv_data[[i]]
    idx_start <- which(data_curves[[i]]$t_optim == 0)[1]:nrow(dat)
    dat <- rbind(dat[idx_start,], dat[-idx_start,])
    dat$t <- dat$t - dat$t[1]
    dat$t <- sapply(dat$t, function(t) ifelse(t < 0, t + 1, t))
    dat
  })
  data_curves_t <- lapply(srv_data_initial, function(srv_data_initial){
    cbind("t" = c(srv_data_initial$t, 1), get_points_from_srv(srv_data_initial))
  })

  #initiate values
  t_optims <- lapply(data_curves, function(curve){
    c(sort(curve$t_optim[-length(curve$t_optim)]), 1)
  })

  # compute weights from covariates
  weight_fun <- function(x_new){
    sigma <- mean((x - mean(x))^2)
    1 + (x - mean(x))*(x_new - mean(x))/sigma
  }

  coefs_list <- lapply(x_new, function(x_new){
    print(x_new)
    weights <- weight_fun(x_new)
    stop_crit <- Inf
    k <- 0
    coefs <- 0
    while(stop_crit > eps & k <= max_iter){
      k <- k + 1
      coefs_old <- coefs
      #SRV spline model fit
      data_curves_t_now <- lapply(1:length(data_curves_t), function(i){
        idx <- c(TRUE, diff(t_optims[[i]]) != 0)
        data.frame("t" = t_optims[[i]][idx], data_curves_t[[i]][idx, -1])
      })
      model_data <- get_frechet_model_data(data_curves_t_now, seq(0,1,0.01), weights)
      design_mat <- make_frechet_design(model_data$m_long, knots, closed = closed, type)

      coefs <- apply(model_data[,-1, drop = FALSE], 2, function(q_m_x_long){
        q_m_x_long[!is.finite(q_m_x_long)] <- NA
        coef(lm(q_m_x_long ~ -1 + design_mat))
      })

      if(closed){
        model_data <- get_frechet_model_data(data_curves_t_now, seq(0,1,0.01), weights)
        #one step of penalised optimisation
        coefs <- elasdics:::one_step_grad(coefs, pen_factor = 10*k, model_data = model_data,
                                          knots = knots, type = type)
      }
      #stop if coefficient don't change much anymore
      stop_crit <- sum((coefs - coefs_old)^2)/sum(coefs^2)

      if(stop_crit < eps & closed){
        #close curve
        coefs <- elasdics:::one_step_grad(coefs, pen_factor = Inf, model_data = model_data,
                                          knots = knots, type = type)}

      # update warping alignment
      t_optims <- lapply(1:length(data_curves), function(i){
        if(type == "smooth"){
          pfun <- function(t){
            t(make_frechet_design(t = t, knots = knots, closed = closed, type = type) %*% coefs)
          }

          elasdics:::find_optimal_t(srv_curve = pfun, s = c(srv_data_initial[[i]][,1],1),
                                    q = t(srv_data_initial[[i]][,-1]), initial_t = t_optims[[i]],
                                    eps = 0.01)
        } else {
          elasdics:::find_optimal_t_discrete(r = knots, p = t(coefs), s = c(srv_data_initial[[i]][,1],1),
                                             q = t(srv_data_initial[[i]][,-1]), initial_t = t_optims[[i]],
                                             eps = 0.01)
        }
      })
    }
    coefs
  })


  names(coefs_list) <- paste0("x=", x_new)
  return(list("coefs_list" = coefs_list, "knots" = knots, "closed" = closed, "type" = type))
}


get_frechet_model_data <- function(data_curves_t, t_grid, weights){
  q_m <- lapply(1:length(data_curves_t), function(i){
    data_curve <- data.frame("t" = t_grid, get_evals(data_curves_t[[i]], t_grid = t_grid))
    get_srv_from_points(data_curve)[,-1]*weights[i]
  })
  #convert in long format
  m_long <- rep(t_grid[-length(t_grid)] + diff(t_grid)/2, length(q_m))
  q_m_long <- do.call(rbind, q_m)
  data.frame("m_long" = m_long, "q_m_long" = q_m_long)
}

# creating the design matrix
make_frechet_design <- function(t, knots, closed = FALSE, type = "smooth") {
  deg <- ifelse(type == "smooth", 1, 0)
  design_mat <- splines:::splineDesign(knots = c(rep(0, deg), knots, rep(1, deg)),
                             x = t, outer.ok = TRUE, ord = deg + 1)
  if(closed == TRUE & type == "smooth"){
    design_mat[,1] <- design_mat[,1] + design_mat[, ncol(design_mat)]
    design_mat <- design_mat[,-ncol(design_mat)]
  }
  design_mat
}

predict_frechet_model <- function(model, t_grid = seq(0,1, 0.01)){
  lapply(model$coefs_list, function(coefs){
    if(model$type == "discrete"){
      pred_data_curve <- get_points_from_srv(data.frame("t" = model$knots[-length(model$knots)], coefs))
    } else {
      srv_mean_curve <- function(t){
        t(make_frechet_design(t = t, model$knots, closed = model$closed, type = model$type) %*% coefs)
        }
      pred_data_curve <- as.data.frame(t(elasdics:::srvf_to_curve(t_grid, srv_mean_curve)))
    }
    #compute translation
    offset <- apply(get_evals(cbind("t" = elasdics:::get_arc_length_param(pred_data_curve),
                                    pred_data_curve)), 2, mean)
    data.frame(t(t(pred_data_curve) - offset))
  })
}
