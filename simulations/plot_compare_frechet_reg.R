library(ggplot2)
library(gridExtra)
library(manipulate)
regression_models <- readRDS("simulation_data/frechet_reg_data")
x <- -5:5/5

g_plots <- lapply(regression_models, function(regression_model){
  curve_data <- lapply(1:length(regression_model[[1]]), function(i){
    regression_model[[1]][[i]]$x <- x[i]
    regression_model[[1]][[i]]
  })
  curve_data <- do.call("rbind", curve_data)
  
  quotient_data <- lapply(1:length(regression_model[[2]]), function(i){
    regression_model[[2]][[i]]$x <- x[i]
    names(regression_model[[2]][[i]]) <- c("y1", "y2", "x")
    regression_model[[2]][[i]]
  })
  quotient_data <- do.call("rbind", quotient_data)
  
  frechet_data <- lapply(1:length(regression_model[[2]]), function(i){
    regression_model[[3]][[i]]$x <- x[i]
    names(regression_model[[3]][[i]]) <- c("y1", "y2", "x")
    regression_model[[3]][[i]]
  })
  frechet_data <- do.call("rbind", frechet_data)
  
  
  bgnd <- theme_get()$panel.background$fill
  ggplot(aes(x = y1, y = y2, group = as.factor(x)), data = curve_data) + geom_path() + facet_grid(~x) + 
    geom_point(size = 1.8, fill = "black", shape = 21, color = bgnd) + coord_fixed() +
    geom_path(data = quotient_data, col = "red", size = 1, alpha = 0.8) +
    geom_path(data = frechet_data, col = "purple", size = 1, alpha = 0.8) 
})

grid.arrange(grobs = g_plots, nrow = length(g_plots))

regression_model <- regression_models[[8]]
manipulate({plot(regression_model[[1]][[i]], type = "b", asp = 1, xlim = c(-5,5), lwd = 3)
  lines(regression_model[[2]][[i]], col = "red", lwd = 3)
  lines(regression_model[[3]][[i]], col = "purple", lwd = 3)
}, 
i = slider(1,11))

