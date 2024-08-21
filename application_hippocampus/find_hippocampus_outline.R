#library(scatterplot3d)
#library(rgl)
#library(alphahull)
#library(concaveman)
library(EBImage)

find_hippocamous_outline <- function(mask){
  # seperate left and right hippocampus
  idx_x <- round(dim(mask)[1]/2)
  mask_left <- mask[1:idx_x,,]
  attr(mask_left, "pixdim") <- attributes(mask)$pixdim
  mask_right <- mask[dim(mask)[1]:idx_x,,]
  attr(mask_right, "pixdim") <- attributes(mask)$pixdim
  
  outline_left <- find_outline(mask_left)
  outline_right <- find_outline(mask_right)
  outline_right[,1] <- -outline_right[,1]
 list(outline_left, outline_right)
}


find_outline <- function(mask){
  # remove empty columns and rows
  mask_cut <- mask[apply(mask, 1, sum) != 0, apply(mask, 2, sum) != 0, apply(mask, 3, sum) != 0]
  mask_cut[mask_cut != 0] <- 1
  
  #convert to points
  points <- data.frame()
  for(i in 1:dim(mask_cut)[1]){
    for(j in 1:dim(mask_cut)[2]){
      idx_z <- which(mask_cut[i,j,] == 1)
      if(length(idx_z) != 0) points <- rbind(points, cbind(x = i, y = j, z = idx_z))
    }
  }
  points <- t(t(points)*attributes(mask)$pixdim)
  
  #scatterplot3d(points$x, points$y, points$z)
  #plot3d(points)
  
  #center points
  points <- t(t(points) - apply(points, 2, mean))
  
  #find optimal rotation around x axis
  pc1 <- prcomp(points[,2:3])$rotation[,1]
  pc1 <- sign(pc1[1])*pc1
  
  rot_mat <- solve(rbind(c(1,0,0), cbind(0, pc1, c(-pc1[2], pc1[1]))))
  angle <- acos(rot_mat[2,2])*180/pi
  
  mask_rot_list <- lapply(1:dim(mask_cut)[1], function(i) rotate(mask_cut[i,,], angle))
  mask_rot <- abind(mask_rot_list, along = 3)
  mask2d <- apply(mask_rot, 1, colSums) >= 1 
  
  #############################################################################
  #create outline
  outline <- ocontour(mask2d)[[1]]
  
  #find pixel dimension
  pixdims_old <- attributes(mask)$pixdim
  pixdims <- c(pixdims_old[1], (90-abs(angle))/90*pixdims_old[2] + 
                 (1 - (90-abs(angle))/90)*pixdims_old[3])
  
  outline <- data.frame(t(t(outline)*pixdims))
  
  # find start point
  idx_start <- which.max(outline[,2])
  outline <- rbind(outline[idx_start:nrow(outline),], outline[2:(idx_start - 1),])
  
  # find right orientation
  if(mean(outline[1:round(0.5*nrow(outline)), 1]) <= mean(outline[, 1])){
    outline <- outline[nrow(outline):1, ]
  }
  
  #remove duplicates
  outline <- elasdics:::remove_duplicate(outline, closed = FALSE)
  
  # #remove co-linear points
  # diff_outline <- apply(outline, 2, diff)
  # co_lin <- sapply(1:(nrow(diff_outline) - 1), function(i){
  #   ind <- sum(diff_outline[i,]*diff_outline[i+1,])/sqrt(sum(diff_outline[i,]^2)*sum(diff_outline[i+1,]^2))
  #   abs(ind - 1) <= 10^-2
  # })
  # co_lin <- c(FALSE, co_lin, FALSE)
  # outline <- outline[!co_lin,]
  # outline <- simplify_outline(outline, eps = 1)
  row.names(outline) <- NULL
  names(outline) <- c("y1", "y2")
  plot(outline, asp = 1, type = "b")
  
  outline
}


