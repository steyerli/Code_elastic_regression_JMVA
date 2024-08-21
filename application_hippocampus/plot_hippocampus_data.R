library(RNifti)
library(ggplot2)
library(grid)
library(gridExtra)
library(elasdics)
library(rgl)
library(png)

HUblue <- colorRampPalette(c(rgb(1,1,1), rgb(0, 55/255, 108/255)))(8)
HUred <- colorRampPalette(c(rgb(1,1,1), rgb(138/255, 15/255, 20/255)))(8)
HUpurple <- colorRampPalette(c(HUred[6],HUblue[6]))(8)
HUsand <- colorRampPalette(c(rgb(1,1,1), rgb(210/255, 192/255, 103/255)))(8)

colours <- c(HUblue[8],  HUsand[8], HUred[8], HUpurple[4])
################################################################################
# do not run !!!!
################################################################################
file_list <- list.files(pattern = ".nii", recursive = TRUE)
mask <- readNifti(file_list[[7]])
box_lengths <- attributes(mask)$pixdim

# select left hippocampus
idx_x <- round(dim(mask)[1]/2)
mask_left <- mask[1:idx_x,,]
attr(mask_left, "pixdim") <- attributes(mask)$pixdim

# remove empty columns and rows
mask_cut <- mask_left[apply(mask_left, 1, sum) != 0, 
                      apply(mask_left, 2, sum) != 0, 
                      apply(mask_left, 3, sum) != 0]
mask_cut[mask_cut != 0] <- 1

#convert to points
points <- data.frame()
for(i in 1:dim(mask_cut)[1]){
  for(j in 1:dim(mask_cut)[2]){
    idx_z <- which(mask_cut[i,j,] == 1)
    if(length(idx_z) != 0) points <- rbind(points, cbind(x = i, y = j, z = idx_z))
  }
}
points <- t(t(points)*box_lengths)

hippo_voxel <- lapply(1:nrow(points), function(i){
  translate3d( 
    scale3d(cube3d(col = "darkgray"), box_lengths[1]/2, box_lengths[2]/2, box_lengths[3]/2),
    points[i,1], points[i,2], points[i,3])
})

open3d()
shapelist3d(hippo_voxel, ylim = c(-10, 45), size = 0.1) 
bbox3d(back = "lines")
#bbox3d(marklen = 1, back = "lines", lsize = 1)
#axes3d(labels = NULL, tick = NULL, expand = 0.5)
points3d(max(points[1,])/20, -0.5, 0, color = "black", alpha = 0)
segments3d(matrix(c(max(points[1,])/20, max(points[1,])/20, 0.1, 0.7, 0 , 0), nrow = 2))
segments3d(matrix(c(0.9, max(points[1,])/10 - 0.9, 0.4, 0.4, 0 , 0), nrow = 2))
text3d(0.6, 0.5 ,0, "left", add = TRUE, adj = 0, cex = 1)
text3d(max(points[1,])/10 -0.7, 0.5,0, "right", add = TRUE, adj = 0, cex = 1)
text3d(1.7, 0.7,0, "anterior", add = TRUE, adj = c(1, 1), cex = 1)
text3d(1.7, -0.3,0, "posterior", add = TRUE, adj = c(1, 1), cex = 1)

rgl.snapshot("../../Figures/3d_hippocampus.png", fmt = "png", top = TRUE)
################################################################################
data_curves_left <- readRDS("data_curves_left.RDS")
meta_data <- read.csv("meta_data_1.csv")

Age_intervals <- cut(as.numeric(meta_data$Age), breaks=c(55,65,70,75, 80, 85, 90), 
                     include.lowest=F)

data_left <- lapply(1:length(data_curves_left), function(i){
  t <- elasdics::get_arc_length_param(data_curves_left[[i]])
  offset <- apply(get_evals(cbind("t" = t, data_curves_left[[i]])), 2, mean)
  data_curve <- data.frame(t(t(data_curves_left[[i]]) - offset))
  data_curve$id <- i
  data_curve$age <- paste0("Age %in%", '"', Age_intervals[i], '"')
  data_curve$status <- paste("Group ==", meta_data$Group[i])
  data_curve
})

plot_data_curves <- do.call("rbind", data_left)
g_age <- ggplot(aes(x = y1, y = y2, group = id), data = plot_data_curves) + geom_path(color = "darkgrey")+ 
  coord_fixed() + facet_grid(status~age, labeller=label_parsed) + xlab(expression(y[1])) + 
  ylab(expression(y[2])) +
  theme(legend.position="top", panel.background = element_blank(), axis.line = element_line(colour = "grey")) 

g_age

################################################################################
hippo_3d <- readPNG("../../Figures/3d_hippocampus.png")

pdf("../../Figures/hippocampus_data.pdf", width = 10, height = 4)
grid.arrange(rasterGrob(hippo_3d),
             g_age, ncol = 2, widths = c(0.4, 0.6))
dev.off()
