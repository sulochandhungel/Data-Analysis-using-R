library(raster)
Wshed_mat = matrix(c(NA,1,1,NA,1,1,1,NA,NA,1,NA,NA),nrow = 3, ncol = 4, byrow=T)
Wshed_mat
plot(raster(Wshed_mat), col = "lightgray", main = "Wshed_mat", xlim = c(0,1), ylim = c(0,1), legend = FALSE, axes =F)
grid(nx = 4, ny = 3,col = "darkgray",lty = 1)
pts = rasterToPoints((raster(Wshed_mat)))
text(pts,labels=pts[,3])

seq_mat = matrix(1:(dim(Wshed_mat)[1]*dim(Wshed_mat)[2]), nrow = 3, ncol = 4)
plot(raster(seq_mat), col = "lightgray", legend = FALSE, axes =F)
grid(nx = 4, ny = 3,col = "darkgray",lty = 1)
as.vector(seq_mat)
pts = rasterToPoints((raster(seq_mat)))
text(pts,labels=pts[,3])

use_mat = seq_mat + (Wshed_mat-Wshed_mat)
use_mat
plot(raster(use_mat), col = "lightgray", legend = FALSE, axes =F)
grid(nx = 4, ny = 3,col = "darkgray",lty = 1)
as.vector(use_mat)
pts = rasterToPoints((raster(use_mat)))
text(pts,labels=pts[,3])

use_vec1 = as.vector(use_mat)
use_vec1

use_vec_wshed = as.vector(use_vec1[!is.na(use_vec1)])
use_vec_wshed

# Assuming a crop raster
crop_mat1 = matrix(c(NA,2,2,NA,2,2,2,2,NA,NA,NA,NA),nrow =3, ncol = 4, byrow=T)
crop_mat1
plot(raster(crop_mat1), col = "lightpink", main = "crop_mat", legend = FALSE, axes =F)
grid(nx = 4, ny = 3,col = "darkgray",lty = 1)
as.vector(crop_mat1)
pts = rasterToPoints((raster(crop_mat1)))
text(pts,labels=pts[,3])

use_mat = seq_mat + (crop_mat1-crop_mat1)
use_mat
plot(raster(use_mat), col = "lightpink", legend = FALSE, axes =F)
grid(nx = 4, ny = 3,col = "darkgray",lty = 1)
as.vector(use_mat)
pts = rasterToPoints((raster(use_mat)))
text(pts,labels=pts[,3])

use_vec1 = as.vector(use_mat)
use_vec_crop = as.vector(use_vec1[!is.na(use_vec1)])

use_vec = intersect(use_vec_crop, use_vec_wshed)

# The vector use_vec shows which positions we care about in any data raster

ET_mat1 = matrix(c(10,70,50,40,20,30,10,0,40,45,0,0),nrow =3, ncol = 4, byrow=T)
ET_mat1
plot(raster(ET_mat1), col = "lightpink", legend = FALSE, axes =F)
grid(nx = 4, ny = 3,col = "darkgray",lty = 1)
as.vector(ET_mat1)
pts = rasterToPoints((raster(ET_mat1)))
text(pts,labels=pts[,3])

as.vector(ET_mat1)
data_vec = as.vector(ET_mat1)[use_vec]
data_vec

# create matrix with the index and data
Reduced_mat = matrix(NA, ncol = 2, nrow = length(use_vec))
Reduced_mat[,1] = use_vec
Reduced_mat[,2] = data_vec

# 2nd Step: convert this Reduced Matrix to original raster
result_vec = rep(NA,(dim(Wshed_mat)[1]*dim(Wshed_mat)[2]))
result_vec[use_vec] = data_vec
result_mat = matrix(result_vec, nrow = dim(Wshed_mat)[1], ncol = dim(Wshed_mat)[2])

plot(raster(result_mat), col = "lightpink", legend = FALSE, axes =F)
grid(nx = 4, ny = 3,col = "darkgray",lty = 1)
pts = rasterToPoints((raster(result_mat)))
text(pts,labels=pts[,3])


# Analyze the data and plot it
const = c(NA, 3,4,NA,2)
const
Analyzed_data_vec = data_vec + const
Analyzed_data_vec

# This analyzed data should go to the place of 2,4,5,6,7,8 position of the matrix
mat_vec = rep(NA,(dim(Wshed_mat)[1]*dim(Wshed_mat)[2]))
mat_vec[use_vec] = Analyzed_data_vec
mat_vec
ans_mat = matrix(mat_vec, nrow = dim(Wshed_mat)[1], ncol = dim(Wshed_mat)[2])
ans_mat
plot(raster(ans_mat),col = "lightgray")

