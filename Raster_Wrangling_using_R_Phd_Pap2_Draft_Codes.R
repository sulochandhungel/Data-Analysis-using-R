library(raster)
#install.packages("rgeos")
#install.packages("spdep")
library(spdep)
library(maptools)
library(grid)

#####
##### STEP 1: Make a list of files to be used #####

file_list = c() # Container for files
fileWpath_list = c() # Container for file paths

######## -----  Folder 1 
 
wd = "J:/METRIC results for Yakima Landsat 5_Feb_2_2016/HRHW_session/ETs"
all_files = list.files(wd)
file_id = "ETrF_"
yr_id = "2007"
site_id = "hrhw"
ind_file = c()
for (i in 1:length(all_files)){
	if (grepl(file_id,all_files[i]) && grepl(yr_id,all_files[i]) && grepl(site_id,all_files[i]) && !grepl(".aux.xml",all_files[i])){
		ind_file= c(ind_file, i)
	}
}
file_list = c(file_list, all_files[ind_file])
fileWpath_list = c(fileWpath_list,paste(wd,"/",all_files[ind_file],sep=""))


######## ----- Folder 2

wd = "J:/METRIC results for Yakima Landsat 7_Feb_3_2016/HRHW_session/ETs"
all_files = list.files(wd)
site_id = "hrhw"
ind_file = c()
for (i in 1:length(all_files)){
	if (grepl(file_id,all_files[i]) && grepl(yr_id,all_files[i]) && grepl(site_id,all_files[i]) && !grepl(".aux.xml",all_files[i])){
		ind_file= c(ind_file, i)
	}
}
file_list = c(file_list, all_files[ind_file])
fileWpath_list = c(fileWpath_list,paste(wd,"/",all_files[ind_file],sep=""))


######## ----- Folder 3

wd = "J:/METRIC results for Yakima Landsat 5_Feb_2_2016/HERO_session_44_28/ETs"
all_files = list.files(wd)
site_id = "hero"
ind_file = c()
for (i in 1:length(all_files)){
	if (grepl(file_id,all_files[i]) && grepl(yr_id,all_files[i]) && grepl(site_id,all_files[i]) && !grepl(".aux.xml",all_files[i])){
		ind_file= c(ind_file, i)
	}
}
file_list = c(file_list, all_files[ind_file])
fileWpath_list = c(fileWpath_list,paste(wd,"/",all_files[ind_file],sep=""))



######## ----- Folder 4


wd = "J:/METRIC results for Yakima Landsat 7_Feb_3_2016/44_28/ETs"
all_files = list.files(wd)
site_id = "hero"
ind_file = c()
for (i in 1:length(all_files)){
	if (grepl(file_id,all_files[i]) && grepl(yr_id,all_files[i]) && grepl(site_id,all_files[i]) && !grepl(".aux.xml",all_files[i])){
		ind_file= c(ind_file, i)
	}
}
file_list = c(file_list, all_files[ind_file])
fileWpath_list = c(fileWpath_list,paste(wd,"/",all_files[ind_file],sep=""))





#####
##### STEP 1 END #######
#####



##
## THE PLOTS MIGHT MISALIGN ... THIS IS A PROBLEM BUT WHEN CHECKED WITH ARCGIS, IT WORKS FINE 
## DON'T WORRY TOO MUCH ABOUT MIS ALIGNMENT OF PLOTS FOR NOW
##

Wshed_ras = raster("I:\\Sulochan\\StatModel\\Wshed_ras.tif")
plot(Wshed_ras,col="lightgray")
Wshed_ex = extent(Wshed_ras)

crop_year = 2007
cropRas_1 = raster(paste("I:/Sulochan/StatModel/CropScape/Crops_only/CDL_",crop_year,"_Proj_30m.tif",sep=""))
cropRas_2 = crop(cropRas_1, Wshed_ex)
plot(cropRas_2, add = T, col ="yellow")

new.cropRas_2 = projectRaster(cropRas_2, Wshed_ras, method = "ngb")
cropRas = mask(new.cropRas_2, Wshed_ras)
plot(cropRas,add=T,col="red")


# WE WILL BE WORKING ONLY ON PIXELS WITH CROP DATA

## FOR QUICKER ANALYSIS, LETS TRY TO AVOID CALCULATING THE ENTIRE MATRIX
## LETS CONVERT MATRIX TO VECTOR WITHOUT NA VALUES and ANALYZE
## THEN CONVERT THIS NON-NA VECTOR TO MATRIX AND FINALLY TO RASTER



#########

### ------ TRIAL WITH SMALL MATRIX START --------

#########
#install.packages("gplots")
#library(gplots)

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

#########

### ------ TRIAL WITH SMALL MATRIX END --------

#########






#### LET's APPLY SAME PRINCIPLE TO OUR DATA
Wshed_mat = as.matrix(cropRas) #We use crop raster as the pixels we want for our analysis
#plot(raster(Wshed_mat))

seq_mat = matrix(1:(dim(Wshed_mat)[1]*dim(Wshed_mat)[2]), nrow = dim(Wshed_mat)[1], ncol = dim(Wshed_mat)[2])
#as.vector(seq_mat)

use_mat = seq_mat + (Wshed_mat-Wshed_mat)
#use_mat
#plot(raster(use_mat))

use_vec1 = as.vector(use_mat)
#use_vec1

use_vec = as.vector(use_vec1[!is.na(use_vec1)])
#use_vec

Wshed_ras = raster("I:\\Sulochan\\StatModel\\Wshed_ras.tif")
Wshed_ex = extent(Wshed_ras)

if (!"data_matrix2008.csv" %in% list.files("I:\\Sulochan\\StatModel")){
	data_matrix = matrix(NA, nrow=length(use_vec), ncol = (length(fileWpath_list)+2))
	data_matrix[,1] = use_vec
	## LETS get ET data to vectors and finally into a data frame
	for (i in 1:length(fileWpath_list)){
		print (i)
		print (fileWpath_list[i])
		ETras1 = raster(fileWpath_list[i])
		ETras2 = projectRaster(ETras1, Wshed_ras)
		ETras3 = crop(ETras2, Wshed_ex)
		ETRas = mask(ETras3, Wshed_ras)
	
		ETmat = as.matrix(ETRas)
		ET_vec = as.vector(ETmat)[use_vec]
		data_matrix[,i+1] = ET_vec
	}
	CropMat = as.matrix(cropRas)
	CropVec = as.vector(CropMat)[use_vec]
	data_matrix[,(length(fileWpath_list)+2)] = CropVec
	
	write.csv(data_matrix, file = "I:\\Sulochan\\StatModel\\data_matrix.csv")
} else {
	tt = read.csv("I:\\Sulochan\\StatModel\\data_matrix2008.csv", header = T)
	data_matrix = as.matrix(tt[,2:dim(tt)[2]])
}

## COLUMN NAMES ###
# lets put in column names
###
my_dates1 = c()
col_names = c()
for (i in 1:length(file_list)){
	L_no = strsplit(file_list[i],"_")[[1]][3]
	yr = strsplit(file_list[i],"_")[[1]][4]
	mo = strsplit(file_list[i],"_")[[1]][5]
	da = strsplit(file_list[i],"_")[[1]][6]

	mydate = paste(yr,"_",mo,"_",da,sep="")
	col_names = c(col_names, paste(L_no,"_",mydate,sep=""))
	my_dates1 = c(my_dates1, mydate)
}
my_dates2 = as.Date(my_dates1, format="%Y_%m_%d")
ndates = length(my_dates2)
colnames(data_matrix) = c("Mat_Id", col_names, "Crop_ID")


## CHECK - trying to see if converting vector to image works
ETras1 = raster(fileWpath_list[1])
x1 = as.vector(data_matrix[,2])
mat_vec = rep(NA,(dim(Wshed_mat)[1]*dim(Wshed_mat)[2]))
mat_vec[data_matrix[,1]] = x1
ans_mat = matrix(mat_vec, nrow = dim(Wshed_mat)[1], ncol = dim(Wshed_mat)[2])
#plot(raster(ans_mat))
cropRas2 = cropRas
cropRas2[,] = ans_mat

layout(matrix(c(1,2), 2, 1, byrow = TRUE))
plot(ETras1)
plot(cropRas2)
#writeRaster(cropRas2, file="I:\\Sulochan\\StatModel\\chk_ras.tif")

## IT WORKS! ##



###
# The columns are not sorted by date, so lets sort them
##
sorted_dates_ind = c()
for (i in 1:ndates){
	sorted_dates_ind = c(sorted_dates_ind, which(my_dates2 == sort(my_dates2)[i]))
}
datamat = cbind(data_matrix[,1],data_matrix[,sorted_dates_ind+1],data_matrix[,dim(data_matrix)[2]])
colnames(datamat) = c("Mat_Id", col_names[sorted_dates_ind], "Crop_ID")

write.csv(datamat, file = "I:\\Sulochan\\StatModel\\datamat.csv")
#datamat_backup = datamat

## Date sorted ET data! ###


## Convert less than zero to NA and greater than 1.2 to 1.2
##
datamat2 = datamat
datamat2[datamat2<0]=NA
n = dim(datamat2)[2]

datamat2[,c(2:(n-1))][datamat2[,c(2:(n-1))]>1.2]=1.2
datamat = datamat2[,c(1,c(2:(n-1)),dim(datamat2)[2])]

head(datamat)
## Done! ##


## Get weights for each row based on how much data is available ##
n_cols = dim(datamat)[2]

LT_dates_unlisted = unlist(strsplit(colnames(datamat)[2:(n_cols-1)],"_"))
LT_dates = paste(LT_dates_unlisted [seq(2,length(LT_dates_unlisted),4)],"_",
			LT_dates_unlisted [seq(3,length(LT_dates_unlisted),4)],"_",
			LT_dates_unlisted [seq(4,length(LT_dates_unlisted),4)],sep="")
				
my_dates2 = as.Date(LT_dates, format="%Y_%m_%d")
sorted_dates = sort(my_dates2)
diff_days = sorted_dates[2:length(sorted_dates)] - sorted_dates[1:(length(sorted_dates)-1)]

wts = c()
wts_mons = c()

# mon_primes = c(2,3,5,7,11,13,17,19,23,29,31,37)
# prod(mon_primes)

for (i in 1:length(diff_days)){
	x = as.numeric(diff_days[i]/2)
	wt = x/as.numeric(sum(diff_days))
	mon = as.numeric(format(sorted_dates[i+1] , "%m"))

	wts_mons = c(wts_mons, mon)
	wts = c(wts, wt)
}

wts1 = c(0,wts/2)
wts2 = c(wts/2,0)

wts_mat = matrix(wts1 + wts2, ncol =1)

ET_datamat = datamat[,2:(n_cols-1)]
binmat = (ET_datamat - ET_datamat+1)
binmat[is.na(binmat)]=0

wt_matrix = binmat %*% (wts_mat)
n = length(sorted_dates)
ndays = sorted_dates[n] - sorted_dates[1] +1
Full_seq_Dates = seq.Date(sorted_dates[1],sorted_dates[n],by="day")

# weights found: wt_matrix



###  DO THESE crop coefficients make sense?
CropList = c(1,2,3,4,5,6,10,11,12,13,14,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,65,66,67,68,69,70,71,72,74,75,76,77,81,82,83,87,88,92,111,112,121,122,123,124,131,141,142,143,152,176,190,195,204,205,206,207,208,209,210,211,212,213,214,216,217,218,219,220,221,222,223,224,225,226,227,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,254)
CropNames = c("Corn","Cotton","Rice","Sorghum","Soybeans","Sunflower","Peanuts","Tobacco","Sweet Corn","Pop or Orn Corn","Mint","Barley","Durum Wheat","Spring Wheat","Winter Wheat","Other Small Grains","Dbl Crop WinWht/Soybeans","Rye","Oats","Millet","Speltz","Canola","Flaxseed","Safflower","Rape Seed","Mustard","Alfalfa","Other Hay/Non Alfalfa","Camelina","Buckwheat","Sugarbeets","Dry Beans","Potatoes","Other Crops","Sugarcane","Sweet Potatoes","Misc Vegs & Fruits","Watermelons","Onions","Cucumbers",
			"Chick Peas","Lentils","Peas","Tomatoes","Caneberries","Hops","Herbs","Clover/Wildflowers","Sod/Grass Seed","Switchgrass","Fallow/Idle Cropland","Forest","Shrubland","Barren","Cherries","Peaches","Apples","Grapes","Christmas Trees","Other Tree Crops","Citrus","Pecans","Almonds","Walnuts","Pears","Clouds/No Data","Developed","Water","Wetlands","Nonag/Undefined","Aquaculture","Open Water","Perennial Ice/Snow","Developed/Open Space","Developed/Low Intensity","Developed/Med Intensity",
			"Developed/High Intensity","Barren","Deciduous Forest","Evergreen Forest","Mixed Forest","Shrubland","Grass/Pasture","Woody Wetlands","Herbaceous Wetlands","Pistachios","Triticale","Carrots","Asparagus","Garlic","Cantaloupes","Prunes","Olives","Oranges","Honeydew Melons","Broccoli","Peppers","Pomegranates","Nectarines","Greens","Plums","Strawberries","Squash","Apricots","Vetch","Dbl Crop WinWht/Corn","Dbl Crop Oats/Corn","Lettuce","Pumpkins","Dbl Crop Lettuce/Durum Wht",
			"Dbl Crop Lettuce/Cantaloupe","Dbl Crop Lettuce/Cotton","Dbl Crop Lettuce/Barley","Dbl Crop Durum Wht/Sorghum","Dbl Crop Barley/Sorghum","Dbl Crop WinWht/Sorghum","Dbl Crop Barley/Corn","Dbl Crop WinWht/Cotton","Dbl Crop Soybeans/Cotton","Dbl Crop Soybeans/Oats","Dbl Crop Corn/Soybeans","Blueberries","Cabbage","Cauliflower","Celery","Radishes","Turnips","Eggplants","Gourds","Cranberries","Dbl Crop Barley/Soybeans")
#write.csv(matrix(c(CropList, CropNames),ncol=2), file="I:\\Sulochan\\StatModel\\CropList.csv")


CropList[which(CropNames == "Alfalfa")]

crops_ = datamat[,dim(datamat)[2]]
crops_fac = as.factor(crops_)
Cnames = CropNames[match(as.numeric(levels(crops_fac)), CropList)]
par(mar = c(9,5,2,2))
plot(crops_fac,las=2, names.arg =Cnames)
box()

crops = unique(datamat[,dim(datamat)[2]])
cropNo = 1
CropList_ind = which(cropNo == CropList)
CropName = CropNames[CropList_ind]
print (CropName)

ind = which(cropNo == crops_)
length(ind)
mydata = datamat[ind,c(2:(dim(datamat)[2]-1))]
yrs = as.numeric(unlist(strsplit(colnames(mydata),"_"))[seq(2,dim(mydata)[2]*4,4)]) # Get years from column names of mydata variable
mos = as.numeric(unlist(strsplit(colnames(mydata),"_"))[seq(3,dim(mydata)[2]*4,4)]) # Get months from column names of mydata variable
das = as.numeric(unlist(strsplit(colnames(mydata),"_"))[seq(4,dim(mydata)[2]*4,4)]) # Get days from column names of mydata variable
sorted_dates = as.Date(paste(yrs,mos,das,sep="/"),"%Y/%m/%d")

plot(sorted_dates, mydata[1,],"b",col="gray",ylim=c(0,1.2), xlim = c(as.Date("2007-05-01"),as.Date("2007-10-01")))
rand_1 = sample(2:dim(mydata)[1],100) 
for (i in rand_1){
	print (i)
	lines(sorted_dates, mydata[i,],"b",col=i)
}

abline(v=as.Date("2007-05-01"))
abline(v=as.Date("2007-06-01"))
abline(v=as.Date("2007-07-01"))
abline(v=as.Date("2007-08-01"))
abline(v=as.Date("2007-09-01"))
abline(v=as.Date("2007-10-01"))

##################################################



##### CHECKING THE TYPES OF INTERPOLATION FUNCTIONS ##
yrs = as.numeric(unlist(strsplit(colnames(ET_datamat),"_"))[seq(2,dim(ET_datamat)[2]*4,4)]) # Get years from column names of mydata variable
mos = as.numeric(unlist(strsplit(colnames(ET_datamat),"_"))[seq(3,dim(ET_datamat)[2]*4,4)]) # Get months from column names of mydata variable
das = as.numeric(unlist(strsplit(colnames(ET_datamat),"_"))[seq(4,dim(ET_datamat)[2]*4,4)]) # Get days from column names of mydata variable
sorted_dates = as.Date(paste(yrs,mos,das,sep="/"),"%Y/%m/%d")

corn_inds = which(crops_ == 1) # 1 => Corn
rand_1 = sample(corn_inds, 100)
for (i in rand_1){
	if (wt_matrix[i,1] > 0.20){
		tt = spline(ET_datamat[i,],xmin = 1, xmax = n, n =ndays,method="natural")
		plot(Full_seq_Dates, tt$y, "l", col = "red",ylim=c(0,1.2))
		tt = approx(x = sorted_dates, y = ET_datamat[i,],xout = Full_seq_Dates,method="linear")
		lines(Full_seq_Dates, tt$y, "b", col = "green")
		mtext(signif(wt_matrix[i,1],digits = 4),3)
		mtext(datamat[i,ncol(datamat)],3,line=1)

		lines(sorted_dates, ET_datamat[i,], "b", lwd=2)
		readline()
	}
}

##########

# Some of the data seems to have high bias. 
# So I will try to select only the best month's data for optimization (May, June, July, August, September, October)

start_date = as.Date("2007/05/01", format = "%Y/%m/%d")
end_date = as.Date("2007/11/01", format = "%Y/%m/%d")

sel_dates = sorted_dates[sorted_dates>start_date & sorted_dates<end_date]
sel_dates_ind = match(sel_dates, sorted_dates) + 1
colnames(datamat1)[sel_dates_ind]

#datamat1 = datamat
datamat =datamat1[,c(1,sel_dates_ind,dim(datamat1)[2])]

#ET_datamat1 = ET_datamat
ET_datamat = ET_datamat1[,sel_dates_ind-1]

#sorted_dates1 = sorted_dates
sorted_dates = sorted_dates1[sel_dates_ind-1]
n=length(sorted_dates)
Full_seq_Dates = seq.Date(sorted_dates[1],sorted_dates[n],by="day")

ref_ET_csv = read.csv("I:\\Sulochan\\StatModel\\HRHW_2007_ETrs_ETos_ETpenman.csv")
str(ref_ET_csv)

ref_ET_dates = as.Date(levels(ref_ET_csv$DATE)[ref_ET_csv$DATE],format="%m/%d/%Y")[1:365]
ETrs_ = ref_ET_csv$ET_RS[1:365] # Long Grass
ETos_ = ref_ET_csv$ET_OS[1:365] # Short grass

plot(ref_ET_dates, ETrs_*25.4, "l", col="red", ylab = "ET (mm)", xlab = "Date")
lines(ref_ET_dates, ETos_*25.4, "l", col="blue")
#CropSyst_ET = scan()
lines(ref_ET_dates, CropSyst_ET, "l", col = "Green")
legend(ref_ET_dates[15], 11, c("Long", "Short", "CropSyst"), col = c("Red", "Blue", "Green"), bty="n", lwd =1)

inds = which(!is.na(match(ref_ET_dates, Full_seq_Dates)))
ETrs = ETrs_[inds]
lines(Full_seq_Dates, ETrs, lwd = 2)

inds = which(!is.na(match(ref_ET_dates, sel_dates)))
ETrs = ETrs_[inds] * 25.4
ETos = ETos_[inds] * 25.4
CSyst_ET = CropSyst_ET[inds]

crops_ = datamat[,dim(datamat)[2]]
corn_inds = which(crops_ == 1)
pixel_no = corn_inds[11]
plot(sel_dates, ETrs*ET_datamat[pixel_no,], col="red", cex = 1, pch = 20, type = "b",ylim = c(0, max(ETrs*ET_datamat[pixel_no,],na.rm=T)))
lines(ref_ET_dates, CropSyst_ET,"l", col ="gray")

#ETs = (ETrs+ETos)/2
#ETrs = ETs

plot(sel_dates, ETrs*ET_datamat[1,], col="gray", cex = 1, pch = 20, type = "p",ylim = c(0, max(ETrs*ET_datamat[corn_inds,],na.rm=T)))
rand_1 = sample(corn_inds, 1000)
for (i in rand_1){
  points(sel_dates, ETrs*ET_datamat[i,], col="gray", cex = 1, pch = 20, type = "p")
}
lines(ref_ET_dates, CropSyst_ET,"l", col ="red")
corn_data = ET_datamat[rand_1,]
corn_data_full = datamat[rand_1,]

points(sel_dates, colMeans(corn_data, na.rm=T)*ETrs, pch = 2, cex =1)

#write.csv(corn_data, file = "I:/Sulochan/StatModel/corn_data.csv")
#write.csv(corn_data_full, file = "I:/Sulochan/StatModel/corn_data_full.csv")




########### START HERE IF FAILS ####
#corn_data = read.csv("I:/Sulochan/StatModel/corn_data.csv", header = T)
#corn_data_full = read.csv("I:/Sulochan/StatModel/corn_data_full.csv", header = T)
ind_dates = which(!is.na(match(ref_ET_dates, sel_dates)))
#ref_ET_dates[ind_dates]
ref_ETrs = ETrs_[ind_dates] * 25.4

pix_no = 700
pix_Kc = corn_data[pix_no,]
pix_ET = pix_Kc * ref_ETrs

non_NA_ind = which(!is.na(pix_ET))
m = length(non_NA_ind)
nonNA_dates = sel_dates[non_NA_ind]
METRIC_ET = pix_ET[non_NA_ind]



# CropSyst ET
# GET THE DATA FROM CROPSYST
# CropSyst = scan()
# cropSyst_ET = CropSyst[ind_dates]

ind_nonNA_dates = which(!is.na(match(ref_ET_dates, nonNA_dates)))
#ref_ET_dates[ind_nonNA_dates]
cropSyst_ET = CropSyst[ind_nonNA_dates]
par(mar = c(6,6,4,4))
plot(ref_ET_dates, CropSyst, col="pink",type = "l", lwd =2, ylim = c(0, max(METRIC_ET, cropSyst_ET)), xlim = c(min(nonNA_dates), max(nonNA_dates)),
     xlab = "Date 2007", ylab = "ET (mm)", cex.lab = 2, cex.axis =2)
#lines(ref_ET_dates[ind_nonNA_dates], cropSyst_ET,type= "b",col="red",lty = 2 )
#lines(nonNA_dates, METRIC_ET, col = "black", type = "b", lty = 2)
points(ref_ET_dates[ind_nonNA_dates], cropSyst_ET,type= "p",col="red",lwd = 2)
points(nonNA_dates, METRIC_ET, col = "black", type = "p", lwd = 2,cex = 1)


init_cost_fn = (1/m) * (sum(abs(cropSyst_ET - METRIC_ET)/METRIC_ET)) # << == THIS IS THE FUNCTION WE WANT TO MINIMIZE


#cropSyst2_ET = CropSyst2[ind_nonNA_dates]
#lines(ref_ET_dates, CropSyst2, col = "gray", type = "l") # Irrigation when Depletion at 0.4
#lines(ref_ET_dates, CropSyst3, col = "gray", type = "l") #  Irrigation when depletion at 0.6
#lines(ref_ET_dates, CropSyst4, col = "gray", type = "l") #  Irrigation when depletion at 0.1
lines(ref_ET_dates, cropSyst6, col = "green", type = "l", lwd = 2) # from 6/3 to 5/6
legend(nonNA_dates[1]-10, max(METRIC_ET, cropSyst_ET), c("METRIC", "CropSyst", "Optimized"), col = c("Black", "Red", "Green"), bty="n", lwd =2, cex = 2)
points(ref_ET_dates[ind_nonNA_dates], cropSyst_ET,type= "p",col="red",lwd = 2)
points(nonNA_dates, METRIC_ET, col = "black", type = "p", lwd = 2,cex = 3)
grid()

cropSyst_ET = cropSyst6[ind_nonNA_dates]
fin_cost_fn = (1/m) * (sum(abs(cropSyst_ET - METRIC_ET)/METRIC_ET))













boxplot(corn_data)
lines(ref_ET_dates, CropSyst_ET,"l", col ="red")

if (!"Total_ET_matrix.csv" %in% list.files("I:\\Sulochan\\StatModel")){
	Total_ET = matrix(NA, nrow = nrow(ET_datamat), ncol = 7)
	colnames(Total_ET) = c("Mat_ID","ET_natural_spline","ET_linear_approx","ET_monthly_spline","ET_monthly_approx","CropID","Weight")
	Total_ET[,1] = datamat[,1]

	Sum_ET_nat_spline = c()
	Sum_ET_lin_approx = c()
	Sum_ET_mon_spline = c()
	Sum_ET_nat_spline_mon = c()
	Sum_ET_lin_approx_mon = c()

	yr = as.numeric(format(sorted_dates,"%Y"))[1]
	mons = as.numeric(format(sorted_dates,"%m"))
	unq_mons = unique(mons)+1
	
	avg_dates = as.Date(paste(yr,"-",unq_mons,"-01",sep=""),format="%Y-%m-%d")
	st_date_avg_dates = as.Date(paste(yr,"-",unq_mons[1],"-01",sep=""),format="%Y-%m-%d")
	en_date_avg_dates = as.Date(paste(yr,"-",unq_mons[length(unq_mons)]-1,"-01",sep=""),format="%Y-%m-%d")

	avg_dates = seq(st_date_avg_dates, en_date_avg_dates, by = "1 month")
	mos = format(avg_dates,"%m")
	unq_mons = as.numeric(mos)	

	ET_mon_mat = matrix(NA, nrow = nrow(ET_datamat), ncol = length(avg_dates)+2)
	colnames(ET_mon_mat) = c("Mat_ID",as.character(avg_dates),"CropID")
	ET_mon_mat[,1] = datamat[,1]
	ET_mon_mat[,dim(ET_mon_mat)[2]] = datamat[,dim(datamat)[2]]
	
	for (ctr in 1:length(unq_mons)){
		if (ctr == 1){
			avg_mon_ind = which(sorted_dates<avg_dates[ctr])
			if (length(avg_mon_ind)>0){
				print (sorted_dates[avg_mon_ind])
				if (length(avg_mon_ind)>1){
					ET_mon_mat[,ctr+1] = rowMeans(ET_datamat[,avg_mon_ind],na.rm=T)
				} else {
					ET_mon_mat[,ctr+1] = ET_datamat[,avg_mon_ind]
				}
			}
		}
	
		if (ctr > 1 && ctr<length(unq_mons)){
			avg_mon_ind = which(sorted_dates>=avg_dates[ctr] & sorted_dates<avg_dates[ctr+1])
			if (length(avg_mon_ind)>0){
				print (sorted_dates[avg_mon_ind])
				ET_mon_mat[,ctr+1] = rowMeans(ET_datamat[,avg_mon_ind],na.rm=T)
			}
		}
	
		if (ctr==length(unq_mons)){
			avg_mon_ind = which(sorted_dates>avg_dates[ctr])
			if (length(avg_mon_ind)>0){
				print (sorted_dates[avg_mon_ind])
				if (length(avg_mon_ind)>1){
					ET_mon_mat[,ctr+1] = rowMeans(ET_datamat[,avg_mon_ind],na.rm=T)
				} else {
					ET_mon_mat[,ctr+1] = ET_datamat[,avg_mon_ind]
				}
			}
		}
	}
	ET_mon_mat[is.nan(ET_mon_mat)]=NA
	
	n = length(sorted_dates)
	ndays = sorted_dates[n] - sorted_dates[1] +1
	
	n_mon = length(avg_dates)
	ndays_mon = avg_dates[n_mon] - avg_dates[1] +1
	
	Full_seq_Dates_mon = seq.Date(avg_dates[1],avg_dates[length(avg_dates)],by="day")
	inds_mon = which(!is.na(match(ref_ET_dates, Full_seq_Dates_mon)))
	ETrs_mon = ETrs_[inds_mon]
	

	ctrs = sample(1:nrow(ET_datamat),100)
	#corn_inds = which(datamat[,35]==36) #68 => Apple #1=> Corn # 36 => Alfalfa
	#ctrs = sample(corn_inds, 5000)
	#for (i in 1:nrow(ET_datamat)){
	#plot(sorted_dates, ET_datamat[1,],
	#	 "b", col=0,
	#	 ylim = c(0,1.2),
	#	 xlim = c(as.Date("2007-05-01"),as.Date("2007-10-01")),
	#	 lwd=1, main = "Interpolation Methods",
	#	 ylab = "Ref ET Fraction", xlab = "Date (2007)")
	grid()
	abline(v=sorted_dates,lwd=2,lty=2,col="green")
	#lst = list()
	for (i in ctrs){
		print (i)
		cropNo = datamat[i,dim(datamat)[2]]
		CropList_ind = which(cropNo == CropList)
		CropName = CropNames[CropList_ind]
		print (CropName)
		#inds_ET_datamat = which(!is.na(match(ref_ET_dates, sorted_dates)))
		plot(sorted_dates, ET_datamat[i,], "p", ylim = c(0,1.4),lwd=2,cex=2,col="gray", main = paste(wt_matrix[i,1], "\n", CropName,sep=""),ylab = "Ref ET Fraction", xlab = "Date (2007)" )
	
		## ALL DATA ##	
		if (length(which(!is.na(ET_datamat[i,2:(dim(ET_datamat)[2]-1)])))>1){
			tt_nat_spline = spline(ET_datamat[i,], xmin = 1, xmax = n, n = ndays, method="natural")
			y1 = tt_nat_spline$y
			y1[y1<0] = 0
			y1[y1>1.2] = 1.2
			Sum_ET_nat_spline = c(Sum_ET_nat_spline, sum(y1*ETrs, na.rm=T))
			lines(Full_seq_Dates, y1, "l", col = "red", lty=2, lwd=1)
		
			tt_lin_approx = approx(x = sorted_dates, y = ET_datamat[i,], xout = Full_seq_Dates, method="linear")
			y1 = tt_lin_approx$y
			y1[y1<0] = 0
			y1[y1>1.2] = 1.2
			Sum_ET_lin_approx = c(Sum_ET_lin_approx, sum(y1*ETrs,na.rm=T))
			lines(Full_seq_Dates, y1, "l", col = "blue", lty=2, lwd=1)
		} else {
			Sum_ET_nat_spline = c(Sum_ET_nat_spline, NA)
			Sum_ET_lin_approx = c(Sum_ET_lin_approx, NA)
		}
		
		### MONTHLY ###
		if (length(which(!is.na(ET_mon_mat[i,2:(dim(ET_mon_mat)[2]-1)])))>1){
	
			tt_nat_spline_mon = spline(ET_mon_mat[i,2:(dim(ET_mon_mat)[2]-1)],xmin = 1, xmax = n_mon, n = ndays_mon, method="natural")
			y1 = tt_nat_spline_mon$y
			y1[y1<0] = 0
			y1[y1>1.2] = 1.2
			Sum_ET_nat_spline_mon = c(Sum_ET_nat_spline_mon, sum(y1*ETrs_mon, na.rm=T))
			lines(Full_seq_Dates_mon, y1, "l", col = "green", lty=2, lwd =3)
			#lst = c(lst, list(y1*ETrs_mon))
	
			tt_lin_approx_mon = approx(x = avg_dates, y = ET_mon_mat[i,2:(dim(ET_mon_mat)[2]-1)], xout = Full_seq_Dates_mon, method="linear")
			y1 = tt_lin_approx_mon$y
			y1[y1<0] = 0
			y1[y1>1.2] = 1.2
			Sum_ET_lin_approx_mon = c(Sum_ET_lin_approx_mon, sum(y1*ETrs_mon, na.rm=T))
			lines(Full_seq_Dates_mon, y1, "l", col = "orange", lty=2, lwd = 3)
		} else {
			Sum_ET_nat_spline_mon = c(Sum_ET_nat_spline_mon, NA)
			Sum_ET_lin_approx_mon = c(Sum_ET_lin_approx_mon, NA)
		}
		#corn_date = seq(as.Date("2007-05-01"),as.Date("2007-09-02"),by="1 day")
		#if (CropName == "Corn"){
			#lines(corn_date, corn_ET_Prosser, "l",lwd=3,col="red")
		#} else if (CropName == "Apples"){
			#lines(corn_date, apple_ET_Prosser, "l",lwd=3,col="red")
		#} else if (CropName == "Alfalfa"){
			#alfalfa_date = seq(as.Date("2007-05-01"),as.Date("2007-10-02"),by="1 day")
			#lines(alfalfa_date, alfalfa_ET_Prosser, "l",lwd=3,col="red")
		#}
		grid()
		legend(as.Date("2007-05-1"), 1.4, c("Nat Spline","Lin Interp","Mon nat Spline","Mon Lin Approx"),col=c("red","blue","green","orange"),lty=2,lwd=2)
		## Plotting ##
		readline()

	}
	#mean_mat = matrix(unlist(lst), ncol = 100, nrow=154)
	#mean_vec = rowMeans(mean_mat)
	#lines(Full_seq_Dates_mon, mean_vec, lwd=2)
	#which(!is.na(match(Full_seq_Dates_mon, corn_date)))

	#Cumulative plots
	
	Total_ET[,2] = Sum_ET_nat_spline
	Total_ET[,3] = Sum_ET_lin_approx
	Total_ET[,4] = Sum_ET_nat_spline_mon
	Total_ET[,5] = Sum_ET_lin_approx_mon
	Total_ET[,6] = datamat[,dim(datamat)[2]]
	Total_ET[,7] = wt_matrix[,1]
	
	#write.csv(Total_ET, file = "I:\\Sulochan\\StatModel\\Total_ET_matrix.csv")
} else {
	tt = read.csv("I:\\Sulochan\\StatModel\\Total_ET_matrix.csv", header = T)
	Total_ET = as.matrix(tt[,2:dim(tt)[2]])
}


### Checking for Corn
MyCrop = "Corn"
crop_ind = which(datamat[,dim(datamat)[2]] == which(CropNames == MyCrop))
length(crop_ind)

crop_data = datamat[crop_ind,]
plot(sorted_dates, crop_data[1,2:34], "b", ylim = c(0,1.2), col = "lightgray")
for (i in 2:100){
  lines(sorted_dates, crop_data[i,2:34], "b", ylim = c(0,1.2), col = "lightgray")
}

boxplot(crop_data[,2:21], border = "lightgray", las = 2)  
apply(crop_data, 2, function(x){length(which(!all(is.na(x))))})*100/180798

dates_removed_ind = c(1,4,8,10,15,18,19,21,23,27,28,32,33)+1
crop_data[1,-dates_removed_ind]

#crop_data_backup = crop_data
crop_data_sel_dates = crop_data[,-dates_removed_ind]

xx = dates_removed_ind-1
sorted_dates_sel_dates = sorted_dates[-(dates_removed_ind-1)]
plot(sorted_dates_sel_dates, crop_data[1,2:21], "b", ylim = c(0,1), col = "gray")
for (i in 1:dim(crop_data)[1][1:200]){
  lines(sorted_dates_sel_dates, crop_data[i,2:21], "b", ylim = c(0,1), col = "gray")
}
  



crop_data = crop_data_sel_dates
ind_nonNA = c()
for (i in 1:dim(crop_data)[1]){
  if (!any(is.na(crop_data[i,2:34]))){
    ind_nonNA = c(ind_nonNA, i)
  }    
}


  
  
  
#dfafsfsd

#STOP HERE!

#corn_ET_Prosser = scan()
#apple_ET_Prosser = scan()
#alfalfa_ET_Prosser = scan()

mat_vec = rep(NA,(dim(Wshed_mat)[1]*dim(Wshed_mat)[2]))
mat_vec[use_vec] = Total_ET[,7]

ans_mat = matrix(mat_vec, nrow = dim(Wshed_mat)[1], ncol = dim(Wshed_mat)[2])
#seas_ET = raster(ans_mat)
#plot(seas_ET)

seas_ET_ras = Wshed_ras
seas_ET_ras[,] = ans_mat
plot(seas_ET_ras, main = colnames(Total_ET)[7])

#writeRaster(seas_ET_ras, file="I:\\Sulochan\\StatModel\\wts.tif")


## To create a Boxplot to compare METRIC and REference ET

boxplot_mat = matrix(NA, ncol = 13, nrow = 337344)
crops_ = c(176, 36, 1, 24, 61, 68, 57, 43, 23, 12, 47, 69, 56)
crops_name = c()
ctr = 1
acs = c()

for (cropNo in crops_){
	#cropNo = 176
	CropList_ind = which(cropNo == CropList)
	CropName = CropNames[CropList_ind]
	crops_name = c(crops_name, CropName)
	

	
	ind_crop = which(datamat[,dim(datamat)[2]] == cropNo)
	ac = length(ind_crop)*900*0.000247105381467165 # acres
	acs = c(acs, ac)
	ET=(Total_ET[ind_crop,4])
	boxplot_mat[1:length(ind_crop), ctr] = ET
	
	ctr = ctr + 1
}
colnames(boxplot_mat) = crops_name
par(mar = c(8,4,1,1))

boxplot(boxplot_mat, las = 2, border= "darkgray")
means_ = colMeans(boxplot_mat, na.rm=T)
points(1:13, means_, lwd=3, col="darkgray", pch = 8, cex = 2 )

Ref_ETs = c(29.19, 33.52, 24.13, 11.62, NA, 26.96, NA, 22.59, NA, 13.43, NA, 10.75, 23.96)
points(1:13, Ref_ETs, lwd=4, col="red")








plot(1:length(diff_days), diff_days,"b")
all_months = as.numeric(format(sorted_dates , "%m"))

used_mons = 5:9 	## Months to be used for analysis
sel_mon_ids = which(!is.na(match(all_months,used_mons)))

used_dates = sorted_dates[sel_mon_ids]

use_mat1 = datamat[,c(1,sel_mon_ids+1,n_cols)]

n_cols = dim(use_mat1)[2]
use_mat1[use_mat1<0] = NA
use_mat1[, 2:(n_cols-1)][use_mat1[, 2:(n_cols-1)]>1.2] = 1.2

ind_mat = matrix(NA, ncol = n_cols-2, nrow = dim(use_mat1)[1])
for (i in 1:(n_cols-2)){
	non_NA = which(!is.na(use_mat1[,i+1]))
	ind_mat[1:length(non_NA),i] = non_NA
}
ttx1 = apply(ind_mat,2,list)
ttx = lapply(ttx1,unlist)
inds = ttx[[1]]
for (i in 2:length(ttx)){
	common_ind = Reduce(intersect, list(ttx[[i]],inds))
	inds = common_ind
	print (length(inds))
}

use_dates_ = sorted_dates[sel_mon_ids]
diff_days = c(0,use_dates_[2:(length(use_dates_))] - use_dates_[1:(length(use_dates_)-1)])



use_dates_ - use_dates_[1]



inds = c()
ind_to_be_removed = c()
for (i in 2:31){
	inds= c(inds,which(!is.na(use_mat1[,i])))
	inds = unique(inds)
	if (i == 2 | i ==4 | i == 7 | i == 9 | i == 11 | i == 14 | i == 17 | i == 20 | i == 22 | i == 24 | i == 26 | i == 28 | i == 30){
		ind1 = which(is.na(use_mat1[,i]))
		ind2 = which(is.na(use_mat1[,i+1]))
		ind_to_be_removed = c(ind_to_be_removed, intersect(ind1,ind2))
	}
}
inds_for_climate = inds
use_mat2 = use_mat1
use_mat1 = use_mat1[inds,]

ET_mat = matrix(NA, ncol = 19, nrow = dim(use_mat1)[1])
ctr = 2
for (i in 2:31){
	if (i == 2 | i ==4 | i == 7 | i == 9 | i == 11 | i == 14 | i == 17 | i == 20 | i == 22 | i == 24 | i == 26 | i == 28 | i == 30){
		ET_mat[,ctr] = rowMeans(use_mat1[,i:(i+1)],na.rm=T)
		ctr = ctr + 1
	}
	if (i == 3 | i == 13 | i == 16 | i == 19){
		ET_mat[,ctr] = use_mat1[,i]
		ctr = ctr + 1
	}
}
dates_ind2 = c(2,4,6,7,9,11,13,14,16,17,19,20,22,24,26,28,30)
colnames(use_mat1)[dates_ind2]
ET_mat[,1] = use_mat1[,1]
ET_mat[,19] = use_mat1[,32]
colnames(ET_mat) = c("Mat_Id", colnames(use_mat1)[dates_ind2], "Crop_ID")

ET_mat[is.nan(ET_mat)] = NA
ET_dates = use_dates_[dates_ind2-1]
ET_dates[2:17] - ET_dates[1:16]

## THIS IS WHAT WE CAN USE
# See if the months have at least one data value

mon_inds = as.numeric(format(ET_dates, "%m"))
mon_sel_mat = matrix(NA, ncol = 7, nrow = dim(ET_mat)[1])
mon_sel_mat[,1] = ET_mat[,1]
mon_sel_mat[,7] = ET_mat[,dim(ET_mat)[2]]

mon_sel_mat[,2] = rowMeans(ET_mat[,2:4],na.rm=T)
mon_sel_mat[,3] = rowMeans(ET_mat[,5:8],na.rm=T)
mon_sel_mat[,4] = rowMeans(ET_mat[,9:11],na.rm=T)
mon_sel_mat[,5] = rowMeans(ET_mat[,12:15],na.rm=T)
mon_sel_mat[,6] = rowMeans(ET_mat[,16:18],na.rm=T)
mon_sel_mat[is.nan(mon_sel_mat)] = NA

inds = c()
for (i in 2:6){
	inds = c(inds,which(is.na(mon_sel_mat[,i])))
	inds = unique(inds)
}
interp_ind  = setdiff(1:dim(mon_sel_mat)[1], inds)

## FINALLY THESE interp_indices can be used to interpolate since they have at least one monthly value
Non_interp_mat = ET_mat[interp_ind,]
xx = Non_interp_mat[1,2:18]


## SPLINE INTERPOLATION AND CALCULATION OF TOTAL ET FOR THE SEASON

#install.packages("zoo")
#library(zoo)
plot(ET_dates, xx, "b",ylim = c(0,1.2))
ndays = as.numeric(ET_dates[17]-ET_dates[1])+1
Full_seq_Dates = seq.Date(ET_dates[1],ET_dates[17],by="day")

tt = spline(xx,xmin = 1, xmax = 17, n =ndays,method="natural")
y1 = tt$y
y1[y1<0] = 0
y1[y1>1.2] = 1.2
lines(Full_seq_Dates, y1, "l", col =2)

ETRS = c(0.27,0.28,0.24,0.22,0.31,0.24,0.29,0.32,0.28,0.29,0.27,0.2,0.28,0.26,0.24,0.29,0.26,0.3,0.35,0.3,0.29,0.31,0.35,0.36,0.32,0.37,0.24,0.23,0.28,0.23,0.26,0.14,0.35,0.28,0.26,0.34,0.28,0.24,0.27,0.33,0.28,0.3,0.38,0.44,0.39,0.25,0.3,0.29,0.33,0.31,0.35,0.27,0.29,0.33,0.36,0.33,0.35,0.38,0.39,0.35,0.35,0.31,0.39,0.39,0.35,0.28,0.3,0.37,0.31,0.27,0.25,0.32,0.23,0.25,0.39,0.36,0.29,0.3,0.36,0.35,0.36,0.32,0.29,0.32,0.35,0.34,0.33,
		0.27,0.28,0.31,0.28,0.27,0.28,0.24,0.28,0.3,0.26,0.3,0.32,0.36,0.27,0.22,0.17,0.11,0.21,0.21,0.25,0.28,0.31,0.23,0.21,0.24,0.25,0.35,0.33,0.21,0.3,0.28,0.24,0.27,0.23,0.28,0.23,0.25,0.23,0.25,0.26,0.26,0.22,0.18,0.18,0.18,0.14,0.19,0.19,0.17,0.22)
ETOS = c(0.21,0.22,0.19,0.16,0.23,0.19,0.22,0.23,0.22,0.21,0.19,0.16,0.21,0.2,0.19,0.23,0.2,0.23,0.26,0.23,0.23,0.24,0.27,0.28,0.26,0.28,0.18,0.17,0.21,0.19,0.21,0.11,0.25,0.22,0.21,0.24,0.22,0.18,0.21,0.25,0.22,0.24,0.29,0.32,0.29,0.2,0.23,0.23,0.26,0.23,0.25,0.2,0.23,0.25,0.27,0.26,0.28,0.3,0.3,0.27,0.27,0.25,0.3,0.29,0.27,0.21,0.24,0.27,0.25,0.2,0.19,0.25,0.17,0.2,0.28,0.27,0.24,0.24,0.28,0.27,0.28,0.25,0.23,0.25,0.27,0.26,
	0.26,0.21,0.22,0.24,0.21,0.22,0.22,0.2,0.22,0.23,0.21,0.23,0.24,0.26,0.21,0.17,0.14,0.09,0.17,0.17,0.19,0.21,0.23,0.17,0.16,0.18,0.19,0.25,0.23,0.17,0.21,0.2,0.18,0.2,0.17,0.2,0.17,0.18,0.17,0.18,0.19,0.18,0.16,0.14,0.13,0.14,0.11,0.13,0.13,0.13,0.15)

#plot(Full_seq_Dates, ETRS,"l")
#lines(Full_seq_Dates, ETOS,"l",col=2)

# WE will be using ETRS
Total_ET = matrix(NA, nrow = nrow(ET_mat), ncol = 3)
Total_ET[,1] = ET_mat[,1]

Sum_ET = c()
for (i in 1:nrow(ET_mat)){
	print (i)
	xx = ET_mat[i,2:18]
	if (length(which(!is.na(xx)))>0){
		tt = spline(xx,xmin = 1, xmax = 17, n =ndays,method="natural")
		y1 = tt$y
		y1[y1<0] = 0
		y1[y1>1.2] = 1.2
	} else {
		y1 = NA*ETRS
	}
	Sum_ET = c(Sum_ET, sum(y1*ETRS))
}
Total_ET[,2] = Sum_ET
Total_ET[,3] = ET_mat[,dim(ET_mat)[2]]



CropList = c(1,2,3,4,5,6,10,11,12,13,14,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,65,66,67,68,69,70,71,72,74,75,76,77,81,82,83,87,88,92,111,112,121,122,123,124,131,141,142,143,152,176,190,195,204,205,206,207,208,209,210,211,212,213,214,216,217,218,219,220,221,222,223,224,225,226,227,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,254)
CropNames = c("Corn","Cotton","Rice","Sorghum","Soybeans","Sunflower","Peanuts","Tobacco","Sweet Corn","Pop or Orn Corn","Mint","Barley","Durum Wheat","Spring Wheat","Winter Wheat","Other Small Grains","Dbl Crop WinWht/Soybeans","Rye","Oats","Millet","Speltz","Canola","Flaxseed","Safflower","Rape Seed","Mustard","Alfalfa","Other Hay/Non Alfalfa","Camelina","Buckwheat","Sugarbeets","Dry Beans","Potatoes","Other Crops","Sugarcane","Sweet Potatoes","Misc Vegs & Fruits","Watermelons","Onions","Cucumbers",
			"Chick Peas","Lentils","Peas","Tomatoes","Caneberries","Hops","Herbs","Clover/Wildflowers","Sod/Grass Seed","Switchgrass","Fallow/Idle Cropland","Forest","Shrubland","Barren","Cherries","Peaches","Apples","Grapes","Christmas Trees","Other Tree Crops","Citrus","Pecans","Almonds","Walnuts","Pears","Clouds/No Data","Developed","Water","Wetlands","Nonag/Undefined","Aquaculture","Open Water","Perennial Ice/Snow","Developed/Open Space","Developed/Low Intensity","Developed/Med Intensity",
			"Developed/High Intensity","Barren","Deciduous Forest","Evergreen Forest","Mixed Forest","Shrubland","Grass/Pasture","Woody Wetlands","Herbaceous Wetlands","Pistachios","Triticale","Carrots","Asparagus","Garlic","Cantaloupes","Prunes","Olives","Oranges","Honeydew Melons","Broccoli","Peppers","Pomegranates","Nectarines","Greens","Plums","Strawberries","Squash","Apricots","Vetch","Dbl Crop WinWht/Corn","Dbl Crop Oats/Corn","Lettuce","Pumpkins","Dbl Crop Lettuce/Durum Wht",
			"Dbl Crop Lettuce/Cantaloupe","Dbl Crop Lettuce/Cotton","Dbl Crop Lettuce/Barley","Dbl Crop Durum Wht/Sorghum","Dbl Crop Barley/Sorghum","Dbl Crop WinWht/Sorghum","Dbl Crop Barley/Corn","Dbl Crop WinWht/Cotton","Dbl Crop Soybeans/Cotton","Dbl Crop Soybeans/Oats","Dbl Crop Corn/Soybeans","Blueberries","Cabbage","Cauliflower","Celery","Radishes","Turnips","Eggplants","Gourds","Cranberries","Dbl Crop Barley/Soybeans")


crops_unq = unique(use_mat1[,dim(use_mat1)[2]])
crops = use_mat1[,dim(use_mat1)[2]]

cropNo = 1

	cropID = crops_unq[cropNo]
	cropName = CropNames[which(cropID == CropList)]
	crop_ind = which(crops == cropID)

for (i in crop_ind){
	plot(use_dates_,use_mat1[i, 2:31],"l",main=cropName)
	#readline()
}

plot_mat = datamat[crop_ind,]
head(plot_mat)










### GETTING CLIMATE DATA IN #####

clim_folder = "I:\\Sulochan\\StatModel\\PRISM\\Resampled"
all_files = list.files(clim_folder)

ind_file = c()

for (i in 1:length(all_files)){
	print (i)
	print (length(all_files))
	if (grepl("_2007_",all_files[i]) && 
		grepl(".tif",all_files[i]) &&
		!grepl(".aux.xml",all_files[i]) &&
		!grepl(".xml",all_files[i]) && 
		!grepl(".ovr",all_files[i])
	    ){
			ind_file= c(ind_file, i)
	}
}
climate_files = all_files[ind_file]
climate_file_paths = paste(clim_folder,"\\",climate_files,sep="")

clim_matrix_2007 = matrix(NA, nrow=length(use_vec), ncol = (length(climate_file_paths)+1))
clim_matrix_2007[,1] = use_vec
## LETS get climate data to vectors and finally into a data frame
for (i in 1:length(climate_file_paths)){
	print (i)
	ETras1 = raster(climate_file_paths[i])
	ETras3 = projectRaster(ETras1, Wshed_ras)
	ETras2 = crop(ETras3, Wshed_ex)
	ETRas = mask(ETras3, Wshed_ras)

	ETmat = matrix(ETRas)
	ET_vec = as.vector(ETmat)[use_vec]
	clim_matrix_2007[,i+1] = ET_vec
}


## COLUMN NAMES ###
# lets put in column names
###
col_names = c()
my_dates1 = c()
for (i in 1:length(climate_files)){
	var = strsplit(climate_files[i],"_")[[1]][1]
	yr = strsplit(climate_files[i],"_")[[1]][2]
	mo = strsplit(climate_files[i],"_")[[1]][3]
	
	my_dates1 = c(my_dates1, paste(yr,"_",mo,"_01",sep=""))
	col_names = c(col_names, paste(var,"_",yr,"_",mo,sep=""))
}
my_dates2 = as.Date(my_dates1, format="%Y_%m_%d")
ndates = length(my_dates2)
colnames(clim_matrix_2007) = c("Mat_Id", col_names)



## GETTING DEM and other topographic data
topo_files = c("DEEM_Resamp.tif", "Slp_Resamp.tif","Aspect_Resamp.tif")
topo_path_files = paste("I:\\Sulochan\\StatModel\\",topo_files,sep="")

Topo_mat = matrix(NA, nrow=length(use_vec), ncol = (length(topo_files)+1))
Topo_mat[,1] = use_vec
## LETS get climate data to vectors and finally into a data frame
for (i in 1:length(topo_path_files)){
	ETras1 = raster(topo_path_files[i])
	ETras3 = projectRaster(ETras1, Wshed_ras)
	ETras2 = crop(ETras3, Wshed_ex)
	ETRas = mask(ETras3, Wshed_ras)

	ETmat = matrix(ETRas)
	ET_vec = as.vector(ETmat)[use_vec]
	Topo_mat[,i+1] = ET_vec
}
colnames(Topo_mat) = c("MatID", "Elev", "Slp", "Asp")



Fin_matrix = matrix(NA, nrow = nrow(Total_ET), ncol = 42)
dim(Total_ET) # Ncol = 2
dim(clim_matrix_2007[inds_for_climate,]) #Ncol = 36
dim(Topo_mat[inds_for_climate,]) #Ncol = 3

Fin_matrix[,1] = Total_ET[,1]
Fin_matrix[,2] = Total_ET[,2]
Fin_matrix[,3:38] = clim_matrix_2007[inds_for_climate, 2:37]
Fin_matrix[,39:41] = Topo_mat[inds_for_climate,2:4]
Fin_matrix[,42] = Total_ET[,3]

colnames(Fin_matrix) = c("StnID", "TotalET", colnames(clim_matrix_2007)[2:37], colnames(Topo_mat)[2:4], "Crop")
Fin_matrix = as.matrix(read.csv("I:\\Sulochan\\StatModel\\Fin_matrix.csv",header = T))

Fin_matrix[,3] = Total_ET[,2]
Anal_df = as.data.frame(Fin_matrix[,3:43])
Anal_df$Crop = as.factor(Fin_matrix[,43])
#Anal_df = Anal_df[:42] 
str(Anal_df)



## CHECK
layout(matrix(c(1,2), 2, 1, byrow = TRUE))
ETras1 = raster(fileWpath_list[1])
plot(ETras1)
x1 = as.vector(Fin_matrix[,3])
mat_vec = rep(NA,(dim(Wshed_mat)[1]*dim(Wshed_mat)[2]))
mat_vec[Fin_matrix[,2]] = x1
ans_mat = matrix(mat_vec, nrow = dim(Wshed_mat)[1], ncol = dim(Wshed_mat)[2])
#plot(raster(ans_mat))
cropRas2 = cropRas
cropRas2[,] = ans_mat
plot(cropRas2)
writeRaster(cropRas2, file="I:\\Sulochan\\StatModel\\seas_ET_ras2.tif")





Fin_matrix = as.matrix(read.csv("I:\\Sulochan\\StatModel\\Fin_matrix.csv",header = T))



#install.packages("randomForest")
library(randomForest)
sel_ind = sample(1:nrow(Fin_matrix), 2000, replace = FALSE)
Anal_df2 = Anal_df[sel_ind,]

ET2007.rf = randomForest(TotalET ~ ., data=Anal_df2, mtry = 3, importance = TRUE, na.action = na.omit)
ET2007.rf = randomForest(TotalET ~ Slp + Elev + tmin_2007_12 + ppt_2007_08 + tmax_2007_01 + Crop, data=Anal_df2, mtry = 3, importance = TRUE, na.action = na.omit)
print(ET2007.rf)
varIplot = varImpPlot(ET2007.rf)
pairs(TotalET ~ .,  data=Anal_df2)
pairs(~ Slp + Elev + tmin_2007_12 + ppt_2007_08 + tmax_2007_01,  data=Anal_df2)

#cor(Anal_df2[,2:40],use = "na.or.complete")
#corrplot(cor(Anal_df2[,2:40],use = "na.or.complete"))


pred = ET2007.rf$predicted[1:1960]

non_NA_ind = which(!is.na(Anal_df2$ppt_2007_02))
obs = Anal_df2$TotalET[non_NA_ind]
non_NA_ind = which(!is.na(obs))
obs = obs[non_NA_ind]

plot(obs, pred, xlab = "METRIC (inches)", ylab = "RF Model (inches)", pch = 16, cex =0.5, main = "Model performance")
cor(obs,pred)

grid()
chk_mat = matrix(NA, nrow = length(obs), ncol = 3)
chk_mat[,1] = pred
chk_mat[,2] = obs
chk_mat[,3] = Anal_df2$Crop[non_NA_ind]

df = data.frame(chk_mat[,1:2],as.factor(chk_mat[,3]))
boxplot(chk_mat)
crop_error = matrix(NA, nrow = length(non_NA_ind), ncol =20)
unq_crops = unique(chk_mat[,3])


CropList = c(1,2,3,4,5,6,10,11,12,13,14,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,65,66,67,68,69,70,71,72,74,75,76,77,81,82,83,87,88,92,111,112,121,122,123,124,131,141,142,143,152,176,190,195,204,205,206,207,208,209,210,211,212,213,214,216,217,218,219,220,221,222,223,224,225,226,227,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,254)
CropNames = c("Corn","Cotton","Rice","Sorghum","Soybeans","Sunflower","Peanuts","Tobacco","Sweet Corn","Pop or Orn Corn","Mint","Barley","Durum Wheat","Spring Wheat","Winter Wheat","Other Small Grains","Dbl Crop WinWht/Soybeans","Rye","Oats","Millet","Speltz","Canola","Flaxseed","Safflower","Rape Seed","Mustard","Alfalfa","Other Hay/Non Alfalfa","Camelina","Buckwheat","Sugarbeets","Dry Beans","Potatoes","Other Crops","Sugarcane","Sweet Potatoes","Misc Vegs & Fruits","Watermelons","Onions","Cucumbers",
			"Chick Peas","Lentils","Peas","Tomatoes","Caneberries","Hops","Herbs","Clover/Wildflowers","Sod/Grass Seed","Switchgrass","Fallow/Idle Cropland","Forest","Shrubland","Barren","Cherries","Peaches","Apples","Grapes","Christmas Trees","Other Tree Crops","Citrus","Pecans","Almonds","Walnuts","Pears","Clouds/No Data","Developed","Water","Wetlands","Nonag/Undefined","Aquaculture","Open Water","Perennial Ice/Snow","Developed/Open Space","Developed/Low Intensity","Developed/Med Intensity",
			"Developed/High Intensity","Barren","Deciduous Forest","Evergreen Forest","Mixed Forest","Shrubland","Grass/Pasture","Woody Wetlands","Herbaceous Wetlands","Pistachios","Triticale","Carrots","Asparagus","Garlic","Cantaloupes","Prunes","Olives","Oranges","Honeydew Melons","Broccoli","Peppers","Pomegranates","Nectarines","Greens","Plums","Strawberries","Squash","Apricots","Vetch","Dbl Crop WinWht/Corn","Dbl Crop Oats/Corn","Lettuce","Pumpkins","Dbl Crop Lettuce/Durum Wht",
			"Dbl Crop Lettuce/Cantaloupe","Dbl Crop Lettuce/Cotton","Dbl Crop Lettuce/Barley","Dbl Crop Durum Wht/Sorghum","Dbl Crop Barley/Sorghum","Dbl Crop WinWht/Sorghum","Dbl Crop Barley/Corn","Dbl Crop WinWht/Cotton","Dbl Crop Soybeans/Cotton","Dbl Crop Soybeans/Oats","Dbl Crop Corn/Soybeans","Blueberries","Cabbage","Cauliflower","Celery","Radishes","Turnips","Eggplants","Gourds","Cranberries","Dbl Crop Barley/Soybeans")


crop_error = matrix(NA, nrow = length(non_NA_ind), ncol =21)
pred1 = matrix(NA, nrow = length(non_NA_ind), ncol =21)
obs1 = matrix(NA, nrow = length(non_NA_ind), ncol =21)

crop_names = c()
crop_lens = c()
for (i in 1:ncol(crop_error)){
	crop_ind = which(df[,3]==unq_crops[i])
	crop_lens = c(crop_lens, length(crop_ind))
	crop_names = c(crop_names,CropNames[unq_crops[i]])
	pred1[1:length(crop_ind),i] = pred[crop_ind]
	obs1[1:length(crop_ind),i] = obs[crop_ind]
	crop_error[1:length(crop_ind),i] = (pred[crop_ind] - obs[crop_ind])
}

colnames(crop_error) = crop_names
par(omi=c(2,0.1,0.1,0.1))
boxplot(crop_error,axes=T,las = 2, ylab = "Model Error (inches)",lwd=2, cex.axis = 1.5, cex.lab=1.5)
#axis(2)
abline(h = 0,lty =2, col = "gray", lwd = 2)



#install.packages("beeswarm")
#library("beeswarm")
#beeswarm(crop_error)




grid()
colnames(pred1) = crop_names
colnames(obs1) = crop_names
boxplot(pred1)
boxplot(obs1,border=c("blue"),las=2)


pred_obs4box = matrix(NA, ncol = 3*dim(pred1)[2], nrow = dim(pred1)[1]) 
ctr = 1
crop_names_ = rep(crop_names,each=3)
for (i in 1:dim(pred1)[2]){
	pred_obs4box[,ctr] = obs1[,i]
	pred_obs4box[,ctr+1] = pred1[,i]
	pred_obs4box[,ctr+2] = rep(NA,nrow(pred1))
	ctr = ctr + 3
}
crop_names_[seq(1,63,by=3)] = ""
crop_names_[seq(3,63,by=3)] = ""

colnames(pred_obs4box) = crop_names_


boxplot(pred_obs4box,las=2,border=c("red"),ylab ="ET (Inches)")
pred_obs4box[,seq(2,63,3)]=NA
boxplot(pred_obs4box,las=2,border=c("blue"),add=T)


ctr =1
for (i in seq(3,63,3)){
	abline(v=i-3,lty=2,col="lightgray")
	text(i-1,0,paste(round(crop_lens[ctr]*100/1960,digits=1),"%"),cex=0.5)
	ctr = ctr +1
}
abline(v=i,lty=2,col="lightgray")





########## TRYING TO BUILD MODEL WITH DATA --- INDEPENDENT ####

library(data.table)
Fin_matrix = data.matrix(fread("I:\\Sulochan\\StatModel\\Fin_matrix.csv",header = T))
#install.packages("randomForest.ddR")
Anal_df = Fin_matrix[,3:43]
Anal_df[,ncol(Anal_df)] = as.factor(Anal_df[,ncol(Anal_df)])

#crop_ind = which(df[,3]==unq_crops[i])
#crop_error1 = (pred[crop_ind][1] - obs[crop_ind][1])*100/obs[crop_ind] 

sel_ind = sample(1:nrow(Fin_matrix), 5000, replace = FALSE) # Took 16 minutes with 7 cores
Anal_df2 = Anal_df[sel_ind,]

Small_mat = matrix(NA, ncol= 17, nrow = 5000)

Small_mat[,1] =Anal_df2[,1]

Small_mat[,2] = rowMeans(Anal_df2[,c(2,3,13)])
Small_mat[,3] = rowMeans(Anal_df2[,c(4,5,6)])
Small_mat[,4] = rowMeans(Anal_df2[,c(7,8,9)])
Small_mat[,5] = rowMeans(Anal_df2[,c(10,11,12)])

Small_mat[,6] = rowMeans(Anal_df2[,c(14,15,25)])
Small_mat[,7] = rowMeans(Anal_df2[,c(16,17,18)])
Small_mat[,8] = rowMeans(Anal_df2[,c(19,20,21)])
Small_mat[,9] = rowMeans(Anal_df2[,c(22,23,24)])

Small_mat[,10] = rowMeans(Anal_df2[,c(26,27,37)])
Small_mat[,11] = rowMeans(Anal_df2[,c(28,29,30)])
Small_mat[,12] = rowMeans(Anal_df2[,c(31,32,33)])
Small_mat[,13] = rowMeans(Anal_df2[,c(34,35,36)])

Small_mat[,14] = Anal_df2[,38]
Small_mat[,15] = Anal_df2[,39]
Small_mat[,16] = Anal_df2[,40]
Small_mat[,17] = Anal_df2[,41]

colnames(Small_mat) = c("TotalET", "ppt_DJF", "ppt_MAM", "ppt_JJA", "ppt_SON",
				 "tmax_DJF", "tmax_MAM", "tmax_JJA", "tmax_SON",
				 "tmin_DJF", "tmin_MAM", "tmin_JJA", "tmin_SON",
				 "Elev", "Slp", "Asp", "Crop")

#pairs(Small_mat)
#corrplot(cor(Small_mat,use = "na.or.complete"))


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y,use = "na.or.complete"))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
#pairs(Small_mat, lower.panel = panel.smooth, upper.panel = panel.cor, pch = 24,font.labels=1.5)

Small_mat_df = data.frame(Small_mat)
Small_mat_df$Crop = as.factor(Small_mat_df$Crop)
train = Small_mat_df

library(randomForest.ddR)
library(randomForest)


st_time = Sys.time()
print (st_time)

#ET2007_small.rf = randomForest(TotalET ~ ., data=Small_mat_df, mtry = 3, importance = TRUE, na.action = na.omit)
en_time1 = Sys.time()
print (en_time1 - st_time)

ET2007_small.rf2_FULL = drandomForest(TotalET ~ . , data=Small_mat_df, mtry = 3, importance = TRUE, na.action = na.omit, nExecutor = 7)
#ET2007_small.rf2 = drandomForest(TotalET ~ Elev + ppt_MAM + tmin_SON + ppt_DJF + tmax_DJF + Asp , data=Small_mat_df, mtry = 3, importance = TRUE, na.action = na.omit, nExecutor = 7)
print (ET2007_small.rf2)
en_time2 = Sys.time()
print (en_time2 - en_time1)


#predict for 2008
##################################################
## 2 0 0 8 ##
#############

Fin_matrix = data.matrix(fread("I:\\Sulochan\\StatModel\\Fin_matrix2008.csv",header = T))

## Using different ETs (Linear, Spline) x (Daily, Monthly) = 4
ET_2008_all = data.matrix(fread("I:/Sulochan/StatModel/Total_ET_matrix2008.csv"))
ET_2008 = ET_2008_all[,6]
#plot(Fin_matrix[,3], ET_2008)

Fin_matrix[,3] = ET_2008 
#install.packages("randomForest.ddR")

Anal_df = Fin_matrix[,3:43]
Anal_df[,ncol(Anal_df)] = as.factor(Anal_df[,ncol(Anal_df)])

sel_ind = sample(1:nrow(Fin_matrix), 10000, replace = FALSE) # Took 16 minutes with 7 cores
Anal_df2 = Anal_df[sel_ind,]

Small_mat = matrix(NA, ncol= 17, nrow = 10000)

Small_mat[,1] =Anal_df2[,1]

Small_mat[,2] = rowMeans(Anal_df2[,c(2,3,13)])
Small_mat[,3] = rowMeans(Anal_df2[,c(4,5,6)])
Small_mat[,4] = rowMeans(Anal_df2[,c(7,8,9)])
Small_mat[,5] = rowMeans(Anal_df2[,c(10,11,12)])

Small_mat[,6] = rowMeans(Anal_df2[,c(14,15,25)])
Small_mat[,7] = rowMeans(Anal_df2[,c(16,17,18)])
Small_mat[,8] = rowMeans(Anal_df2[,c(19,20,21)])
Small_mat[,9] = rowMeans(Anal_df2[,c(22,23,24)])

Small_mat[,10] = rowMeans(Anal_df2[,c(26,27,37)])
Small_mat[,11] = rowMeans(Anal_df2[,c(28,29,30)])
Small_mat[,12] = rowMeans(Anal_df2[,c(31,32,33)])
Small_mat[,13] = rowMeans(Anal_df2[,c(34,35,36)])

Small_mat[,14] = Anal_df2[,38]
Small_mat[,15] = Anal_df2[,39]
Small_mat[,16] = Anal_df2[,40]
Small_mat[,17] = Anal_df2[,41]

colnames(Small_mat) = c("TotalET", "ppt_DJF", "ppt_MAM", "ppt_JJA", "ppt_SON",
                        "tmax_DJF", "tmax_MAM", "tmax_JJA", "tmax_SON",
                        "tmin_DJF", "tmin_MAM", "tmin_JJA", "tmin_SON",
                        "Elev", "Slp", "Asp", "Crop")

Small_mat_df = data.frame(Small_mat)
Small_mat_df$Crop = as.factor(Small_mat_df$Crop)

st_time = Sys.time()
print (st_time)

#ET2007_small.rf = randomForest(TotalET ~ ., data=Small_mat_df, mtry = 3, importance = TRUE, na.action = na.omit)
en_time1 = Sys.time()
print (en_time1 - st_time)

# Prediction using drandomForest model
Small_mat_df$Crop <- factor(Small_mat_df$Crop, levels = levels(train$Crop))
predict_2008 = predict.drandomForest(ET2007_small.rf2_FULL, Small_mat_df)
#ET2007_small.rf2 = drandomForest(TotalET ~ ., data=Small_mat_df, mtry = 3, importance = TRUE, na.action = na.omit, nExecutor = 8)

plot(predict_2008, Small_mat_df$TotalET)
################################










ET2007Small.rf = randomForest(TotalET ~ ., data=Small_mat_df, mtry = 3, importance = TRUE, na.action = na.omit)

ET2007Small.rf
varImpPlot(ET2007Small.rf)



save(ET2007Small.rf, file="ET2007small_rf.RData")

ET2007.rf = randomForest(TotalET ~ Elev + ppt_MAM + tmin_SON + ppt_DJF + tmax_DJF + Asp , data = Small_mat_df, importance = TRUE, na.action = na.omit)

save(ET2007.rf, file="ET2007small_rf.RData")


pairs(TotalET ~ ., data=Anal_df2, lower.panel = panel.smooth, upper.panel = panel.cor)




fit <- lm(TotalET ~ ., data=Small_mat_df)
summary(fit) # show results

plot(fit)


















### Linear Model Trial ##

Fin_matrix = as.matrix(read.csv("I:\\Sulochan\\StatModel\\Fin_matrix.csv",header = T))
Anal_df = as.data.frame(Fin_matrix[,3:42])
#Anal_df$Crop = as.factor(Fin_matrix[,43])
str(Anal_df)

Anal_df1 = Anal_df
dim(Anal_df1)

sel_ind = sample(1:nrow(Fin_matrix), 2000, replace = FALSE)
Anal_df2 = Anal_df[sel_ind,]
sel_ind =  which(!is.na(Anal_df2[,4]))
Anal_df = Anal_df2[sel_ind,]
dim(Anal_df)


fit <- lm(TotalET ~ ., data=Anal_df)
summary(fit) # show results


plot(fit)


# Stepwise Regression
library(MASS)
fit <- lm(TotalET ~ ., data=Anal_df)
step <- stepAIC(fit, direction="both")
step$anova # display results


df.pca = prcomp(Anal_df[,2:37],center=TRUE, scale. = TRUE)
plot(df.pca, type = "l")










































inds = names(pred)
for (i in 1:length(inds)){
	in_ = which(inds[i] == Anal_df2

#install.packages("bigmemory")
library(bigmemory)





plot(2:ndates,datamat[1,2:ndates],"l")

crops = datamat[,dim(datamat)[2]]
crop_ind = which(crops == crops[1])

plot_mat = datamat[crop_ind,]
head(plot_mat)




























































plot(raster(Wshed_mat))

## -------------
##Step 2: Lets put unique sequence for non-NA values

n = length(which(!is.na(Wshed_mat)))
n

mat_id = matrix(NA, ncol = 3, nrow = n)
colnames(mat_id) = c("Row_no","Col_no", "Mat_ID")
head(mat_id)

# This is the new matrix with unique values
Wshed_mat2 = Wshed_mat
chk_mat = matrix(0, nrow = dim(Wshed_mat)[1],ncol= dim(Wshed_mat)[2])
ctr = 1
Mat_place_ctr = 1
for (j in 1:dim(Wshed_mat)[2]){
	for (i in 1:dim(Wshed_mat)[1]){
		if (!is.na(Wshed_mat[i,j])){
			mat_id[ctr,1] = i
			mat_id[ctr,2] = j
			mat_id[ctr,3] = Mat_place_ctr
			Wshed_mat2[i,j] = Mat_place_ctr
			ctr = ctr + 1
		}
		chk_mat[i,j] = Mat_place_ctr
		Mat_place_ctr = Mat_place_ctr + 1
	}
}

head(mat_id)
Wshed_mat2

chk_mat
as.vector(chk_mat)

plot(raster(Wshed_mat2))


## ---------------
## Step 3: Make a vector of matrix
Wshed_vec = as.vector(Wshed_mat2[!is.na(Wshed_mat2)])

# We can only care about data from these 6 positions for our work

## Step 3: Make a data matrix
data_mat1 = matrix(c(NA,NA,2,3,1,7,5,4,2,3,1,0,4,4,0,0),nrow =4, ncol = 4, byrow=T)
data_mat1
plot(raster(data_mat1))

data_vec1 = as.vector(data_mat1)[Wshed_vec]
data_vec1






Wshed_mat = as.matrix(Wshed_ras)
Wshed_mat2 = Wshed_mat  		
dim(Wshed_mat)[1] * dim(Wshed_mat)[2]
n = length(which(!is.na(Wshed_mat)))
mat_id = matrix(NA, ncol = 3, nrow = n)
colnames(mat_id) = c("Row_no","Col_no", "Mat_ID")

####
#### THE FOLLOWING TAKES A FEW MINUTES 
####

ctr = 1
for (i in 1:dim(Wshed_mat)[1]){
	for (j in 1:dim(Wshed_mat)[2]){
		if (!is.na(Wshed_mat[i,j])){
			mat_id[ctr,1] = i
			mat_id[ctr,2] = j
			mat_id[ctr,3] = ctr
			Wshed_mat2[i,j] = ctr
			ctr = ctr + 1
		}
	}
}


##  Some checks
##

rr = raster(Wshed_mat2)
plot(rr)
length(which(!is.na(Wshed_mat2[100,])))
length(which(mat_id[,1]==100))

#Wshed_mat2[100,which(!is.na(Wshed_mat2[100,]))] == mat_id[which(mat_id[,1]==100),3]
length(which(Wshed_mat2[100,which(!is.na(Wshed_mat2[100,]))] == mat_id[which(mat_id[,1]==100),3]))

###
### CONVERT MATRIX TO VECTOR AND VICE VERSA
###

Wshed_vec = as.vector(Wshed_mat2[!is.na(Wshed_mat2)])
Data_mat = Wshed_mat + 10
plot(raster(Data_mat))

rows_ = mat_id[,1]
cols_ = mat_id[,2]

x = sample(1:10)
x
match(c(4,6),x)

row_col_ind = match(Wshed_vec, mat_id[,3])
new_mat = matrix(NA, nrow = dim(Wshed_mat)[1], ncol = dim(Wshed_mat)[2])


for (i in 1:length(row_col_ind)){
	print (i)
	new_mat[mat_id[row_col_ind[i],1], mat_id[row_col_ind[i],2]] = Wshed_vec[i]
}
plot(raster(new_mat))





row_col_ind1 = row_col_ind[1:10]
Wshed_mat2[mat_id[row_col_ind1,1], mat_id[row_col_ind1,2]]
new_mat = matrix(Wshed_vec[1:100], nrow = 10, ncol = 10)
new_mat

new_mat[mat_id[row_col_ind1,1], mat_id[row_col_ind1,2]] = Wshed_vec[1:100]
#windows()
plot(raster(new_mat))

Wshed_mat2[head(mat_id[row_col_ind1,1]),head(mat_id[row_col_ind1,2])]
new_mat[head(mat_id[row_col_ind1,1]),head(mat_id[row_col_ind1,2])] = Wshed_vec[row_col_ind1]






## The vector "Wshed_vec" and matrix "mat_id" now can be used to convert vector to matrix to raster and vice-versa.

#new_mat = matrix(NA, nrow = dim(Wshed_mat)[1], ncol = dim(Wshed_mat)[2])
#ind_vec = c()
time0 = Sys.time()
for (i in 474139:length(rows_)){
#for (i in 1:length(rows_)){
	print (i)
	ind = which(Wshed_vec[i]==mat_id[,3])
	ind_vec = c(ind_vec, ind)
	new_mat[mat_id[ind,1], mat_id[ind,2]] = Wshed_vec[i]
}
time1 = Sys.time()
time1-time0

########
######## THIS PROCESS TAKES WAYYY TOO LONG #####
########

## LET'S TRY TO USE MYSQL TO DO THIS ##########
write.csv(mat_id, file="I:/Sulochan/StatModel/mat_id.csv")
write.csv(Wshed_vec, file="I:/Sulochan/StatModel/Wshed_vec.csv")


plot(raster(new_mat))
plot(raster(Wshed_mat2))




















ras_crop1 = projectRaster(ras_crop, Wshed_ras)
ras_crop2 = mask(ras_crop1, Wshed_ras)
plot(ras_crop2, add= T)



# Find maximum extent of these files
Xmin = c()
Xmax = c()
Ymin = c()
Ymax = c()

for (i in 1:length(fileWpath_list)){
	ras_crop = raster(fileWpath_list[i])
	Xmin = c(Xmin, extent(ras_crop)@xmin)
	Xmax = c(Xmax, extent(ras_crop)@xmax)
	Ymin = c(Ymin, extent(ras_crop)@ymin)
	Ymax = c(Ymax, extent(ras_crop)@ymax)
}
max_extent = extent(c(max(Xmin),max(Xmax),max(Ymin),max(Ymax)))

#wshed = readShapeSpatial("I:\\Sulochan\\StatModel\\Wshed_proj.shp")
#plot(wshed)


cropRas_ = raster(paste("I:/Sulochan/StatModel/CropScape/Crops_only/CDL_",crop_year,"_Proj.tif",sep=""))
cropRas = crop(cropRas_, max_extent)
plot(cropRas)

Wshed_ras = cropRas - cropRas + 0
plot(Wshed_ras, add =T, col ="black")

plot(ras_crop,add=T)

names = c()
mydates = c()
for(i in 1:length(ETrF_files)){
	xx = strsplit(ETrF_files[i],"_")[[1]]
	names = c(names, paste(xx[1],"_",xx[2],"_",xx[3],"_",xx[4],sep=""))
	mydates = c(mydates, paste(xx[2],"-",xx[3],"-",xx[4],sep=""))
}


Xmin = c()
Xmax = c()
Ymin = c()
Ymax = c()

for (i in 1:length(ETrF_files)){
	ras_crop = raster(ETrF_files[i])
	Xmin = c(Xmin, extent(ras_crop)@xmin)
	Xmax = c(Xmax, extent(ras_crop)@xmax)
	Ymin = c(Ymin, extent(ras_crop)@ymin)
	Ymax = c(Ymax, extent(ras_crop)@ymax)
}
min_extent = extent(c(min(Xmin),min(Xmax),min(Ymin),min(Ymax)))


i = 3
ras1 = raster(ETrF_files[i])
ras = crop(ras1, min_extent)
print (i)
ETrF_files[i]
mat = as.matrix(ras)
dim(mat)
vec = as.vector(mat)
length(vec)

crp2007 = raster("I:/Sulochan/StatModel/ETrF_2007/Crop2007.tif")
crp = crop(crp2007, min_extent)
crp_mat = as.matrix(crp)
crp_vec = as.vector(crp_mat)

ETrF_array = array(NA, dim =c(1987, 2412, 22))
for (i in 1:22){
	ras1 = raster(ETrF_files[i])
	ras = crop(ras1, min_extent)
	mat = as.matrix(ras)
	ETrF_array[,,i] = mat
}

ETrF_vec = matrix(NA, nrow=4792644, ncol = 22)
for (i in 1:22){
	ras1 = raster(ETrF_files[i])
	ras = crop(ras1, min_extent)
	mat = as.matrix(ras)
	vec = as.vector(mat)
	ETrF_vec[,i] = vec
}
ETrF_crp_vec = (cbind(ETrF_vec, crp_vec))

ind_nonNA_crp = c()
ind_nonNA = c()
chk = 2595765
for (i in 1:4792644){
	if (!is.na(ETrF_crp_vec[i,23])){
		ind_nonNA_crp = c(ind_nonNA_crp, i)
		n1 = length(which(is.na(ETrF_crp_vec[i,1:22])))
		if (n1<22){
			ind_nonNA = c(ind_nonNA, i)
		}
	}
}

## Get all the non-NA values
ETrF_vec = ETrF_crp_vec[ind_nonNA,]
colnames(ETrF_vec) = c(names,"Crp")
mydates_ = as.Date(mydates, format = "%Y-%m-%d")


all_nonNA = c()
for (i in 1:4792644){
	n = length(which(!is.na(ETrF_crp_vec[i,])))
	if (n==23){
		all_nonNA = c(all_nonNA, i)
	}
}
ETrF_vec = ETrF_crp_vec[all_nonNA,]
colnames(ETrF_vec) = c(names,"Crp")

mydates_ = as.Date(mydates, format = "%Y-%m-%d")
sorted_dates_ind = c()
for (i in 1:length(mydates_)){
	sorted_dates_ind = c(sorted_dates_ind, which(mydates_ == sort(mydates_)[i]))
}
ETrF_data = cbind(ETrF_vec[,sorted_dates_ind],ETrF_vec[,23])

CropList = c(1,2,3,4,5,6,10,11,12,13,14,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,63,64,65,66,67,68,69,70,71,72,74,75,76,77,81,82,83,87,88,92,111,112,121,122,123,124,131,141,142,143,152,176,190,195,204,205,206,207,208,209,210,211,212,213,214,216,217,218,219,220,221,222,223,224,225,226,227,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,254)
CropNames = c("Corn","Cotton","Rice","Sorghum","Soybeans","Sunflower","Peanuts","Tobacco","Sweet Corn","Pop or Orn Corn","Mint","Barley","Durum Wheat","Spring Wheat","Winter Wheat","Other Small Grains","Dbl Crop WinWht/Soybeans","Rye","Oats","Millet","Speltz","Canola","Flaxseed","Safflower","Rape Seed","Mustard","Alfalfa","Other Hay/Non Alfalfa","Camelina","Buckwheat","Sugarbeets","Dry Beans","Potatoes","Other Crops","Sugarcane","Sweet Potatoes","Misc Vegs & Fruits","Watermelons","Onions","Cucumbers",
			"Chick Peas","Lentils","Peas","Tomatoes","Caneberries","Hops","Herbs","Clover/Wildflowers","Sod/Grass Seed","Switchgrass","Fallow/Idle Cropland","Forest","Shrubland","Barren","Cherries","Peaches","Apples","Grapes","Christmas Trees","Other Tree Crops","Citrus","Pecans","Almonds","Walnuts","Pears","Clouds/No Data","Developed","Water","Wetlands","Nonag/Undefined","Aquaculture","Open Water","Perennial Ice/Snow","Developed/Open Space","Developed/Low Intensity","Developed/Med Intensity",
			"Developed/High Intensity","Barren","Deciduous Forest","Evergreen Forest","Mixed Forest","Shrubland","Grass/Pasture","Woody Wetlands","Herbaceous Wetlands","Pistachios","Triticale","Carrots","Asparagus","Garlic","Cantaloupes","Prunes","Olives","Oranges","Honeydew Melons","Broccoli","Peppers","Pomegranates","Nectarines","Greens","Plums","Strawberries","Squash","Apricots","Vetch","Dbl Crop WinWht/Corn","Dbl Crop Oats/Corn","Lettuce","Pumpkins","Dbl Crop Lettuce/Durum Wht",
			"Dbl Crop Lettuce/Cantaloupe","Dbl Crop Lettuce/Cotton","Dbl Crop Lettuce/Barley","Dbl Crop Durum Wht/Sorghum","Dbl Crop Barley/Sorghum","Dbl Crop WinWht/Sorghum","Dbl Crop Barley/Corn","Dbl Crop WinWht/Cotton","Dbl Crop Soybeans/Cotton","Dbl Crop Soybeans/Oats","Dbl Crop Corn/Soybeans","Blueberries","Cabbage","Cauliflower","Celery","Radishes","Turnips","Eggplants","Gourds","Cranberries","Dbl Crop Barley/Soybeans")

crops = unique(ETrF_data[,23])
cropNo = 4
print (crops[cropNo])
Cname = CropNames[which(CropList == crops[cropNo])]
print (Cname)
ind = which(crops[cropNo] == ETrF_data[,23])
length(ind)
mydata = ETrF_data[ind,1:22]


dates_2007 = seq(as.Date("2007/1/1"), as.Date("2008/1/1"), by = "days")
HRHW_ETRS =c(0.01,0.06,0.04,0.07,0.08,0.1,0.13,0.05,0.11,0.06,0.04,0.02,0.01,0.01,0.01,0.01,
0.01,0.01,0.01,0.07,0.03,0.02,0.03,0.03,0.02,0.01,0.02,0.01,0.02,0.02,0.01,0.03,0.02,0.01,
0.02,0.02,0.03,0.02,0.02,0.03,0.02,0.03,0.05,0.02,0.02,0.15,0.07,0.08,0.17,0.12,0.14,0.07,
0.03,0.08,0.02,0.03,0.1,0.05,0.12,0.07,0.02,0.05,0.03,0.05,0.08,0.13,0.15,0.1,0.09,0.19,
0.22,0.11,0.16,0.07,0.1,0.21,0.19,0.24,0.2,0.12,0.1,0.11,0.21,0.19,0.11,0.19,0.14,0.14,
0.21,0.17,0.17,0.13,0.11,0.13,0.14,0.21,0.17,0.23,0.24,0.24,0.13,0.19,0.14,0.12,0.17,
0.16,0.22,0.18,0.17,0.18,0.09,0.12,0.21,0.3,0.21,0.2,0.23,0.21,0.22,0.19,0.15,0.22,0.16,
0.18,0.2,0.18,0.29,0.39,0.27,0.28,0.24,0.22,0.31,0.24,0.29,0.32,0.28,0.29,0.27,0.2,0.28,
0.26,0.24,0.29,0.26,0.3,0.35,0.3,0.29,0.31,0.35,0.36,0.32,0.37,0.24,0.23,0.28,0.23,0.26,
0.14,0.35,0.28,0.26,0.34,0.28,0.24,0.27,0.33,0.28,0.3,0.38,0.44,0.39,0.25,0.3,0.29,0.33,
0.31,0.35,0.27,0.29,0.33,0.36,0.33,0.35,0.38,0.39,0.35,0.35,0.31,0.39,0.39,0.35,0.28,0.3,
0.37,0.31,0.27,0.25,0.32,0.23,0.25,0.39,0.36,0.29,0.3,0.36,0.35,0.36,0.32,0.29,0.32,0.35,
0.34,0.33,0.27,0.28,0.31,0.28,0.27,0.28,0.24,0.28,0.3,0.26,0.3,0.32,0.36,0.27,0.22,0.17,
0.11,0.21,0.21,0.25,0.28,0.31,0.23,0.21,0.24,0.25,0.35,0.33,0.21,0.3,0.28,0.24,0.27,0.23,
0.28,0.23,0.25,0.23,0.25,0.26,0.26,0.22,0.18,0.18,0.18,0.14,0.19,0.19,0.17,0.22,0.19,0.18,
0.18,0.17,0.16,0.14,0.13,0.05,0.18,0.18,0.18,0.1,0.1,0.12,0.16,0.09,0.1,0.14,0.07,0.07,0.09,
0.09,0.07,0.1,0.11,0.06,0.11,0.13,0.05,0.08,0.09,0.13,0.1,0.11,0.08,0.08,0.07,0.1,0.06,0.08,
0.08,0.08,0.1,0.09,0.06,0.07,0.05,0.05,0.1,0.1,0.13,0.09,0.03,0.04,0.08,0.04,0.04,0.01,0.03,
0.02,0.03,0.01,0.02,0.02,0.01,0.08,0.01,0.01,0.01,0.01,0.02,0.03,0.12,0.03,0.01,0.01,0.03,
0.01,0.01,0.01,0.01,0.01,0.01,0.02,0.02,0.03,0.01,0.04,0.09,0.04,0.01,0.01,0.06,0.02,0.04,
0.01,0.01,0.02,0.09,0.05,0.02)
dates_ind = c()
ETRS = c()
for (i in 1:length(mydates_)){
	dates_ind = c(dates_ind, which(mydates_[i] == dates_2007))
	ETRS = c(ETRS, HRHW_ETRS[which(mydates_[i] == dates_2007)])
}


plot(sort(mydates_), mydata[1,], ylim = c(min(mydata),max(mydata)),type="l",main=Cname)
for (i in 2:dim(mydata)[1]){
	plot(sort(mydates_),  mydata[i,], type="b",col=i, ylim =c (0.32, 1.2))
	x = c(0.32,0.32,0.32,0.32,0.48,0.68,0.80,0.80,0.76,0.56,0.32,0.32)
	lines(seq(as.Date("2007/1/1"), as.Date("2007/12/31"), by = "months"), x, "b",lwd=3)
	readline()
}



x = c(0,0,0,0.36,3.4,6.03,9.42,7.53,4.55,1.04,0,0)
x = c(0,0,0,0,1.36,5.36,10.85,2.67,0,0,0,0)
x = c(0.32,0.32,0.32,0.32,0.48,0.68,0.80,0.80,0.76,0.56,0.32,0.32)
lines(seq(as.Date("2007/1/1"), as.Date("2007/12/31"), by = "months"), x, "b",lwd=3)


# Try to interplote values with at least one monthly value

mydata_mon_ = ETrF_crp_vec[ind_nonNA_crp,]		# This dataframe contains only rows with crop values
colnames(mydata_mon_) = c(as.character(mydates_),"Crp")	# dates as column names and crops

# The columns are not in sorted date order
# change it so that the dates are in ascending order
mydates_ = as.Date(mydates, format = "%Y-%m-%d")
mydates = sort(mydates_)
sorted_dates_ind = c()
for (i in 1:length(mydates_)){
	sorted_dates_ind = c(sorted_dates_ind, which(mydates_ == sort(mydates_)[i]))
}
mydata_mon = cbind(mydata_mon_[,sorted_dates_ind],mydata_mon_[,23])

crops = unique(mydata_mon[,23])
cropNo = 4
print (crops[cropNo])
Cname = CropNames[which(CropList == crops[cropNo])]
print (Cname)
ind = which(crops[cropNo] == mydata_mon[,23])
length(ind)
mydata = mydata_mon[ind,1:22]

plot(sort(mydates_), mydata[1,], ylim = c(min(mydata,na.rm=T),max(mydata,na.rm=T)),type="l",main=Cname)
for (i in 2:dim(mydata)[1]){
	plot(sort(mydates_),  mydata[i,], type="b",col=i)
	#x = c(0.32,0.32,0.32,0.32,0.48,0.68,0.80,0.80,0.76,0.56,0.32,0.32)
	#lines(seq(as.Date("2007/1/1"), as.Date("2007/12/31"), by = "months"), x, "b",lwd=3)
	readline()
}


# Make indices of rows with at least one data value per month
mons = unique(as.numeric(format(mydates,"%m")))
reqd_ind = c()
for (mon_ctr in 1:length(mons)){
	mon_ind = which(as.numeric(format(mydates,"%m"))==mons[mon_ctr])
	print (mons[mon_ind])
	for (i in 1:nrow(mydata_mon)){
		if (length(which(is.na(mydata_mon[i,mon_ind]))) < length(mon_ind)){
			reqd_ind = c(reqd_ind, i)
		}
	}
}

	













crps = crp_vec[ind_nonNA]
ind_crps = (which(!is.na(crps)))


ind_1 = which(ind_crps == 1)

ETrF_vec[ind_crps,]

length(ind_nonNA)



non_NA_count_mat = mat-mat


