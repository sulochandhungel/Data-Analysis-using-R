DailyMeans.R = function(sites,outfile="Daily Means.csv",stnsDir = "C:/Users"){
DMeanMat=matrix(NA,ncol=length(sites),nrow=366)
colnames(DMeanMat)=sites
workdir=getwd()
for (i in 1:length(sites)){
	stano=sites[i]
	
	setwd (stnsDir)
	USGSdata=getData(stano) # Get data from USGS downloaded file
	setwd(workdir)

	WYmat=WY_conv.R(USGSdata) # Convert the data to Water year
	WYmat[WYmat=="Ice"]=NA

	if (colnames(WYmat)[1]<=1965){indyr1=which(colnames(WYmat)==1965)} else {indyr1=1}
	nyr=dim(WYmat)[2]
	if (colnames(WYmat)[nyr]==2010){indyr2=which(colnames(WYmat)==2010)} else {indyr2=nyr}
	WYmat2=WYmat[,indyr1:indyr2]

	y1=rowMeans(WYmat2,na.rm=T)
	DMeanMat[,i]=y1

}
setwd(workdir)
write.csv(DMeanMat,file=outfile)
DMeanMat
}