getData=function(stano)
{
if (nchar(stano)<8) {stano=paste("0",stano,sep="")} 

chk_ctr=23
chk_char=""
while (chk_char!="USGS") {
	junk_arr1=read.table(file=as.character(stano),skip=(chk_ctr),fill=T)
	chk_char=as.character(junk_arr1[1,1])
	chk_ctr=chk_ctr+1
}
chk_ctr=chk_ctr-1

data=read.table(file=as.character(stano),skip=chk_ctr,flush=TRUE,comment.char="#",fill=T)
getData=data
}

WY_conv.R=function(USGSdata)
{
	data=USGSdata
	theDate=as.Date(data$V3)	## Date
	theDailyFlow=data$V4		## Discharge
	yr=as.numeric(format(theDate,"%Y")) ## Extracting years 
	mo=as.numeric(format(theDate,"%m")) ## Extracting Months
	lenofdata=length(yr) ## Length of Data
	ybeg=range(yr)[1]		## beginning Year
	yend=range(yr)[2]		## End Year
	years=ybeg:yend

	if (mo[1]>=1 & mo[1]<=9){
		WYst=yr[1]
		stdate4wy=as.Date(paste(WYst-1,"-10-1",sep=""))
		stdate=as.Date(theDate[1])
		days=as.numeric(as.Date(paste(WYst,"-09-30",sep=""))-stdate4wy)+1
		ind=(theDate>=stdate4wy & theDate<=as.Date(paste(WYst,"-09-30",sep="")))
		flow=theDailyFlow[ind]
		daystoskip=as.numeric(stdate-stdate4wy)
		flow1=as.vector(c(rep(NA,daystoskip),flow))
		if (mo[lenofdata]>=1 & mo[lenofdata]<=9){
			WYend=yr[lenofdata]
			nyears=(yend-ybeg+1)
			Wyears=(WYend-WYst+1)
			WYmat=matrix(NA,ncol=Wyears,nrow=366)
			colnames(WYmat)=c(WYst:WYend)
			WYmat[1:days,1]=flow1
			for (i in 2:(nyears-1)){
				begdate1=as.Date(paste(years[i]-1,"-10-1",sep=""))
				enddate1=as.Date(paste(years[i],"-9-30",sep=""))
				days=as.numeric(enddate1-begdate1)+1
				ind=(theDate>=begdate1 & theDate<=enddate1)
				flow=theDailyFlow[ind]
				flow2=as.numeric(as.character(flow))
				WYmat[1:days,i]=flow2
			}
			begdate1=as.Date(paste(years[i],"-10-1",sep=""))
			enddate1=as.Date(paste(years[i]+1,"-9-30",sep=""))
			enddate2=as.Date(theDate[lenofdata])
			days=as.numeric(enddate2-begdate1)+1
			daysinWY=as.numeric(enddate1-begdate1)+1
			ind=(theDate>=begdate1 & theDate<=enddate2)
			flow=theDailyFlow[ind]
			daystobefilled=daysinWY-days
			flow2=as.vector(c(as.numeric(as.character(flow)),rep(NA,daystobefilled)))
			WYmat[1:daysinWY,(i+1)]=flow2

		} else {

			WYend=yr[lenofdata]+1
			nyears=(yend-ybeg+1)
			Wyears=(WYend-WYst+1)
			WYmat=matrix(NA,ncol=Wyears,nrow=366)
			colnames(WYmat)=c(WYst:WYend)
			WYmat[1:days,1]=flow1
			for (i in 2:nyears){
				begdate1=as.Date(paste(years[i]-1,"-10-1",sep=""))
				enddate1=as.Date(paste(years[i],"-9-30",sep=""))
				days=as.numeric(enddate1-begdate1)+1
				ind=(theDate>=begdate1 & theDate<=enddate1)
				flow=theDailyFlow[ind]
				flow2=as.numeric(as.character(flow))
				WYmat[1:days,i]=flow2
			}
			begdate1=as.Date(paste(years[i],"-10-1",sep=""))
			enddate1=as.Date(paste(years[i]+1,"-9-30",sep=""))
			enddate2=as.Date(theDate[lenofdata])
			days=as.numeric(enddate2-begdate1)+1
			daysinWY=as.numeric(enddate1-begdate1)+1
			ind=(theDate>=begdate1 & theDate<=enddate2)
			flow=theDailyFlow[ind]
			daystofill=daysinWY-days
			flow2=as.vector(c(as.numeric(as.character(flow)),rep(NA,daystofill)))
			WYmat[1:daysinWY,(i+1)]=flow2
		}
			
	} else {
		WYst=yr[1]+1
		stdate4wy=as.Date(paste((WYst-1),"-10-1",sep=""))
		stdate=as.Date(theDate[1])
		daysinWY=as.numeric(as.Date(paste(WYst,"-09-30",sep=""))-stdate4wy)+1
		days=as.numeric(as.Date(paste(WYst,"-09-30",sep=""))-stdate)+1
		ind=(theDate>=stdate4wy & theDate<=as.Date(paste(WYst,"-09-30",sep="")))
		flow=theDailyFlow[ind]
		daystofill=as.numeric(stdate-stdate4wy)
		flow1=as.vector(c(rep(NA,daystofill),flow))
		if (mo[lenofdata]>=1 & mo[lenofdata]<=9){
			WYend=yr[lenofdata]
			nyears=(yend-ybeg+1)
			Wyears=(WYend-WYst+1)
			WYmat=matrix(NA,ncol=Wyears,nrow=366)
			colnames(WYmat)=c(WYst:WYend)
			WYmat[1:daysinWY,1]=flow1
			for (i in 2:(Wyears-1)){
				begdate1=as.Date(paste(years[i],"-10-1",sep=""))
				enddate1=as.Date(paste(years[i]+1,"-9-30",sep=""))
				days=as.numeric(enddate1-begdate1)+1
				ind=(theDate>=begdate1 & theDate<=enddate1)
				flow=theDailyFlow[ind]
				flow2=as.numeric(as.character(flow))
				WYmat[1:days,i]=flow2
			}
			i=i+1
			begdate1=as.Date(paste(years[i],"-10-1",sep=""))
			enddate1=as.Date(paste(years[i]+1,"-9-30",sep=""))
			enddate2=as.Date(theDate[lenofdata])
			days=as.numeric(enddate2-begdate1)+1
			daysinWY=as.numeric(enddate1-begdate1)+1
			ind=(theDate>=begdate1 & theDate<=enddate2)
			flow=theDailyFlow[ind]
			daystofill=daysinWY-days
			flow2=as.vector(c(as.numeric(as.character(flow)),rep(NA,daystofill)))
			WYmat[1:daysinWY,i]=flow2

		} else {

			WYend=yr[lenofdata]+1
			nyears=(yend-ybeg+1)
			Wyears=(WYend-WYst+1)
			WYmat=matrix(NA,ncol=Wyears,nrow=366)
			colnames(WYmat)=c(WYst:WYend)
			WYmat[1:daysinWY,1]=flow1
			for (i in 2:(nyears-1)){
				begdate1=as.Date(paste(years[i],"-10-1",sep=""))
				enddate1=as.Date(paste(years[i]+1,"-9-30",sep=""))
				days=as.numeric(enddate1-begdate1)+1
				ind=(theDate>=begdate1 & theDate<=enddate1)
				flow=theDailyFlow[ind]
				flow2=as.numeric(as.character(flow))
				WYmat[1:days,i]=flow2
			}
			i=i+1
			begdate1=as.Date(paste(years[i],"-10-1",sep=""))
			enddate1=as.Date(paste(years[i]+1,"-9-30",sep=""))
			enddate2=as.Date(theDate[lenofdata])
			days=as.numeric(enddate2-begdate1)+1
			daysinWY=as.numeric(enddate1-begdate1)+1
			ind=(theDate>=begdate1 & theDate<=enddate2)
			flow=theDailyFlow[ind]
			daystofill=daysinWY-days
			flow2=as.vector(c(as.numeric(as.character(flow)),rep(NA,daystofill)))
			WYmat[1:daysinWY,i]=flow2
		}
	}
WYmat[WYmat=="Ice"]=NA
WYmat
}



zeroday.R=function(WYmat){
	styr=as.numeric(colnames(WYmat)[1])
	noyr=dim(WYmat)[2]
	temp=matrix(NA,nrow=noyr,ncol=2)
	for (i in 1:noyr){
		yr=styr+(i-1)
		if ((yr%%400 == 0 || (yr%%100 !=0 && yr%%4==0))){days=366} else {days=365}
		yrdis=WYmat[,i]
		naval=length(yrdis[is.na(yrdis)])
		if (naval<36) {
			non_NA_vals=length(na.omit(yrdis))
			zero_vals=length(which(yrdis==0))
			temp[i,1]=(zero_vals/non_NA_vals)*days
			temp[i,2]=temp[i,1]/days ## Fraction of zero days
		} else {
			temp[i,1] = NA
		}
	}
	zerodays=mean(temp[,1],na.rm=T)
	x=1:noyr
	y=temp[,1]
	
	a=length(which(is.na(y)==F))
	slp=NA
	pval=NA

	frax_y=temp[,2]
	mean_frax_z=mean(temp[,2],na.rm=T)

	if (a>2){
		lm.zeroday=lm(y~x)
		slp=as.numeric(coef(lm.zeroday)[2])
		pval=as.numeric(summary(lm.zeroday)$coefficients[2,4])
	}
	
	yrs=as.numeric(colnames(WYmat))
	ans=list(zerodays,slp,pval,temp[,1],frax_y,yrs,mean_frax_z)
	return(ans)
}



BFI_whole.R=function(WYmat){
	noyr=dim(WYmat)[2]
	temp=matrix(NA,nrow=noyr,ncol=1)
	mintemp=matrix(NA,nrow=noyr,ncol=1)
	maxtemp=matrix(NA,nrow=noyr,ncol=1)
	meantemp=matrix(NA,nrow=noyr,ncol=1)
	for (i in 1:noyr){
		yrdis=WYmat[,i]
		naval=length(yrdis[is.na(yrdis)])
		if (naval<36) {
			mintemp[i,1]=min(WYmat[,i],na.rm=TRUE)
			maxtemp[i,1]=max(WYmat[,i],na.rm=TRUE)
			meantemp[i,1]=mean(WYmat[,i],na.rm=TRUE)
			temp[i,1]=(min(WYmat[,i],na.rm=TRUE)/(mean(WYmat[,i],na.rm=TRUE)))*100
		}else{
			temp[i,1]=NA
			maxtemp[i,1]=NA
			mintemp[i,1]=NA
			meantemp[i,1]=NA
		}
	}
	BFI_whole=mean(temp[,1],na.rm=T)
	x=1:noyr
	y=temp[,1]

	a=length(which(is.na(y)==F))
	slp=NA
	pval=NA
	
	if (a>2){
		lm.BFI=lm(y~x)
		slp=as.numeric(coef(lm.BFI)[2])
		pval=as.numeric(summary(lm.BFI)$coefficients[2,4])
	}
	
	minflow=mintemp[,1]
	maxflow=maxtemp[,1]
	meanflow=meantemp[,1]
	
	yrs=as.numeric(colnames(WYmat))
	ans=list(BFI_whole,slp,pval,temp[,1],yrs,minflow,maxflow,meanflow)
	return(ans)
}

SI.R=function (WYmat)
{


noyr=dim(WYmat)[2]
temp=matrix(NA,nrow=noyr,ncol=1)


bfi_list=BFI_whole.R(WYmat)
BFImat=unlist(bfi_list[4])


zero_list=zeroday.R(WYmat)
ZEROmat=unlist(zero_list[5])

for (i in 1:noyr){
	temp[i,1]=(BFImat[i]/100) - ZEROmat[i]
}
SI1=mean(temp,na.rm=T)
x=1:noyr
y=temp[,1]

a=length(which(is.na(y)==F))
slp=NA
pval=NA

if (a>2){
	lm.SI=lm(y~x)
	slp=as.numeric(coef(lm.SI)[2])
	pval=as.numeric(summary(lm.SI)$coefficients[2,4])
}

SImat=temp[,1]
ans=list(SI1,slp,pval,SImat)
return(ans)
}
