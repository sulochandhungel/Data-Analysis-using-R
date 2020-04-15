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