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