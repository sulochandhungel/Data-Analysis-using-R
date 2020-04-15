COVyr.R=function(WYmat){
	noyr=dim(WYmat)[2]
	temp=matrix(NA,nrow=noyr,ncol=1)
	sdmat=matrix(NA,nrow=noyr,ncol=1)
	for (i in 1:noyr){
		yrdis=WYmat[,i]
		naval=length(yrdis[is.na(yrdis)])
		if (naval<36) {
			temp[i,1] = (sd(yrdis,na.rm=T)/mean(yrdis,na.rm=T))
			sdmat[i,1]=sd(yrdis,na.rm=T)
		} else {
			temp[i,1] = NA
		}
	}
	CoV=mean(temp[,1],na.rm=T)
	x=1:noyr
	y=temp[,1]

	a=length(which(is.na(y)==F))
	slp=NA
	pval=NA

	if (a>2){
		lm.CoV=lm(y~x)
		slp=as.numeric(coef(lm.CoV)[2])
		pval=as.numeric(summary(lm.CoV)$coefficients[2,4])
	}
	
	ans=list(CoV,slp,pval,temp[,1],sdmat[,1])
	return(ans)
}