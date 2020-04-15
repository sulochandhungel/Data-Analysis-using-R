MEANyr.R=function(yrdata){
	noyr=dim(WYmat)[2]
	temp=matrix(NA,nrow=noyr,ncol=1)
	
	for (i in 1:noyr){
		yrdis=WYmat[,i]
		naval=length(yrdis[is.na(yrdis)])
		if (naval<36) {
			temp[i,1] = mean(yrdis,na.rm=T)
		} else {
			temp[i,1] = NA
		}
	}
	qmean=mean(temp[,1],na.rm=T)
	x=1:noyr
	y=temp[,1]/qmean
	
	a=length(which(is.na(y)==F))
	slp=NA
	pval=NA
	
	if (a>2){
		lm.qmean=lm(y~x)
		slp=as.numeric(coef(lm.qmean)[2])
		pval=as.numeric(summary(lm.qmean)$coefficients[2,4])
	}
	
	ans=list(qmean,slp,pval,temp[,1])
	return(ans)
}