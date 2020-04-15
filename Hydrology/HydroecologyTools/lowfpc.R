flw_puls_cnt.R=function(WYmat){
	noyr=dim(WYmat)[2]
	temp=matrix(NA,nrow=noyr,ncol=2)

#	q5=as.numeric(quantile(WYmat,p=c(0.05),na.rm=T)[1])
	q25=as.numeric(quantile(WYmat,na.rm=T)[2])
	q75=as.numeric(quantile(WYmat,na.rm=T)[4])


	qmean=mean(WYmat,na.rm=T)
	mean5=0.05*qmean

	for (i in 1:noyr){
		yrdis=WYmat[,i]
		naval=length(yrdis[is.na(yrdis)])
		if (naval<36) {
			temp[i,1]=sum(yrdis<q25)
			temp[i,2]=sum(yrdis>q75)
			yrdis2=yrdis
			yrdis2[yrdis2<q25]=-1
		} else {
			temp[i,1]=NA
			temp[i,2]=NA
		}
		
	}
	lowfpc=mean(temp[,1],na.rm=T)
	highfpc=mean(temp[,2],na.rm=T)
	ans=list(lowfpc,highfpc)
	return(ans)
}


