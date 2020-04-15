q167_lp3.R=function(WYmat){
	lp3ff=function(cs,T)
	{
		F=1/T
		shp=4/(cs*cs)
		if(abs(cs)<0.00001)
			{ans=qnorm(F)}
		else
			{if(cs>0){
				ans=qgamma(F,shp)*cs/2-2/cs
			}
			else
			{
			ans=qgamma(1-F,shp)*cs/2-2/cs
			}
		}
		return(ans)
	}

	temp=matrix(NA,nrow=dim(WYmat)[2],ncol=1)
	for (i in 1:dim(WYmat)[2]){
		if ((366-length(WYmat[,i][WYmat[,i]=="NA"])>36)){
			temp[i,1]=max(WYmat[,i],na.rm=T)
		} else {
			temp[i,1]=NA
		}
	}
	maxflow=as.vector(temp)
	maxflow2=maxflow[which(maxflow!=0)]
	y1=as.vector(maxflow2)
	n=length(y1)
	y=log10(y1)
	ybar=mean(y,na.rm=T)
	sy=sd(y,na.rm=T)
	Cs=(n*sum((y-ybar)^3))/((n-1)*(n-2)*(sy^3))
	KT=lp3ff(Cs,1.67)
	yt=ybar+sy*KT
	q=10^yt
	
	temp_flddur=matrix(NA,nrow=dim(WYmat)[2],ncol=1)
	fld=0
	for (i in 1:dim(WYmat)[2]) {
		temp2=WYmat[,i]
		b=temp2[is.na(temp2)==FALSE]
		
		if (length(b)>=1){
			fld = length(b[b>=q])
		} else {
			fld=NA
		}
		temp_flddur[i,1]=fld
	}

	flddur=mean(temp_flddur[,1],na.rm=T)
	noyr=dim(WYmat)[2]
	x=1:noyr
	y=temp_flddur[,1]
	lm.flddur=lm(y~x)
	slp=as.numeric(coef(lm.flddur)[2])
	pval=as.numeric(summary(lm.flddur)$coefficients[2,4])
	
	ans=list(q,flddur,slp,pval,temp_flddur)
	
	return(ans)
}