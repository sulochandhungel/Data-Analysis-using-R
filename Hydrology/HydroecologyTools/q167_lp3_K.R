q167_lp3_K.R=function(WYmat){

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
		if (366-length(WYmat[,i][WYmat[,i]=="NA"])>1){
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
	N=0
	flddays=0
	for (i in 1:dim(WYmat)[2]) {
		temp2=WYmat[,i]
		b=temp2[is.na(temp2)==FALSE]
		
		if (length(b)>=166){
			fld = length(b[b>=q])
			flddays=flddays+fld
			N=N+1
		}
	}

	flddur=flddays/N
	
	ans=list(flddur,q)

	return(ans)
}