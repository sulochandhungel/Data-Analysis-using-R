q167.R=function(WYmat){
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
	y=as.vector(maxflow2)
	
	my=mean(y,na.rm=TRUE)   		#  mean of data
	sy=sqrt(var(y,na.rm=TRUE)) 		#  std dev of data
	vy=sy/my 			# coeff of var of data
	sig2=log(vy^2+1)  	#  Benjamin and Cornell 3.3.35.  var of log of data
	mty=my*exp(-0.5*sig2)	# Benjamin and Cornell 3.3.34   mean of log of data

	#  mty and sig2 are parameters meanlog and sdlog of log normal distribution
	#  These were estimated fitting the moments of the actual data
 
	#  qlnorm is the quantile function that is the inverse of the cumulative distribution 
	#  function.  Input is the desired probability 1/1.67.  Output is the corresponding
	#  quantile of the data, i.e. the value with a probability 1/1.67 of not being exceeded

	q167=qlnorm(1/1.67,meanlog=log(mty),sdlog=sqrt(sig2))
	
	temp_flddur=matrix(NA,nrow=dim(WYmat)[2],ncol=1)
	fld=0
	for (i in 1:dim(WYmat)[2]) {
		temp2=WYmat[,i]
		b=temp2[is.na(temp2)==FALSE]
		
		if (length(b)>=1){
			fld = length(b[b>=q167])
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
	
	ans=list(q167,flddur,slp,pval,temp_flddur)
	
	return(ans)
}
