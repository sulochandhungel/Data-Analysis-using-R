q167_RP=function(WYmat,ff){
RP=1/(1-ff)
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

	q167=qlnorm(1/RP,meanlog=log(mty),sdlog=sqrt(sig2))
	q167

}
