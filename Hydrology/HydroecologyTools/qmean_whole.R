qmean_whole.R=function(WYmat){
	WYmatAsVec=as.vector(WYmat)
	qmean=mean(WYmatAsVec,na.rm=T)
	qmean
}