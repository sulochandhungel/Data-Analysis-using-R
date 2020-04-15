COV.R=function(WYmat){
	WYmatAsVec=as.vector(WYmat)
	COV=(sd(WYmatAsVec,na.rm=T)/mean(WYmatAsVec,na.rm=T))
return(COV)
}