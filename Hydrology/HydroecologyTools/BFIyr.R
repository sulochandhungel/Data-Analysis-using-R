BFIyr.R=function(yrdata){
	if (sum(is.na(yrdata))<=36){	## to see if the year data has more than 10% NA
		BFI = (min(yrdata,na.rm=T)/mean(yrdata,na.rm=T))*100
	} else {
		BFI = NA
	}
BFI
}