Q7MaxMin.R=function(WYmat,m=7)
{
nyr=dim(WYmat)[2]
temp_min7_ar=rep(NA,nyr)
temp_max7_ar=rep(NA,nyr)
for (ctr in 1:nyr){
	yrdis=WYmat[,ctr]
	naval=length(yrdis[is.na(yrdis)])
	if (naval<36) {
		yvals=WYmat[,ctr]
		n=length(yvals)
		aves=rep(NA,n-m+1)
		for(i in 1:(n-m+1)){
			i1=i
			i2=i+m-1
			aves[i]=mean(yvals[i1:i2])
		}
		temp_min7_ar[ctr]=min(aves,na.rm=T)
		temp_max7_ar[ctr]=max(aves,na.rm=T)
	}
}
sevmin=mean(temp_min7_ar,na.rm=T)
sevmax=mean(temp_max7_ar,na.rm=T)

x=1:nyr

y1=temp_min7_ar
y2=temp_max7_ar

a=length(which(is.na(y1)==F))
b=length(which(is.na(y2)==F))

slp_min=NA
pval_min=NA
if (a>2){
	lm.smin=lm(y1~x)
	slp_min=as.numeric(coef(lm.smin)[2])
	pval_min=as.numeric(summary(lm.smin)$coefficients[2,4])
}

slp_max=NA
pval_max=NA
if (b>2){
	lm.smax=lm(y2~x)
	slp_max=as.numeric(coef(lm.smax)[2])
	pval_max=as.numeric(summary(lm.smax)$coefficients[2,4])
}


sevmaxmin=list(sevmin,slp_min,pval_min,y1,sevmax,slp_max,pval_max,y2)
return(sevmaxmin)
}
