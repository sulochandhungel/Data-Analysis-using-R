flowrev=function(WYmat){

nyr=dim(WYmat)[2]
tmat=rep(NA,nyr)
for (ctr in 1:nyr){
	yrdis=WYmat[,ctr]
	naval=length(yrdis[is.na(yrdis)])
	if (naval<36){
		b=yrdis[is.na(yrdis)==FALSE]
		if (length(b)>=3){
			fr=0
			for (j in 1:(length(b)-2)) {
				a1=b[j]
				a2=b[j+1]
				a3=b[j+2]
				if ((a3<a2 & a2>a1) | (a3>a2 & a2<a1)) {
					fr =fr+1
				}
			}
			fr=fr+1
			tmat[ctr]=fr
		}
	}
}
FR=mean(tmat,na.rm=T)
x=1:nyr
y=tmat

a=length(which(is.na(y)==F))
slp=NA
pval=NA
if (a>2){
	lm.FR=lm(y~x)
	slp=as.numeric(coef(lm.FR)[2])
	pval=as.numeric(summary(lm.FR)$coefficients[2,4])
}

result=list(FR,slp,pval,tmat)
return(result)
}