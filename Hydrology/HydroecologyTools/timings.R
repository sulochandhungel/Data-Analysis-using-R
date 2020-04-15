t25t50t75=function(WYmat){
nyr=dim(WYmat)[2]
tmat=matrix(NA,nrow=nyr,ncol=3)
for (ctr in 1:nyr){
	cumdis=rep(NA,365)
	yrdis=WYmat[,ctr]
	naval=length(yrdis[is.na(yrdis)])
	indt25=NA
	indt50=NA
	indt75=NA
	if (naval<36) {
		cumdis[1]=yrdis[1]
		for (i in 2:366){cumdis[i]=sum(cumdis[i-1],yrdis[i],na.rm=T)}
		q25=0.25*cumdis[366]
		q50=0.50*cumdis[366]
		q75=0.75*cumdis[366]
		indt25=which(cumdis>=q25)[1]
		indt50=which(cumdis>=q50)[1]
		indt75=which(cumdis>=q75)[1]
	}
	tmat[ctr,1]=indt25
	tmat[ctr,2]=indt50
	tmat[ctr,3]=indt75
}
t25=mean(tmat[,1],na.rm=T)
t50=mean(tmat[,2],na.rm=T)
t75=mean(tmat[,3],na.rm=T)

x=1:nyr
y1=tmat[,1]
y2=tmat[,2]
y3=tmat[,3]

a=length(which(is.na(y1)==F))
b=length(which(is.na(y2)==F))
c=length(which(is.na(y3)==F))

slpT25=NA
pvalT25=NA

slpT50=NA
pvalT50=NA

slpT75=NA
pvalT75=NA



if (a>2){
	lm.T25=lm(y1~x)
	slpT25=as.numeric(coef(lm.T25)[2])
	pvalT25=as.numeric(summary(lm.T25)$coefficients[2,4])
}

if (b>2){
	lm.T50=lm(y2~x)
	slpT50=as.numeric(coef(lm.T50)[2])
	pvalT50=as.numeric(summary(lm.T50)$coefficients[2,4])
}

if (c>2){
	lm.T75=lm(y3~x)
	slpT75=as.numeric(coef(lm.T75)[2])
	pvalT75=as.numeric(summary(lm.T75)$coefficients[2,4])
}

timings=list(t25,slpT25,pvalT25,t50,slpT50,pvalT50,t75,slpT75,pvalT75,y1,y2,y3)

return (timings)

}