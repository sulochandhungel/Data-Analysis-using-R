PCM=function(USGSdata){

dflow= as.numeric(as.vector(USGSdata$V4))
mflow=mean(dflow,na.rm=T)
mydate=as.Date(USGSdata$V3)
y=as.numeric(format(mydate,"%Y"))
m=as.numeric(format(mydate,"%m"))
d=as.numeric(format(mydate,"%d"))

binmat=matrix(data=0,nrow=7,ncol=12)
for (mo_ctr in 1:12){
	ind1=which(m==mo_ctr)
	flow_m=dflow[ind1]
	bin1_ind=which(flow_m<(0.5*mflow))
	bin2_ind=which(flow_m>=(0.5*mflow) & flow_m<(1.0*mflow))
	bin3_ind=which(flow_m>=(1.0*mflow) & flow_m<(1.5*mflow))
	bin4_ind=which(flow_m>=(1.5*mflow) & flow_m<(2.0*mflow))
	bin5_ind=which(flow_m>=(2.0*mflow) & flow_m<(2.5*mflow))
	bin6_ind=which(flow_m>=(2.5*mflow) & flow_m<(3.0*mflow))
	bin7_ind=which(flow_m>=(3.0*mflow))
	
	binmat[1,mo_ctr]=length(bin1_ind)
	binmat[2,mo_ctr]=length(bin2_ind)
	binmat[3,mo_ctr]=length(bin3_ind)
	binmat[4,mo_ctr]=length(bin4_ind)
	binmat[5,mo_ctr]=length(bin5_ind)
	binmat[6,mo_ctr]=length(bin6_ind)
	binmat[7,mo_ctr]=length(bin7_ind)
}

xj=matrix(data=0, nrow=1,ncol=12)
i=1
while (i!=13) {
	for (ctr in 1:7) {xj[1,i]=xj[1,i]+binmat[ctr,i]}
	i=i+1
}
hs=0
hsmat=matrix(data=NA,nrow=7,ncol=12)
for (i in 1:7) {
	for (j in 1:12) {
		pk=binmat[i,j]/xj[1,j]
		hs=(pk*(log(pk)))
		hsmat[i,j]=hs
		if (hsmat[i,j]=="NaN") {hsmat[i,j]=NA}
	}
}

hsmatsum=matrix(data=NA,nrow=7,ncol=1)
hs=0
for (i in 1:7) {
	for (j in 1:12) {
		hsmatsum[i,1]=sum(hsmatsum[i,1],hsmat[i,j],na.rm=TRUE)
	}
	hsmatsum[i,1]=(-1/12)*hsmatsum[i,1]
	hs=hs+hsmatsum[i,1]
}

p=1-(hs/log(7)) ## Predictability

yi=matrix(data=0,nrow=7,ncol=1)
j=1
hc=0
n=0
while (j!=8) {
	for (ctr in 1:12) {yi[j,1]=yi[j,1]+binmat[j,ctr]}
	j=j+1
}
for (i in 1:7) {
	for (j in 1:12) {
		n=n+binmat[i,j]
	}
}
for (i in 1:7) {
	if (yi[i,1]!=0) { 
		hc=hc+((yi[i,1]/n)*(log(yi[i,1]/n)))
	} else {
		hc = hc+0
	}
}
hc=-hc
c=1-(hc/log(7)) ## Constancy

m= p -c ## Contingency


result = list(p,c,m,binmat)

return(result)
}

