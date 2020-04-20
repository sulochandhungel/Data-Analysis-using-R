tpth=function(WYmat){
DMean=rowMeans(WYmat, na.rm = T) ## daily mean
n=365
tt=1:n
Z=DMean[tt]

# Do fourier series on DMean[1:365]
 ak=matrix(data=0,nrow=184,ncol=1)
 bk=matrix(data=0,nrow=184,ncol=1)

##  Calculating fourier coefficients
for(k in 0:(365/2))
{
if(k==0)
{
ak[k+1]=1/n*sum(Z*cos(2*pi*k*tt/n))
}else
{
ak[k+1]=2/n*sum(Z*cos(2*pi*k*tt/n))
}
bk[k+1]=2/n*sum(Z*sin(2*pi*k*tt/n))
}

##  Putting back together
# Calculating the series from the fourier coefficients
Zrecon=matrix(0,ncol=183,nrow=365)
for(k in 0:(365/2))
{
Zrecon[,k+1]=ak[k+1]*cos(2*pi*k*tt/n)+bk[k+1]*sin(2*pi*k*tt/n)
}

har1=tt[Zrecon[,2]==max(Zrecon[,2])]
pkflow=tt[DMean==max(Z)][1]
pkflowhar1=list(pkflow,har1,Z,Zrecon)
return(pkflowhar1)
}
