SIwhole.R=function (BFImat,ZEROfmat)
{
source("C:/Users/Sulochan/Sulochan/Sulochan_Backup/Work/GAGES_data/Revised R-codes/Functions for R/getData.R")
source("C:/Users/Sulochan/Sulochan/Sulochan_Backup/Work/GAGES_data/Revised R-codes/Functions for R/WY_conv.R")
source("C:/Users/Sulochan/Sulochan/Sulochan_Backup/Work/GAGES_data/Revised R-codes/Functions for R/zeroday.R")
source("C:/Users/Sulochan/Sulochan/Sulochan_Backup/Work/GAGES_data/Revised R-codes/Functions for R/BFI_whole.R")

noyr=dim(WYmat)[2]
temp=matrix(NA,nrow=noyr,ncol=1)
for (i in 1:noyr){
	temp[i,1]=(BFImat[i]/100) - ZEROfmat[i]
}
SI1=mean(temp,na.rm=T)
SImat=temp[,1]
ans=list(SI1,SImat)
return(ans)
}
