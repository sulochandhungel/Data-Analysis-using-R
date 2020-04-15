flw_puls_evnt.R=function(WYmat){
	noyr=dim(WYmat)[2]
	temp=matrix(NA,nrow=noyr,ncol=3)

	q5=as.numeric(quantile(WYmat,p=c(0.05),na.rm=T)[1])
#	q25=as.numeric(quantile(WYmat,na.rm=T)[2])
	q75=as.numeric(quantile(WYmat,na.rm=T)[4])


	for (i in 1:noyr){
		yrdis=WYmat[,i]
		naval=length(yrdis[is.na(yrdis)])
		if (naval<36) {
			ind_lfe=which((yrdis<q5)==T)
			if (length(ind_lfe)>0){
				ind2_lfe=rep(NA,length(ind_lfe)-1)
				for(ctr in 2:length(ind_lfe)){ind2_lfe[ctr-1]=ind_lfe[ctr]-ind_lfe[ctr-1]}
				lfe=length(which(ind2_lfe>1))+1
				temp[i,1]=lfe			
			} else {
				temp[i,1]=0
			}
			ind_hfe=which((yrdis>q75)==T)
			if (length(ind_hfe)>0){
				ind2_hfe=rep(NA,length(ind_hfe)-1)
				for(ctr in 2:length(ind_hfe)){ind2_hfe[ctr-1]=ind_hfe[ctr]-ind_hfe[ctr-1]}
				hfe=length(which(ind2_hfe>1))+1
				temp[i,2]=hfe
			} else {
				temp[i,2]=0
			}

			ind_zfe=which(yrdis==0)
			if (length(ind_zfe)>0){
				ind2_zfe=rep(NA,length(ind_zfe)-1)
				for(ctr in 2:length(ind_zfe)){ind2_zfe[ctr-1]=ind_zfe[ctr]-ind_zfe[ctr-1]}
				zfe=length(which(ind2_zfe>1))+1
				temp[i,3]=zfe			
			} else {
				temp[i,3]=0
			}


		} else {
			temp[i,1]=NA
			temp[i,2]=NA
			temp[i,3]=NA
		}
	}
	lfe_m=mean(temp[,1],na.rm=T)
	hfe_m=mean(temp[,2],na.rm=T)
	zfe_m=mean(temp[,3],na.rm=T)
	ans=list(lfe_m,hfe_m,zfe_m)
	return(ans)
}


