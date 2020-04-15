flw_puls_evnt_5_95.R=function(WYmat){
	noyr=dim(WYmat)[2]
	temp=matrix(NA,nrow=noyr,ncol=3)

	q5=as.numeric(quantile(WYmat,p=c(0.05),na.rm=T)[1])
#	q25=as.numeric(quantile(WYmat,na.rm=T)[2])
	q95=as.numeric(quantile(WYmat,p=c(0.95),na.rm=T)[1])


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
			ind_hfe=which((yrdis>q95)==T)
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
	x=1:noyr
	lfe=temp[,1]
	hfe=temp[,2]
	zfe=temp[,3]

	a=length(which(is.na(lfe)==F))
	slp_lfe=NA
	pval_lfe=NA

	b=length(which(is.na(hfe)==F))
	slp_hfe=NA
	pval_hfe=NA

	c=length(which(is.na(zfe)==F))
	slp_zfe=NA
	pval_zfe=NA

	if (a>2){
		lm.lfe=lm(lfe~x)
		slp_lfe=as.numeric(coef(lm.lfe)[2])
		pval_lfe=as.numeric(summary(lm.lfe)$coefficients[2,4])
	}

	if (b>2){
		lm.hfe=lm(hfe~x)
		slp_hfe=as.numeric(coef(lm.hfe)[2])
		pval_hfe=as.numeric(summary(lm.hfe)$coefficients[2,4])
	}

	if (c>2){
		lm.zfe=lm(zfe~x)
		slp_zfe=as.numeric(coef(lm.zfe)[2])
		pval_zfe=as.numeric(summary(lm.zfe)$coefficients[2,4])
	}

	colnames(temp)=c("LFE","HFE","ZFE")
	lfe_m=mean(temp[,1],na.rm=T)
	hfe_m=mean(temp[,2],na.rm=T)
	zfe_m=mean(temp[,3],na.rm=T)

	ans=list(lfe_m,hfe_m,zfe_m,lfe,hfe,zfe,slp_lfe,pval_lfe,slp_hfe,pval_hfe,slp_zfe,pval_zfe)
	return(ans)
}


