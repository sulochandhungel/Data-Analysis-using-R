working_dir = "C:/Users/Sulochan/Desktop/EcolVars"
setwd(working_dir)
source("Functions for R/getData.r")
source("Functions for R/WY_conv.R")
source("Functions for R/PCM.R")
source("Functions for R/Q7MaxMin.R")
source("Functions for R/timings.R")
source("Functions for R/TPTH.R")
source("Functions for R/flowrev.R")
source("Functions for R/BFI_whole.R")
source("Functions for R/COVyr.R")
source("Functions for R/MEANyr.R")
source("Functions for R/zeroday.R")
source("Functions for R/q167.R")
source("Functions for R/q167_lp3.R")
source("Functions for R/COV.R")
source("Functions for R/qmean_whole.R")
source("Functions for R/qmean_whole.R")
source("Functions for R/SI.R")
source("Functions for R/flw_puls_evnt_5_95.R")



setwd (working_dir)

stns=read.table("StnList.txt")
nsta=dim(stns)[1]
stnvar=matrix(NA,nrow=nsta,ncol=58)
#nsta=2

# Column name:
# Slp = Linear model slope of annual values 
# Pval = P-value of the linear model slope
# Unless specified, values of stream variables are average annual values

#	BFI = Base Flow Index 
#	CoV_whole = Coefficient of variation of daily flows (Entire series)
#	CoV_yr = Mean of annual coefficent of variation of daily flows 
#	Qmean_whole = Mean flow of daily flow (Entire Series)
#	Qmean_yr = Mean of annual mean flow of daily flow
#	Zero = Zero flow days
#	BFF_Ln = Bank Full Flow (Using Log Normal)
# 	BFF_Lp3 = Bank Full Flow (Using Log Pearson 3)
#	FD_Ln = Flood Duration (Using BFF_Ln)
#	FD_Lp3 = Flood Duration (Using BFF_Lp3)
#	P = Predictability
# 	C = Constancy
# 	M = Contingency
#	T25 = 25% Timing of flow
#	T50 = 50% Timing of flow
#	T75 = 75% Timing of flow
#	Pk_time = Time of peak
# 	H1 = Time of first harmonic
#	Q7min = 7-day minimum flow
#	Q7max = 7-day maximum flow
#	FR = Flow reversal
# 	SI = Steadiness Index (Later named Extended Low Flow Index  - ELFI)
#	HFE = High Flow Events
#	LFE = Low Flow Events
#	ZFE = Zero Flow Events


col_name=c("BFI","Slp_BFI","Pval_BFI",
		"CoV_whole",
		"CoV_yr","Slp_CoV","Pval_CoV",
		"Qmean_whole",
		"Qmean_yr","Slp_Qmean","Pval_Qmean",
		"Zero","Slp_Zero","Pval_Zero",
		"BFF_Ln",
		"FD_Ln","Slp_FD_Ln","Pval_FD_Ln",
		"BFF_Lp3",
		"FD_Lp3","Slp_FD_Lp3","Pval_FD_Lp3",
		"P","C","M",
		"T25","Slp_T25","Pval_T25",
		"T50","Slp_T50","Pval_T50",
		"T75","Slp_T75","Pval_T75",
		"Pk_time","H1",
		"Q7min","Slp_Q7min","Pval_Q7min",
		"Q7max","Slp_Q7max","Pval_Q7max",
		"FR","Slp_FR","Pval_FR",
		"SI","Slp_SI","Pval_SI",
		"HFE","Slp_HFE","Pval_HFE",
		"LFE","Slp_LFE","Pval_LFE",
		"ZFE","Slp_ZFE","Pval_ZFE",
		"Stn_No")
colnames(stnvar)=col_name

for (stn_ctr in 1:nsta){
	stano=stns[stn_ctr,1]
	stnvar[stn_ctr,58]=stano

	setwd("C:/Users/Sulochan/Desktop/EcolVars/stns")

	USGSdata=getData(stano) # Get data from USGS downloaded file
	WYmat=WY_conv.R(USGSdata) # Convert the data to Water year

	timing=t25t50t75(WYmat)
	t25=as.numeric(timing[1])
	t50=as.numeric(timing[4])
	t75=as.numeric(timing[7])
	slope_t25=as.numeric(timing[2])
	slope_t50=as.numeric(timing[5])
	slope_t75=as.numeric(timing[8])
	pval_t25=as.numeric(timing[3])
	pval_t50=as.numeric(timing[6])
	pval_t75=as.numeric(timing[9])
	# if matrix of T25, T50 and T75 is needed:
	# matT25=unlist(timing[10])
	# matT50=unlist(timing[11])
	# matT75=unlist(timing[12])
	stnvar[stn_ctr,26]=t25
	stnvar[stn_ctr,27]=slope_t25
	stnvar[stn_ctr,28]=pval_t25
	stnvar[stn_ctr,29]=t50
	stnvar[stn_ctr,30]=slope_t50
	stnvar[stn_ctr,31]=pval_t50
	stnvar[stn_ctr,32]=t75
	stnvar[stn_ctr,33]=slope_t75
	stnvar[stn_ctr,34]=pval_t75

	pkhar1=tpth(WYmat)
	pktime=as.numeric(pkhar1[1])
	har1=as.numeric(pkhar1[2])
	stnvar[stn_ctr,35]=pktime
	stnvar[stn_ctr,36]=har1

	max7min7=Q7MaxMin.R(WYmat)
	Q7min=as.numeric(max7min7[1])
	slope_Q7min=as.numeric(max7min7[2])
	pval_Q7min=as.numeric(max7min7[3])

	Q7max=as.numeric(max7min7[5])
	slope_Q7max=as.numeric(max7min7[6])
	pval_Q7max=as.numeric(max7min7[7])
	stnvar[stn_ctr,37]=Q7min
	stnvar[stn_ctr,38]=slope_Q7min
	stnvar[stn_ctr,39]=pval_Q7min
	stnvar[stn_ctr,40]=Q7max
	stnvar[stn_ctr,41]=slope_Q7max
	stnvar[stn_ctr,42]=pval_Q7max

	fr_list= flowrev(WYmat)
	fr=as.numeric(fr_list[1])
	slope_fr=as.numeric(fr_list[2])
	pval_fr=as.numeric(fr_list[3])

	# if matrix of yearly Flow Reversal is needed:
	# mat=unlist(fr_list[4])

	stnvar[stn_ctr,43]=fr
	stnvar[stn_ctr,44]=slope_fr
	stnvar[stn_ctr,45]=pval_fr

	bfi_list=BFI_whole.R(WYmat)
	BFI=as.numeric((bfi_list)[1])
	slope_BFI=as.numeric((bfi_list)[2])
	pval_BFI=as.numeric((bfi_list)[3])
	stnvar[stn_ctr,1]=BFI
	stnvar[stn_ctr,2]=slope_BFI
	stnvar[stn_ctr,3]=pval_BFI
	
	# if matrix of yearly BFI is needed:
	# mat=unlist(bfi_list[4])

	cov=COV.R(WYmat)
	stnvar[stn_ctr,4]=cov	

	cov_list=COVyr.R(WYmat)
	cov_yr=as.numeric(cov_list[1])
	slope_cov=as.numeric(cov_list[2])
	pval_cov=as.numeric(cov_list[3])
	# if matrix of yearly CoV is needed:
	# mat=unlist(cov_list[4])
	stnvar[stn_ctr,5]=cov_yr
	stnvar[stn_ctr,6]=slope_cov
	stnvar[stn_ctr,7]=pval_cov

	qmean1=qmean_whole.R(WYmat)
	stnvar[stn_ctr,8]=qmean1
	
	qmean_list=MEANyr.R(WYmat)
	qmean_yr=as.numeric(qmean_list[1])
	slope_qmean=as.numeric(qmean_list[2])
	pval_qmean=as.numeric(qmean_list[3])

	# if matrix of yearly Qmean is needed:
	# mat=unlist(qmean_list[4])

	stnvar[stn_ctr,9]=qmean_yr
	stnvar[stn_ctr,10]=slope_qmean
	stnvar[stn_ctr,11]=pval_qmean
	
	zero_list=zeroday.R(WYmat)
	zero_yr=as.numeric(zero_list[1])
	slope_zero=as.numeric(zero_list[2])
	pval_zero=as.numeric(zero_list[3])
	# if matrix of yearly Qmean is needed:
	# mat=unlist(zero_list[4])
	stnvar[stn_ctr,12]=zero_yr
	stnvar[stn_ctr,13]=slope_zero
	stnvar[stn_ctr,14]=pval_zero

	bff_ln_list=q167.R(WYmat) ## bank Full Flow = Q1.67 using log-Normal
	bff_ln=as.numeric(bff_ln_list[1])
	flddur_ln=as.numeric(bff_ln_list[2]) ## Flood Duration = Q1.67 using Log-Normal
	slope_flddur=as.numeric(bff_ln_list[3])
	pval_flddur=as.numeric(bff_ln_list[4])
	# if matrix of yearly Flood Duration is needed:
	# mat=unlist(bff_ln_list[5])
	stnvar[stn_ctr,15]=bff_ln
	stnvar[stn_ctr,16]=flddur_ln
	stnvar[stn_ctr,17]=slope_flddur
	stnvar[stn_ctr,18]=pval_flddur

	bff_lp3_list=q167_lp3.R(WYmat) ## bank Full Flow = Q1.67 using log-Pearson III
	bff_lp3=as.numeric(bff_lp3_list[1])
	flddur_lp3=as.numeric(bff_lp3_list[2]) ## Flood Duration = Q1.67 using Log-Pearson III
	slope_flddur_lp3=as.numeric(bff_lp3_list[3])
	pval_flddur_lp3=as.numeric(bff_lp3_list[4])
	# if matrix of yearly Flood Duration is needed:
	# mat=unlist(bff_lp3_list[5])
	stnvar[stn_ctr,19]=bff_lp3
	stnvar[stn_ctr,20]=flddur_lp3
	stnvar[stn_ctr,21]=slope_flddur_lp3
	stnvar[stn_ctr,22]=pval_flddur_lp3
	
	pcm_list=PCM(USGSdata)
	p=as.numeric(pcm_list[1])
	c=as.numeric(pcm_list[2])
	m=as.numeric(pcm_list[3])
	# if contingency table (bin matrix) is needed:
	# vec = unlist(pcm_list[4])
	# mat = matrix(vec,nrow=7,ncol=12)
	stnvar[stn_ctr,23]=p
	stnvar[stn_ctr,24]=c
	stnvar[stn_ctr,25]=m
	
	SI_list=SI.R(WYmat)
	SI=as.numeric(SI_list[1])
	slope_SI=as.numeric(SI_list[2])
	pval_SI=as.numeric(SI_list[3])
	# if matrix of yearly Qmean is needed:
	# mat=unlist(SI_list[4])
	stnvar[stn_ctr,46]=SI
	stnvar[stn_ctr,47]=slope_SI
	stnvar[stn_ctr,48]=pval_SI

	FE_list=flw_puls_evnt_5_95.R(WYmat)
	LFE=as.numeric(FE_list[1])
	HFE=as.numeric(FE_list[2])
	ZFE=as.numeric(FE_list[3])

	slope_LFE=as.numeric(FE_list[7])
	pval_LFE=as.numeric(FE_list[8])
	slope_HFE=as.numeric(FE_list[9])
	pval_HFE=as.numeric(FE_list[10])
	slope_ZFE=as.numeric(FE_list[11])
	pval_ZFE=as.numeric(FE_list[12])

	# if matrix of yearly HFE,LFE and ZFE is needed:
	# matLFE=unlist(FE_list[4])
	# matHFE=unlist(FE_list[5])
	# matZFE=unlist(FE_list[6])

	stnvar[stn_ctr,49]=HFE
	stnvar[stn_ctr,50]=slope_HFE
	stnvar[stn_ctr,51]=pval_HFE

	stnvar[stn_ctr,52]=LFE
	stnvar[stn_ctr,53]=slope_HFE
	stnvar[stn_ctr,54]=pval_HFE

	stnvar[stn_ctr,55]=ZFE
	stnvar[stn_ctr,56]=slope_ZFE
	stnvar[stn_ctr,57]=pval_ZFE
}

setwd (working_dir)
write.csv(stnvar,file="GAGES_Stn_Var.csv")


