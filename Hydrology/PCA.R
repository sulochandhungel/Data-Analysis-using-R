#(Reference used :Statistics for Environmental Engineers - Paul Mac Berthouex, Linfield C. Brown)
#(Page 67)

setwd("C:/Users/Sulochan/Sulochan/Sulochan_Backup/Work/Report_Jan_3_2011/Final_GAGES_set/Trends_1965_2010/PCA_with_wards")
tt=read.csv("Vars_for_PCA.csv",header=T)
stnlist=as.vector(tt[,2])

vc=dim(tt)[2]-3 # Serial No, Station ID and SI

flowvars=as.matrix(tt[,4:dim(tt)[2]])
flowvar=matrix(as.numeric(flowvars),nrow=nrow(tt),ncol=vc,byrow=F)
varname = colnames(flowvars)

SI=as.matrix(tt[,3])

pdf(file="Q_Q_plot.pdf")
for (i in 1:vc){
	x=flowvar[,i]	
	boxplot(x)
	title(main=paste(varname[i]),sub="Box Plot")
	qqnorm(x,main=paste("Normal Q-Q plot for ",varname[i],sep=""))
}
x=SI[,1]
boxplot(x)
title(main=paste("SI"),sub="Box Plot")
qqnorm(x,main=paste("Normal Q-Q plot for ","SI",sep=""))
dev.off()

## BOX - COX Power Transformation #################
lam_matrix=matrix(NA,ncol=4,nrow=vc)
pdf(file="Lamda_values.pdf")
par(mar=c(9,5,5,3))
for (varctr in 1:vc){
	lam_matrix[varctr,1]=varname[varctr]
	x=flowvar[,varctr]
	x=x/mean(x)
	lam=-5
	shap=matrix(0,nrow=101,ncol=4)

	for (i in 1:50){
		#xt=replace(x,which(x==0),1e-4)
		xt=x[x!=0]
		gm=exp(mean(log(xt)))
		#xi=((xt^lam)-1)/(lam*(gm^(lam-1)))
		xi=((xt^lam)-1)/(lam)
		shap[i,1]=lam
		shap[i,2]=as.numeric(shapiro.test(xi)[1])
		shap[i,3]=as.numeric(shapiro.test(xi)[2])
		#shap[i,4]=var(xi)
		lam=-5+(0.1*i)
	}

	i=i+1	

	lam=0
	#xt=replace(x,which(x==0),1e-4)	
	xt=x[x!=0]
	gm=exp(mean(log(xt)))
	xi=log(xt)
	shap[i,1]=lam
	shap[i,2]=as.numeric(shapiro.test(xi)[1])
	shap[i,3]=as.numeric(shapiro.test(xi)[2])
	#shap[i,4]=var(xi)

	lam=0.1
	for (i in 52:101){
		#xt=replace(x,which(x==0),1e-4)	
		xt=x[x!=0]
		gm=exp(mean(log(xt)))
		xi=((xt^lam)-1)/(lam)
		shap[i,1]=lam
		shap[i,2]=as.numeric(shapiro.test(xi)[1])
		shap[i,3]=as.numeric(shapiro.test(xi)[2])
		#shap[i,4]=var(xi)
		lam=0.1+(0.1*(i-51))
	}
	
	plot(shap[,1],shap[,2],ylim=c(0,1),type="l",xlab="Lamda",ylab="W-statistics",main=varname[varctr])
	abline(h=max(shap[,2]))
	abline(v=shap[,1][which(max(shap[,2])==shap[,2])])
	mtext(paste("W-statistics =",signif(max(shap[,2]),digits=5)),side=1,line=5)
	mtext(paste("Lamda =",shap[,1][which(max(shap[,2])==shap[,2])]),side=1,line=6)
	ind=which(max(shap[,2])==shap[,2])
	mtext(paste("P-value =",signif(as.numeric(shap[ind,3]),digits=5)),side=1,line=7)
	
	lam_matrix[varctr,2]= as.numeric(shap[ind,1]) ## Lamda
	lam_matrix[varctr,3]= as.numeric(shap[ind,2]) ## W-stat
	lam_matrix[varctr,4]= as.numeric(shap[ind,3]) ## P-value for W-stat
}
colnames(lam_matrix)=c("Variable", "Lamda", "W-stat", "P-val W-Stat")
dev.off()
write.csv(lam_matrix,file="lamda_matrix.csv")
##### BOX - COX Complete ###########


#### Finding the parameters from BoxCox.fit{geoR} function ######
require(sp)
require(geoR)
lam_mat2=matrix(NA,nrow=vc,ncol=4)
colnames(lam_mat2)=c("Variable", "No of Zero Days", "Lamda (From function)", "Lamda (Using W-stat)")
for (varctr in 1:vc){
	lam_mat2[varctr,1]=varname[varctr] ## variable name
	x=flowvar[,varctr]
	lam_mat2[varctr,2]=length(x[x==0]) ## No of zero values 
	xt=x[x!=0]	
	ml1=boxcoxfit(xt)
	lam_mat2[varctr,3]=as.numeric(ml1[1]) ## Lamda value from function
	lam_mat2[varctr,4]=lam_matrix[varctr,2] ## Lamda value from trial
}
write.csv(lam_mat2,file="lamda_matrix_using_function.csv")	
#### ####

### Select the best transformations and form a matrix ###########
flowvar_t1=matrix(0,ncol=vc,nrow=nrow(tt))
flowvar_t2=matrix(0,ncol=vc,nrow=nrow(tt))
for (varctr in 1:vc){
	lam1=as.numeric(lam_matrix[varctr,2]) ## TRIAL method
	lam2=as.numeric(lam_mat2[varctr,3]) ## From the function
	x=flowvar[,varctr]
	if (lam1!=0) {
		xi=((x^lam1)-1)/(lam1)
	} else {
		xi=log(x)
	}
	flowvar_t1[,varctr]=xi

	if (lam2!=0) {
		xi=((x^lam2)-1)/(lam2)
	} else {
		xi=log(x)
	}
	flowvar_t2[,varctr]=xi
}

Tvarname=rep("",vc)
for (i in 1:vc){
	Tvarname[i]=paste(varname[i],"_T",sep="")
}
colnames(flowvar_t1)=Tvarname
colnames(flowvar_t2)=Tvarname


### QQ-plot for comparison of the best parameter ##
pdf(file="QQ-plot for both.pdf",width=21,height=7)
shap_test_mat=matrix(NA,nrow=(vc),ncol=7)
for (varctr in 1:(vc)){
	layout(matrix(c(1:3),nrow=1,ncol=3))
	qqnorm(flowvar[,varctr],main=paste("N Q-Q plot for ",varname[varctr],sep=""),cex=0.6)
	qqnorm(flowvar_t1[,varctr],main=paste("N Q-Q plot for Transformed (Trial Method)",varname[varctr],sep=""),cex=0.6)
	qqnorm(flowvar_t2[,varctr],main=paste("N Q-Q plot for Transformed (Using Func)",varname[varctr],sep=""),cex=0.6)
	x=flowvar[,varctr]
	x1=flowvar_t1[,varctr]
	x2=flowvar_t2[,varctr]
	shap_test_mat[varctr,1]=varname[varctr]
	shap_test_mat[varctr,2]=as.numeric(shapiro.test(x)[1])
	shap_test_mat[varctr,3]=as.numeric(shapiro.test(x)[2])
	shap_test_mat[varctr,4]=as.numeric(shapiro.test(x1)[1])
	shap_test_mat[varctr,5]=as.numeric(shapiro.test(x1)[2])
	shap_test_mat[varctr,6]=as.numeric(shapiro.test(x2)[1])
	shap_test_mat[varctr,7]=as.numeric(shapiro.test(x2)[2])
}
dev.off()
colnames(shap_test_mat)=c("Variable","W-stat_original","P-val_original","W-stat_Trans_1","p_val_Trans_1","W-stat_Trans_2","p_val_Trans_2")
write.csv(shap_test_mat,file="shapiro_test_after_transformation.csv")		
###### 

###### We will be using Box-Cox TRANSFORMED DATA IN flowvar_t1 ############

###### Scaling of transformed variables ##################


for (varctr in 1:vc){
	x=flowvar_t1[,varctr]
	m=mean(x)
	sdv=sd(x)
	x1=(x-m)/sdv
	flowvar_t1[,varctr]=x1
}

#### Scaling of SI ####
x=SI[,1]
m=mean(x)
sdv=sd(x)
x1=(x-m)/sdv
si_sc=x1  ## SI scaled


vc=vc+1
flowvar_t3=matrix(NA,nrow=nrow(tt),ncol=vc)
flowvar_t3[,1]=si_sc
flowvar_t3[,2:(vc)]=flowvar_t1

flowvar_t1=flowvar_t3
varname2=rep("",vc)
for (i in 2:vc){
	varname2[i]=varname[i-1]
}
varname2[1]="SI"
varname=varname2

Svarname=rep("",vc)
for (i in 1:vc){
	Svarname[i]=paste(varname[i],"_Sc",sep="")
}
colnames(flowvar_t1)=Svarname

si=flowvar_t1[,1]
cov=flowvar_t1[,2]
qm=flowvar_t1[,3]
q167=flowvar_t1[,4]
flddur=flowvar_t1[,5]
p=flowvar_t1[,6]
c=flowvar_t1[,7]
m=flowvar_t1[,8]
t50=flowvar_t1[,9]
tp=flowvar_t1[,10]
q7min=flowvar_t1[,11]
q7max=flowvar_t1[,12]
r=flowvar_t1[,13]
fpeh=flowvar_t1[,14]
fpel=flowvar_t1[,15]
fpez=flowvar_t1[,16]

colnames(flowvar_t1)=Svarname


## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

pdf(file="Scatterplot.pdf",width=42,height=42)
pairs(~si+cov+qm+q167+flddur+p+c+m+t50+tp+q7min+q7max+r+fpel+fpeh+fpez,upper.panel=panel.cor)
dev.off()


######## PERFORM PRINCIPAL COMPONENT ANALYSIS ON TRANSFORMED DATA USING  function princomp ########
x=flowvar_t1
colnames(x)=varname
covx=cov(x) # Covariance Matrix for scaled transformed variables
ex=eigen(covx)
egval=ex$values #Eigen values
egvec=ex$vectors #Eigen vectors

pc.1=princomp(x) #Principal component Analysis

plot(pc.1)
biplot(pc.1)
pcloading=loadings(pc.1)
## plot(pc.1,type="lines")
biplot(pc.1)
varimax(pcloading)

require(psych)
egval
# As the number of eigenvalues greater than 1 = 4, we take nfactors=4 for varimax rotation
cor_mat=cor(x)
pc.va=principal(cor_mat,nfactors=4,rotate="varimax",scores=F)
pc_var_loading=pc.va$loadings
write.csv(pc_var_loading,file="Loadings_PC_after_Var_Rot_4.csv")
write.csv(pc_var_loading,file="loadings4.csv")

pc.va=principal(cor_mat,nfactors=5,rotate="varimax",scores=T)
pc_var_loading=pc.va$loadings
write.csv(pc_var_loading,file="Loadings_PC_after_Var_Rot_5.csv")
write.csv(pc_var_loading,file="loadings5.csv")


#### CALCULATE THE FACTORS BY MULTIPLYING loadings with original variables ###########
pc.load=read.csv("loadings5.csv",header=T)[,2:6] ######### CHANGE this if PC changes ######
pairs(~fac_sco[,1]+fac_sco[,2]+fac_sco[,3]+fac_sco[,4]+fac_sco[,5],upper.panel=panel.cor)

fac_sco=t(pc.va$weights) %*% t(flowvar_t1)
fac_sco=t(fac_sco)

rownames(fac_sco) = stnlist
write.csv((fac_sco),file="factor_score.csv")

fac_sco2 = fac_sco[,c("RC1","RC2","RC3","RC4","RC5")]
fac_sco=fac_sco2
colnames(fac_sco)=c("L","F","M","T","C")

fac_sco[which(fac_sco[,1]==min(fac_sco[,1])),]
which(fac_sco[,5]==quantile(fac_sco[,5],prob=c(1)))

factormat = matrix(NA,nrow = 5,ncol =5)
colnames(factormat) = c("Min","5%","50%","95%","Max")
rownames(factormat) = c("L","F","M","T","C")
quant=c(0,0.05,0.5,0.95,1)
for (i in 1:5){
	for (ctr in 1:5){
		factormat[i,ctr]=fac_sco[which(fac_sco[,i]==quantile(fac_sco[,i],prob=c(quant[ctr]))),i]
	}
}


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = 2)
}




