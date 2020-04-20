setwd("C:/Users/Sulochan/Sulochan/Sulochan_Backup/Work/Report_Jan_3_2011/Final_GAGES_set/Trends_1965_2010/PCA_with_wards")
tt=read.csv("factor_score.csv")
tt = tt[,c("X","RC1","RC2","RC3","RC4","RC5")]
fac_sco=tt[,2:6]
colnames(fac_sco)=c("L","F","M","T","C")
d=dist(fac_sco,method="euclidean")

fit=hclust((d^2),method="ward")
jpeg(file="plot_fit.jpg",width=18,height=110,units="in",res=200)
pdf(file="plot.fit.pdf",width=98,height=8)
require(graphics)
require(utils)
library(graphics)
library(utils)

fith=as.dendrogram(fit)
plot(fith,horiz=T,cex=0.1,col=0)
groups3 <- cutree(fit, k=3) # cut tree into 3 clusters
groups4 <- cutree(fit, k=4) # cut tree into 4 clusters
groups5 <- cutree(fit, k=5) # cut tree into 5 clusters
groups6 <- cutree(fit, k=6) # cut tree into 6 clusters
groups7 <- cutree(fit, k=7) # cut tree into 7 clusters
groups8 <- cutree(fit, k=8) # cut tree into 8 clusters

# draw dendogram with red borders around the 5 clusters 
#rect.hclust(fit, k=8, border="red")
dev.off()


# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram

op = par(bg="#EFEFEF")

### 3 Classes ####

pdf(file="plot3.fit.pdf",width=167,height=14)

groups3lett=groups3
groups3lett[which(groups3==1)]="A"
groups3lett[which(groups3==2)]="C"
groups3lett[which(groups3==3)]="B"

fit$labels=groups3lett

A2Rplot(fit, k=3, boxes = FALSE, col.up = "black", col.down = c("blue4","magenta","olivedrab"),show.labels=T,horiz=T,main="")
dev.off()


### 4 Classes ####

pdf(file="plot4.fit.pdf",width=167,height=14)

groups4lett=groups4
groups4lett[which(groups4==1)]="A"
groups4lett[which(groups4==2)]="C2"
groups4lett[which(groups4==3)]="C1"
groups4lett[which(groups4==4)]="B"

fit$labels=groups4lett

A2Rplot(fit, k=4, boxes = FALSE, col.up = "black", col.down = c("blue4","magenta","olivedrab","olivedrab1"),show.labels=T,horiz=T,main="")
dev.off()


### 5 Classes ####

pdf(file="plot5.fit.pdf",width=167,height=14)

groups5lett=groups5
groups5lett[which(groups5==1)]="A"
groups5lett[which(groups5==2)]="C22"
groups5lett[which(groups5==3)]="C21"
groups5lett[which(groups5==4)]="C1"
groups5lett[which(groups5==5)]="B"

fit$labels=groups5lett

fit$labels=groups5lett
A2Rplot(fit, k=5, boxes = FALSE, col.up = "black", col.down = c("blue4","magenta","olivedrab","olivedrab3","olivedrab1"),show.labels=T,horiz=T,main="")
dev.off()

### 6 Classes ####

pdf(file="plot6.fit.pdf",width=167,height=14)

groups6lett=groups6
groups6lett[which(groups6==1)]="A"
groups6lett[which(groups6==2)]="C22"
groups6lett[which(groups6==3)]="C21"
groups6lett[which(groups6==4)]="C1"
groups6lett[which(groups6==5)]="B2"
groups6lett[which(groups6==6)]="B1"

fit$labels=groups6lett

A2Rplot(fit, k=6, boxes = FALSE, col.up = "black", col.down = c("blue4","magenta","red","olivedrab","olivedrab3","olivedrab1"),show.labels=T,horiz=T,main="")
dev.off()

### 7 Classes ####

pdf(file="plot7.fit.pdf",width=167,height=14)

groups7lett=groups7
groups7lett[which(groups7==1)]="A2"
groups7lett[which(groups7==2)]="C22"
groups7lett[which(groups7==3)]="C21"
groups7lett[which(groups7==4)]="A1"
groups7lett[which(groups7==5)]="C1"
groups7lett[which(groups7==6)]="B2"
groups7lett[which(groups7==7)]="B1"

fit$labels=groups7lett
A2Rplot(fit, k=7, boxes = FALSE, col.up = "black", col.down = c("blue4","lightskyblue","magenta","red","olivedrab","olivedrab3","olivedrab1"),show.labels=T,horiz=T,main="")
dev.off()

### 8 Classes ####

pdf(file="plot8.fit.pdf",width=167,height=14)

groups8lett=groups8
groups8lett[which(groups8==1)]="A2"
groups8lett[which(groups8==2)]="C22"
groups8lett[which(groups8==3)]="C21"
groups8lett[which(groups8==4)]="A1"
groups8lett[which(groups8==5)]="C1"
groups8lett[which(groups8==6)]="B21"
groups8lett[which(groups8==7)]="B1"
groups8lett[which(groups8==8)]="B22"

fit$labels=groups8lett
A2Rplot(fit, k=8, boxes = FALSE, col.up = "black", col.down = c("blue4","lightskyblue","magenta","red","darkorange","olivedrab","olivedrab3","olivedrab1"),show.labels=T,horiz=T,main="")
dev.off()

par(op)






setwd("C:/Users/Sulochan/Sulochan/Sulochan_Backup/Work/Report_Jan_3_2011/Final_GAGES_set/Trends_1965_2010/PCA_with_wards")
tt=read.csv("Vars_for_PCA.csv",header=T)
stnlist=as.vector(tt[,2])
setwd("C:/Users/Sulochan/Sulochan/Sulochan_Backup/Work/Report_Jan_3_2011/Final_GAGES_set/Trends_1965_2010/PCA_with_wards/Wards")
nsta=length(stnlist)


groupmat3=as.matrix(groups3lett)
groupmat4=as.matrix(groups4lett)
groupmat5=as.matrix(groups5lett)
groupmat6=as.matrix(groups6lett)
groupmat7=as.matrix(groups7lett)
groupmat8=as.matrix(groups8lett)


rownames(groupmat3)=stnlist
rownames(groupmat4)=stnlist
rownames(groupmat5)=stnlist
rownames(groupmat6)=stnlist
rownames(groupmat7)=stnlist
rownames(groupmat8)=stnlist

write.csv(as.matrix(groupmat3),file="wards3.csv")
write.csv(as.matrix(groupmat4),file="wards4.csv")
write.csv(as.matrix(groupmat5),file="wards5.csv")
write.csv(as.matrix(groupmat6),file="wards6.csv")
write.csv(as.matrix(groupmat7),file="wards7.csv")
write.csv(as.matrix(groupmat8),file="wards8.csv")

allclass=matrix(NA,nrow=607,ncol=6)
allclass[,1]=groupmat3
allclass[,2]=groupmat4
allclass[,3]=groupmat5
allclass[,4]=groupmat6
allclass[,5]=groupmat7
allclass[,6]=groupmat8
rownames(allclass)=stnlist

colnames(allclass)=c("Class3","Class4","Class5","Class6","Class7","Class8")
write.csv(allclass,file="allclass.csv")

wardsabc=as.matrix(read.csv("allclass.csv",header=T)[,2:7])




rownames(fac_sco)=stnlist

maxy=max(fac_sco)
miny=min(fac_sco)

#maxy=10
#miny=-10

groups8 <- cutree(fit, k=8) # cut tree into 8 clusters
groups=groups8
pdf("Class8.pdf")

ind1=which(groups==1)
fac_sco[ind1,]
boxplot(fac_sco[ind1,],main="Class A2",axes=F,lwd=3,cex.main=5,ylim=c(miny,maxy))
abline(h=0,col=8)
text(4,miny,nrow(fac_sco[ind1,]),cex=3)
par(mar=c(3,4,4,2))
axis(1,at=1:5,c("L","F","M","T","C"),font=2,cex.axis=3)
axis(2,font=2,cex.axis=3)
box()



ind2=which(groups==2)
fac_sco[ind2,]
boxplot(fac_sco[ind2,],main="Class C22",axes=F,lwd=3,cex.main=5,ylim=c(miny,maxy))
abline(h=0,col=8)
text(4,miny,nrow(fac_sco[ind2,]),cex=3)
par(mar=c(3,4,4,2))
axis(1,at=1:5,c("L","F","M","T","C"),font=2,cex.axis=3)
axis(2,font=2,cex.axis=3)
box()

ind3=which(groups==3)
fac_sco[ind3,]
boxplot(fac_sco[ind3,],main="Class C21",axes=F,lwd=3,cex.main=5,ylim=c(miny,maxy))
abline(h=0,col=8)
text(4,miny,nrow(fac_sco[ind3,]),cex=3)
par(mar=c(3,4,4,2))
axis(1,at=1:5,c("L","F","M","T","C"),font=2,cex.axis=3)
axis(2,font=2,cex.axis=3)
box()



ind4=which(groups==4)
fac_sco[ind4,]
boxplot(fac_sco[ind4,],main="Class A1",axes=F,lwd=3,cex.main=5,ylim=c(miny,maxy))
abline(h=0,col=8)
text(4,miny,nrow(fac_sco[ind4,]),cex=3)
par(mar=c(3,4,4,2))
axis(1,at=1:5,c("L","F","M","T","C"),font=2,cex.axis=3)
axis(2,font=2,cex.axis=3)
box()



ind5=which(groups==5)
fac_sco[ind5,]
boxplot(fac_sco[ind5,],main="Class C1",axes=F,lwd=3,cex.main=5,ylim=c(miny,maxy))
abline(h=0,col=8)
text(4,miny,nrow(fac_sco[ind5,]),cex=3)
par(mar=c(3,4,4,2))
axis(1,at=1:5,c("L","F","M","T","C"),font=2,cex.axis=3)
axis(2,font=2,cex.axis=3)
box()



ind6=which(groups==6)
fac_sco[ind6,]
boxplot(fac_sco[ind6,],main="Class B21",axes=F,lwd=3,cex.main=5,ylim=c(miny,maxy))
abline(h=0,col=8)
text(4,miny,nrow(fac_sco[ind6,]),cex=3)
par(mar=c(3,4,4,2))
axis(1,at=1:5,c("L","F","M","T","C"),font=2,cex.axis=3)
axis(2,font=2,cex.axis=3)
box()



ind7=which(groups==7)
fac_sco[ind7,]
boxplot(fac_sco[ind7,],main="Class B1",axes=F,lwd=3,cex.main=5,ylim=c(miny,maxy))
abline(h=0,col=8)
text(4,miny,nrow(fac_sco[ind7,]),cex=3)
par(mar=c(3,4,4,2))
axis(1,at=1:5,c("L","F","M","T","C"),font=2,cex.axis=3)
axis(2,font=2,cex.axis=3)
box()



ind8=which(groups==8)
fac_sco[ind8,]
boxplot(fac_sco[ind8,],main="Class B22",axes=F,lwd=3,cex.main=5,ylim=c(miny,maxy))
abline(h=0,col=8)
text(4,miny,nrow(fac_sco[ind8,]),cex=3)
par(mar=c(3,4,4,2))
axis(1,at=1:5,c("L","F","M","T","C"),font=2,cex.axis=3)
axis(2,font=2,cex.axis=3)
box()

dev.off()



classlst=c("A2","C22","C21","A1","C1","B21","B1","B22")


indA1 = ind4
indA2 = ind1
indB1 = ind7
indB21 = ind6
indB22 = ind8
indC1 = ind5
indC21 = ind3
indC22 = ind2

classList = c("A1","A2","B1","B21","B22","C1","C21","C22")



groups3 <- cutree(fit, k=3) # cut tree into 3 clusters
groups=groups3
pdf("Class3.pdf")

ind1=which(groups==1)
fac_sco[ind1,]
boxplot(fac_sco[ind1,],main="Class A",axes=F,lwd=3,cex.main=5,ylim=c(miny,maxy))
abline(h=0,col=8)
text(4,miny,nrow(fac_sco[ind1,]),cex=3)
par(mar=c(3,4,4,2))
axis(1,at=1:5,c("L","F","M","T","C"),font=2,cex.axis=3)
axis(2,font=2,cex.axis=3)
box()



ind2=which(groups==2)
fac_sco[ind2,]
boxplot(fac_sco[ind2,],main="Class C",axes=F,lwd=3,cex.main=5,ylim=c(miny,maxy))
abline(h=0,col=8)
text(4,miny,nrow(fac_sco[ind2,]),cex=3)
par(mar=c(3,4,4,2))
axis(1,at=1:5,c("L","F","M","T","C"),font=2,cex.axis=3)
axis(2,font=2,cex.axis=3)
box()

ind3=which(groups==3)
fac_sco[ind3,]
boxplot(fac_sco[ind3,],main="Class B",axes=F,lwd=3,cex.main=5,ylim=c(miny,maxy))
abline(h=0,col=8)
text(4,miny,nrow(fac_sco[ind3,]),cex=3)
par(mar=c(3,4,4,2))
axis(1,at=1:5,c("L","F","M","T","C"),font=2,cex.axis=3)
axis(2,font=2,cex.axis=3)
box()

dev.off()






pdf("class_bxplots.pdf")

Smat=matrix(NA,nrow=nsta,ncol=8)
Smat[indA1,1]=fac_sco[indA1,1]
Smat[indA2,2]=fac_sco[indA2,1]
Smat[indB1,3]=fac_sco[indB1,1]
Smat[indB21,4]=fac_sco[indB21,1]
Smat[indB22,5]=fac_sco[indB22,1]
Smat[indC1,6]=fac_sco[indC1,1]
Smat[indC21,7]=fac_sco[indC21,1]
Smat[indC22,8]=fac_sco[indC22,1]

boxplot(Smat,main="Low Flow (L)",axes=F,lwd=3,cex.main=3)
for (i in 1:8){abline(v=i+0.5,col=8)}
abline(h=0,col=8)
par(mar=c(3,4,4,2))
axis(1,1:8,labels=classList,font=2,cex.axis=1)
axis(2,font=2,cex.axis=1.5)
box()


Fmat=matrix(NA,nrow=nsta,ncol=8)
Fmat[indA1,1]=fac_sco[indA1,2]
Fmat[indA2,2]=fac_sco[indA2,2]
Fmat[indB1,3]=fac_sco[indB1,2]
Fmat[indB21,4]=fac_sco[indB21,2]
Fmat[indB22,5]=fac_sco[indB22,2]
Fmat[indC1,6]=fac_sco[indC1,2]
Fmat[indC21,7]=fac_sco[indC21,2]
Fmat[indC22,8]=fac_sco[indC22,2]

boxplot(Fmat,main="Flashiness (F)",axes=F,lwd=3,cex.main=3)
for (i in 1:8){abline(v=i+0.5,col=8)}
abline(h=0,col=8)
par(mar=c(3,4,4,2))
axis(1,1:8,labels=classList,font=2,cex.axis=1)
axis(2,font=2,cex.axis=1.5)
box()


Mmat=matrix(NA,nrow=nsta,ncol=8)
Mmat[indA1,1]=fac_sco[indA1,3]
Mmat[indA2,2]=fac_sco[indA2,3]
Mmat[indB1,3]=fac_sco[indB1,3]
Mmat[indB21,4]=fac_sco[indB21,3]
Mmat[indB22,5]=fac_sco[indB22,3]
Mmat[indC1,6]=fac_sco[indC1,3]
Mmat[indC21,7]=fac_sco[indC21,3]
Mmat[indC22,8]=fac_sco[indC22,3]

boxplot(Mmat,main="Magnitude (M)",axes=F,lwd=3,cex.main=3)
for (i in 1:8){abline(v=i+0.5,col=8)}
abline(h=0,col=8)
par(mar=c(3,4,4,2))
axis(1,1:8,labels=classList,font=2,cex.axis=1)
axis(2,font=2,cex.axis=1.5)
box()


Tmat=matrix(NA,nrow=nsta,ncol=8)
Tmat[indA1,1]=fac_sco[indA1,4]
Tmat[indA2,2]=fac_sco[indA2,4]
Tmat[indB1,3]=fac_sco[indB1,4]
Tmat[indB21,4]=fac_sco[indB21,4]
Tmat[indB22,5]=fac_sco[indB22,4]
Tmat[indC1,6]=fac_sco[indC1,4]
Tmat[indC21,7]=fac_sco[indC21,4]
Tmat[indC22,8]=fac_sco[indC22,4]

boxplot(Tmat,main="Timing (T)",axes=F,lwd=3,cex.main=3)
for (i in 1:8){abline(v=i+0.5,col=8)}
abline(h=0,col=8)
par(mar=c(3,4,4,2))
axis(1,1:8,labels=classList,font=2,cex.axis=1)
axis(2,font=2,cex.axis=1.5)
box()


Cmat=matrix(NA,nrow=nsta,ncol=8)
Cmat[indA1,1]=fac_sco[indA1,5]
Cmat[indA2,2]=fac_sco[indA2,5]
Cmat[indB1,3]=fac_sco[indB1,5]
Cmat[indB21,4]=fac_sco[indB21,5]
Cmat[indB22,5]=fac_sco[indB22,5]
Cmat[indC1,6]=fac_sco[indC1,5]
Cmat[indC21,7]=fac_sco[indC21,5]
Cmat[indC22,8]=fac_sco[indC22,5]

boxplot(Cmat,main="Constancy (C)",axes=F,lwd=3,cex.main=3)
for (i in 1:8){abline(v=i+0.5,col=8)}
abline(h=0,col=8)
par(mar=c(3,4,4,2))
axis(1,1:8,labels=classList,font=2,cex.axis=1)
axis(2,font=2,cex.axis=1.5)
box()


dev.off()



fit=kmeans(fac_sco,5)





