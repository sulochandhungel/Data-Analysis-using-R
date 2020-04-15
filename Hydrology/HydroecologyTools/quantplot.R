# Function to plot allmat
# Max, Min and Quantiles of flow ClassA  #

quantplot.r = function(MnDFlowMat,plotfile="QunatPlot.pdf",maxlim="q95",minlim="q5",meanreq=F,maintit="Flow Range")
{

#What to plot as max and min 
minind = 2
maxind = 6

if (minlim=="q0")
	{minind = 1
	elseif (minlim=="q5")
		{minind = 2
		elseif (minlim=="q25")
			{minind = 3
			elseif (minlim=="q50")
				{minind = 4
				elseif (minlim=="q75")
					{minind = 5
					elseif (minlim=="q95")
						{minind = 6
						elseif (minlim=="q100")
							{minind = 7
						}
					}
				}
			}
		}
	}
}


if (maxlim=="q0")
	{maxind = 1
	elseif (maxlim=="q5")
		{maxind = 2
		elseif (maxlim=="q25")
			{maxind = 3
			elseif (maxlim=="q50")
				{maxind = 4
				elseif (maxlim=="q75")
					{maxind = 5
					elseif (maxlim=="q95")
						{maxind = 6
						elseif (maxlim=="q100")
							{maxind = 7
						}
					}
				}
			}
		}
	}
}
if (meanreq==T) {meanind=9} else {meanind=0}

plotmat=matrix(NA,nrow=366,ncol=8)
colnames(plotmat)=c("Min","5q","25q","Median","75q","95q","Max","Mean")
for (i3 in 1:366){
	plotmat[i3,1:7]=quantile(MnDFlowMat[i3,],probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
	plotmat[i3,8]=mean(MnDFlowMat[i3,],na.rm=T)
}

## First we decide what is the max and min limit we want to plot #
maxplot = plotmat[,maxind]

#Similarly, minimum limit 
minplot = plotmat[,minind] 

pdf(plotfile,width=8,height=6)
par(mar=c(5,5,4,2)+0.1)

plot(1:366,maxplot,"l",ylim=c(0,max(maxplot)),col=9,lty=0,main=maintit,ylab="Flow (cfs)",xlab="Days in WY",font=2,cex.lab=1.5,cex.axis=1.5) 
lines(1:366,minplot,col=meanind,lty=2)


# For grey area polygon#
grey_max = plotmat[,2]
grey_min = plotmat[,6]

polyx=c(1:366,366,365:1)
polyy=c(grey_max,rev(grey_min))
polygon(x=polyx,y=polyy,col="gray")

lines(1:366,plotmat[,2],col=8)
lines(1:366,plotmat[,3],col=1)
lines(1:366,plotmat[,4],col=2,lwd=2)
lines(1:366,plotmat[,5],col=1)
lines(1:366,plotmat[,6],col=8)
#lines(1:366,plotmat[,8],col=1,lwd=2,lty=2)

dev.off()
}
