lp3ff=function(cs,T)
{
F=1/T
shp=4/(cs*cs)
if(abs(cs)<0.00001)
{ans=qnorm(F)}
else
{if(cs>0){
ans=qgamma(F,shp)*cs/2-2/cs
}
else
{
ans=qgamma(1-F,shp)*cs/2-2/cs
}
}
return(ans)
}