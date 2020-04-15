getData=function(stano)
{
if (nchar(stano)<8) {stano=paste("0",stano,sep="")} 

chk_ctr=23
chk_char=""
while (chk_char!="USGS") {
	junk_arr1=read.table(file=as.character(stano),skip=(chk_ctr),fill=T)
	chk_char=as.character(junk_arr1[1,1])
	chk_ctr=chk_ctr+1
}
chk_ctr=chk_ctr-1

data=read.table(file=as.character(stano),skip=chk_ctr,flush=TRUE,comment.char="#",fill=T)
getData=data
}