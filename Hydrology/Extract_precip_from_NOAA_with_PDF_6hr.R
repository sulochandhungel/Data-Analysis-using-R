

folder_name = "Q:/Projects/FEMA/Washington_County/400 - Technical/Phase 2/Hydrology/Springdale_Tribs/NOAA work"
input_file_name = "Input1.csv"
output_file_name = "Output_6hr.csv"

NeedPDF = TRUE # Change this if you need printed PDFs

Recur_interval_reqd = c("2yr", "10yr", "25yr", "50yr", "100yr", "500yr")
Reqd_Storm = c("6hr")


# CHOICE FOR Recurrence Interval and Required storm

# c("1yr","2yr","5yr","10yr","25yr","50yr","100yr","200yr","500yr","1000yr")
# c("5min", "10min", "15min", "30min", "60min", "2hr", "3hr", "6hr", "12hr",
#				 "24hr", "2day", "3day", "4day", "7day", "10day", "20day",
#				 "30day", "45day", "60day")


# -------------------------------------


# 2.5 Can I get NOAA Atlas 14 data through web scraping?
# NOAA Atlas 14 data for a selected location can be obtained
# through web scraping by generating the URL link with the following
# information: a) latitude (in decimal degrees; negative number
# should be entered for southern hemisphere latitude),
# b) longitude (in decimal degrees; negative number should be entered
# for western hemisphere longitude), c) type of precipitation
# ("pf" for precipitation or "rf" for rainfall), d) data type
# ("depth" or "intensity"), e) time series type ("pds" or "ams"),
# f) units of the estimates ("english" or "metric").

# For example, if PDS-based estimates in inches are needed at
# 37.4000°N, 119.2000°W, the following links should be used:
# https://hdsc.nws.noaa.gov/cgi-bin/hdsc/
# new/cgi_readH5.py?lat=37.4000&lon=-119.2000&type=pf&data=depth&units=english&series=pds


get_NOAA_data = function(lat_, lon_,
				 reqd_recint = Recur_interval_reqd,
				 reqd_storm = Reqd_Storm)
{	
#install.packages("pagedown")
library("pagedown")
			
#lat_ = 37.45517	
#lon_= -113.84851
print (paste("Lat =", lat_, sep = ""))
print (paste("Lon =", lon_, sep = ""))
print ("")

link_ = paste("https://hdsc.nws.noaa.gov/cgi-bin/hdsc/new/cgi_readH5.py?lat=",
			 lat_,
			 "&lon=",
			 lon_,
			 "&type=pf&data=depth&units=english&series=pds")





tt = readLines(url(link_))
quant_ind = grep("quantiles =", tt)
quants_str = tt[quant_ind]
strsplit1 = strsplit(quants_str, "quantiles =")[[1]][2]
strsplit2 = strsplit(strsplit1, "\\[")[[1]]
strsplit3 = unlist(strsplit(strsplit2, "]"))[seq(2,38,2)]
res_mat = matrix(NA, nrow = 19, ncol = 10)
for (i in 1:19){
	x = strsplit3[i]
	res_mat[i,]= as.numeric(unlist(strsplit(unlist(strsplit(x, ",")),"'"))[seq(2, 20,2)])
}

colnames(res_mat) = c("1yr","2yr","5yr","10yr","25yr","50yr","100yr","200yr","500yr","1000yr")
rownames(res_mat) = c("5min", "10min", "15min", "30min", "60min", "2hr", "3hr", "6hr", "12hr",
				 "24hr", "2day", "3day", "4day", "7day", "10day", "20day",
				 "30day", "45day", "60day")

res_mat_df = data.frame(res_mat)

# reqd_recint = c("2yr", "10yr", "25yr", "50yr", "100yr", "500yr")
# reqd_storm = c("24hr")

ans_mat = matrix(NA, ncol = length(reqd_recint), nrow = length(reqd_storm))
colnames(ans_mat) = reqd_recint
rownames(ans_mat) = reqd_storm

for (i in 1:length(rownames(ans_mat))){
	for (j in 1:length(colnames(ans_mat))){
		ind_row = which(rownames(res_mat) == rownames(ans_mat)[i])
		ind_col = which(colnames(res_mat) == colnames(ans_mat)[j])
		ans_mat[i,j] = res_mat[ind_row, ind_col]
	}
}

ans = list(ans_mat, res_mat)
return (ans)



}

input_file = paste(folder_name,"/",input_file_name, sep = "")
output_file = paste(folder_name,"/",output_file_name, sep = "")

inp = read.csv(input_file)
wsheds = levels(inp$Watershed)[inp$Watershed]

for (i in 1:nrow(inp)){
	wshed_name = wsheds[i]
	lat_ = inp$Latitude[i]
	lon_ = inp$Longitude[i]
	
	if (i == 1){
		ans = get_NOAA_data(lat_, lon_)[[1]]
	} else {
		ans = rbind(ans, get_NOAA_data(lat_, lon_)[[1]])
	}
	
	if (NeedPDF){
		webp_2print = paste("https://hdsc.nws.noaa.gov/hdsc/pfds/pfds_printpage.html?lat=",
					lat_,
					"&lon=",
					lon_,
					"&data=depth&units=english&series=pds", sep = "")
		pdf_name = paste(folder_name, "/pdf/", wshed_name,".pdf", sep="")
		print (paste("Saving ... ", wshed_name, ".pdf - (Wait 15 seconds)", sep =""))
		chrome_print(webp_2print, output = pdf_name, wait = 15, format = "pdf")
		print (paste(wshed_name, ".pdf SAVED!", sep = ""))
	}
}
res_df = cbind(inp, ans)
write.csv(res_df, output_file)

