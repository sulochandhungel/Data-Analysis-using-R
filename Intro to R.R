
################# SECTION 1 #######################
##
## R IS CASE-SENSITIVE!! ##
##
# Symbol used for Comments
# To copy paste a list of data from excel
flow = scan()

# See the data
flow

# See the first and last six data
head(flow)
tail(flow)

# What is the mean flow for that day?
mean(flow)

# How many flow values are there?
length(flow)

# General summary statistics of flow
summary(flow)

# Generate a box plot
boxplot(flow)

# How did flow vary within the day?
plot(1:96, flow) # only points
plot(1:96, flow, type = "l") # only line
plot(1:96, flow, type = "b") # both points and line
# Need to know more about plot?
?plot
plot(1:96, flow, type = "b", 
	main = "Instantaneous flow for \n HANGMAN CREEK AT SPOKANE, WA",
	xlab = "Time (15-min interval)",
	ylab = "Flow (cfs)",
	lwd = 2,
	col = "red")
grid()

# Plot only the first 6 hours data
6*(60/15) 
plot(1:24, flow[1:24], type = "b")

# Plot data from 2nd hour to 10th hour
2*(60/15) 
10 * (60/15)
plot(9:40, flow[9:40], type = "b")

# Let's try putting time
# Try copy and pasting the time like before for time
mytime = scan()
mytime

# Did not work
# Let's try using a different method
# Reading from a csv file (Comma-sepearated values)
#

### TRY PRACTICE 1 ###











################## SECTION 2 #########################

# Check the current directory
getwd()

# Change to the directory where file is located
setwd("C:/Users/Sulochan/Google Drive/R_for_Surface_Water_Quality modeling")
# Make sure that "\" are replaced either by "\\" or "/"

# Check the current directory
getwd()

flow_data = read.csv("Hangman_Creek_flow_15min_2007_10_01.csv",header=T)
head(flow_data) # the dots represent special characters
str(flow_data) 	# Shows structure of the variable
			# Shows what the "flow_data" is -> data frame
			# Shows what each individual columns are:
			# Time -> Factors
			# Flow..cfs -> Integers

## What are factors?
# Factors are "category-type" data
# For efficiency, R can use these types of data as factors
# rather than using them as texts

# e.g.Gender -> Male or Female (1,2)
#	Race -> White, Hispanic, Black (1,2,3)
#	Site Qualifiers -> Active sites or Inactive sites (1,2)
#	Data Qualifiers -> Checked data, Estimated from model, Measured, Final (1,2,3,4)

# R uses factor/text as something it does not understand
# Here R did not understand time so it stored it as factor
# We need to make R understand this is time and time is associated with a date

levels(flow_data$Time)
time_str = levels(flow_data$Time)[flow_data$Time] # Time converted as string or character

# Now lets attach date to it
date_str = "2007-10-01"
dt_str = paste(date_str, time_str) #Concatenation of strings


# Now lets convert string to date

dt = strptime(dt_str, "%Y-%m-%d %H:%M", tz = "US/Pacific")


# This allows us to do a lot more with time or dates
plot(dt,flow_data$Flow_cfs,"b") # Notice the time axis at bottom


# Lets plot for first 6 hours
st_time = dt[1]
end_time = st_time + (6*60*60) #Add seconds

ind = which(dt>=st_time & dt<=end_time)
plot(dt[ind],flow_data$Flow_cfs[ind],"b")


# Let's plot between 3:30 AM to 5:45PM (17:40)
st_time = strptime("2007-10-01 03:30", "%Y-%m-%d %H:%M", tz = "US/Pacific")
end_time = strptime("2007-10-01 17:40", "%Y-%m-%d %H:%M", tz = "US/Pacific")

ind = which(dt>=st_time & dt<=end_time)
plot(dt[ind], flow_data$Flow_cfs[ind], "b", xaxt = "n")
axis.POSIXct(1, at = seq(st_time, end_time,by=2*60*60), format="%H:%M")











########### SECTION 3 ################


# Let's apply this for instantaneous flow data from USGS
# Download data using steps from Presentation

flow_txt = read.table("12424000.txt", skip = 29)
head(flow_txt)

my_date = flow_txt$V3
date_str = levels(my_date)[my_date]

my_time = flow_txt$V4
time_str = levels(my_time)[my_time]

tz_str = levels(flow_txt$V5)[flow_txt$V5]

dt_str = paste(date_str, time_str)
my_dt = strptime(dt_str, "%Y-%m-%d %H:%M", tz = "US/Pacific")

# Time zones recognized by R
# https://en.wikipedia.org/wiki/List_of_tz_database_time_zones

my_flow = flow_txt$V6
flow = as.numeric(levels(my_flow)[my_flow])

plot(my_dt, flow,"l")

# Plot with axis interval 1 year
plot(my_dt, flow,"l",xaxt = "n")
axis.POSIXct(1, at = seq(my_dt[1], tail(my_dt,1)[1],by="years"), format="%Y/%m")

# Plot with axis interval 1 year starting from January
plot(my_dt, flow,"l",xaxt = "n")
st_time1 = strptime("2007-01-01 00:00", "%Y-%m-%d %H:%M", tz = "US/Pacific")
end_time1 = strptime("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "US/Pacific")
axis.POSIXct(1, at = seq(st_time1,end_time1,by="years"), format ="%Y/%m")


## Plot data for 2010
st_time = strptime("2010-01-01 00:00", "%Y-%m-%d %H:%M", tz = "US/Pacific")
end_time = strptime("2011-01-01 00:00", "%Y-%m-%d %H:%M", tz = "US/Pacific")

ind = which(my_dt>=st_time & my_dt<end_time)
plot(my_dt[ind], flow[ind], "l")

# Plot with axis interval 1 month
plot(my_dt[ind], flow[ind], "l", xaxt = "n")
axis.POSIXct(1, at = seq(st_time, end_time-1,by="months"), format="%b/%d")

## Plot data for 2010
plot(my_dt[ind], flow[ind], "l", xaxt = "n")
st_time1 = strptime("2010-01-01 00:00", "%Y-%m-%d %H:%M", tz = "US/Pacific")
end_time1 = strptime("2011-01-01 00:00", "%Y-%m-%d %H:%M", tz = "US/Pacific")
axis.POSIXct(1, at = seq(st_time1,end_time1,by="months"), tick = TRUE, labels = FALSE)
axis.POSIXct(1, at = seq(st_time1 + (15*24*60*60) ,end_time1+(15*24*60*60),by="months"), format ="%b", labels=TRUE, tick = FALSE)


# Add 2010 plot to the full time series plot
plot(my_dt, flow,"l",xaxt = "n")
axis.POSIXct(1, at = seq(my_dt[1], tail(my_dt,1)[1],by="years"), format="%Y/%m")
lines(my_dt[ind], flow[ind], "l", axis = F,col=2,lwd=2)


# Aggregate to monthly data
st_datetime = my_dt[1]
en_datetime = tail(my_dt,1)[1]
st_datetime = strptime("2007-10-01 00:00", "%Y-%m-%d %H:%M", tz = "US/Pacific")
en_datetime = strptime("2008-10-01 00:00", "%Y-%m-%d %H:%M", tz = "US/Pacific")


reqd_seq = seq(from = st_datetime, to = en_datetime, by = "months")
n = length(reqd_seq)-1
reqd_flow = rep(NA,n)
for (i in 1:n){
	ind = which(my_dt>=reqd_seq[i] & my_dt<reqd_seq[i+1])
	print (i)
	reqd_flow[i] =  mean(flow[ind])
}

ind = which(my_dt>=st_datetime & my_dt<en_datetime)
plot(my_dt[ind], flow[ind], "l", xlab = "Months", ylab = "flow (cfs)")
lines(reqd_seq[1:12], reqd_flow, "b",col=2, lwd = 2)
grid()



# THIS IS THE QUICKEST WAY FOR A REGULAR SEQUENCE BUT THE SEQUENCE SOMETIMES IS NOT 
# REGULAR SO IT NEEDS TO BE DONE IN A WAY WHICH TAKES A LONGER TIME
reqd_flow = c()
for (i in 1:(length(flow)-3)){
	reqd_flow = c(reqd_flow,mean(flow[i:(i+3)]))
	print (i)
}




############# SECTION 4 ################

wqdata = read.csv("56A070.csv",header=T)
var_names = colnames(wqdata)

# Metadata on what the variables and identifiers mean
# https://fortress.wa.gov/ecy/eap/riverwq/parameters_ref.html

grepl("\\QX.\\E",var_names,ignore.case=FALSE) # \\Q and \\E is used to find special characters

var_names[grepl("\\QX.\\E",var_names,ignore.case=FALSE)]=""
var_names

var_name_w_sp = var_names[grepl("\\Q.\\E",var_names,ignore.case=FALSE)] #Find all variables with space as "."
var_name_w_sp

var_names[grepl("\\Q.\\E",var_names,ignore.case=FALSE)] = gsub("\\Q.\\E","",var_name_w_sp)
var_names

seq(4,length(var_names),2)
var_names[seq(4,length(var_names),2)] = paste(var_names[seq(4,length(var_names),2)-1],"_dq",sep="")

wqdata = read.csv("56A070.csv",header=T,skip=1,check.names=F)
var_units = colnames(wqdata)
var_units1 = gsub("\\(|\\)","",var_units) #gsub command replaces one character with another
var_units1							# here we're removing all special characters like "(" or ")" or "#" or "/"
							# Special characters need to be enclosed in "\\" 
var_units2 = gsub("\\/","_per_",var_units1) 
var_units2
	
var_units3 = gsub("\\#","",var_units2)
var_units3				

var_units = var_units3
var_units


wqdata = read.csv("56A070.csv",header=F,skip=2)
head(wqdata)
str(wqdata)

colnames(wqdata) = var_names
str(wqdata)

wqdate_str = levels(wqdata$date)[wqdata$date]
wqtime_str = levels(wqdata$time)[wqdata$time]
wqdt_str = paste(wqdate_str,wqtime_str)
wqdt = strptime(wqdt_str, "%m/%d/%Y %H:%M", tz = "US/Pacific")
head(wqdt)

## Let's plot dissolved oxygen
DO = wqdata$OXYGEN
str(DO)
plot(wqdt, DO, type="b")

# Find units associated with oxygen
which(var_names == "OXYGEN")
var_units[15]
plot(wqdt, DO, type="b", lwd =2, pch = 16, xlab = "Date", ylab = "Dissolved Oxygen (mg/L)")

# Let's identify which measurements were flagged
DO_flag = levels(wqdata$OXYGEN_dq)[wqdata$OXYGEN_dq]
ind_flagged = which(DO_flag != "")
points(wqdt[ind_flagged], DO[ind_flagged], cex=2, lwd =2, col="red")

# Remove the points which are flagged
DO_clean = DO
DO_clean[ind_flagged] = NA
plot(wqdt, DO_clean, type="b", lwd =2, pch = 16, xlab = "Date", ylab = "Dissolved Oxygen (mg/L)")

# Lets put the DO standard criteria
# DO criteria is it should not go below 8 mg/L
abline(h=8,lwd=2,col="blue",lty=2)

#Lets try to find how many observations were below the 8mg/L and find the dates
DO_ind_lt_8 = which(DO<8)
DO[DO_ind_lt_8]
wqdt[DO_ind_lt_8]

#Lets export this data into csv
DO_lt_8 = data.frame(wqdt[DO_ind_lt_8],DO[DO_ind_lt_8])
colnames(DO_lt_8) = c("Date", "DO (mg/L)")
write.csv(DO_lt_8, file="DO_lt_8.csv")














############# SECTION 5 ################


# Get only the measured data out, convert it into numeric and put it in a matrix
meas_df = wqdata[,seq(3,36,2)]
meas = matrix(NA, nrow = dim(meas_df)[1], ncol = dim(meas_df)[2])
for (i in 1:dim(meas_df)[2]){
	if (is.factor(meas_df[,i])){
		x = meas_df[,i]
		meas[,i] = as.numeric(levels(x)[x])
	} else {
		meas[,i] = as.numeric(meas_df[,i])
	}
}
colnames(meas) = var_names[seq(3,36,2)]
boxplot(meas, cex.axis=0.7)

# Let's scale to which which variable has the most variability

sc_meas = matrix(NA, nrow = dim(meas)[1], ncol = dim(meas)[2])
for (i in 1:dim(meas)[2]){
	if (length(meas[,i]) != length(is.na(meas[i]))){
		sc_meas[,i] = meas[,i]/max(meas[,i],na.rm=T)
	}
}
colnames(sc_meas) = var_names[seq(3,36,2)]
boxplot(sc_meas,cex.axis = 0.5)

# What is the maximum and minimum value it used?
# How many non-NA data points did it use?
for (i in 1:dim(meas)[2]){
	text(i,-0.05,min(meas[,i],na.rm=T),cex=0.7)
	text(i,1.03,max(meas[,i],na.rm=T),cex=0.7)
	mtext(length(which(is.na(meas[,i]))),side = 1,at=i, line=2,cex=0.7,)
}

# Let's see which variables have criteria to be fulfilled but did not
# FC < 200
# Oxygen >8
#  6 > ph < 8.5
# Temp < 18

which(colnames(meas) == "FC")
colnames(meas)[c(2, 7, 8, 11)]

points(2,200/max(meas[,2],na.rm=T),lwd=3,col="red")
points(7,8/max(meas[,7],na.rm=T),lwd=3,col="blue")
points(8,6/max(meas[,8],na.rm=T),lwd=3,col="blue")
points(8,8.5/max(meas[,8],na.rm=T),lwd=3,col="red")
points(11,18.5/max(meas[,11],na.rm=T),lwd=3,col="red")


# There was a variable which did not have any usable values 
# Let's remove that entire variable 
ind_allNA = which(colnames(meas)=="TP_PInLine")
meas1 = meas[,-13]

# Pairwise scatter plot
pairs(meas1)
?pairs

# Here is a function in example which we can use
# We had to change it to also use values with NA in the "cor" function
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y,use = "na.or.complete")) # This has been edited to get cor values from vectors with NA values
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(meas1,  upper.panel = panel.cor)



# Get output in a pdf
?pdf
pdf("Corr_plot.pdf")
pairs(meas1, upper.panel = panel.cor)
dev.off()






############# SECTION 6 ###############
# Find the rate of reaction

times = c(0,100,150,200,250,300,350)
par_press = c(450,350,284,230,193,170,150,130)

plot(times, par_press,type="b")
plot(times, log(par_press),lwd =2, type="b", xlab="Time (seconds)", ylab = "Log of Par. Pressure")


ln_par_press = log(par_press)
lm.r = lm(ln_par_press~times)
abline(lm.r, lty =2)
grid()

lm.r
str(lm.r)
summ = summary(lm.r)
r_sq = signif(summ$r.squared, 3)

slp = signif((summ$coefficients)[2,1], 3)
intr = signif((summ$coefficients)[1,1], 3)
eqn = paste("ln P = ", intr, slp,"* t")  

text(300, 6, eqn)
text(300, 5.8, paste("R-sq = ", r_sq))
text(300, 5.9, paste("k = ", abs(slp), "per sec"))


















########### PRACTICE 1 #####################


# Aggregate this data to hourly data
?aggregate
hrs = as.numeric(format(dt,"%H"))
hrs_asFac = as.factor(hrs)
ans =aggregate(flow_data$Flow_cfs, by=list(hrs_asFac), FUN="mean")
str(ans)
hr_flow = ans$x

st_time = strptime("2007-10-01 00:00", "%Y-%m-%d %H:%M", tz = "US/Pacific")
end_time = strptime("2007-10-01 23:00", "%Y-%m-%d %H:%M", tz = "US/Pacific")
dt = seq(from = st_time, by = 60*60/2, to=end_time)
lines(dt, hr_flow,col=2,"b")

my_date = as.Date("2007-10-01","%Y-%m-%d")
my_date
str(my_date)
?as.Date #Look more for examples


# Its 15-min interval data, lets try to convert it to hourly data
#1st hour = 1st 15-minute mark
flow[1:4]
mean(flow[1:4])

#2nd hour = 5th 15-minute mark
flow[5:8]
mean(flow[5:8])

# ....
# 23rd hour = 93rd 15-minute mark
flow[93:96]
mean(flow[93:96])

seq(1,93,4) #Create sequence from 1 to 93 with interval of 4

hr_flow = c() 	#Empty vector to store hourly data
for (i in seq(1,93,4)){
	print (i)
	each_hr_flow = flow[i:(i+3)]
	print (each_hr_flow)
	hr_flow = c(hr_flow, mean(each_hr_flow)) #Adding mean of flows to the empty vector
}

# plot both hourly and daily data
plot(1:96, flow, type="b")
# To plot in an existing plot, use lines
#lines(1:24,hr_flow,type="b",col=2) # Why would this not work?
lines(seq(3,95,4),hr_flow,type="b",col=2,lwd=2)

################ END PRACTICE 1 #########################




