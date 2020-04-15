leap_yr_days.R = function (yr){
	if ((yr%%400 == 0 || (yr%%100 !=0 && yr%%4==0))){days=366} else {days=365}
	days
}