library(hydroGOF)
library(sf)
library(maptools)
#install.packages("formattable")
library(formattable)

pipes = sf::st_read("C:/Users/sulochan.dhungel/Sulochan/MCCMongolia/400-Technical/402 Hydraulic Models/Draft Design Hydraulics/Merged_27Apr_2020/Rwork/Pipes.shp")

nrmse_func <-  function(obs, pred, type = "sd") {
  
  squared_sums <- sum((obs - pred)^2, na.rm = T)
  mse <- squared_sums/length(obs)
  rmse <- sqrt(mse)
  if (type == "sd") nrmse <- rmse/sd(obs, na.rm = T)
  if (type == "mean") nrmse <- rmse/mean(obs, na.rm = T)
  if (type == "maxmin") nrmse <- rmse/ (max(obs, na.rm = T) - min(obs, na.rm = T))
  if (type == "iq") nrmse <- rmse/ (quantile(obs, 0.75, na.rm = T) - quantile(obs, 0.25, na.rm = T))
  if (!type %in% c("mean", "sd", "maxmin", "iq")) message("Wrong type!")
  nrmse <- round(nrmse, 3)
  return(nrmse)
  
}

setwd("C:/Users/sulochan.dhungel/Sulochan/MCCMongolia/400-Technical/402 Hydraulic Models/Draft Design Hydraulics/Merged_27Apr_2020/Rwork")
tt = read.csv("SCADA Element.csv")

ans_elem = c()
ans_var = c()
ans_NSE = c()
ans_RMSE = c()
ans_x = c()
ans_y = c()


mod_elems = levels(tt$Model.Element)[tt$Model.Element]
mod_elems_unq = unique(mod_elems)

pdf("model_comparison1.pdf", width = 10, height = 9 )
length(mod_elems_unq)
for (i in 1:length(mod_elems_unq)){
  layout(matrix(c(1,2,3,3), 2,2, byrow = T))
  sel_elem = mod_elems_unq[i]
  sel_row_ind = which(mod_elems == sel_elem)
  
  data_rows = tt[sel_row_ind,]
  data_labs = levels(data_rows$Label)[data_rows$Label]
  
  p_ind = which(grepl("_Pr", data_labs)==TRUE)
  #min_p_ind = which(grepl("_min_Pr", data_labs)==TRUE)
  
  f_ind = which(grepl("_Fl", data_labs)==TRUE)
  #min_f_ind = which(grepl("_min_Fl", data_labs)==TRUE)
  
  data_fields = levels(data_rows$Field)[data_rows$Field]
  
  
  # DateTime
  dt_str = levels(data_rows$Timestep)[data_rows$Timestep]
  dt1 = strptime(dt_str, "%m/%d/%Y %l:%M:%S %p")
  
  # Pr
  Pr = data_rows[p_ind,]$Model.Element.Value..Numeric.
  #Pr = data_rows[min_p_ind,]$Model.Element.Value..Numeric.
  dt = dt1[p_ind]
  #dt = dt1[min_p_ind]
  
  # Max_Pr
  MaxPr = data_rows[p_ind,]$Historical.Signal.Value..Numeric.
  
  # Min_Pr
  MinPr = data_rows[p_ind,]$Historical.Signal.Value..Numeric.
  
  if (length(Pr) > 0){
    plot(dt, Pr, 'b', lwd = 2, 
         ylim = c(min(MaxPr, MinPr, Pr, na.rm = T), max(MaxPr, MinPr, Pr, na.rm = T)),
         main = paste(sel_elem, "_Pres", sep=""),
         xlab = "Time",
         ylab = "Pressure (atm)"
    )
    lines(dt, Pr, 'l', lwd = 2)
    lines(dt, MaxPr, 'p', lwd =2, col = 'red')
    lines(dt, MinPr, 'p', lwd =2, col = 'blue')
    Avg_Pr = (MinPr + MaxPr)*0.5
    NSE_pr = NSE(Pr, Avg_Pr)
    
    lines(dt, Avg_Pr, 'p', lwd =2, col = 'green')
    lines(dt, Avg_Pr, 'l', lwd =2, col = 'green')
    grid()
    mtext(paste("NSE = ", round(NSE_pr,1), sep = ""), 3, line =0)
    legend("bottomleft", legend = c("SCADA Avg", "WGEMS"),
           lwd = 2,
           lty = c(1,1),
           col = c("green", "black"),
           text.col = c("green", "black"),
           pch = 1)
    
    
    lims_ = c(min(Avg_Pr, Pr, na.rm =T), max(Avg_Pr, Pr,na.rm =T))
    plot(Pr, Avg_Pr, xlim = lims_, ylim = lims_, lwd =2,
         main = paste(sel_elem, "_Pres", sep=""),
         xlab = "WaterGEMS Pressure (atm)",
         ylab = "Avg SCADA Pressure (atm)")
    abline(0,1, lwd = 2, lty =2)
    mtext(paste("RMSE = ",
                round((nrmse_func(Avg_Pr, Pr, type = "maxmin")),1), sep = ""),
          3, line =0)
    grid()
    
    plot(pipes$geometry, col = 'gray', main = "Location of the node")
    x_lev = data_rows[p_ind,]$X
    x_str = levels(x_lev)[x_lev]
    x_ = as.numeric(sub(",","", x_str[1], fixed = TRUE))
    
    y_lev = data_rows[p_ind,]$Y
    y_str = levels(y_lev)[y_lev]
    y_ = comma(y_str[1])
    
    points(x_, y_, lwd = 4, col = 'red')
    
    
    ans_RMSE = c(ans_RMSE, nrmse_func(Avg_Pr, Pr, type = "maxmin"))
    ans_NSE = c(ans_NSE, NSE_pr)
    ans_x = c(ans_x, x_)
    ans_y = c(ans_y, y_)
    
  } else {
    ans_NSE = c(ans_NSE, NA)
    ans_RMSE = c(ans_RMSE, NA)
    x_ = NA
    y_ = NA
    ans_x = c(ans_x, x_)
    ans_y = c(ans_y, y_)
  }  
  ans_elem = c(ans_elem, sel_elem)
  ans_var = c(ans_var, "Pressure")
  
  
  # Fl
  Fl = data_rows[f_ind,]$Model.Element.Value..Numeric.
  #Fl = data_rows[min_f_ind,]$Model.Element.Value..Numeric.
  dt = dt1[f_ind]
  #dt = dt1[min_f_ind]
  
  # Max_Fl
  MaxFl = data_rows[f_ind,]$Historical.Signal.Value..Numeric.
  
  # Min_Fl
  MinFl = data_rows[f_ind,]$Historical.Signal.Value..Numeric.
  
  if (length(Fl) > 0){
    plot(dt, Fl, 'b', lwd = 2,
         main = paste(sel_elem, "_Flow", sep=""),
         ylim = c(min(MaxFl, MinFl, Fl, na.rm = T), max(MaxFl, MinFl, Fl, na.rm = T)),
         xlab = "Time",
         ylab = "Flow (m^3/hr)")
    lines(dt, Fl, lwd =2)
    #lines(dt, MaxFl, 'p', lwd =2, col = 'red')
    #lines(dt, MinFl, 'p', lwd =2, col = 'blue')
    Avg_Fl = (MinFl + MaxFl)*0.5
    NSE_Fl = NSE(Fl, Avg_Fl)
    
    lines(dt, Avg_Fl, 'p', lwd =2, col = 'green')
    lines(dt, Avg_Fl, 'l', lwd =2, col = 'green')
    mtext(paste("NSE = ", round(NSE_Fl,1), sep = ""), 3, line =0)
    grid()
    legend("bottomleft", legend = c("SCADA Avg", "WGEMS"),
           lwd = 2,
           lty = c(1,1),
           col = c("green", "black"),
           text.col = c("green", "black"),
           pch = 1)
    
    
    lims_ = c(min(Avg_Fl, Fl, na.rm =T), max(Avg_Fl, Fl,na.rm =T))
    plot(Fl, Avg_Fl, xlim = lims_, ylim = lims_, lwd =2,
         main = paste(sel_elem, "_Flow", sep=""),
         xlab = "WaterGEMS Flow (m^3/hr)",
         ylab = "Avg SCADA Flow (m^3/hr)")
    abline(0,1, lwd = 2, lty =2)
    mtext(paste("n RMSE = ",
                round((nrmse_func(Avg_Fl,Fl,type = "maxmin")),1), sep = ""),
          3, line =0)
    grid()
    
    plot(pipes$geometry, col = 'gray', main = "")
    x_lev = data_rows[f_ind,]$X
    x_str = levels(x_lev)[x_lev]
    x_ = as.numeric(sub(",","", x_str[1], fixed = TRUE))
    
    y_lev = data_rows[f_ind,]$Y
    y_str = levels(y_lev)[y_lev]
    y_ = comma(y_str[1])
    
    points(x_, y_, lwd = 4, col = 'red')
    
    ans_x = c(ans_x, x_)
    ans_y = c(ans_y, y_)
    ans_RMSE = c(ans_RMSE, nrmse_func(Avg_Fl, Fl, type = "maxmin"))
    ans_NSE = c(ans_NSE, NSE_Fl)
  } else {
    x_ = NA
    y_ = NA
    ans_x = c(ans_x, x_)
    ans_y = c(ans_y, y_)
    ans_RMSE = c(ans_RMSE, NA)
    ans_NSE = c(ans_NSE, NA)
  }
  
  ans_elem = c(ans_elem, sel_elem)
  ans_var = c(ans_var, "Flow")

}
ans_RMSE[is.infinite(ans_RMSE)] = NA
ans_df = data.frame(ans_elem, ans_x, ans_y, ans_var, ans_NSE, ans_RMSE)
write.csv(ans_df, file = "R_Results_Model_comp_SCADA.csv")

layout(1,1)
plot(pipes$geometry, col = 'gray', main = "RMSE")
scaled_RMSE = (ans_RMSE - min(ans_RMSE, na.rm = T))*5/(max(ans_RMSE, na.rm = T) - min(ans_RMSE, na.rm = T))
points(ans_df$ans_x, ans_df$ans_y, cex = 0.1, lwd = 2)
points(ans_df$ans_x, ans_df$ans_y, cex = scaled_RMSE, pch = 13)

plot(pipes$geometry, col = 'gray', main = "NSE")
scaled_NSE = (ans_NSE - min(ans_NSE, na.rm = T))*4/(max(ans_NSE, na.rm = T) - min(ans_NSE, na.rm = T))
points(ans_df$ans_x, ans_df$ans_y, col = scaled_NSE, pch = 13)


dev.off()
