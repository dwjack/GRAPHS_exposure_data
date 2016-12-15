require(plyr)
require(zoo)

# read-in DailyPM
DailyPM_Date = readRDS("/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/MicroPEM_Data/DailyPM.rds")   # read-in DailyPM
filepath = "/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/MicroPEM_Data/MicroPEM_Dataset/"
compliance_threshold = 0.01 # threshold for wearing compliance      
window_width = 10   # if no movement during the window (x minute centered in current minute), then the compliance in current minute is 0.

for (k in 1:nrow(DailyPM_Date)) {
  Data1 = convertOutput(DailyPM_Date$participantID[k])  
  Data2 = Data1$measures
  Data4 = DailyPM_Date[k,]
  
  Data3 = Data2[Data2$timeDate>=(Data4$starttime_new)&Data2$timeDate<=(Data4$endtime_new),]    #extract data during the measurement time   
  Data5 = Data3[!is.na(Data3$nephelometer),]                                      #drop rows without nephelometer reading
  Data6 = Data5[Data5$relativeHumidity>0 & Data5$relativeHumidity<100,]           #drop measurement with RH <=0 or RH>=100
  
  # HEPA correction
  if(!is.na(Data4$HEPASt) & !is.na(Data4$HEPAEnd)) Data6$nephelometer_corr = Data6$nephelometer - seq(Data4$HEPASt, Data4$HEPAEnd, (Data4$HEPAEnd-Data4$HEPASt)/(length(Data6$nephelometer)-1))
  if(!is.na(Data4$HEPASt) & is.na(Data4$HEPAEnd)) Data6$nephelometer_corr = Data6$nephelometer - Data4$HEPASt
  if(is.na(Data4$HEPASt) & !is.na(Data4$HEPAEnd)) Data6$nephelometer_corr = Data6$nephelometer - Data4$HEPAEnd
  if(is.na(Data4$HEPASt) & is.na(Data4$HEPAEnd)) Data6$nephelometer_corr = Data6$nephelometer
  
  Data6$unique_min <- floor_date(Data6$timeDate, unit = "minute")          #minute nephelometer data
  Data7 = ddply(Data6, .(unique_min), summarise,
                nephelometer_RH_min = mean(nephelometer), 
                nephelometer_HEPA_min = mean(nephelometer_corr),
                X.axis_mean = round(mean(as.numeric(xAxis), na.rm=TRUE), digits = 4), 
                Y.axis_mean = round(mean(as.numeric(yAxis), na.rm=TRUE), digits = 4), 
                Z.axis_mean = round(mean(as.numeric(zAxis), na.rm=TRUE), digits = 4), 
                Vector.Sum.Composite_mean = round(mean(as.numeric(vectorSum), na.rm=TRUE), digits = 4))
  
  Data7$nephelometer_FINAL_min = Data7$nephelometer_HEPA_min*Data4$CF_new
  
  Data7$sd_composite <- round(rollapply(Data7$Vector.Sum.Composite_mean, width=window_width,  FUN = sd, align = "right", na.rm = TRUE, fill = NA), digits=3)
  Data7$sd_X.axis <- round(rollapply(Data7$X.axis_mean, width=window_width,  FUN = sd, align = "right", na.rm = TRUE, fill = NA), digits=3)
  Data7$sd_Y.axis <- round(rollapply(Data7$Y.axis_mean, width=window_width,  FUN = sd, align = "right", na.rm = TRUE, fill = NA), digits=3)
  Data7$sd_Z.axis <- round(rollapply(Data7$Z.axis_mean, width=window_width,  FUN = sd, align = "right", na.rm = TRUE, fill = NA), digits=3)
  
  Data7$sd_above_threshold = ifelse(Data7$sd_X.axis > compliance_threshold|Data7$sd_Y.axis > compliance_threshold|Data7$sd_Z.axis > compliance_threshold, 1, 0)
  
  title = paste(filepath, Data4$deviceSerial.x,"_", Data4$filterID, ".rds", sep="")
  
  saveRDS(Data7, file = title)  # save DailyPM dataset

  if(round(k/50)*50==k)               
    print(k)  
}

