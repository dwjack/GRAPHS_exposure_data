## HOBO Accelerometer Data (for ECM and Lascar) and MicroPEM accelerometer data
## Compliance measured by taking the average of sd_vectorsum_squared over every 3 minutes and setting a compliance threshold depending on the logging interval. 
# For MicroPEM, select only those samples at :00 or :30 to mimic 30-second sampling, then continue as above.
# Need to process 3 ECM files separately (#20, 26, 27) as no HOBO recorded for them.
# Last modified Jan 2015

require(lubridate)
require(plyr)
require(dplyr)
require(zoo)

ECM <- paste0("ECMF", c("01", "02", "18", "19", "20", "26", "27", "28", "29", "30"))
Lascar <- paste0("ECMF",c("08", "09", "10", "16", "17", "21", "22", "23", "24", "25"))
MicroPEM <- paste0("ECMF", c("03", "04", "05", "06", "07", "11", "12", "13", "14", "15"))


# ECM pilot HOBO files: /Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/ECM_Pilot/Feasibility Pilot/HOBO Accelerometer data/
files <- list.files("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/ECM_Pilot/Feasibility Pilot/HOBO Accelerometer data/", pattern = ".csv", recursive=F, full.names=T) 
length(files) 

hobo_import <- function(file) {
# 10 s: /Users/ashlinn/Dropbox/Ghana project/ECM Pilot/HOBO accelerometer data/Hobo_10622229_Accelerometer_0.csv
# 30 s: /Users/ashlinn/Dropbox/Ghana project/ECM Pilot/HOBO accelerometer data/10622211.csv
  data <- read.csv(file, skip = 1, stringsAsFactors = FALSE)
  SN <- regmatches(names(data[11]), regexpr("106.....|107.....", names(data[11]))) # Serial number starts with 106 or 107 followed by 5 numbers. What to do with this?
  data <- data[2:11]
  names(data)[1:5] <- c("date_time", "x_accel_g", "y_accel_g", "z_accel_g", "vectorsum_g")[1:5]
  for (i in 6:10) {
    names(data)[i] <- strsplit(names(data)[i], "\\.\\.")[[1]][1]
  }
  data$SN <- SN
  data$log_interval_s <- as.numeric(difftime(data$date_time[10], data$date_time[9], units = "secs"))
  data$date_time <- dmy_hms(data$date_time, tz = "GMT")
  data$minute <- floor_date(data$date_time, "minute")
  data$centered_vectorsum_squared <- (data$vectorsum_g - mean(data$vectorsum, na.rm = TRUE))^2
  # divide by 3 minutes: this section is slow, don't know how to avoid the for loop
  mins_sequence <- seq(min(data$minute), max(data$minute),180) # 3 minute sequence
  data$three_minutes <- NA
  for (i in 1:nrow(data)) {
    value <- which.min(abs(data$minute[i] - mins_sequence))
    data$three_minutes[i] <-mins_sequence[value]
  }

  
  # average it per 3 minutes
  grouped_data <- data %>% group_by(three_minutes) %>% dplyr:: summarise(file = basename(file), SN = SN[1],log_interval_s = log_interval_s[1], mean_vectorsum_g = mean(vectorsum_g, na.rm = TRUE), sd_vectorsum_g = sd(vectorsum_g, na.rm = TRUE), sd_vectorsum_squared = sd(centered_vectorsum_squared, na.rm = TRUE))
  grouped_data$three_minutes <- as.POSIXct(grouped_data$three_minute, origin = "1970-1-1", tz = "GMT")
 
  
  
  grouped_data
}
  

hobo_stacked <- ldply(files, hobo_import, .progress = "text")   # note: no compliance info
  

stacked_files <- unique(hobo_stacked$file)


# calculate compliance

width <- 5 # setting a 15-minute (3*5) window for calculating the rolling average

 pdf(file = paste0("HOBO_accel_data_",format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 10)
par(mfrow = c(2,2))
hours_summary <- data.frame()
compliance_data <- data.frame()
for (i in 1:length(stacked_files)) {
  plotdata <- hobo_stacked[hobo_stacked$file == stacked_files[i],]
  compliance_threshold <- ifelse(plotdata$log_interval_s[1] == 30, 0.002, 0.001) # set compliance threshold to 0.002 for 30-second logging and 0.001 for 10-second logging
  plotdata$sd_above_threshold <- ifelse(plotdata$sd_vectorsum_squared > compliance_threshold, 1, 0)
  plotdata$sd_rollmean <- as.numeric(rollapply(plotdata$sd_above_threshold, width=width,  FUN = mean, align = "center", na.rm = TRUE, fill = NA)) 
  plotdata$compliance_rollmean <- ifelse(plotdata$sd_rollmean > 0, 1, 0)
  percent_compliant <- mean(plotdata$compliance_rollmean, na.rm = TRUE) # % of time
  duration <- as.numeric(difftime(max(plotdata$three_minutes), min(plotdata$three_minutes), unit = "hours"))
  hours_worn <- round(as.numeric(difftime(last(plotdata$three_minutes),first(plotdata$three_minutes), units = "hours")*percent_compliant), digits = 1)
  hours_summary[i,1] <- plotdata$file[i]
  hours_summary[i,2] <- hours_worn
  hours_summary[i,3] <- round(percent_compliant[1]*100, digits = 1)
  hours_summary[i,4] <- duration
  hours_summary[i,5] <- round((hours_worn/16)*100, digits = 1)
  
  compliance_data <- rbind(compliance_data, plotdata)
  
  studyid <- substr(hours_summary[i,1], start = regexpr("ECMF", hours_summary[i,1] ), stop = regexpr("ECMF", hours_summary[i,1] ) + 5)
  
  unit_type <- ifelse(studyid %in% ECM, "ECM", ifelse(studyid %in% Lascar, "Lascar", NA))
  
  # the plot
  plot(plotdata$three_minutes, plotdata$mean_vectorsum_g, type = "l", ylim = c(0, max(plotdata$mean_vectorsum_g)), main = paste(unit_type, studyid, "\n Compliance Threshold =", compliance_threshold, "Window =", width*3, "minutes \n Logger interval =", plotdata$log_interval_s[1], "s, Duration = ", round(duration, digits = 1), ", Hours worn =", hours_worn), cex.main = 0.8, ylab = "mean vector sum (g force)", xlab = "", xaxt = "n")
  lines(plotdata$three_minutes, plotdata$compliance_rollmean/1.5, col = "aquamarine3")
  text(x  = mean(plotdata$three_minutes), y = 0.69, labels = "<--compliant-->", cex = 0.8)
  axis.POSIXct(1, at=seq(from = floor_date(plotdata$three_minutes[1], unit = "hour"), to = ceiling_date(plotdata$three_minutes[nrow(plotdata)], unit = "hour"), by = "hour", labels=format(plotdata$three_minutes, "%h")), las = 2)
}
dev.off()




names(hours_summary) <- c("file", "hours_worn", "percent_worn", "duration_hrs", "percent_worn_16")
hours_summary$studyid <- substr(hours_summary$file, start = regexpr("ECMF", hours_summary$file ), stop = regexpr("ECMF", hours_summary$file ) + 5)
hours_summary$unit_type <- NA
hours_summary$unit_type <- ifelse(hours_summary$studyid %in% ECM, "ECM", ifelse(hours_summary$studyid %in% Lascar, "Lascar", NA))

saveRDS(compliance_data, file = paste0("ecm_co_compliance_data_", format(Sys.Date(), format = "%b%d"), ".rds"))
saveRDS(hours_summary, file = paste0("ecm_co_hours_summary_", format(Sys.Date(), format = "%b%d"), ".rds"))
############################ Jan 4
# get upem_summary (from Micropem_Compliance_ECM_Pilot.R)-----
hours_summary <- readRDS("/Users/ashlinn/Dropbox/Ghana project/Ghana R stuff/ecm_co_hours_summary_Jan05.rds")
upem_hours_summary <- readRDS("/Users/ashlinn/Dropbox/Ghana project/Ghana R stuff/upem_hours_summary_Jan05.rds")

# ECM files with no hobo ----------
## Load and process  ECM data
ECM_files <- list.files("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/ECM_Pilot/Feasibility Pilot/ECM data/ECM_noHOBO/", recursive=F, full.names=T)
length(ECM_files) #3
ECM_files <- ECM_files[c(1,3)] # #26 has no data

# Set compliance threshold and averaging window width (minutes) - for 30 second sampling


ECM_import <- function(file) {
  
  data <- read.csv(as.character(file),col.names = c("Date","Time",  "RH.Corrected Nephelometer"  ,"Temp",  "RH",  "Battery",  "Inlet.Press",  "Flow.Orifice.Press",  "Flow",  "X.axis",  "Y.axis",  "Z.axis",  "Vector.Sum.Composite", "Stop.Descriptions"  ), header=F, sep=",", fill=T, stringsAsFactors=FALSE) # IMPORTING ACTUAL DATASET INTO R. 
  
  data_header <- data[1:26,1:7]
  colnames(data_header) <- c("variable", "V1", "V2", "V3", "V4", "V5", "V6")
  
  serialnumber <- paste("PM", as.character(sub("^......", "", data_header[4,2])), sep = "")
  serialnumber_full <- data_header[4,2]
  
  data <- data[27:nrow(data),]  
  
  stop.desc=data$Stop.Descriptions
  stop.desc= cbind(data[,c(1:2)],stop.desc)
  stop.desc$stop.desc = as.character(stop.desc$stop.desc)
  stop.desc = data.frame(sapply(stop.desc, blank2na), stringsAsFactors = FALSE)
  colnames(stop.desc) <- c("V1", "V2", "variable")
  stop.desc = stop.desc[complete.cases(stop.desc),]    
  stop.desc <- stop.desc[,c(3,1,2)]
  
  
  data.withoutdesc <- data[5:nrow(data), 1:13]
  
  ######### CREATING DATA2 DATASET - ALL DATA INCLUDING HEPA SESSIONS, RH CORRECTED. #####
  
  
  data2 = data.frame(sapply(data.withoutdesc, blank2na))
  for(i in 1:11){data2[,i+2] = as.numeric(levels(data2[,i+2]))[as.integer(data2[,i+2])]}
  
  # get rid of NA rows
  data2 <- data2[rowSums(is.na(data2)) != ncol(data2),]
  
  data2$Date <- as.character(data2$Date)
  data2$Time <- as.character(data2$Time)
  data2$datetime <- paste(data2$Date, data2$Time)
  
  
  data2$datetime <- mdy_hms(data2$datetime, tz =  "GMT")
  
  data2$RH.Corrected.Nephelometer = ifelse(data2$RH <0, NA, data2$RH.Corrected.Nephelometer) # First removes RH corrected if RH < 0
  data2$RH[data2$RH < 0] = NA   
  
  
  data2$unique_min <- floor_date(data2$datetime, unit = "minute")
  data2$unique_hour <- floor_date(data2$datetime, unit = "hour")
  
  
  # group by 3 minutes
 
  data2$centered_vectorsum_squared <- (data2$Vector.Sum.Composite - mean(data2$Vector.Sum.Composite, na.rm = TRUE))^2
  
  # divide by 3 minutes: this section is slow, don't know how to avoid the for loop
  mins_sequence <- seq(min(data2$unique_min), max(data2$unique_min),180) # 3 minute sequence
  data2$three_minutes <- NA
  
  for (i in 1:nrow(data2)) {
    value <- which.min(abs(data2$unique_min[i] - mins_sequence))
    data2$three_minutes[i] <-mins_sequence[value]
  }
  
  
  grouped_accel <- data2 %>% group_by(three_minutes) %>% dplyr:: summarise(file = basename(file)[1], mean_nephelometer = mean(RH.Corrected.Nephelometer, na.rm = TRUE), mean_vectorsum_g = mean(Vector.Sum.Composite, na.rm = TRUE), sd_vectorsum_g = sd(Vector.Sum.Composite, na.rm = TRUE), sd_vectorsum_squared = sd(centered_vectorsum_squared, na.rm = TRUE))
  grouped_accel$three_minutes <- as.POSIXct(grouped_accel$three_minutes, origin = "1970-1-1", tz = "GMT")        
  
  grouped_accel
}

# Create stacked data ---------
ECM_stacked <- ldply(ECM_files, ECM_import, .progress = "text")

ECM_files <- unique(ECM_stacked$file) 

# calculate compliance
width <- 5 # setting a 15-minute (3*5) window for calculating the rolling average
compliance_threshold <- 0.002


pdf(file = paste0("ECM_compliance_noHOBO",format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 10)
par(mfrow = c(2,2), mar = c(5,4,4,3))

ecm_compliance_data <- data.frame()
ecm_hours_summary <- data.frame()
for (i in 1:length(ECM_files)) {
  plotdata <- ECM_stacked[ECM_stacked$file == ECM_files[i],]
  
  # calculate compliance
  plotdata$sd_above_threshold <- ifelse(plotdata$sd_vectorsum_squared > compliance_threshold, 1, 0)
  plotdata$sd_rollmean <- as.numeric(rollapply(plotdata$sd_above_threshold, width=width,  FUN = mean, align = "center", na.rm = TRUE, fill = NA)) 
  plotdata$compliance_rollmean <- ifelse(plotdata$sd_rollmean > 0, 1, 0)
  plotdata$percent_compliant <- mean(plotdata$compliance_rollmean, na.rm = TRUE) # % of time
  plotdata$log_interval_s <- 30
  
  ecm_compliance_data <- rbind(ecm_compliance_data, plotdata)
  
  duration <- as.numeric(difftime(max(plotdata$three_minutes), min(plotdata$three_minutes), unit = "hours"))
  hours_worn <- round(as.numeric(difftime(last(plotdata$three_minutes),first(plotdata$three_minutes), units = "hours")*plotdata$percent_compliant[1]), digits = 1)
  
  ecm_hours_summary[i,1] <- plotdata$file[i]
  ecm_hours_summary[i,2] <- hours_worn
  ecm_hours_summary[i,3] <- round(plotdata$percent_compliant[1]*100, digits = 1)
  ecm_hours_summary[i,4] <- duration
  ecm_hours_summary[i,5] <- round((hours_worn/16)*100, digits = 1)
  
  studyid <- substr(ecm_hours_summary[i,1], start = regexpr("ECMF", ecm_hours_summary[i,1] ), stop = regexpr("ECMF", ecm_hours_summary[i,1] ) + 5)
  
  unit_type <- "ECM"
  
  # the plot
  plot(plotdata$three_minutes, plotdata$mean_vectorsum_g, type = "l", ylim = c(0, max(plotdata$mean_vectorsum_g, na.rm = TRUE)), main = paste( unit_type, studyid,"\n Compliance Threshold =", compliance_threshold, "Window =", width*3, "minutes \n Logger interval =", plotdata$log_interval_s[1], "s, Percent compliant = ", round(plotdata$percent_compliant[1]*100, digits = 1), "Hours worn =", hours_worn), cex.main = 0.8, ylab = "", xlab = "", xaxt = "n")
  lines(plotdata$three_minutes, plotdata$compliance_rollmean/1.5, col = "aquamarine3")
  text(x  = mean(plotdata$three_minutes), y = 0.69, labels = "<--compliant-->", cex = 0.8)
  axis.POSIXct(1, at=seq(from = floor_date(plotdata$three_minutes[1], unit = "hour"), to = ceiling_date(plotdata$three_minutes[nrow(plotdata)], unit = "hour"), by = "hour"), las = 2, cex.axis = 0.8)
  
  # to add nephelometer
  # lines(plotdata$three_minutes, scale(plotdata$mean_nephelometer, center = FALSE), col = "orange")
  # axis(side = 4, at = round(scale(plotdata$mean_nephelometer, center = FALSE)), col.ticks = "orange", col.axis = "orange")
  # mtext(side = 4, line = 1.8, text = "scaled PM (ug/m^3)", col = "orange", cex = 0.7)
  
  mtext(side = 2, line = 2, text = "mean vector-sum accel (g-force)", cex = 0.7)
}
dev.off()

names(ecm_hours_summary) <- c("file", "hours_worn", "percent_worn", "duration_hrs", "percent_worn_16")
ecm_hours_summary$studyid <- substr(ecm_hours_summary$file, start = regexpr("ECMF", ecm_hours_summary$file ), stop = regexpr("ECMF", ecm_hours_summary$file ) + 5)
ecm_hours_summary$unit_type <- "ECM"

saveRDS(ecm_hours_summary, file = paste0("ECM_hours_summary_nohobo_", format(Sys.Date(), format = "%b%d"), ".rds"))
saveRDS(ecm_compliance_data, file = paste0("ECM_compliance_nohobo_", format(Sys.Date(), format = "%b%d"), ".rds"))
############
hobo_hours_summary <- readRDS("/Users/ashlinn/Dropbox/Ghana project/Ghana R stuff/ecm_co_hours_summary_Jan05.rds")
upem_hours_summary <- readRDS("/Users/ashlinn/Dropbox/Ghana project/Ghana R stuff/upem_hours_summary_Jan05.rds")
hours_summary <- rbind(hobo_hours_summary, upem_hours_summary, ecm_hours_summary)

saveRDS(hours_summary, file = paste0("ECM_pilot_hours_worn", format(Sys.Date(), format = "%b%d"), ".rds"))

write.csv(hours_summary, file = paste0("ECM_pilot_hours_worn", format(Sys.Date(), format = "%b%d"), ".csv"), row.names = FALSE)

# then go in and eyeball the sleep/wake times - these are now saveD in the .csv file

# SUMMARY STATS------
hours_summary <- read.csv("/Users/ashlinn/Dropbox/Ghana project/Ghana R stuff/ECM_pilot_hours_wornJan06.csv", header = TRUE, stringsAsFactors = FALSE)

hours_summary <- hours_summary[!hours_summary$studyid == "ECMF03",] # the bad MicroPEM that only ran 0.8 hrs

summary <- group_by(hours_summary, unit_type, age)

# wearing time overall
summarise(summary, hrs_worn = mean(hours_worn, na.rm = TRUE), sd_hours_worn = sd(hours_worn, na.rm = TRUE), n = n())

summarise(hours_summary %>% group_by(unit_type),  hrs_worn = mean(hours_worn, na.rm = TRUE), sd_hours_worn = sd(hours_worn, na.rm = TRUE), n = n())

summarise(hours_summary %>% group_by(age),  hrs_worn = mean(hours_worn, na.rm = TRUE), sd_hours_worn = sd(hours_worn, na.rm = TRUE), n = n())

summarise(hours_summary,  hrs_worn = mean(hours_worn, na.rm = TRUE), sd_hours_worn = sd(hours_worn, na.rm = TRUE), n = n())

# greater than percents out of 16 hours
# 40% 16*.4 = 6.4
# 60% 16*.6 = 9.6
# 80% 16*.8 = 12.8

hours_summary$more40 <- ifelse(hours_summary$hours_worn >= 6.4, TRUE, FALSE)
hours_summary$more60 <- ifelse(hours_summary$hours_worn >= 9.6, TRUE, FALSE) 
hours_summary$more80 <- ifelse(hours_summary$hours_worn >= 12.8, TRUE, FALSE)

# unit type and age
summary <- hours_summary %>% group_by(unit_type, age)
pcts <- summarise(summary, more40 = sum(more40), more60 = sum(more60), more80 = sum(more80), n = n())
pcts <- mutate(pcts, more40_pct = more40/n*100, more60_pct = more60/n*100, more80_pct = more80/n*100)

# unit type
summary <- hours_summary %>% group_by(unit_type)
pcts <- summarise(summary, more40 = sum(more40), more60 = sum(more60), more80 = sum(more80), n = n())
pcts <- mutate(pcts, more40_pct = more40/n*100, more60_pct = more60/n*100, more80_pct = more80/n*100)

# age
summary <- hours_summary %>% group_by(age)
pcts <- summarise(summary, more40 = sum(more40), more60 = sum(more60), more80 = sum(more80), n = n())
pcts <- mutate(pcts, more40_pct = more40/n*100, more60_pct = more60/n*100, more80_pct = more80/n*100)

# overall
summary(hours_summary)

# bar chart
library(reshape2)
bar_data <- melt(hours_summary[, c("studyid", "age", "unit_type", "hours_worn")], id.vars = c("studyid", "unit_type", "age"))
bar_data_ECM <- bar_data[bar_data$unit_type == "ECM",]

barstuff_ECM <- hours_summary[hours_summary$unit_type == "ECM",]
barstuff_ECM <- barstuff_ECM[order(barstuff_ECM$hours_worn),]
barstuff_ECM$order <- as.factor(order(barstuff_ECM$hours_worn))

barstuff_upem <- hours_summary[hours_summary$unit_type == "MicroPEM",]
barstuff_upem <- barstuff_upem[order(barstuff_upem$hours_worn),]
barstuff_upem$order <- as.factor(order(barstuff_upem$hours_worn))

barstuff_lascar <- hours_summary[hours_summary$unit_type == "Lascar",]
barstuff_lascar <- barstuff_lascar[order(barstuff_lascar$hours_worn),]
barstuff_lascar$order <- as.factor(order(barstuff_lascar$hours_worn))

library(grid)
library(gridExtra)
tiff(file = "Barplot_ECM_Pilot.tif", width = 1600, height = 1600, res = 200)
p1 <- ggplot(barstuff_ECM,aes(x=order,y=hours_worn,fill=factor(age)))+
  geom_bar(stat="identity",position="dodge")+
 scale_fill_discrete(name="Age")+
  xlab("Subject #")+ylab("Hours Worn") + theme_bw() + theme(plot.title = element_text(hjust=0)) + coord_flip() + ylim(0, 24)  + ggtitle("A. Enhanced Child Monitor")

p2 <- ggplot(barstuff_upem,aes(x=order,y=hours_worn,fill=factor(age)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Age")+
  xlab("Subject #")+ylab("Hours Worn") + theme_bw()+ theme(plot.title = element_text(hjust=0)) + coord_flip() + ylim(0, 24)  + ggtitle("B. MicroPEM Monitor")

p3 <- ggplot(barstuff_lascar,aes(x=order,y=hours_worn,fill=factor(age)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Age")+
  xlab("Subject #")+ylab("Hours Worn") + theme_bw() + theme(plot.title = element_text(hjust=0)) + coord_flip() + ylim(0, 24)  +  ggtitle("C. Lascar CO Monitor")

grid.arrange(p1,p2,p3, ncol = 1)
dev.off()

# # histograms ------
# pdf(file = "ECM_Pilot_Hours_Worn.pdf", width = 10, height = 10)
# par(mfrow = c(3,2), oma = c(0,0,3,0))
# hist(hours_summary$hours_worn[hours_summary$unit_type == "Lascar"], col = "lightblue", main = paste("Hours Worn: Lascar (n =", nrow(hours_summary[hours_summary$unit_type == "Lascar",]), ") \n Mean Hours =", round(mean(hours_summary$hours_worn[hours_summary$unit_type == "Lascar"]), digits = 1)), xlim = c(0, 25), xlab = "Hours")
# hist(upem_hours_summary$percent_worn[hours_summary$unit_type == "Lascar"], col = "lightcoral", main = paste("Percent of Time Worn: Lascar (n =", nrow(hours_summary[hours_summary$unit_type == "Lascar",]), ")\n Mean Percent =", round(mean(hours_summary$percent_worn[hours_summary$unit_type == "Lascar"]), digits = 1)), xlim = c(0, 100), xlab = "Percent of Time Worn")
# 
# hist(hours_summary$hours_worn[hours_summary$unit_type == "ECM"], col = "lightblue", main = paste("Hours Worn: ECM (n =", nrow(hours_summary[hours_summary$unit_type == "ECM",]), ") \n Mean Percent = ", round(mean(hours_summary$hours_worn[hours_summary$unit_type == "ECM"]), digits = 1)), xlim = c(0, 25), xlab = "Hours")
# hist(upem_hours_summary$percent_worn[hours_summary$unit_type == "ECM"], col = "lightcoral", main = paste("Percent of Time Worn: ECM (n =", nrow(hours_summary[hours_summary$unit_type == "ECM",]), ") \n Mean Percent =", round(mean(hours_summary$percent_worn[hours_summary$unit_type == "ECM"]), digits = 1)),xlim = c(0, 100), xlab = "Percent of Time Worn")
# 
# hist(hours_summary$hours_worn[hours_summary$unit_type == "MicroPEM"], col = "lightblue", main = paste("Hours Worn: MicroPEM (n =", nrow(hours_summary[hours_summary$unit_type == "MicroPEM",]), ")\n Mean Percent = ", round(mean(hours_summary$hours_worn[hours_summary$unit_type == "MicroPEM"]), digits = 1)), xlim = c(0, 25), xlab = "Hours")
# hist(hours_summary$percent_worn[hours_summary$unit_type == "MicroPEM"], col = "lightcoral", main = paste("Percent of Time Worn: MicroPEM (n =", nrow(hours_summary[hours_summary$unit_type == "MicroPEM",]), ")\n Mean Percent =", round(mean(hours_summary$percent_worn[hours_summary$unit_type == "MicroPEM"]), digits = 1)),xlim = c(0, 100), xlab = "Percent of Time Worn")
# mtext("Feasibility Pilot", side = 3, outer = TRUE, cex = 1.5)
# 
# dev.off()
# 
