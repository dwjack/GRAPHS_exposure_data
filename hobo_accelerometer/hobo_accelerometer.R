## HOBO Accelerometer Data
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

# pdf(file = paste0("HOBO_accel_data_",format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 10)
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

############################ Jan 4
# make upem_summary-----
names(upem_hours_summary) <- c("file", "hours_worn", "percent_worn", "duration", "percent_worn_16")


hours_summary$studyid <- substr(hours_summary$file, start = regexpr("ECMF", hours_summary$file ), stop = regexpr("ECMF", hours_summary$file ) + 5)
hours_summary$unit_type <- NA
hours_summary$unit_type <- ifelse(hours_summary$studyid %in% ECM, "ECM", ifelse(hours_summary$studyid %in% Lascar, "Lascar", NA))

hours_summary <- rbind(hours_summary, upem_hours_summary)

saveRDS(hours_summary, file = "ECM_pilot_hours_worn.rds")
write.csv(hours_summary, file = "ECM_pilot_hours_worn.csv")

# add deployment duration and percent worn out of 16 hours to hours_summary


pdf(file = "ECM_Pilot_Hours_Worn.pdf", width = 10, height = 10)
par(mfrow = c(3,2), oma = c(0,0,3,0))
hist(hours_summary$hours_worn[hours_summary$unit_type == "Lascar"], col = "lightblue", main = paste("Hours Worn: Lascar (n =", nrow(hours_summary[hours_summary$unit_type == "Lascar",]), ") \n Mean Hours =", round(mean(hours_summary$hours_worn[hours_summary$unit_type == "Lascar"]), digits = 1)), xlim = c(0, 25), xlab = "Hours")
hist(upem_hours_summary$percent_worn[hours_summary$unit_type == "Lascar"], col = "lightcoral", main = paste("Percent of Time Worn: Lascar (n =", nrow(hours_summary[hours_summary$unit_type == "Lascar",]), ")\n Mean Percent =", round(mean(hours_summary$percent_worn[hours_summary$unit_type == "Lascar"]), digits = 1)), xlim = c(0, 100), xlab = "Percent of Time Worn")

hist(hours_summary$hours_worn[hours_summary$unit_type == "ECM"], col = "lightblue", main = paste("Hours Worn: ECM (n =", nrow(hours_summary[hours_summary$unit_type == "ECM",]), ") \n Mean Percent = ", round(mean(hours_summary$hours_worn[hours_summary$unit_type == "ECM"]), digits = 1)), xlim = c(0, 25), xlab = "Hours")
hist(upem_hours_summary$percent_worn[hours_summary$unit_type == "ECM"], col = "lightcoral", main = paste("Percent of Time Worn: ECM (n =", nrow(hours_summary[hours_summary$unit_type == "ECM",]), ") \n Mean Percent =", round(mean(hours_summary$percent_worn[hours_summary$unit_type == "ECM"]), digits = 1)),xlim = c(0, 100), xlab = "Percent of Time Worn")

hist(hours_summary$hours_worn[hours_summary$unit_type == "MicroPEM"], col = "lightblue", main = paste("Hours Worn: MicroPEM (n =", nrow(hours_summary[hours_summary$unit_type == "MicroPEM",]), ")\n Mean Percent = ", round(mean(hours_summary$hours_worn[hours_summary$unit_type == "MicroPEM"]), digits = 1)), xlim = c(0, 25), xlab = "Hours")
hist(hours_summary$percent_worn[hours_summary$unit_type == "MicroPEM"], col = "lightcoral", main = paste("Percent of Time Worn: MicroPEM (n =", nrow(hours_summary[hours_summary$unit_type == "MicroPEM",]), ")\n Mean Percent =", round(mean(hours_summary$percent_worn[hours_summary$unit_type == "MicroPEM"]), digits = 1)),xlim = c(0, 100), xlab = "Percent of Time Worn")
mtext("Feasibility Pilot", side = 3, outer = TRUE, cex = 1.5)

dev.off()

