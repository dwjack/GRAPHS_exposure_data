## HOBO Accelerometer Data
require(lubridate)
require(plyr)
require(dplyr)
require(zoo)

files <- list.files("/Users/ashlinn/Dropbox/Ghana project/ECM Pilot/HOBO accelerometer data/May 6/", recursive=F, full.names=T) 
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
  data$date_time <- mdy_hms(data$date_time, tz = "GMT")
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
  grouped_data <- data %.% group_by(three_minutes) %.% dplyr:: summarise(file = basename(file), SN = SN[1],log_interval_s = log_interval_s[1], mean_vectorsum_g = mean(vectorsum_g, na.rm = TRUE), sd_vectorsum_g = sd(vectorsum_g, na.rm = TRUE), sd_vectorsum_squared = sd(centered_vectorsum_squared, na.rm = TRUE))
  grouped_data$three_minutes <- as.POSIXct(grouped_data$three_minute, origin = "1970-1-1", tz = "GMT")
 
  
  
  grouped_data
}
  

hobo_stacked <- ldply(files, hobo_import, .progress = "text")   # note: no compliance info
  

stacked_files <- unique(hobo_stacked$file)


# calculate compliance

width <- 5 # setting a 15-minute (3*5) window for calculating the rolling average

pdf(file = paste0("HOBO_accel_data_",format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 10)
par(mfrow = c(2,2))
for (i in 1:length(stacked_files)) {
  plotdata <- hobo_stacked[hobo_stacked$file == stacked_files[i],]
  compliance_threshold <- ifelse(plotdata$log_interval_s[1] == 30, 0.002, 0.001) # set compliance threshold to 0.002 for 30-second logging and 0.001 for 10-second logging
  plotdata$sd_above_threshold <- ifelse(plotdata$sd_vectorsum_squared > compliance_threshold, 1, 0)
  plotdata$sd_rollmean <- as.numeric(rollapply(plotdata$sd_above_threshold, width=width,  FUN = mean, align = "center", na.rm = TRUE, fill = NA)) 
  plotdata$compliance_rollmean <- ifelse(plotdata$sd_rollmean > 0, 1, 0)
  plotdata$percent_compliant <- mean(plotdata$compliance_rollmean, na.rm = TRUE) # % of time
  
  
  plot(plotdata$three_minutes, plotdata$mean_vectorsum_g, type = "l", ylim = c(0, max(plotdata$mean_vectorsum_g)), main = paste("Compliance Threshold =", compliance_threshold, "Window =", width*3, "minutes \n Logger interval =", plotdata$log_interval_s[1], "s, Percent compliant = ", round(plotdata$percent_compliant[1]*100, digits = 1)), cex.main = 0.8, ylab = "mean vector sum (g force)", xlab = "time (3-minute average)", xaxt = "n")
  lines(plotdata$three_minutes, plotdata$compliance_rollmean/1.5, col = "aquamarine3")
  text(x  = mean(plotdata$three_minutes), y = 0.69, labels = "<--compliant-->", cex = 0.8)
  axis.POSIXct(1, at=seq(from = floor_date(plotdata$three_minutes[1], unit = "hour"), to = ceiling_date(plotdata$three_minutes[nrow(plotdata)], unit = "hour"), by = "hour", labels=format(plotdata$three_minutes, "%h")), las = 2)
}
dev.off()




