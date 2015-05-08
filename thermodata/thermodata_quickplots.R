# ibutton thermochron data
library(lubridate)

# read in the data file
data <- read.csv("/Users/ashlinn/Desktop/AD0000011CA7D526_1505071753_new.csv", col.names = c("datetime", "temp"), stringsAsFactors = FALSE)
# /Users/ashlinn/Desktop/AD0000011CA7D526_1505071753_new.csv
# "/Users/ashlinn/Desktop/AD0000011CA7D526_1505052027.csv"

# get rid of unwanted fields
unwanted <- c("Logger location:", "Logger type:", "Start delayed:", "Total mission samples:", "Available mission samples:", "Total device samples:", "Mission in progress:", "Rollover enabled:", "Mission start date/time:", "Earliest available sample date/time:", "Sample rate (seconds):", "Temperature logging resolution °C:", "Temperature low alarm limit °C:", "Temperature high alarm limit °C:", "Log data: date/time", "Date/time logger downloaded:", "Date/time data exported", "Timezone", "TDHC Manager","download complete")
data <- data[!data$datetime %in% unwanted,]

pattern <- "Temperature logging resolution |Temperature low alarm|Temperature high alarm"
oddsymbol <-regexpr(pattern, data[,1])
data <- data[oddsymbol == -1,]


# create an index vector "sections" that will inform where the sections start and end
sections <- which(data$datetime == "Logger serial number:") 
sections <- append(sections, (nrow(data)))

# for each section, insert serialnumber, then bind all together
newdata <- data.frame()
for (i in 1:(length(sections) -1)) {
  section <- data[(sections[i]):(sections[i+1] -1),]
  section$serialnumber <- section$temp[1]
  section.i <- section[3:nrow(section),1:3]
  section.i$download <- i
  newdata <- rbind(newdata, section.i)
}

# test for date format and process dates accordingly
if (!is.na(suppressWarnings(mdy_hm(newdata$datetime[1])))) {
  newdata$datetime2 <- suppressWarnings(mdy_hm(newdata$datetime, tz = "GMT"))
} else if (!is.na(suppressWarnings(ymd_hms(newdata[1,1])))) {
  newdata$datetime2 <- suppressWarnings(ymd_hms(newdata$datetime, tz = "GMT"))
} else newdata$datetime2 <- NA



# Plot and save
pdf(file = paste0("Downloads",format(Sys.Date(), format = "%b%d"), ".pdf"))
par(mfrow = c(2,2))
for (i in 1:length(unique(newdata$download))) {
  plotdata <- newdata[newdata$download == i,]
  plot(plotdata$datetime2, plotdata$temp, type = "l", main = paste("Download", i, plotdata$serialnumber[1], sep = "_"), ylim = c(25, 85), xlab = "Time", ylab = "Temp (ºC)")
}
dev.off()


