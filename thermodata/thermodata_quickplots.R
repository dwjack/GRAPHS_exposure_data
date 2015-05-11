# ibutton thermochron data
library(lubridate)
library(ggplot2)


# read in the data file
data <- read.csv("/Users/ashlinn/Dropbox/Ghana project/ECM Pilot/SUMS files/AD0000011CA7D526_1505071753_new2.csv", col.names = c("datetime", "temp"), stringsAsFactors = FALSE)
# /Users/ashlinn/Desktop/AD0000011CA7D526_1505071753_new.csv
# "/Users/ashlinn/Desktop/AD0000011CA7D526_1505052027.csv"
# "/Users/ashlinn/Dropbox/Ghana project/ECM Pilot/SUMS files/4D0000011C8BAD26_1505081150.csv



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

summary(duplicated(newdata[,1:3]))
newdata$duplicated <- ifelse(duplicated(newdata[,1:3]), 1, 0)
newdata <- newdata[!duplicated(newdata[,1:3]),]

# test for date format and process dates accordingly
if (!is.na(suppressWarnings(mdy_hm(newdata$datetime[1])))) {
  newdata$datetime <- suppressWarnings(mdy_hm(newdata$datetime, tz = "GMT"))
} else if (!is.na(suppressWarnings(ymd_hms(newdata[1,1])))) {
  newdata$datetime <- suppressWarnings(ymd_hms(newdata$datetime, tz = "GMT"))
} else newdata$datetime <- newdata$datetime

# change temp to numeric
newdata$temp <- as.numeric(newdata$temp)

newdata <- newdata[!is.na(newdata$temp),]


# Plot and save individual logger downloads
pdf(file = paste0("Downloads",format(Sys.Date(), format = "%b%d"), ".pdf"))
par(mfrow = c(2,2))
for (i in unique(newdata$download)) {
  plotdata <- newdata[newdata$download == i,]
  plot(plotdata$datetime, plotdata$temp, type = "l", main = paste("Download", i, plotdata$serialnumber[1], sep = "_"), ylim = c(25, 85), xlab = "Time", ylab = "Temp (ºC)")
}
dev.off()



#### USING SPREADSHEET OF BUFFER INFORMATION ####

# import thick/thin info and merge with newdata
g <- read.csv("/Users/ashlinn/Dropbox/Ghana project/ECM Pilot/SUMS files/6HH_pilot_thick_vs_thin.csv", stringsAsFactors = F)
names(g)[1] <- "serial"
newdata$serial <- substr(newdata$serialnumber, 9,16)

alldata <- merge(newdata, g, by = "serial")

alldata$locationkey <- paste(alldata$MID, alldata$stovetype, alldata$location, sep = "_")

# fix wrong dates on ambient and original monitors: 61 days off
alldata$datetime <- ifelse(alldata$buffer %in% c("original", "ambient"), alldata$datetime - days(61), alldata$datetime)

alldata$datetime <- as.POSIXct(alldata$datetime, origin = "1970-1-1", tz = "GMT")


uniquelocations <- unique(alldata$locationkey)
for (i in 1:length(unique(alldata$locationkey))) { 
  print(qplot(datetime, temp, data = alldata[alldata$locationkey == uniquelocations[i],], col = buffer, geom = "line", main = paste("Location", i, uniquelocations[i], sep = "_")))
}


  