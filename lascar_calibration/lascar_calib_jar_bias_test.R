# Testing for jar bias



# note that on some computers the pathname is ~/Dropbox/Ghana_exposure_data_SHARED_2014 while on others it is ~/Dropbox/Ghana_exposure_data_SHARED_2014
# do a replace-all


require(plyr)
require(dplyr)
require(ggplot2)
require(lubridate)
require(scales)
library(lattice)

# functions
lascar.import <- function(x){
  dt <- read.csv(x, stringsAsFactors=F, header=T)[,c(2,3,4)]
  names(dt) <- c('datetime','co','SN')
  dt$datetime <- dmy_hms(dt$datetime, tz="GMT")
  dt$SN<-dt$SN[1]
  dt
}

# get the "jarinfo" file. This has been hand-entered from the logsheets (columns for lascar, run, and jar)
jarinfo <- read.csv("~/Dropbox/Ghana_exposure_data_SHARED_2014/CO_calibration_documents/Jar_info_from_logsheets.csv", header = TRUE)
nrow(jarinfo)
jarinfo <- jarinfo[,-4]
jarinfo[,1] <- paste0("CU_C0_",formatC(jarinfo[,1], width = 3, format = "d", flag = "0"))
names(jarinfo)[1] <- "newlascar"
jarinfo$date <- substr(jarinfo$run, 1,9)
jarinfo$mergeinfo <- paste(jarinfo$date, jarinfo$newlascar, sep = "_")



jarinfo <- subset(jarinfo, select = -newlascar)



path<-"~/Dropbox/Ghana_exposure_data_SHARED_2014/CO_calibration_files/"

files<-list.files(path,full.names=T, recursive = TRUE, include.dirs = FALSE)
length(files) 

files <- files[which(regexpr("ogsheet", files) == -1)] # get rid of logsheets
length(files) # one run is max 12*3 = 36 files? #825


names(files)<-basename(files)
calib <- ldply(files,lascar.import)

#create lascar variable
lascar_pattern <- "(CU_CO_...|CU_C0_...|CO_USB_...|COL_USB_...|CU-CO_...|CU-C0_...|CO-USB_...|COL-USB_...)" #note requires that lascar values be entered as three digits
lascar_match<-regexpr(lascar_pattern, calib$.id)
calib$lascar<-regmatches(calib$.id, lascar_match)
calib$newlascar <- paste0("CU_C0_", substr(calib$lascar, 7,10))
calib$date <- format(floor_date(calib$datetime, unit = "day"), format = "%d%b%Y")
calib$mergeinfo <- paste(calib$date, calib$newlascar, sep = "_")

# merge calib data and jarinfo
calib <- merge(calib[,c(2:5,8)], jarinfo, by = "mergeinfo", all.x = TRUE)
calib <- calib[!is.na(calib$run),]


calib <- subset(calib, select = c("mergeinfo", "lascar", "datetime", "co", "run", "jar"))
calib2 <- melt(calib, id.vars = c("datetime", "mergeinfo","lascar", "run", "jar"))


# plot by run
library(gridExtra)
pdf(file = "calib_plots_by_jar.pdf", onefile = TRUE)
for (i in 1:length(unique(calib2$run))) {
  plotdata <- calib2[calib2$run == unique(calib2$run)[i],]
  plotdata$mediantime <- median(plotdata$datetime)
  mediantime <- median(plotdata$mediantime)
  plotdata <- filter(plotdata, datetime >= mediantime - minutes(40) & datetime <= mediantime + minutes(40))
  p <- ggplot(plotdata,aes(x=datetime,y=value,group = mergeinfo, colour = lascar))+geom_line(show.legend = FALSE) + ggtitle(plotdata$run[1])
  p <- p + facet_grid(.~ jar) + coord_cartesian(ylim = c(-5, 75))
  print(p)
}
dev.off()



# working!
