#version 0
#stack the calibration data and plot it
#created 24 Feb 2014
require(plyr)
require(dplyr)
require(ggplot2)
require(lubridate)

###############################################
#enter file to examine (calibration run)
###############################################
run <- "27Feb2014"   
###############################################
###############################################

path<-paste("~/Dropbox/Ghana_exposure_data_SHARED_2014/CO_calibration_files/", run, sep="")
path
files<-list.files(path,full.names=T)

lascar.import <- function(x){
    dt <- read.csv(x, stringsAsFactors=F, header=T)[,c(2,3,4)]
    names(dt) <- c('datetime','co','SN')
    dt$datetime <- dmy_hms(dt$datetime, tz="GMT")
    dt$SN<-dt$SN[1]
    dt
}
names(files)<-basename(files)
calib <- ldply(files,lascar.import)


#create lascar variable
lascar_pattern<-"CU_C._..." #note requires that lascar values be entered as three digits
lascar_match<-regexpr(lascar_pattern, calib$.id)
calib$lascar<-regmatches(calib$.id, lascar_match)


calib<-calib%.%filter(co<80)
length(calib)
ggplot(calib,aes(x=datetime,y=co,colour=lascar))+geom_line()



#check times - some of the lasars appear to be set to the wrong time.
meantime<-calib %.% group_by(lascar) %.% dplyr::summarise(datetime=mean(datetime),mean(co)) %.% arrange(desc(datetime))
meantime[,2] <- as.POSIXct(meantime[,2], origin="1970-01-01 12:00:00", tz='UTC')

meantime #table for inspection

###############################################
# drop any problem records and replot
###############################################

calib_cleaned <- calib %.% filter(lascar!="CU_CO_143")
calib_cleaned <- calib_cleaned %.% filter(lascar!="CU_CO_169") #repeat this row to remove additonal units from the plot
calib_cleaned <- calib_cleaned %.% filter(lascar!="CU_CO_156") #repeat this row to remove additonal units from the plot

ggplot(calib_cleaned,aes(x=datetime,y=co,colour=lascar))+geom_line()


