#version 0
#stack the calibration data and plot it
#created 24 Feb 2014

path<-"~/Documents/projects/Biomass_working_group/Ghana_R01/exposure_data_assessment/CO_CALIBRATION"
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
lascar_pattern<-"CU_CO_..." #note requires that lascar values be entered as three digits
lascar_match<-regexpr(lascar_pattern, calib$.id)
calib$lascar<-regmatches(calib$.id, lascar_match)

head(calib)

ggplot(calib,aes(x=datetime,y=co,colour=lascar))+geom_line()