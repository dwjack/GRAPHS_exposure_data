require(plyr)
require(dplyr)
library(reshape)
#create a vector of file names  -- works!!
files<-list.files("~/Dropbox/ghana_exposure_data_shared_2014/Main_study_exposure_assessment",recursive=T,pattern="^CU_CO", full.names=T)
head(files)

files<-files[1:10] # this is just to keep it to a manageable size

# need to modify the import function so that we retain the sensor expiration date and the 

lascar.import <- function(x){
  dt <- read.csv(x, stringsAsFactors=F, header=T)[,c(2,3)]
  names(dt) <- c('datetime','co')
  dt$datetime <- dmy_hms(dt$datetime, tz="GMT")
  dt <- melt(dt, id.var="datetime",na.rm=T)
  dt$rd.datetime <- as.character(round(dt $datetime, 'min'))
  dt <- ddply(dt,.(variable, rd.datetime),summarize,value=mean(as.numeric(value),na.rm=T))
  colnames(dt)[2] <- "datetime"
  dt$datetime <- ymd_hms(dt$datetime)
  dt
}

names(files)<-files #for reasons I don't understand, this forces ldply to include a column with the file name, for parsing below
CO_stacked <- ldply(files, lascar.import) #this creates a single dataframe 

#create hhid variable
hhid_pattern<-"BM...."
hhid_match<-regexpr(hhid_pattern, CO_stacked$.id)
CO_stacked$hhid<-regmatches(CO_stacked$.id, hhid_match)

#create village id variable
vill_pattern<-"vil_.."
vill_match<-regexpr(vill_pattern, CO_stacked$.id)
CO_stacked$vill<-regmatches(CO_stacked$.id, vill_match)

#create session id variable
session_pattern<-"s_.."
session_match<-regexpr(session_pattern, CO_stacked$.id)
CO_stacked$session<-regmatches(CO_stacked$.id, session_match)

#need to recover these variables by modifying the import.lascar function
# CO_stacked <- rename(CO_stacked, c(CO.ppm.="CO"))
# CO_stacked <- rename(CO_stacked, c(Serial.Number ="serial_number"))
# CO_stacked <- rename(CO_stacked, c(Sensor.Life.Expiry="sensor_expiration"))

CO_stacked$hhid<-factor(CO_stacked$hhid)
CO_stacked$vill<-factor(CO_stacked$vill)
CO_stacked$session<-factor(CO_stacked$session)

CO_stacked<-as.tbl(CO_stacked)
by_hhid<-group_by(CO_stacked,hhid)

#next steps

#     2.  figure out how to apply dplyr magic