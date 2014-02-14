require(plyr)
require(dplyr)
library(reshape)
#create a vector of file names  -- works!!
files<-list.files("~/Dropbox/ghana_exposure_data_shared_2014/Main_study_exposure_assessment",recursive=T,pattern="^CU_CO", full.names=T)
head(files)

files<-files[1:10] # this is just to keep it to a manageable size


require(plyr)
names(files)<-files #for reasons I don't understand, this forces ldply to include a column with the file name, for parsing below
CO_stacked <- ldply(files, read.csv) #this creates a single dataframe 

hhid_pattern<-"BM...."
hhid_match<-regexpr(hhid_pattern, CO_stacked$.id)
CO_stacked$hhid<-regmatches(CO_stacked$.id, hhid_match)

vill_pattern<-"vil_.."
vill_match<-regexpr(vill_pattern, CO_stacked$.id)
CO_stacked$vill<-regmatches(CO_stacked$.id, vill_match)

session_pattern<-"s_.."
session_match<-regexpr(session_pattern, CO_stacked$.id)
CO_stacked$session<-regmatches(CO_stacked$.id, session_match)
CO_stacked <- rename(CO_stacked, c(CO.ppm.="CO"))
CO_stacked <- rename(CO_stacked, c(Serial.Number ="serial_number"))
CO_stacked <- rename(CO_stacked, c(Sensor.Life.Expiry="sensor_expiration"))


keep<-c("Time", "CO", "serial_number", "sensor_expiration", "hhid", "vill", "session" )
CO_stacked<-CO_stacked[keep]

#next steps
#     1.  get rid of extraneous variables
#     2.  figure out how to apply dplyr magic