require(plyr)
require(dplyr)
require(reshape)
require(lubridate)
#create a vector of file names  -- works!!
files<-list.files("~/Dropbox/ghana_exposure_data_shared_2014/Main_study_exposure_assessment",recursive=T,pattern="^CU_C.", full.names=T)
length(files)
#files<-files[1:10] # this is just to keep it to a manageable size

# need to modify the import function so that we retain the sensor expiration date, serial # 

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

#create village id variable - inefficient 2 step process, but works
vill_pattern<-"vil_.."
vill_match<-regexpr(vill_pattern, CO_stacked$.id,ignore.case=T)
CO_stacked$vill<-regmatches(CO_stacked$.id, vill_match)
CO_stacked$village_code<-substr(CO_stacked$vill,5,6)

#create session id variable
session_pattern<-"s_.."
session_match<-regexpr(session_pattern, CO_stacked$.id)
CO_stacked$session<-regmatches(CO_stacked$.id, session_match)
CO_stacked$session<-substr(CO_stacked$session,3,4)

#convert strings to factors
CO_stacked$hhid<-factor(CO_stacked$hhid)
CO_stacked$village_code<-factor(CO_stacked$village_code)
CO_stacked$session<-factor(CO_stacked$session)

#not sure that this is necessary (for dplyr)
CO_stacked<-as.tbl(CO_stacked)

#write.table(CO_stacked, "~/Documents/projects/Biomass_working_group/Ghana_R01/exposure_data_assessment/CO_stacked.txt", sep=",")
