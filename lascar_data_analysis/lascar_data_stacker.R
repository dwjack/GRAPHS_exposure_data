# To do:
# replace "ldply" with something that's parallelized   

require(plyr)
require(dplyr)
require(reshape)
require(lubridate)
require(foreach)
#create a vector of file names  -- works!!
files<-list.files("~/Dropbox/ghana_exposure_data_shared_2014/Main_study_exposure_assessment",recursive=T,pattern="^(CU_CO|CU_C0|CO_USB|COL_USB|CU-CO|CU-C0|CO-USB|COL-USB)", full.names=T) #4059 unprocessed files
length(files)
#files<-files[1:100] # this is just to keep it to a manageable size

# need to modify the import function so that we retain the sensor expiration date, serial # 

lascar.import <- function(x){
  dt <- read.csv(x, stringsAsFactors=F, header=T)[,c(2,3)]
  names(dt) <- c('datetime','co')
  dt$datetime <- dmy_hms(dt$datetime, tz="GMT")
  dt$rd.datetime <- as.character(round(dt $datetime, 'min'))
  dt<-dt %.% group_by(rd.datetime) %.% dplyr::summarise(mean(co)) #replaced the plyr approach that ajay provided w/ ddply
  names(dt) <- c('datetime','co')
  dt$datetime <- ymd_hms(dt$datetime)
  dt
}

names(files) <- files #for reasons I don't understand, this forces ldply to include a column with the file name, for parsing below

ptm <- proc.time()

CO_stacked <- ldply(files, lascar.import, .progress = "text" ) #this creates a single dataframe 

proc.time() - ptm

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

save(CO_stacked, file="~/Documents/projects/Biomass_working_group/Ghana_R01/exposure_data_assessment/CO_stacked_july_7.Rdata", safe=T)
