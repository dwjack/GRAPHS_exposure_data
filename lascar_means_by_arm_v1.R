require(dplyr)
#create a vector of file names  -- works!!
files<-list.files("~/Dropbox/ghana_exposure_data_shared_2014/Main_study_exposure_assessment",recursive=T,pattern="^CU_CO", full.names=T)
head(files)

files<-files[1:10] # this is just to keep it to a manageable size


require(plyr)
names(files)<-files #for reasons I don't understand, this forces ldply to include a column with the file name, for parsing below
all <- ldply(files, read.csv) #this creates a single dataframe 

hhid_pattern<-"BM...."
hhid_match<-regexpr(hhid_pattern, all$.id)
all$hhid<-regmatches(all$.id, hhid_match)

vill_pattern<-"vil_.."
vill_match<-regexpr(vill_pattern, all$.id)
all$vill<-regmatches(all$.id, vill_match)

session_pattern<-"s_.."
session_match<-regexpr(session_pattern, all$.id)
all$session<-regmatches(all$.id, session_match)


#next steps
#     1.  get rid of extraneous variables
#     2.  