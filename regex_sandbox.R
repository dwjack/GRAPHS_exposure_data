for(i in files){
  #add function to parse file name
  #print(i)
  hhid_pattern<-"BM...."
  hhid_match<-regexpr(hhid_pattern, i)
  hhid<-regmatches(i, hhid_match)
  
  vill_pattern<-"vil_.."
  vill_match<-regexpr(vill_pattern, i)
  vill<-regmatches(i, vill_match)
  
  session_pattern<-"s_.."
  session_match<-regexpr(session_pattern, i)
  session<-regmatches(i, session_match)

  
  name<-paste(hhid, vill, session, sep="_")
  d <- lapply(i, read.csv, header=T) # note that this gives a list of dataframes (i.e., a list wherein each element is a datafrome)

}

require(plyr)
names(files)<-files
all <- ldply(files, read.csv)


######################http://stackoverflow.com/questions/18092102/how-can-i-turn-the-filename-into-a-variable-when-reading-multiple-csvs-into-r
