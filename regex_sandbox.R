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
  d <- lapply(i, read.csv) # note that this gives a list of dataframes (i.e., a list )
}


