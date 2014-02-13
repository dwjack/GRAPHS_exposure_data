for(i in files){
  #add function to parse file name
  print(i)
  temp<-regmatches("CU_CO",i,value=T)
  print(temp)
}