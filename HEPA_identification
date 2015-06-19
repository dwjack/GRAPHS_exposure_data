# HEPA-identification
## FIND THE DATA FILES ### 
files<-list.files("/Users/zhengzhou/Desktop/Ghana_exposure_data_SHARED_2014/Main_study_exposure_assessment",recursive=T,pattern="UGF[[:alnum:]]*_[[:alnum:]]*_", full.names=T) # this grabs the processed upem files
logsheets <- files[grep("logsheet", files)]    # separates out the logsheets
files <- files[!(files %in% logsheets)]        # removes the logsheets from the files
length(files) #the number of files

###Create some empty variables which will be used later ###
filepath = NULL
filterid = NULL
lowbattery = NULL
deadbattery = NULL
HEPA1value = NULL
HEPA2value = NULL
HEPA1sttime = NULL
HEPA1endtime = NULL
HEPA2sttime = NULL
HEPA2endtime = NULL
nHEPASt = NULL
nHEPAEnd = NULL
RH100 = NULL
RH0 = NULL
Temp50 = NULL
Temp0 = NULL
Duration = NULL

### function to convert empty cells to NA ###
blank2na <- function(x){ 
  z <- gsub("\\s+", "", x)                 #make sure it's "" and not " " etc
  x[z==""] <- NA 
  return(x)
}

###loop through all upem files###
for(k in 1:length(files)){
  data <- read.csv(as.character(files[k]),col.names = c("Date","Time",  "RH.Corrected Nephelometer"  ,"Temp",  "RH",  "Battery",  "Inlet.Press",  "Flow.Orifice.Press",  "Flow",  "X.axis",  "Y.axis",  "Z.axis",  "Vector.Sum.Composite", "Stop.Descriptions"  ), 
                   header=F, sep=",", fill=T, stringsAsFactors=FALSE)         # import ONE upem file into R
  filterid1 = data[8,2]
  filterid1[filterid1==""] = data[9,2]
  filterid = rbind(filterid, filterid1)            #get filter id      
  filepath =rbind(filepath,files[k])               #grab the location of file
  lowbattery1 = sum(sapply(data[,14], match,"Low Battery Stop", nomatch=0))      #detect low battery stop 
  deadbattery1 = sum(sapply(data[,14], match,"Battery dead", nomatch=0))         #detect battery dead
  lowbattery = rbind(lowbattery, lowbattery1)                                    #save low battery stop information into matrix
  deadbattery = rbind(deadbattery, deadbattery1)                                 #save battery dead information into matrix

  data <- data[25:nrow(data),]                     #drop instrument parameters
  data1 <- data[7:nrow(data), 1:13]                #drop text description
  data2 <- data.frame(sapply(data1, blank2na)) #replace empty cells with NA
  for(j in 1:11){data2[,j+2] = as.numeric(levels(data2[,j+2]))[as.integer(data2[,j+2])]}           #change variable to numeric
  data3 <- data2[!is.na(data2$RH.Corrected.Nephelometer),]                                         #drop rows with no Nephelometer reading
  data3$Date <- as.character(data3$Date)
  data3$Time <- as.character(data3$Time)
  data3$datetime <- paste(data3$Date, data3$Time)            #combine date and time into datetime variable
  
  nRH100 <- sum(data3$RH>100, na.rm=T)    #count how many RH >100
  nRH0 <- sum(data3$RH<0, na.rm=T)        #count how many RH <0
  nTemp50 <- sum(data3$Temp>50, na.rm=T)  #count how many Temperature >50
  nTemp0 <- sum(data3$Temp<0, na.rm=T)    #count how many Temperature <0
  
###forward loop through second row to last row in the data file###
  for(i in 2:nrow(data3)){
    avg1=mean(data3$RH.Corrected.Nephelometer[2:i])   #mean of 2nd reading to ith reading
    dif1=abs(data3$RH.Corrected.Nephelometer[i+1]-avg1) #difference between i+1th reading and mean of 2nd reading to ith reading
    if (dif1>5 & abs(dif1/avg1)>0.2) {               #if the difference >5 and the ratio of the difference over the mean > 0.2, then ith reading is considered as the end of HEPA period
      HEPAvalue1 = avg1                              #Start HEPA value = the mean of 2nd reading to ith reading
      HEPAsttime1 = data3$datetime[2]                #Start HEPA start time = the time of 2nd readiing
      HEPAendtime1 = data3$datetime[i]               #Start HEPA end time = the time of ith reading
      nHEPA1 = i-1                                   #number of Start HEPA readings = i-1
      break
    }
  }
  
###backward loop through second to last row to first row in the data file###
  for(x in (nrow(data3)-1):1){
    avg2=mean(data3$RH.Corrected.Nephelometer[x:(nrow(data3)-1)])
    dif2=abs(data3$RH.Corrected.Nephelometer[x-1]-avg2)
    if (dif2>5 & abs(dif2/avg2)>0.2) {
      HEPAvalue2 = avg2                              #End HEPA value
      HEPAsttime2 = data3$datetime[x]                #End HEPA start time
      HEPAendtime2 = data3$datetime[nrow(data3)-1]   #End HEPA end time
      nHEPA2 = nrow(data3) - x                       #number of End HEPA readings
      break
    }
  }
  
###save variables into matrix###
  HEPA1value =rbind(HEPA1value,HEPAvalue1)
  HEPA2value =rbind(HEPA2value,HEPAvalue2)
  HEPA1sttime =rbind(HEPA1sttime,HEPAsttime1)
  HEPA1endtime =rbind(HEPA1endtime,HEPAendtime1)
  HEPA2sttime =rbind(HEPA2sttime,HEPAsttime2)
  HEPA2endtime =rbind(HEPA2endtime,HEPAendtime2)
  nHEPASt = rbind(nHEPASt,nHEPA1)
  nHEPAEnd = rbind(nHEPAEnd,nHEPA2)
  RH100 = rbind(RH100, nRH100)
  RH0 = rbind(RH0, nRH0)
  Temp50 = rbind(Temp50, nTemp50)
  Temp0 = rbind(Temp0, nTemp0)
  Duration1 = length(data3$RH.Corrected.Nephelometer)/(3*60)
  Duration = rbind(Duration, Duration1)
  
  if(round(k/50)*50==k)               
    print(k)                                   #show progress
}                                              #end of loop through all upem files

HEPANew = as.data.frame(cbind(filterid, nHEPASt,HEPA1value,nHEPAEnd,HEPA2value,HEPA1sttime,HEPA1endtime,HEPA2sttime,HEPA2endtime,lowbattery, 
                              deadbattery,RH100, RH0, Temp50, Temp0,filepath), row.names=F, stringsAsFactors=FALSE)        #combine all variables into a dataframe
colnames(HEPANew) <- c("filterid", "nHEPASt", "HEPAStValue","nHEPAEnd", "HEPAEndValue","HEPASttime1","HEPAEndtime1","HEPASttime2", "HEPAEndtime2","lowbattery", "deadbattery", "RH100","RH0", "Temp50","Temp0", "filepath")  #assign colume names
HEPANew = HEPANew[!duplicated(HEPANew$filterid),]             # drop duplicates with same filter id

######nHEPASt, nHEPAEnd, and Duration will be used to determine whether a HEPA period is valid. nHEPASt, nHEPAEnd should >9 and Duration should >66. We haven't finalized the criteria.
