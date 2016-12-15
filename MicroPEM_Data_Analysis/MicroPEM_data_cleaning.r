################################# FIND THE DATA FILES ############################################ 
# define a file directory
filedirectory <- "/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/MicroPem Raw Data/Nephelometer_processed_correct"
Datafiles = list.files(filedirectory,full.names = TRUE)           # grab all MicroPEM files in the directory; includes both data as downloaded in Ghana and some RTI corrected files (RH issue)

# find files with no nephelometer reading
excludelist <- c("KHC0234")   # bad file, contains no data.                                    
excludefiles <- Datafiles[unlist(sapply(excludelist, function(x) grep(x, Datafiles)))] 
Datafiles <- Datafiles[!(Datafiles %in% excludefiles)]            # removes empty files with no nephelometer reading
MicroPEMfiles <- as.data.frame(Datafiles)                         # create a dataframe of datafiles

######################## DEFINE A FUNCTION TO READ AND STORE MICROPEM RAW FILE####################
# for each microPEM file, creates 2 matrices:  (i) setting parameters; (ii) time varying measurements
require(dplyr)
require(akima)
require(lubridate)
require(readr)
require(namespace)

convertOutput <- function(path) {
###########################################
# READ THE DATA
###########################################
dataPEM <- read.csv(path, skip = 28, header = FALSE, fill = TRUE)
dataPEM <- dataPEM[dataPEM[, 1] != "Errored Line", ]
dataPEM[, 1] <- as.character(dataPEM[, 1])
dataPEM[, 2] <- as.character(dataPEM[, 2])
dataPEM <- dplyr::tbl_df(dataPEM)
dataPEM <- dataPEM[,1:14]                  #get rid of empty column

# isolate names and erase spaces and hyphens
namesPEM <- read.csv(path, skip = 24, header = FALSE, nrow = 1)
namesPEM <- unlist(lapply(as.list(namesPEM), toString))
namesPEM <- gsub(" ", "", namesPEM)
namesPEM <- sub("-", "", namesPEM)
namesPEM <- namesPEM[1:14]                  #get rid of extra column names
names(dataPEM) <- namesPEM
# convert month names if they are abbreviated
dataPEM$Date <- tolower(dataPEM$Date)
dataPEM$Date <- gsub("jan", "01", dataPEM$Date)
dataPEM$Date <- gsub("feb", "02", dataPEM$Date)
dataPEM$Date <- gsub("mar", "03", dataPEM$Date)
dataPEM$Date <- gsub("apr", "04", dataPEM$Date)
dataPEM$Date <- gsub("may", "05", dataPEM$Date)
dataPEM$Date <- gsub("jun", "06", dataPEM$Date)
dataPEM$Date <- gsub("jul", "07", dataPEM$Date)
dataPEM$Date <- gsub("aug", "08", dataPEM$Date)
dataPEM$Date <- gsub("sep", "09", dataPEM$Date)
dataPEM$Date <- gsub("oct", "10", dataPEM$Date)
dataPEM$Date <- gsub("nov", "11", dataPEM$Date)
dataPEM$Date <- gsub("dec", "12", dataPEM$Date)
dataPEM <- dataPEM[dataPEM$Date!="",]       #get rid of empty rows

# get original date time
originalDateTime <- paste(dataPEM$Date, dataPEM$Time, sep = " ")

# Warning: Time does not have time zone
# create a variable with date and time together

timeDate <- dmy_hms(originalDateTime, tz="GMT")
timeDate[grep("/",originalDateTime)] = mdy_hms(originalDateTime, tz="GMT")

nephelometer <- as.numeric(dataPEM$RHCorrectedNephelometer)
temperature <- as.numeric(dataPEM$Temp)
relativeHumidity <- as.numeric(dataPEM$RH)
battery <- as.numeric(dataPEM$Battery)
inletPressure <- as.numeric(dataPEM$InletPress)
orificePressure <- as.numeric(dataPEM$FlowOrificePress)
if(length(orificePressure)==0) orificePressure <- as.numeric(dataPEM$OrificePress)
flow <- as.numeric(dataPEM$Flow)
xAxis <- as.numeric(dataPEM$Xaxis)
yAxis <- as.numeric(dataPEM$Yaxis)
zAxis <- as.numeric(dataPEM$Zaxis)
vectorSum <- as.numeric(dataPEM$VectorSumComposite)
names(dataPEM)[14] <- "shutDownReason"
shutDownReason <- as.character(dataPEM$shutDownReason)
wearingCompliance <- rep(NA, length(flow))
validityWearingComplianceValidation <- rep(0, length(flow))

###########################################
# READ THE TOP OF THE FILE
###########################################
participantID <- read.csv(path, skip = 7, header = FALSE, nrow = 1)[1, 2]
if (is.na(participantID)) {
  participantID <- path
}


downloadDate <- read.csv(path, skip = 1,
                             header = FALSE,
                             nrow = 1, stringsAsFactors=FALSE)[1, 2]

totalDownloadTime <- read.csv(path, skip = 2,
                              header = FALSE,
                              nrow = 1, stringsAsFactors=FALSE)[1, 2]

deviceSerial <- read.csv(path, skip = 4,
                         header = FALSE,
                         nrow = 1, stringsAsFactors=FALSE)[1, 2]

dateTimeHardware <- read.csv(path,skip = 5,
                              header = FALSE,
                              nrow = 1, stringsAsFactors=FALSE)[1, 2]

dateTimeSoftware <- read.csv(path, skip = 6,
                            header = FALSE,
                            nrow = 1, stringsAsFactors=FALSE)[1, 2]

version <- read.csv(path, skip = 6, header = FALSE,
                    nrow = 1, stringsAsFactors=FALSE)[1, 3]

filterID <- as.character(read.csv(path, skip = 8,
                              header = FALSE,
                              nrow = 1, stringsAsFactors=FALSE)[1, 2])

participantWeight <- read.csv(path, skip = 9,
                              header = FALSE,
                              nrow = 1, stringsAsFactors=FALSE)[1, 2]

inletAerosolSize <- read.csv(path, skip = 10,
                             header = FALSE,
                             nrow = 1, stringsAsFactors=FALSE)[1, 2]

laserCyclingVariablesDelay <- read.csv(path, skip = 11,
                                       header = FALSE,
                                       nrow = 1, stringsAsFactors=FALSE)[1, 2]

laserCyclingVariablesSamplingTime <- read.csv(path, skip = 11,
                                              header = FALSE,
                                              nrow = 1, stringsAsFactors=FALSE)[1, 3]

laserCyclingVariablesOffTime <- read.csv(path, skip = 11,
                                         header = FALSE,
                                         nrow = 1, stringsAsFactors=FALSE)[1, 4]

SystemTimes <- paste0(read.csv(path, skip = 12,
                               header = FALSE,
                               nrow = 1, stringsAsFactors=FALSE)[1, 2],
                      read.csv(path, skip = 12,
                               header = FALSE,
                               nrow = 1, stringsAsFactors=FALSE)[1, 3])

tempTable <- as.data.frame(read.csv(path, skip = 14,
                      header = FALSE, nrow = 10))
if(ncol(tempTable)==6){
  tempTable[6,7] = tempTable[7,1]
  tempTable = tempTable[-7,]
}

tempTable <- cbind(tempTable[, 2:ncol(tempTable)],
                   rep(NA, nrow(tempTable)))
nephelometerSlope <- tempTable[1, 1]
nephelometerOffset <- tempTable[1, 2]
nephelometerLogInterval <- tempTable[1, 3]
temperatureSlope <- tempTable[2, 1]
temperatureOffset <- tempTable[2, 2]
temperatureLog <- tempTable[2, 3]
humiditySlope <- tempTable[3, 1]
humidityOffset <- tempTable[3, 2]
humidityLog <- tempTable[3, 3]
inletPressureSlope <- tempTable[4, 1]
inletPressureOffset <- tempTable[4, 2]
inletPressureLog <- tempTable[4, 3]
inletPressureHighTarget <- tempTable[4, 4]
inletPressureLowTarget <- tempTable[4, 5]
orificePressureSlope <- tempTable[5, 1]
orificePressureOffset <- tempTable[5, 2]
orificePressureLog <- tempTable[5, 3]
orificePressureHighTarget <- tempTable[5, 4]
orificePressureLowTarget <- tempTable[5, 5]
flowLog <- tempTable[6, 3]
flowHighTarget <- tempTable[6, 4]
flowLowTarget <- tempTable[6, 5]
flowRate <- tempTable[6, 6]
accelerometerLog <- tempTable[7, 3]
batteryLog <- tempTable[8, 3]
ventilationSlope <- tempTable[9, 1]
ventilationOffset <- tempTable[9, 2]

###########################################
# control table
###########################################
control <- data.frame(downloadDate = downloadDate,
                      totalDownloadTime = totalDownloadTime,
                      deviceSerial = deviceSerial,
                      dateTimeHardware = dateTimeHardware,
                      dateTimeSoftware = dateTimeSoftware,
                      version = version,
                      participantID = participantID,
                      filterID = filterID,
                      participantWeight = participantWeight,
                      inletAerosolSize = inletAerosolSize,
                      laserCyclingVariablesDelay =
                        laserCyclingVariablesDelay,
                      laserCyclingVariablesSamplingTime =
                        laserCyclingVariablesSamplingTime,
                      laserCyclingVariablesOffTime =
                        laserCyclingVariablesOffTime,
                      SystemTimes = SystemTimes,
                      nephelometerSlope = nephelometerSlope,
                      nephelometerOffset = nephelometerOffset,
                      nephelometerLogInterval =
                        nephelometerLogInterval,
                      temperatureSlope = temperatureSlope,
                      temperatureOffset = temperatureOffset,
                      temperatureLog = temperatureLog,
                      humiditySlope = humiditySlope,
                      humidityOffset = humidityOffset,
                      humidityLog = humidityLog,
                      inletPressureSlope =
                        inletPressureSlope,
                      inletPressureOffset =
                        inletPressureOffset,
                      inletPressureLog = inletPressureLog,
                      inletPressureHighTarget =
                        inletPressureHighTarget,
                      inletPressureLowTarget =
                        inletPressureLowTarget,
                      orificePressureSlope =
                        orificePressureSlope,
                      orificePressureOffset =
                        orificePressureOffset,
                      orificePressureLog =
                        orificePressureLog,
                      orificePressureHighTarget =
                        orificePressureHighTarget,
                      orificePressureLowTarget =
                        orificePressureLowTarget,
                      flowLog = flowLog,
                      flowHighTarget = flowHighTarget,
                      flowLowTarget = flowLowTarget,
                      flowRate = flowRate,
                      accelerometerLog = accelerometerLog,
                      batteryLog = batteryLog,
                      ventilationSlope = ventilationSlope,
                      ventilationOffset = ventilationOffset)
control <- dplyr::tbl_df(control)

###########################################
# CREATE THE OBJECT
###########################################

measures <- data.frame(timeDate = timeDate,
                       nephelometer = nephelometer,
                       temperature = temperature,
                       relativeHumidity = relativeHumidity,
                       battery = battery,
                       orificePressure = orificePressure,
                       inletPressure = inletPressure,
                       flow = flow,
                       xAxis = xAxis,
                       yAxis = yAxis,
                       zAxis = zAxis,
                       vectorSum = vectorSum,
                       shutDownReason = shutDownReason,
                       wearingCompliance = wearingCompliance,
                       validityWearingComplianceValidation =
                         validityWearingComplianceValidation,
                       originalDateTime = originalDateTime)

measures <- dplyr::tbl_df(measures)
measures <- measures %>%
  mutate_(shutDownReason = quote(as.character(shutDownReason))) %>%
  mutate_(originalDateTime = quote(as.character(originalDateTime)))

#microPEMObject <- MicroPEM$new(control = control,
#                               calibration = list(NA),
#                               measures = measures,
#                               original = TRUE)

microPEMObject <-list(control = control,
                      calibration = list(NA),
                      measures = measures,
                      original = TRUE)

return(microPEMObject)
}

###################################CHECK MICROPEM SETTINGS################################
QualityControl = NULL                           # create an empty data frame to store MicroPEM setting and basic summary information
# each row in QualityControl will represent one microPEM session
  
for(k in 1:nrow(MicroPEMfiles)){ 
  Data1 = convertOutput(as.character(MicroPEMfiles$Datafiles[k]))             # use the defined function convertOutput to readin MicroPEM raw file
  Data2 = Data1$measures                                                      # extract all time-varying variables 
  Data3 = Data2[!is.na(Data2$nephelometer),]                                  # remove rows without nephelometer reading
  Data4 = as.data.frame(Data1$control, stringsAsFactors=FALSE)                # extract microPEM setting of the sample
  
  # add start time, endt ime, mininum time, maximum time, mean, minimum and maximum neph reading into Data 4
  Data4$starttime = Data3$timeDate[1]                                        
  Data4$endtime = Data3$timeDate[nrow(Data3)]
  Data4$mintime = sort(Data3$timeDate, decreasing =F, na.rm=T)[1]
  Data4$maxtime = sort(Data3$timeDate, decreasing =T, na.rm=T)[1]
  Data4$mean = mean(Data3$nephelometer)
  Data4$min = min(Data3$nephelometer)
  Data4$max = max(Data3$nephelometer)
  
  # add warning meassages into Data 4 
  Data5 = Data2[Data2$timeDate>=Data4$starttime & Data2$timeDate<=(Data4$endtime-200),]
  Data4$startbutton = ifelse(nrow(Data5)==0, NA, sum(sapply(Data5$shutDownReason, match,"Start button", nomatch=0)))
  Data4$button1 = ifelse(nrow(Data5)==0, NA, sum(sapply(Data5$shutDownReason, match,"Button 1 pressed", nomatch=0)))
  Data4$button2 = ifelse(nrow(Data5)==0, NA, sum(sapply(Data5$shutDownReason, match,"Button 2 pressed", nomatch=0)))
  Data4$lowbattery = sum(sapply(Data2$shutDownReason, match,"Low Battery Stop", nomatch=0))
  Data4$deadbattery = sum(sapply(Data2$shutDownReason, match,"Battery dead", nomatch=0))
  
  # find the number of time jump back occurance and add into Data 4
  Data3$timeDate1 = as.POSIXlt(c(Data3$timeDate[-1], (Data3$timeDate[length(Data3$timeDate)]+1)), tz="GMT")  
  Data3$timediff = Data3$timeDate1 - Data3$timeDate
  Data4$timeerror = length(which(Data3$timediff<0))
  
  # add Data 4 into the data frame of MicroPEM setting and basic summary information
  QualityControl = rbind(QualityControl, Data4)
  
  # print loop progress
  if(round(k/50)*50==k)               
    print(k)   
}

#######correct Filterid typo
QualityControl$filterID = as.character(QualityControl$filterID) # 
QualityControl$filterID[QualityControl$filterID== "KH00123"] = "KHC0123"              
QualityControl$filterID[QualityControl$filterID== "KHC3392"] = "KHC0392" 
QualityControl$filterID[QualityControl$filterID== "LHC0232"] = "KHC0232"
unique(sort(QualityControl$filterID))       #check unique filterID

#######correct MicroPEMid typo
QualityControl$deviceSerial = as.character(QualityControl$deviceSerial)                                  
QualityControl$deviceSerial[QualityControl$deviceSerial=="UGF32/2012"] = "UGF320415N" 
QualityControl$deviceSerial[QualityControl$deviceSerial=="UGF320"]="UGF320429N"       
QualityControl$deviceSerial[QualityControl$deviceSerial=="UGF320429"]="UGF320429N"
QualityControl$deviceSerial[QualityControl$deviceSerial=="UFF320401N"]="UGF320401N"
QualityControl$deviceSerial[QualityControl$deviceSerial=="UGF220414N"]="UGF320414N"
QualityControl$deviceSerial[QualityControl$deviceSerial=="UGF220486N"]="UGF320486N"
QualityControl$deviceSerial[QualityControl$deviceSerial=="UGF300422N"]="UGF320422N"
QualityControl$deviceSerial[QualityControl$deviceSerial=="UGF300444N"]="UGF320444N"
QualityControl$deviceSerial[QualityControl$deviceSerial=="UGF300599N"]="UGF320599N"
QualityControl$deviceSerial[QualityControl$deviceSerial=="UG3320463N"]="UGF320463N"
unique(sort(QualityControl$deviceSerial)) 

########make sure the class of filepath is character##################
QualityControl$participantID = as.character(QualityControl$participantID) 

######### drop duplicated files which have wrong MicroPEM settings (those files have been reprocessed via MicroPEM Docking Station)
QualityControl = QualityControl[as.numeric(QualityControl$nephelometerSlope)>=3,]           #  correct nephelometerSlope is 3     
QualityControl = QualityControl[as.numeric(QualityControl$humiditySlope)<=1 & as.numeric(QualityControl$humiditySlope)>0,]   # correct humiditySlope is 1
QualityControl = QualityControl[as.numeric(QualityControl$humidityOffset)>-8 & as.numeric(QualityControl$humidityOffset)<10,]  # correct humidityOffset is between -5 and 5

##################################CORRECT WRONG DATETIME##########################
  # correcting errors that Zheng identified manually (by inspection)
  
QualityControl$starttime_new = QualityControl$starttime
QualityControl$endtime_new = QualityControl$endtime

QualityControl$endtime_new[QualityControl$filterID=="KHC0221"] = mdy_hms("12/6/2013  9:24:00", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC0245"] = mdy_hms("12/12/2013  22:35:30", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC0268"] = mdy_hms("12/20/2013  8:35:30", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC0514"] = mdy_hms("5/22/2014  14:57:30", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC0679"] = mdy_hms("6/25/2014  23:15:11", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC0867"] = mdy_hms("9/18/2014  16:32:20", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC0936"] = mdy_hms("10/9/2014  15:20:00", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC1042"] = mdy_hms("12/18/2014  13:24:30", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC1222"] = mdy_hms("2/15/2015  7:52:30", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC1274"] = mdy_hms("2/21/2015 20:27:30", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC1499"] = mdy_hms("4/21/2015  16:20:15", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC1976"] = mdy_hms("1/26/2016  18:01:30", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHCD111B"] = mdy_hms("6/16/2015  8:04:30", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHCD68C"] = mdy_hms("11/8/2014  9:20:30", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHCD86A"] = mdy_hms("11/11/2014  11:22:30", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="GN058"] = mdy_hms("4/1/2014  11:10:10", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="GN084"] = mdy_hms("4/11/2014  10:31:10", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="GN106"] = mdy_hms("4/15/2014  11:14:10", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHC0095"] = mdy_hms("10/30/2013  5:52:19", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHC0181"] = mdy_hms("11/21/2013  8:59:15", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHC0271"] = mdy_hms("2/16/2013  16:15:05", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHC0365"] = mdy_hms("2/21/2014  11:49:40", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHC0566"] = mdy_hms("5/29/2014  12:02:40", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC0575"] = mdy_hms("6/5/2014  6:07:20", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC0661"] = mdy_hms("6/19/2014  14:51:15", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC1018"] = mdy_hms("12/14/2014 9:13:20", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC1021"] = mdy_hms("12/14/2014 6:51:20", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC1054"] = mdy_hms("12/20/2014 8:46:20", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC1080"] = mdy_hms("12/25/2014 8:06:15", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC1092"] = mdy_hms("12/31/2014 13:46:27", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHC1141"] = mdy_hms("1/30/2015  7:56:00", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC1335"] = mdy_hms("3/6/2015 10:36:09", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHC1821"] = mdy_hms("9/28/2015 11:40:50", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHC1938"] = mdy_hms("12/31/2015 11:00:00", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHCD04A"] = mdy_hms("8/3/2014  10:34:15", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHCD11C"] = mdy_hms("8/22/2014  6:30:19", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHCD77B"] = mdy_hms("11/14/2014  12:28:10", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHCD78B"] = mdy_hms("11/14/2014  11:30:10", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHCD93C"] = mdy_hms("12/18/2014  16:37:20", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC0915"] = mdy_hms("10/3/2014  6:52:30", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHC1144"] = mdy_hms("1/31/2015  9:04:40", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHC1542"] = mdy_hms("5/4/2015  10:07:50", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHC1555"] = mdy_hms("5/5/2015  8:57:50", tz="GMT")
QualityControl$starttime_new[QualityControl$filterID=="KHC1674"] = mdy_hms("7/23/2015  8:44:20", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC1794"] = mdy_hms("9/17/2015  6:00:00", tz="GMT")
QualityControl$endtime_new[QualityControl$filterID=="KHC0123"] = mdy_hms("11/11/2013  7:01:30", tz="GMT")                    
QualityControl$endtime_new[QualityControl$filterID=="KHC0266"] = mdy_hms("12/20/13 8:13:30", tz="GMT")                        
QualityControl$starttime_new[QualityControl$filterID=="KHC0356"] = mdy_hms("2/19/2014  10:10:25", tz="GMT")                   
QualityControl$starttime_new[QualityControl$filterID=="KHC0429"] = mdy_hms("4/23/2014  10:45:05", tz="GMT")                   
QualityControl$starttime_new[QualityControl$filterID=="KHC0526"] = mdy_hms("5/21/2014  9:43:35", tz="GMT")                   
QualityControl$endtime_new[QualityControl$filterID=="KHC1273"] = mdy_hms("2/22/15 10:06:30", tz="GMT")                        
QualityControl$starttime_new[QualityControl$filterID=="KHC1540"] = mdy_hms("6/24/2015 7:57:10", tz="GMT")                    
QualityControl$starttime_new[QualityControl$filterID=="KHC1566"] = mdy_hms("5/8/2015 7:38:09", tz="GMT")                     
QualityControl$endtime_new[QualityControl$filterID=="KHC1964"] = mdy_hms("1/22/2016  14:23:30", tz="GMT")                     
QualityControl$starttime_new[QualityControl$filterID=="KHC2022"] = mdy_hms("2/24/2016 14:22:47", tz="GMT")                  
QualityControl$endtime_new[QualityControl$filterID=="KHCD07A"] = mdy_hms("8/3/14 10:05:30", tz="GMT")                    
QualityControl$starttime_new[QualityControl$filterID=="KHCD44A"] = mdy_hms("10/16/2014  10:30:00", tz="GMT")              

#######################################READ IN MICROPEM LOG DATA##################################
require(readstata13)
require(stringr)
require(lubridate)
# make sure the filepath is correct 
MicroPEM = read.dta13("/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/Data/Survey_Data/MicroPem.dta")

#add leading 0 to time variables if hour is a single digit (e.g. 825 to 0825)
MicroPEM$labsetdtt = str_pad(MicroPEM$labsetdtt, 4, pad = "0")         
MicroPEM$fieldsetdt = str_pad(MicroPEM$fieldsetdt, 4, pad = "0")
MicroPEM$thepaon1 = str_pad(MicroPEM$thepaon1, 4, pad = "0")
MicroPEM$pickupdtt = str_pad(MicroPEM$pickupdtt, 4, pad = "0")
MicroPEM$thepaon2 = str_pad(MicroPEM$thepaon2, 4, pad = "0")
MicroPEM$thepaoff2 = str_pad(MicroPEM$thepaoff2, 4, pad = "0")
MicroPEM$tupemoff = str_pad(MicroPEM$tupemoff, 4, pad = "0")

#assign NA to HEPA end times if Micorpem was not running when retrieving
MicroPEM$thepaon2[MicroPEM$thepaon2=="0000"|MicroPEM$thepaon2=="9999"]=NA
MicroPEM$thepaoff2[MicroPEM$thepaoff2=="0000"|MicroPEM$thepaoff2=="9999"]=NA

#HEPA start Datetime in logsheet
MicroPEM$HEPA1St = paste(MicroPEM$datevisit, MicroPEM$thepaon1)         
MicroPEM$HEPA1St = dmy_hm(as.character(MicroPEM$HEPA1St), tz="GMT")
range(MicroPEM$HEPA1St)

#HEPA end Datetime in logsheet
MicroPEM$HEPA2St = paste(MicroPEM$pickupdtd, MicroPEM$thepaon2)         
MicroPEM$HEPA2St = dmy_hm(as.character(MicroPEM$HEPA2St), tz="GMT")
which(is.na(MicroPEM$HEPA2St))
range(MicroPEM$HEPA2St, na.rm=T)

MicroPEM$HEPA2End = paste(MicroPEM$pickupdtd, MicroPEM$thepaoff2)         
MicroPEM$HEPA2End = dmy_hm(as.character(MicroPEM$HEPA2End), tz="GMT")
which(is.na(MicroPEM$HEPA2End))
range(MicroPEM$HEPA2End, na.rm=T)

#correct a filterid typo
MicroPEM$filterid[which(is.na(MicroPEM$mstudyid))]   
which(MicroPEM$filterid=="KHC031B")
MicroPEM$filterid[MicroPEM$filterid=="KHC031B"] = "KHCD31B"           

##################################IDENTIFY HEPA CHANGEPOINT###################################
  # generate plots of nephalometer time series for visual inspection (plots first 100 and last 100 observations)
  # note use of package "changepoint"
require(changepoint)
QualityControl1 = QualityControl[order(QualityControl$filterID),]     # sort observation by filterID

HEPAdata = NULL     # creat an empty data frame to store HEPA information

# Define a directory to output figures pdf
plotdirectory <- "/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/MicroPEM_Data/"    
pdf(file = paste0(plotdirectory, "HEPAplot",".pdf"), height = 8, width = 8)
par(mfrow = c(2,2))
par(mar=c(3,3,3,1))

for(k in 1:nrow(QualityControl1)){ 
  Data1 = convertOutput(QualityControl1$participantID[k])
  Data2 = Data1$measures
  Data3 = Data2[!is.na(Data2$nephelometer),]
  Data4 = QualityControl1[k,]
  
  Data5 = Data3[Data3$timeDate>=(Data4$starttime_new)&Data3$timeDate<=(Data4$endtime_new),]    # extract readings between starttime and endtime
  Data5 = Data5[Data5$relativeHumidity>0 & Data5$relativeHumidity<100,]           # drop measurements with RH <=0 or RH>=100
  # find the starttime in the logsheet
  Data4$startlog = ifelse(length(MicroPEM$thepaon1[MicroPEM$filterid==as.character(Data4$filterID)])==0, 
                                  NA, MicroPEM$thepaon1[MicroPEM$filterid==as.character(Data4$filterID)])
  # find the endtime in the logsheet
  Data4$endlog = ifelse(length(MicroPEM$tupemoff[MicroPEM$filterid==as.character(Data4$filterID)])==0, 
                                  NA, MicroPEM$tupemoff[MicroPEM_Compound$filterid==as.character(Data4$filterID)])
  Data4$nephelometer_avg = mean(Data5$nephelometer)   # add mean nephelometer reading
  
  # find the point when nephelometer reading changed significantly at the start
  Data6 = Data5[7:100,]   # exclude the first 6 readings (about 1 minute)
  HEPASt = cpt.meanvar(Data6$nephelometer,method="BinSeg", Q=3, minseglen=8)
  Data4$HEPAstnumber = HEPASt@cpts[1]        # identify the place where changepoint is
  if(Data4$HEPAstnumber==94){                # if there is no changepoint in the first 100 readings then no start HEPA
    Data4$HEPAsttime1 = NA
    Data4$HEPAsttime2 = NA
    Data4$HEPAstvalue1 = NA
    Data4$HEPAstvalue2 = NA
  } else {
    Data4$HEPAsttime1 = Data5$timeDate[7]              # the starttime of start HEPA
    Data4$HEPAsttime2 = Data5$timeDate[Data4$HEPAstnumber+6]   # the endtime of start HEPA
    # the mean nephelometer reading in start HEPA, excluding max and min readings
    Data4$HEPAstvalue1 = mean(Data6$nephelometer[1:Data4$HEPAstnumber], trim=1/Data4$HEPAstnumber) 
    # the mean nephelometer reading after start HEPA period (within the first 100 readings)
    Data4$HEPAstvalue2 = mean(Data6$nephelometer[(Data4$HEPAstnumber+1):94])
  }
  # plot the first 100 readings and show the changepoint
  title <- paste(Data4$filterID, Data4$deviceSerial, format(Data4$starttime_new, format = "%b %d %Y"), "StHEPA=", Data4$HEPAstnumber,
                 "\n",  Data4$starttime_new, "startlog=", Data4$startlog)
  plot(HEPASt,cpt.width=3)  
  title(main = title,  cex.main = 0.7, col.main = "black") 
  
  # find the point when nephelometer reading changed significantly at the end
  Data7 = Data5[(nrow(Data5)-3):(nrow(Data5)-99),]     # exclude the last 3 readings (about 1 minute)
  HEPAEnd = cpt.meanvar(Data7$nephelometer,method="BinSeg", Q=3, minseglen=8)
  Data4$HEPAendnumber = HEPAEnd@cpts[1]        # identify the place where changepoint is
  if(Data4$HEPAendnumber==97){                 # if there is no changepoint in the last 100 readings then no end HEPA
    Data4$HEPAendtime1 = NA
    Data4$HEPAendtime2 = NA
    Data4$HEPAendvalue1 = NA
    Data4$HEPAendvalue2 = NA
  } else {
    Data4$HEPAendtime1 = Data5$timeDate[nrow(Data5)-2-Data4$HEPAendnumber]   # the starttime of end HEPA
    Data4$HEPAendtime2 = Data5$timeDate[nrow(Data5)-3]                       # the endtime of end HEPA
    # the mean nephelometer reading in end HEPA, excluding max and min readings
    Data4$HEPAendvalue1 = mean(Data7$nephelometer[1:Data4$HEPAendnumber], trim=1/Data4$HEPAendnumber)
    # the mean nephelometer reading before end HEPA period (within the last 100 readings)
    Data4$HEPAendvalue2 = mean(Data7$nephelometer[(Data4$HEPAendnumber+1):97])
  }
  # plot the last 100 readings and show the changepoint
  title <- paste(Data4$filterID, Data4$deviceSerial, format(Data4$starttime_new, format = "%b %d %Y"), "EndHEPA=", Data4$HEPAendnumber,
                 "\n",  Data4$Endtime_new,"endlog=", Data4$endlog, "lowbat=", Data4$lowbattery, "deadbat=", Data4$deadbattery)
  plot(HEPAEnd,cpt.width=3) #plot end HEPA
  title(main = title,  cex.main = 0.7, col.main = "black") 
  
  HEPAdata = rbind(HEPAdata, Data4)        # update HEPAdata data frame
  
  if(round(k/50)*50==k)               
    print(k) 
}
dev.off()

##################################HEPA PERIOD IDENTIFICATION#########################
  # To generate HEPAtime.rds, Zheng visually inspected the plots crated by cpt.meanvar -- if he detected errors, he manually updated HEPAdata, then renamed HEPAdata as HEPAtime.rds 
# make sure the filepath is correct; note that this data file is also availble in the github directory.
HEPAtime = readRDS("/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/MicroPEM_Data/HEPAtime.rds") 
# convert character into datetime format
HEPAtime$HEPAsttime1 = mdy_hms(HEPAtime$HEPAsttime1,tz="GMT")
HEPAtime$HEPAsttime2 = mdy_hms(HEPAtime$HEPAsttime2,tz="GMT")
HEPAtime$HEPAendtime1 = mdy_hms(HEPAtime$HEPAendtime1,tz="GMT")
HEPAtime$HEPAendtime2 = mdy_hms(HEPAtime$HEPAendtime2,tz="GMT")  
  
#################################NEPHELOMETER SAMPLE SUMMARY######################################
QualityControl2 = merge(QualityControl1, HEPAtime, by="filterID", all=T)  # merge MicroPEM settings with correct HEPA datetimes

Nephelometer = NULL # create an empty data frame to store Nephelometer summary statistics

# Define a directory to output figures pdf
plotdirectory <- "/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/MicroPEM_Data/"
pdf(file = paste0(plotdirectory, "ValidationPlot",".pdf"), height = 8, width = 8)
par(mfrow = c(3,3))
par(mar=c(2,2,3,1))
# loop over microPEM files to apply HEPA correction
for(k in 1:nrow(QualityControl2)){ 
  Data1 = convertOutput(QualityControl2$participantID[k])
  Data2 = Data1$measures
  Data4 = QualityControl2[k,]
  
  # create a variable for startdate of sampling and correct some MicroPEM system time errors
  Data4$Startdate = as.Date(Data4$starttime_new)
  Data4$Startdate[Data4$filterID=="KHC0271"] = as.Date("2014-01-01", TZ="GMT")  
  Data4$Startdate[Data4$filterID=="KHC0272"] = as.Date("2014-01-01", TZ="GMT")
  Data4$Startdate[Data4$filterID=="KHC0273"] = as.Date("2014-01-01", TZ="GMT")
  Data4$Startdate[Data4$filterID=="KHC0274"] = as.Date("2014-01-01", TZ="GMT")
  Data4$Startdate[Data4$filterID=="KHC0277"] = as.Date("2014-01-01", TZ="GMT")
  Data4$Startdate[Data4$filterID=="KHC0278"] = as.Date("2014-01-01", TZ="GMT")
  Data4$Startdate[Data4$filterID=="KHC0279"] = as.Date("2014-01-01", TZ="GMT")
  Data4$Startdate[Data4$filterID=="KHC0280"] = as.Date("2014-01-01", TZ="GMT")
  Data4$Startdate[Data4$filterID=="KHC0281"] = as.Date("2014-01-01", TZ="GMT")
  Data4$Startdate[Data4$filterID=="KHC0398"] = as.Date("2014-03-17", TZ="GMT")
  Data4$Startdate[Data4$filterID=="KHC0967"] = as.Date("2014-11-14", TZ="GMT")
  Data4$Startdate[Data4$filterID=="KHC1269"] = as.Date("2015-02-19", TZ="GMT")
  Data4$Startdate[Data4$filterID=="KHC1867"] = as.Date("2015-10-20", TZ="GMT")
  Data4$Startdate[Data4$filterID=="KHC1881"] = as.Date("2015-10-28", TZ="GMT")
  Data4$Startdate[Data4$filterID=="KHCD103A"] = as.Date("2015-06-01", TZ="GMT")   
  Data4$Startdate[Data4$filterID=="KHCD103B"] = as.Date("2015-06-11", TZ="GMT") 
  
  Data3 = Data2[Data2$timeDate>=(Data4$starttime_new)&Data2$timeDate<=(Data4$endtime_new),]      #rdata between start and end time of measurement
  Data5 = Data3[!is.na(Data3$nephelometer),]                                      #drop rows without nephelometer readings
  Data6 = Data5[Data5$relativeHumidity>0 & Data5$relativeHumidity<100,]           #drop measurement with RH <=0 or RH>=100

  if(!is.na(Data4$HEPAsttime1) & !is.na(Data4$HEPAendtime1)){                     #calculate HEPA correction values
    Start = Data6[Data6$timeDate>=(Data4$HEPAsttime1)&Data6$timeDate<=(Data4$HEPAsttime2),]         #start HEPA readings
    End = Data6[Data6$timeDate>=(Data4$HEPAendtime1)&Data6$timeDate<=(Data4$HEPAendtime2),]         #end HEPA readings
    Data4$HEPAstnumber = nrow(Start)              #number of readings in start HEPA                             
    Data4$HEPAendnumber = nrow(End)               #number of readings in end HEPA
    Data4$HEPASt = mean(Start$nephelometer, trim=1/Data4$HEPAstnumber)         #average reading in start HEPA excluding max and min readings
    Data4$HEPAEnd = mean(End$nephelometer, trim=1/Data4$HEPAendnumber)         #average reading in end HEPA excluding max and min readings
    # Apply HEPA correction to all nephelometer readings
    Data6$nephelometer_corr = Data6$nephelometer - seq(Data4$HEPASt, Data4$HEPAEnd, (Data4$HEPAEnd-Data4$HEPASt)/(length(Data6$nephelometer)-1))       
  } else if (!is.na(Data4$HEPAsttime1) & is.na(Data4$HEPAendtime1)) {          #if no start HEPA, then only end HEPA is used for correction
    Start = Data6[Data6$timeDate>=(Data4$HEPAsttime1)&Data6$timeDate<=(Data4$HEPAsttime2),]
    End = NA
    Data4$HEPAstnumber = nrow(Start)
    Data4$HEPAendnumber = NA
    Data4$HEPASt = mean(Start$nephelometer, trim=1/Data4$HEPAstnumber)
    Data4$HEPAEnd = NA
    Data6$nephelometer_corr = Data6$nephelometer - Data4$HEPASt
  } else if (is.na(Data4$HEPAsttime1) & !is.na(Data4$HEPAendtime1)) {          #if no end HEPA, then only start HEPA is used for correction
    Start = NA
    End = Data6[Data6$timeDate>=(Data4$HEPAendtime1)&Data6$timeDate<=(Data4$HEPAendtime2),]
    Data4$HEPAstnumber = NA
    Data4$HEPAendnumber = nrow(End)
    Data4$HEPASt = NA
    Data4$HEPAEnd = mean(End$nephelometer, trim=1/Data4$HEPAendnumber)
    Data6$nephelometer_corr = Data6$nephelometer - Data4$HEPAEnd
  } else if (is.na(Data4$HEPAsttime1) & is.na(Data4$HEPAendtime1)) {           #if no start and end HEPA, then no correction
    Data4$HEPAstnumber = NA
    Data4$HEPAendnumber = NA
    Data4$HEPASt = NA
    Data4$HEPAEnd = NA
    Data6$nephelometer_corr = Data6$nephelometer
  }
  
  Data6$unique_min <- floor_date(Data6$timeDate, unit = "minute")          #get minute from datetime
  Data4$Duration = length(unique(Data6$unique_min))/60                     #duration of measurement
  #average of raw and HEPA-corrected nephelometer readings  
  Data7 = ddply(Data6, .(unique_min), summarise,                           
                nephelometer_min = mean(nephelometer), 
                nephelometer_corr_min = mean(nephelometer_corr))
  
  Data4$nephelometer_avg = mean(Data7$nephelometer_min) 
  Data4$nephelometer_corr_avg = mean(Data7$nephelometer_corr_min)
  
  Data8 = Data3[!is.na(Data3$flow),]                                       #flow rate data
  Data8$flow_cor = Data8$flow 
  if(Data4$Startdate < as.Date(mdy("04/03/2014"))) {                       #adjust the data with flow meter backward issue
    Data8$flow_cor = Data8$flow*0.8211 - 0.0139                             # manual correction from Steve Chillrud
  }     
  Data8 = Data8[Data8$shutDownReason!="Flow blocked 1",]                   #drop rows with flow blocked note
  Data8$unique_min <- floor_date(Data8$timeDate, unit = "minute")          
  
  Data9 = ddply(Data8, .(unique_min), summarise,                           #get minute flow rate
                Flow = round(max(as.numeric(flow_cor), na.rm=TRUE), digits = 3))
  
  Data4$vol = sum(Data9$Flow, na.rm=T)/2                                   #calculate total air volume of sampling
  Data4$vol[levels(Data4$filterID)=="KHC0100"] = sum(Data9$Flow, na.rm=T)  #correct volume for KHC0100 which is always on
  Data4$flow.avg = mean(Data9$Flow, na.rm=T)
  Data4$flow.sd = sd(Data9$Flow, na.rm=T)
  Data4$flow.min = min(Data9$Flow, na.rm=T)
  Data4$flow.max = max(Data9$Flow, na.rm=T)
  # percent of time that flow rate is between 0.28 and 0.55 LPM
  Data4$flow28.good = sum(ifelse(Data9$Flow<= 0.55 & Data9$Flow >= 0.28, 1, 0), na.rm=T)/nrow(Data9) 
  # percent of time that flow rate is between 0.30 and 0.55 LPM
  Data4$flow30.good = sum(ifelse(Data9$Flow<= 0.55 & Data9$Flow >= 0.30, 1, 0), na.rm=T)/nrow(Data9)
  Data4$Negative1 = length(which(Data6$nephelometer< (-10)))/nrow(Data6)        #calculate percent of negative readings in raw data
  Data4$Negative2 = length(which(Data6$nephelometer_corr< (-10)))/nrow(Data6)   #calculate percent of negative readings in HEPA-corrected data
  
  Nephelometer = rbind(Nephelometer, Data4)  # update Nephelometer data frame
  
  #plots of nephelometer readings which are used for visual validation
  title <- paste(Data4$filterID, Data4$deviceSerial, format(Data4$Startdate, format = "%b %d %Y"), "Duration=", round(Data4$Duration), 
                 "\n StHEPA=", round(Data4$HEPASt), "EndHEPA=",round(Data4$HEPAEnd), "mean=", round(Data4$nephelometer_avg),  "Adj_m=", round(Data4$nephelometer_corr_avg), 
                 "\n Neg1=", round(Data4$Negative1, digit=3), "Neg2=", round(Data4$Negative2, digit=3))
  plot(Data6$timeDate, Data6$nephelometer, type = "l",  main = "" , xlab="", ylab = "", ylim=c(-100,500), lwd = 2, col = "blue")
  abline(h=0, col="grey")
  if (Data4$Negative2>= 0.2) lines(Data6$timeDate, Data6$nephelometer_corr, col = alpha("red",0.6), lwd = 2)
  if (Data4$Negative2< 0.2) lines(Data6$timeDate, Data6$nephelometer_corr, col = alpha("black", 0.6), lwd = 2)
  
  if (Data4$Negative2>= 0.2) title(main = title,  cex.main = 0.7, col.main = "red") 
  if (Data4$Negative2< 0.2) title(main = title,  cex.main = 0.7, col.main = "black") 
  
  if(round(k/50)*50==k)               
    print(k) 
}
dev.off()   
  
##################################NEPHELOMETER VALIDATION#########################
# read in validation index of nephelometer data; also available in the github directory
Validation = read.csv("/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/MicroPEM_Data/VisualizedValidation.csv", header=TRUE)    
# this contains the results of Zheng's visual inspection of the data.    
table(Validation$Validity)    # frequency of visual validity

# merge nephelometer summary statistics with visual validation index
Nephelometer1 = merge(Nephelometer, Validation , by="filterID", all=T) 
# create an indicator for Harmattan
Nephelometer1$Harmattan = 0                                                            
Nephelometer1$Harmattan[Nephelometer1$Note=="elevated baseline"] = 1

# drop samples with invalid nephelometer data
Nephelometer2 = Nephelometer1[Nephelometer1$Validity!=4,]
  
#################################GRAVIMETRIC SAMPLE DATA ########################################
# read in gravimetric PM data, make sure filepath is correct; also in github
GravimetricPM = read.csv("/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/MicroPEM_Data/FilterWeight.csv", header=TRUE,stringsAsFactors=FALSE)   

Damagedfilter = c("GN012", "GN016", "GN019", "GN020", "GN021", "GN022", "GN025", "GN028", "GN030", "GN031", "GN032", "GN033",
                  "GN034", "GN036", "GN044", "GN046", "GN052", "GN053", "GN054", "GN055", "GN057", "GN059", "GN060", "GN065", "GN067", "GN068",
                  "GN069", "GN076", "GN078", "GN091", "KHC0304", "KHC0305", "KHC0306", "KHC0307", "KHC0308", "KHC0310", "KHC0311", "KHC0312", "KHC0313", "KHC0325",
                  "KHC0332", "KHC0333", "KHC0365", "KHC0366", "KHC0367", "KHC0368", "KHC0369", "KHC0373", "KHC0375", "KHC0376", "KHC0377", "KHC0378", "KHC0379",
                  "KHC0380", "KHC0381", "KHC0382","KHC0383","KHC0384","KHC0385","KHC0386","KHC0387", "KHC0389", "KHC0390","KHC0391", "KHC0392", "KHC0393", "KHC0394",
                  "KHC0395", "KHC0396", "KHC0397", "KHC0398", "KHC0399", "KHC0463", "KHC1025", "KHC1026", "KHC1251","KHC1567")
                 
Outlier = c("KHC0353", "KHC0783", "KHC1000", "KHC1325", "KHC1864")

Unmatched = c("KHC0133", "KHC0163", "KHC0168", "KHC0203", "KHC0275", "KHC0276", "KHC0421", "KHC0452", "KHC0459", 
              "KHC0536", "KHC0625", "KHC0673", "KHC0679", "KHC0686", "KHC0816", "KHC0947", "KHC1005", "KHC1160", "KHC1173", "KHC1347", 
              "KHC1348", "KHC1373", "KHC1406", "KHC1432", "KHC1622", "KHC1623", "KHC1624", "KHC1625", "KHC1626", "KHC1627", "KHC1628", 
              "KHC1629", "KHC1630", "KHC1631", "KHC1652", "KHC1696", "KHC1720", "KHC1769", "KHC1771", "KHC1783", "KHC1833")

# create an index to categorize Gravimetric Sample
GravimetricPM$index = "GOOD"
GravimetricPM$index[GravimetricPM$filterID %in% Unmatched] = "Unmatched"
GravimetricPM$index[GravimetricPM$filterID %in% Outlier] = "Outlier"
GravimetricPM$index[GravimetricPM$filterID %in% Damagedfilter] = "Damaged" 

################################Merge Nephelometer and Gravimetric Data###################################
PM_Data = merge(Nephelometer2, GravimetricPM , by="filterID", all.x=T)    # merge valid nephelometer data with Gravimetric PM data

#create an index for gravimetric PM >22 hrs
PM_Data$duration_index = 1              
PM_Data$duration_index[PM_Data$Duration<22] = 0

#create an index for normal flow rate > 85% of time
PM_Data$flow_index = 1                
PM_Data$flow_index[PM_Data$flow28.good<0.85] = 0

PM_Data$PM = ((PM_Data$netmass-0.005)*1000)/(as.numeric(PM_Data$vol)/(1000))     # gravimetric concentration
PM_Data$CF =  PM_Data$PM/PM_Data$nephelometer_corr_avg       # gravimetric correction factor

# create an index for gravimetric correction factor, 0 if no or problematic gravimetric PM, short duration, or out-of-range flow rate
PM_Data$CF_index = 0              
PM_Data$CF_index[!is.na(PM_Data$index) & PM_Data$index=="GOOD" & PM_Data$duration_index == 1 & PM_Data$flow_index == 1] = 1

####################################PLOT GRAVIMETRIC VS NEPHELOMETER#######################################
#subset the dataset for CF estimation for each MicroPEM
PM_Data1 = PM_Data[!is.na(PM_Data$index) & PM_Data$index=="GOOD" & PM_Data$duration_index == 1 & PM_Data$flow_index == 1,]  
summary(PM_Data1$CF[PM_Data1$Harmattan==0])
summary(PM_Data1$CF[PM_Data1$Harmattan==1])

plotdirectory <- "/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/MicroPEM_Data/"
pdf(file = paste0(plotdirectory, "PMcomparison.pdf"), height = 8, width = 8)
par(mfrow = c(3,3))
par(mar=c(4,4,3,1))

for (i in 1:length(unique(as.character(PM_Data1$deviceSerial.x)))) {
  McPEMID = unique(as.character(PM_Data1$deviceSerial.x))[i]
  filesbySN <- PM_Data1[PM_Data1$deviceSerial.x==McPEMID,]
  filesbySN0 = filesbySN[filesbySN$Harmattan==0,]
  filesbySN1 = filesbySN[filesbySN$Harmattan==1,]
  reg = lm(filesbySN0$PM~filesbySN0$nephelometer_corr_avg+0)
  plot(filesbySN0$PM~filesbySN0$nephelometer_corr_avg, xlim=c(0,350), ylim=c(0,350), xlab=expression(paste("Nephelometer (", mu,"g/", m^{3},')')), 
       ylab=expression(paste("Gravimetric (", mu,"g/", m^{3},')')), col="blue")
  points(filesbySN1$PM~filesbySN1$nephelometer_corr_avg, col="red")
  abline(reg)
  title(main = McPEMID,  cex.main = 1) 
  legend('right', 'bottom', c(paste("H", "(", nrow(filesbySN1), ")"), paste("Non-H", "(", nrow(filesbySN0), ")")), col=c("red", "blue"), pch=1)
}
dev.off() 
    
##################################OBTAIN GRAVIMETRIC CORRECTION FACTOR#####################################   
# read in gravimetric correction factor for each MicroPEM device , make sure filepath is correct

GravimeticCF = read.csv("/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/MicroPEM_Data/GravimetricFactor.csv", header=TRUE)

# if there is a problem on gravimetric PM sample, then use device correction factor for the nephelometer readings
PM_Data$CF_new = PM_Data$CF
for (i in 1: nrow(PM_Data)){
  if (PM_Data$CF_index[i] == 0)  PM_Data$CF_new[i] = GravimeticCF$Ratio[GravimeticCF$MicroPEMID == as.character(PM_Data$deviceSerial.x[i])]
}

####################################DAILY PM AVERAGE DATA ###############################################
DailyPM_Data = NULL   # create an empty data frame to store Daily PM data

for (k in 1:nrow(PM_Data)) {
  Data1 = convertOutput(PM_Data$participantID[k])  
  Data2 = Data1$measures
  Data4 = PM_Data[k,]       # Get PM summary statistics from PM_Data 
  
  Data3 = Data2[Data2$timeDate>=(Data4$starttime_new)&Data2$timeDate<=(Data4$endtime_new),]    #extract data during the measurement time   
  Data5 = Data3[!is.na(Data3$nephelometer),]                                      #drop rows without nephelometer reading
  Data6 = Data5[Data5$relativeHumidity>0 & Data5$relativeHumidity<100,]           #drop measurement with RH <=0 or RH>=100
  
  # HEPA correction
  if(!is.na(Data4$HEPASt) & !is.na(Data4$HEPAEnd)) Data6$nephelometer_corr = Data6$nephelometer - seq(Data4$HEPASt, Data4$HEPAEnd, (Data4$HEPAEnd-Data4$HEPASt)/(length(Data6$nephelometer)-1))
  if(!is.na(Data4$HEPASt) & is.na(Data4$HEPAEnd)) Data6$nephelometer_corr = Data6$nephelometer - Data4$HEPASt
  if(is.na(Data4$HEPASt) & !is.na(Data4$HEPAEnd)) Data6$nephelometer_corr = Data6$nephelometer - Data4$HEPAEnd
  if(is.na(Data4$HEPASt) & is.na(Data4$HEPAEnd)) Data6$nephelometer_corr = Data6$nephelometer
  
  #minute nephelometer data
  Data6$unique_min <- floor_date(Data6$timeDate, unit = "minute")          
  Data7 = ddply(Data6, .(unique_min), summarise,
                nephelometer_min = mean(nephelometer), 
                nephelometer_corr_min = mean(nephelometer_corr))
  
  Data7$nephelometer_final_min = Data7$nephelometer_corr_min*Data4$CF_new
  Data4$nephelometer_avg = mean(Data7$nephelometer_min) 
  Data4$nephelometer_corr_avg = mean(Data7$nephelometer_corr_min)
  Data4$nephelometer_final_avg = mean(Data7$nephelometer_final_min)
  
  #minute accelerometer data  
  Data3$unique_min <- floor_date(Data3$timeDate, unit = "minute")          
  Data3$hours <- hour(Data3$timeDate)
  Data8 = ddply(Data3, .(unique_min), summarise,
                X.axis_mean = round(mean(as.numeric(xAxis), na.rm=TRUE), digits = 4), 
                Y.axis_mean = round(mean(as.numeric(yAxis), na.rm=TRUE), digits = 4), 
                Z.axis_mean = round(mean(as.numeric(zAxis), na.rm=TRUE), digits = 4), 
                Vector.Sum.Composite_mean = round(mean(as.numeric(vectorSum), na.rm=TRUE), digits = 4), 
                X.axis_SD = round(sd(xAxis, na.rm=TRUE), digits = 3), 
                Y.axis_SD = round(sd(yAxis, na.rm=TRUE), digits = 3), 
                Z.axis_SD = round(sd(zAxis, na.rm=TRUE), digits = 3), 
                Vector.Sum.Composite_SD = round(sd(vectorSum, na.rm=TRUE), digits = 3),
                hours = mean(hours)) 
  
  # define threshold and moving window for accelerometer data
  compliance_threshold = 0.01               
  window_width = 10                           
  
  # if no movement during the window (x minute centered in current minute), then the compliance in current minute is 0.
  Data8$sd_composite <- round(rollapply(Data8$Vector.Sum.Composite_mean, width=window_width,  FUN = sd, align = "right", na.rm = TRUE, fill = NA), digits=3)
  Data8$sd_X.axis <- round(rollapply(Data8$X.axis_mean, width=window_width,  FUN = sd, align = "right", na.rm = TRUE, fill = NA), digits=3)
  Data8$sd_Y.axis <- round(rollapply(Data8$Y.axis_mean, width=window_width,  FUN = sd, align = "right", na.rm = TRUE, fill = NA), digits=3)
  Data8$sd_Z.axis <- round(rollapply(Data8$Z.axis_mean, width=window_width,  FUN = sd, align = "right", na.rm = TRUE, fill = NA), digits=3)
  
  Data8$sd_above_threshold = ifelse(Data8$sd_X.axis > compliance_threshold|Data8$sd_Y.axis > compliance_threshold|Data8$sd_Z.axis > compliance_threshold, 1, 0)
  Data8$sd_X_above_threshold = ifelse(Data8$sd_X.axis > compliance_threshold, 1, 0)    
  Data8$sd_Y_above_threshold = ifelse(Data8$sd_Y.axis > compliance_threshold, 1, 0) 
  Data8$sd_Z_above_threshold = ifelse(Data8$sd_Z.axis > compliance_threshold, 1, 0)  
  
  wakehour = c(7:22)   # define wake hour is between 7 am and 10 pm
  
  # divide the sampling period into 24 hour blocks 
  Data4$PMday_1 = Data7$unique_min[1]
  Data4$PMday_2 = Data4$PMday_1 + 24*60*60
  Data4$PMday_3 = Data4$PMday_2 + 24*60*60
  Data4$PMday4 = Data4$PMday_3 + 24*60*60
  
  Data4$Day_1 = 1
  Data4$Day_2 = 2
  Data4$Day_3 = 3
  
  # average of uncorrected nephelometer reading in each 24 hour
  Data4$OldPM_1 = round(mean(Data7[Data7$unique_min>=Data4$PMday_1&Data7$unique_min<Data4$PMday_2,]$nephelometer_corr_min), digits=3)
  Data4$OldPM_2 = round(mean(Data7[Data7$unique_min>=Data4$PMday_2&Data7$unique_min<Data4$PMday_3,]$nephelometer_corr_min), digits=3)
  Data4$OldPM_3 = round(mean(Data7[Data7$unique_min>=Data4$PMday_3&Data7$unique_min<Data4$PMday4,]$nephelometer_corr_min), digits=3)
  
  # average of corrected nephelometer reading in each 24 hour
  Data4$CorPM_1 = round(mean(Data7[Data7$unique_min>=Data4$PMday_1&Data7$unique_min<Data4$PMday_2,]$nephelometer_final_min), digits=3)
  Data4$CorPM_2 = round(mean(Data7[Data7$unique_min>=Data4$PMday_2&Data7$unique_min<Data4$PMday_3,]$nephelometer_final_min), digits=3)
  Data4$CorPM_3 = round(mean(Data7[Data7$unique_min>=Data4$PMday_3&Data7$unique_min<Data4$PMday4,]$nephelometer_final_min), digits=3)
  
  # number of nephelometer reading in each 24 hour
  Data4$PMn_1 = nrow(Data7[Data7$unique_min>=Data4$PMday_1&Data7$unique_min<Data4$PMday_2,])
  Data4$PMn_2 = nrow(Data7[Data7$unique_min>=Data4$PMday_2&Data7$unique_min<Data4$PMday_3,])
  Data4$PMn_3 = nrow(Data7[Data7$unique_min>=Data4$PMday_3&Data7$unique_min<Data4$PMday4,])  
  
  # wearing compliance (% of time) in each 24 hour
  Data4$compliance_1 = sum(Data8[Data8$unique_min>=Data4$PMday_1 & Data8$unique_min<Data4$PMday_2,]$sd_above_threshold, na.rm=T)
  Data4$compliance_2 = sum(Data8[Data8$unique_min>=Data4$PMday_2 & Data8$unique_min<Data4$PMday_3,]$sd_above_threshold, na.rm=T)
  Data4$compliance_3 = sum(Data8[Data8$unique_min>=Data4$PMday_3 & Data8$unique_min<Data4$PMday4,]$sd_above_threshold, na.rm=T)
  
  # wearing compliance (% of wake time) in each 24 hour
  Data4$complianceWake_1 = sum(Data8[Data8$unique_min>=Data4$PMday_1 & Data8$unique_min<Data4$PMday_2 & Data8$hours %in% wakehour,]$sd_above_threshold, na.rm=T)
  Data4$complianceWake_2 = sum(Data8[Data8$unique_min>=Data4$PMday_2 & Data8$unique_min<Data4$PMday_3 & Data8$hours %in% wakehour,]$sd_above_threshold, na.rm=T)
  Data4$complianceWake_3 = sum(Data8[Data8$unique_min>=Data4$PMday_3 & Data8$unique_min<Data4$PMday4 & Data8$hours %in% wakehour,]$sd_above_threshold, na.rm=T)
  
  # accumulative PM averages for 24, 48, and 72 hour
  Data4$PMAverage24=round(mean(Data7[Data7$unique_min>=Data4$PMday_1&Data7$unique_min<Data4$PMday_2,]$nephelometer_final_min), digits=3)
  Data4$PMAverage48=round(mean(Data7[Data7$unique_min>=Data4$PMday_1&Data7$unique_min<Data4$PMday_3,]$nephelometer_final_min), digits=3)
  Data4$PMAverage72=round(mean(Data7[Data7$unique_min>=Data4$PMday_1&Data7$unique_min<Data4$PMday4,]$nephelometer_final_min), digits=3)
  
  DailyPM_Data = rbind(DailyPM_Data, Data4)  # update dailyPM data frame
  
  if(round(k/50)*50==k)               
    print(k)  
}

# save DailyPM dataset, please check the output directory
saveRDS(DailyPM_Data, file = "/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/MicroPEM_Data/DailyPM.rds") 
    
##############################MERGE DAILYPM WITH MICROPEM LOG DATA#############################
DailyPM = merge(DailyPM_Date, MicroPEM, by.x="filterID", by.y="filterid", all.x=T)
DailyPM[which(is.na(DailyPM$mstudyid)),] # missing three MircoPEM logsheet  KHC0729, KHC1015, KHCD48C

#####################################READ IN CO DATA############################################
COdata = readRDS("/Users/zhengzhou/Dropbox/Ghana_exposure_data_SHARED_2014/CO_files_processed/FINAL_CO_parameters_withvalidation_2016Jun14.rds")
COdata$Startdate = as.Date(COdata$firstdate)                                #get the start date of CO measurements
COdata1 = COdata[is.na(COdata$cstudyid),]                                   #exclude child CO measurements  

##########################MERGE PM AND CO DATA##########################################
PMCO = merge(DailyPM, COdata1, by=c("mstudyid", "Startdate"), all.x=T)

# change co colume names
colnames(PMCO)[colnames(PMCO)=="co_day1_mean"] = "OldCO_1"
colnames(PMCO)[colnames(PMCO)=="co_day2_mean"] = "OldCO_2"
colnames(PMCO)[colnames(PMCO)=="co_day3_mean"] = "OldCO_3"

colnames(PMCO)[colnames(PMCO)=="co_day1_mean_corr"] = "CorCO_1"
colnames(PMCO)[colnames(PMCO)=="co_day2_mean_corr"] = "CorCO_2"
colnames(PMCO)[colnames(PMCO)=="co_day3_mean_corr"] = "CorCO_3"
    
##############################WIDE TO LONG FORMAT#########################################
PMCO1 <-reshape(PMCO, 
                     varying=c(grep("PMday_",colnames(PMCO)),
                               grep("OldPM_",colnames(PMCO)),
                               grep("CorPM_",colnames(PMCO)),
                               grep("compliance_",colnames(PMCO)),
                               grep("complianceWake_",colnames(PMCO)),
                               grep("PMn_",colnames(PMCO)),
                               grep("OldCO_",colnames(PMCO)),
                               grep("CorCO_",colnames(PMCO)),
                               grep("Day_",colnames(PMCO))),
                     idvar="id", 
                     direction="long",  sep="_")

PMCO2 = PMCO1[PMCO1$visually_valid!=3,]    # exclude samples with invalid CO readings
PMCO2 = PMCO2[PMCO2$PMn>1320,]      #exclude PM sample-day < 22hrs
PMCO2 = PMCO2[!is.na(PMCO2$CorCO),]  #exclude CO sample-day < 24hrs
PMCO2 = PMCO2[PMCO2$CorPM>0,]       #exclude PM <0
# calculate compliance measure and categorize the measure into 7 buckets
PMCO2$complianceWakePct = PMCO2$complianceWake/PMCO2$PMn   
PMCO2$complianceWakePctGP = cut(PMCO2$complianceWakePct, seq(0, 0.7, 0.1), labels=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7), right=FALSE)

# save PMCO data frame as a Rdata
saveRDS(PMCO2, file = "/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/MicroPEM_Data/PMCO.rds") 
