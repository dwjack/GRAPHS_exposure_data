################################# FIND THE DATA FILES ############################################ 
# define a file directory
filedirectory <- "/Volumes/My Passport for Mac/WD passport/Columbia-Ghana Project/MicroPem Raw Data/Nephelometer_processed_correct"
Datafiles = list.files(filedirectory,full.names = TRUE)           # grab all MicroPEM files in the directory

# find files with no nephelometer reading
excludelist <- c("KHC0234")                                       
excludefiles <- Datafiles[unlist(sapply(excludelist, function(x) grep(x, Datafiles)))] 
Datafiles <- Datafiles[!(Datafiles %in% excludefiles)]            # removes empty files with no nephelometer reading
MicroPEMfiles <- as.data.frame(Datafiles)                         # create a dataframe of datafiles

######################## DEFINE A FUNCTION TO READ AND STORE MICROPEM RAW FILE####################
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
QualityControl = NULL                           # creat an empty data frame to store MicroPEM setting and basic summary information

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
require(changepoint)
QualityControl1 = QualityControl[order(QualityControl$filterID),]     # sort observation by filterID

HEPAdata = NULL     # creat an empty data frame to store HEPA information

# Define a directory to output fiugres
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
  if(Data4$HEPAstnumber==94){
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
  Data7 = Data5[(nrow(Data5)-3):(nrow(Data5)-99),]
  HEPAEnd = cpt.meanvar(Data7$nephelometer,method="BinSeg", Q=3, minseglen=8)
  Data4$HEPAendnumber = HEPAEnd@cpts[1]        # identify the place where changepoint is
  if(Data4$HEPAendnumber==97){
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
  
  HEPAdata = rbind(HEPAdata, Data4)
  
  if(round(k/50)*50==k)               
    print(k) 
}
dev.off()
