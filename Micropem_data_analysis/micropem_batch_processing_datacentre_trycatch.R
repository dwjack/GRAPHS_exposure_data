# Working

## This script:
# Loads a current .csv spreadsheet of logsheet information from the data center. 
# Finds all the upem processed files in the dropbox and matches the data file names to the logsheet file names
# Checks a few things in the files before processing. If any of the errors below are encountered, information about the file is passed to a separate file, "problem_files": 
  ## Does the logsheet info have a matching upem datafile? (If not, need to locate the data file. File "no_match" is generated to summarize these.)
  ## Are the dates/times in the data file recognizable? (If not, this might mistakenly be a raw file labeled as a pre-processed file)
  ## Does the first hepa session end after the data  begins, and does the second session begin before the data file ends? 

# The script then processes the micropem files. This code is copied from the shiny app. Right now it is only saving the summary and minute averages for each file, but could easily be tweaked to save plots, etc.
# It also creates a summary table of all the files that were run, and plots histograms for them.
# If R encounters an error while processing the file, the file is skipped. Information about the skipped file is passed to "problem_files". 
# All generated files are saved to your default R directory



#### LOADING REQUIRED PACKAGES AND FUNCTIONS ####

require(shiny)
require(ggplot2)
require(xts) 
require(zoo)
require(plyr)
require(scales)
require(grid)
require(lubridate)


# function to put values in format xx.xx
timeformat <- function(x){
  x <- sprintf("%.2f", x)
  return(x)
}

# function to replace "0.00" with NA
zero2na <- function(x){ 
  z <- gsub("\\s+", "0.00", x)  # "\\s+" means match all
  x[z=="0.00"] <- NA 
  return(x)
}


# function to convert blanks to NA

blank2na <- function(x){ 
  z <- gsub("\\s+", "", x)  #make sure it's "" and not " " etc
  x[z==""] <- NA 
  return(x)
}


#### SETTING COMPLIANCE THRESHOLD #####

compliance_threshold <- 0.02

##### SETTING TIMEZONE #####
timezone <- "GMT"

 
######### PROCESSING LOGSHEET INFO -----

### FIND THE DATA FILES ###
#create a vector of upem file names  
files<-list.files("~/Dropbox/Ghana_exposure_data_SHARED (1)/",recursive=T,pattern="UGF[[:alnum:]]*_[[:alnum:]]*_", full.names=T) # this grabs the preprocessed upem files rather than the raw ones, plus the logsheets

logsheets <- files[grep("logsheet", files)] # separates out the logsheets

files <- files[!(files %in% logsheets)] # removes the logsheets from the files
length(files)

# get logsheet from data centre
logsheet_data <-  read.csv("~/Dropbox/Ghana project/microPEM_logsheets_through_late_feb_2014.csv", stringsAsFactors = FALSE) # replace filepath with the latest file

# assemble required data from logsheet_data
# Mstudyid, Upemid, Filterid, Fieldsetd, Thepaon1, Thepaoff1, Pickupdtd, Upemun, NComment, Thepaon2, Thepaoff2, Comments
loginfo <- logsheet_data[, c(3, 16:17, 25, 27:29, 31:32, 35:36, 41, 13)] 

# format the HEPA session times as needed for xts subsetting
loginfo[, c(5:6, 10:11)] <- lapply(X = loginfo[, c(5:6, 10:11)], FUN = timeformat)
loginfo[, c(5:6, 10:11)] <- lapply(X = loginfo[, c(5:6, 10:11)], FUN = zero2na)


loginfo$hepatimes1 <- paste0(mdy_hm(paste(loginfo$Fieldsetd, loginfo$Thepaon1)), "/",  mdy_hm(paste(loginfo$Fieldsetd, loginfo$Thepaoff1), tz = timezone))
loginfo$hepatimes2 <- paste0(mdy_hm(paste(loginfo$Pickupdtd, loginfo$Thepaon2)), "/",  mdy_hm(paste(loginfo$Pickupdtd, loginfo$Thepaoff2), tz = timezone))

loginfo$hepatimes1[grep("NA",loginfo$hepatimes1)] <- NA
loginfo$hepatimes2[grep("NA",loginfo$hepatimes2)] <- NA

# matching files to loginfo
matched_files <- as.data.frame(files[substr(gsub("^.*KHC", "KHC", files), 1,7) %in% loginfo$Filterid])
matched_files[,2] <- substr(gsub("^.*KHC", "KHC", matched_files[,1]), 1,7)
colnames(matched_files) <- c("datafile", "Filterid")
loginfo <- merge(loginfo, matched_files, by = "Filterid", all = TRUE)

# set aside the unmatched datafiles
no_match <- loginfo[is.na(loginfo$datafile),]
no_match$problem <- "no matching datafile"

# remove the files with no match from loginfo
loginfo <- loginfo[!is.na(loginfo$datafile),]

# sort loginfo chronologically by date of pickup
loginfo <- loginfo[order(mdy(loginfo$Pickupdtd)),]


# ### Check alignment of HEPATIMES with data start/end times ###
# NOTE this step takes quite a while, as it loads each data file in separately. Could alter the "i in" statement if you are only processing a subset of files rather than the whole lot of them.
for (i in 1:nrow(loginfo)) {
  filter <- loginfo$Filterid[i]
  data <- read.csv(as.character(loginfo$datafile[i]),col.names = c("Date","Time",  "RH.Corrected Nephelometer"  ,"Temp",  "RH",  "Battery",  "Inlet.Press",  "Flow.Orifice.Press",  "Flow",  "X.axis",  "Y.axis",  "Z.axis",  "Vector.Sum.Composite", "Stop.Descriptions"	), header=F, sep=",", fill=T, stringsAsFactors=FALSE) # IMPORTING ACTUAL DATASET INTO R. 
  
  
  data <- data[25:nrow(data),]  
  
  data.withoutdesc <- data[5:nrow(data), 1:13]
  
  data2 = data.frame(sapply(data.withoutdesc, blank2na))
  for(j in 1:11){data2[,j+2] = as.numeric(levels(data2[,j+2]))[as.integer(data2[,j+2])]}
  
  data2$Date <- as.character(data2$Date)
  data2$Time <- as.character(data2$Time)
  data2$datetime <- paste(data2$Date, data2$Time)

  
if (!is.na(mdy(data2[1,1]))) {
  loginfo$dateformat[i] <- "mdy" 
  data2$datetime <- mdy_hms(data2$datetime, tz = timezone)
 } else if (!is.na(dmy(data2[1,1]))) {
  loginfo$dateformat[i] <- "dmy" 
  data2$datetime <- dmy_hms(data2$datetime, tz = timezone)
  loginfo$data_start[i] <- data2$datetime[1]
  loginfo$data_end[i] <-   data2$datetime[nrow(data2)]
  } else loginfo$dateformat[i] <- NA

if (!is.na(loginfo$dateformat[i])) {
loginfo$data_start[i] <- data2$datetime[1]
loginfo$data_end[i] <-   data2$datetime[nrow(data2)]
 
  loginfo$hepa1_ok[i] <- data2$datetime[10] < mdy_hm(paste(loginfo$Fieldsetd[i], loginfo$Thepaoff1[i]), tz = timezone)
  loginfo$hepa2_ok[i] <- data2$datetime[(nrow(data2)-10)] > mdy_hm(paste(loginfo$Pickupdtd[i], loginfo$Thepaon2[i]), tz = timezone)
}
}



# set aside a data frame of data files that are probably raw rather than processed files & remove these from loginfo
nodateformat <- loginfo[is.na(loginfo$dateformat),]
nodateformat$problem <- "raw file?"
loginfo <- loginfo[!is.na(loginfo$dateformat),]

# set aside a data fram eof data files that have a known hepa problem, & remove these from loginfo
hepa_problem <- loginfo[(loginfo$hepa1_ok == FALSE & !is.na(loginfo$hepa1_ok) | loginfo$hepa2_ok == FALSE & !is.na(loginfo$hepa2_ok)),]
hepa_problem$problem <- "hepa problems"
loginfo <- loginfo[!(loginfo$hepa1_ok == FALSE & !is.na(loginfo$hepa1_ok) | loginfo$hepa2_ok == FALSE & !is.na(loginfo$hepa2_ok)),]


# combine the  problem files
problem_files <- rbind.fill(nodateformat, hepa_problem, no_match)

# save loginfo and problem_files to file
write.csv(loginfo, file = paste0("loginfo_",Sys.Date(),".csv"))
write.csv(problem_files, file = paste0("problem_files_", Sys.Date()))



# Print the number of files with problems
print(paste0(nrow(problem_files), " files have problems, see the file: problem_files_", Sys.Date()))

#### END LOGSHEET PROCESSING----


### PROCESS THE GOOD DATA --------

### CREATING BLANK SUMMARY TABLE ###
# summary_table <- as.data.frame(matrix(nrow = 0, ncol = 17)) # setting up summary table to capture the info

summary_table <- data.frame(stringsAsFactors = FALSE)

#### PROCESSING THE DATA #####

for (n in 1:nrow(loginfo)) { # would be n in nrow(loginfo)
  
  ErrorHandler <- tryCatch({
    
  HEPATIMES <- matrix(data = NA, nrow = 2, ncol = 1, byrow = TRUE)
  HEPATIMES[1,1] <- loginfo$hepatimes1[n]
  HEPATIMES[2,1] <- loginfo$hepatimes2[n]
  
subject <- loginfo$Mstudyid[n]
session <- paste0("S_", loginfo$Vround[n]) 
filter <- loginfo$Filterid[n]



window_width <- 10

Sys.setenv(TZ = timezone)

data <- read.csv(as.character(loginfo$datafile[n]),col.names = c("Date","Time",  "RH.Corrected Nephelometer"  ,"Temp",  "RH",  "Battery",  "Inlet.Press",  "Flow.Orifice.Press",  "Flow",  "X.axis",  "Y.axis",  "Z.axis",	"Vector.Sum.Composite", "Stop.Descriptions"	), header=F, sep=",", fill=T, stringsAsFactors=FALSE) # IMPORTING ACTUAL DATASET INTO R. 

data_header <- data[1:22,1:7]
colnames(data_header) <- c("variable", "V1", "V2", "V3", "V4", "V5", "V6")

serialnumber <- paste("PM", as.character(sub("^......", "", data_header[4,2])), sep = "")
serialnumber_full <- data_header[4,2]

data <- data[25:nrow(data),]  

stop.desc=data$Stop.Descriptions
stop.desc= cbind(data[,c(1:2)],stop.desc)
stop.desc$stop.desc = as.character(stop.desc$stop.desc)
stop.desc = data.frame(sapply(stop.desc, blank2na), stringsAsFactors = FALSE)
colnames(stop.desc) <- c("V1", "V2", "variable")
stop.desc = stop.desc[complete.cases(stop.desc),]    
stop.desc <- stop.desc[,c(3,1,2)]


data.withoutdesc <- data[5:nrow(data), 1:13]

######### CREATING DATA2 DATASET - ALL DATA INCLUDING HEPA SESSIONS, RH CORRECTED. #####

data2 = data.frame(sapply(data.withoutdesc, blank2na))
for(i in 1:11){data2[,i+2] = as.numeric(levels(data2[,i+2]))[as.integer(data2[,i+2])]}

data2$Date <- as.character(data2$Date)
data2$Time <- as.character(data2$Time)
data2$datetime <- paste(data2$Date, data2$Time)


if (loginfo$dateformat[n] == "mdy") data2$datetime <- mdy_hms(data2$datetime, tz = timezone)
if (loginfo$dateformat[n] == "dmy") data2$datetime <- dmy_hms(data2$datetime, tz = timezone)


data2$RH.Corrected.Nephelometer = ifelse(data2$RH <0, NA, data2$RH.Corrected.Nephelometer) # First removes RH corrected if RH < 0
data2$RH[data2$RH < 0] = NA   


data2$unique_min <- floor_date(data2$datetime, unit = "minute")
data2$unique_hour <- floor_date(data2$datetime, unit = "hour")




#############  HEPA STUFF #########################



days.xts <- as.xts(data2, order.by = data2$datetime, .RECLASS = TRUE)

data2.HEPA1 <- matrix(nrow = 0, ncol = ncol(data2))
data2.HEPA2 <- matrix(nrow = 0, ncol = ncol(data2))
data2.HEPA3 <- matrix(nrow = 0, ncol = ncol(data2))

#### INDEXING HEPA INTERVALS/ REMOVING HEPA SESSIONS FROM DATA ###

NEWHEPATIMES <- HEPATIMES[!is.na(HEPATIMES)]

for (i in 1:length(NEWHEPATIMES)) {
  assign(paste("data2.HEPA",i, sep = ""), days.xts[as.character(NEWHEPATIMES[i])])
} 

if (length(NEWHEPATIMES) ==1) {
  days.xts2 <- days.xts[!time(days.xts) %in% index(data2.HEPA1)]} else
    if (length(NEWHEPATIMES) ==2) {
      days.xts2 <- days.xts[!time(days.xts) %in% index(data2.HEPA1) & !time(days.xts) %in% index(data2.HEPA2)]} else
        if (length(NEWHEPATIMES) ==3) {
          days.xts2 <- days.xts[!time(days.xts) %in% index(data2.HEPA1) & !time(days.xts) %in% index(data2.HEPA2) & !time(days.xts) %in% index(data2.HEPA3)]
      #  }
} else 
  days.xts2 <- days.xts


#### CALCULATING AVERAGE NEPHELOMETER VALUES DURING HEPA SESSIONS (1ST AND LAST MINUTES TRIMMED TO ACCOUNT FOR POTENTIAL TIME SYNC MISMATCH) ####

HEPA1.nephelometer <- NA
HEPA2.nephelometer <- NA
HEPA3.nephelometer <- NA

if(!is.na(HEPATIMES[1,1]))  {
  NEWHEPATIMES <- HEPATIMES[!is.na(HEPATIMES)]
  for (i in 1:length(NEWHEPATIMES)) {
    if (i >=1) {
      data2.HEPA1_trim = data2.HEPA1[!time(data2.HEPA1) %in%  index(first(data2.HEPA1, '1 minute')) & !time(data2.HEPA1) %in% index(last(data2.HEPA1, '1 minute'))]
      data2.HEPA1_trim = as.data.frame(data2.HEPA1_trim, stringsAsFactors = FALSE)
      HEPA1.nephelometer = round(mean(as.numeric(data2.HEPA1_trim$RH.Corrected.Nephelometer), na.rm=TRUE), digits = 2)} else (HEPA1.nephelometer <- NA)
  
    if( i>=2) {
      data2.HEPA2_trim = data2.HEPA2[!time(data2.HEPA2) %in%  index(first(data2.HEPA2, '1 minute'))  & !time(data2.HEPA2) %in% index(last(data2.HEPA2, '1 minute'))]
      data2.HEPA2_trim = as.data.frame(data2.HEPA2_trim, stringsAsFactors = FALSE)
      HEPA2.nephelometer = round(mean(as.numeric(data2.HEPA2_trim$RH.Corrected.Nephelometer), na.rm=TRUE), digits = 2)} else (HEPA2.nephelometer <- NA)
  }}


### CREATING DATASET OF HEPA SESSION INFO ####

data2.HEPA1 <- as.data.frame(data2.HEPA1, stringsAsFactors = FALSE)
data2.HEPA2 <- as.data.frame(data2.HEPA2, stringsAsFactors = FALSE)
data2.HEPA3 <- as.data.frame(data2.HEPA3, stringsAsFactors = FALSE)
hepainfo <- rbind(data2.HEPA1, data2.HEPA2, data2.HEPA3)

  ###### CREATING "ACTIVE DATA" DATASET (HEPA SESSIONS REMOVED) #########
  
  active.data <- as.data.frame(days.xts2, stringsAsFactors = FALSE) 
  
  ##### CALCULATING ACTIVE MINUTE AVERAGES #####
  
  active.minute.average = ddply(active.data, .(unique_min), summarise, 
                                RH.Corrected.Nephelometer = round(mean(as.numeric(RH.Corrected.Nephelometer), na.rm=TRUE), digits = 3),
                                Temp = round(mean(as.numeric(Temp), na.rm=TRUE), digits = 3),
                                RH = round(mean(as.numeric(RH), na.rm=TRUE), digits = 3),    						
                                Battery = round(mean(as.numeric(Battery), na.rm=TRUE), digits = 3),
                                Inlet.Press = round(mean(as.numeric(Inlet.Press), na.rm=TRUE), digits = 3), 
                                Flow.Orifice.Press = round(mean(as.numeric(Flow.Orifice.Press), na.rm=TRUE), digits = 3),
                                Flow = round(mean(as.numeric(Flow), na.rm=TRUE), digits = 3),
                                X.axis_mean = round(mean(as.numeric(X.axis), na.rm=TRUE), digits = 4), 
                                Y.axis_mean = round(mean(as.numeric(Y.axis), na.rm=TRUE), digits = 4), 
                                Z.axis_mean = round(mean(as.numeric(Z.axis), na.rm=TRUE), digits = 4), 
                                Vector.Sum.Composite_mean = round(mean(as.numeric(Vector.Sum.Composite), na.rm=TRUE), digits = 4), 
                                X.axis_SD = round(sd(X.axis, na.rm=TRUE), digits = 3), 
                                Y.axis_SD = round(sd(Y.axis, na.rm=TRUE), digits = 3), 
                                Z.axis_SD = round(sd(Z.axis, na.rm=TRUE), digits = 3), 
                                Vector.Sum.Composite_SD = round(sd(Vector.Sum.Composite, na.rm=TRUE), digits = 3), 
                                unique_hour = unique_hour[1])	
  
  
  #### ADDING COMPLIANCE CRITERIA ###
  
  active.minute.average$sd_composite_above_threshold = ifelse(active.minute.average$Vector.Sum.Composite_SD > compliance_threshold, 1, 0)
  active.minute.average$sd_x_above_threshold = ifelse(active.minute.average$X.axis_SD > compliance_threshold, 1, 0)		
  active.minute.average$sd_y_above_threshold = ifelse(active.minute.average$Y.axis_SD > compliance_threshold, 1, 0) 
  active.minute.average$sd_z_above_threshold = ifelse(active.minute.average$Z.axis_SD > compliance_threshold, 1, 0)  
  
  active.minute.average$sd_composite_rollmean <- as.numeric(rollapply(active.minute.average$sd_composite_above_threshold, width=window_width,  FUN = mean, align = "center", na.rm = TRUE, fill = NA))  ## **** NOTE **** To change the width of the rolling mean window for compliance, change the parameter for "width" w. 
  
  active.minute.average$compliance_rollmean <- ifelse(active.minute.average$sd_composite_rollmean > 0, 1, 0)
  
  if (sum(!is.na(active.minute.average$compliance_rollmean)) > 0) {
    active.minute.average.complete <-  active.minute.average[complete.cases(active.minute.average),] 
  } else {
    active.minute.average.complete <- active.minute.average
  }
  
  
  
  ### SUBSETTING INTO 24 HOUR PERIODS ###
  active.minute.average.complete$unique_min <- ymd_hms(active.minute.average.complete$unique_min, tz = timezone)
  no.days <- ceiling(as.numeric(as.duration(active.minute.average.complete$unique_min[nrow(active.minute.average.complete)] - active.minute.average.complete$unique_min[1]))/86400) # calculates the difference in time between last and first datetime observation (in seconds), transforms it into days and returns the ceiling of days
  
  
  dayindex <- active.minute.average.complete$unique_min[1] + hours(seq(from = 24, to = no.days*24, by = 24))
  
  active.minute.average.complete$unique_24h <- 1
  
  for (i in 1:no.days) { 
    active.minute.average.complete$unique_24h <- ifelse ((active.minute.average.complete$unique_min > dayindex[i]),  i+1, active.minute.average.complete$unique_24h)
  } 
  
  
  #### CALCULATING HOUR AVERAGES ####
  
  active.hour.average = ddply(active.minute.average.complete, .(unique_hour), summarise, 
                              RH.Corrected.Nephelometer = round(mean(RH.Corrected.Nephelometer, na.rm=TRUE), digits = 3),
                              Temp = round(mean(Temp, na.rm=TRUE), digits = 3),
                              RH = round(mean(RH, na.rm=TRUE), digits = 3),								
                              Battery = round(mean(Battery, na.rm=TRUE), digits = 3),
                              Inlet.Press = round(mean(Inlet.Press, na.rm=TRUE), digits = 3), 
                              Flow.Orifice.Press = round(mean(Flow.Orifice.Press, na.rm=TRUE), digits = 3),
                              Flow = round(mean(Flow, na.rm=TRUE), digits = 3),
                              count_composite_above_threshold = sum(sd_composite_above_threshold, na.rm=TRUE), 
                              percent_composite_above_threshold = round(mean(sd_composite_above_threshold, na.rm=TRUE), digits = 3), 
                              x_above_threshold = sum(sd_x_above_threshold, na.rm=TRUE), 
                              x_percent_above_threshold  = round(mean(sd_x_above_threshold, na.rm=TRUE), digits = 3),
                              y_above_threshold = sum(sd_y_above_threshold, na.rm=TRUE), 
                              y_percent_above_threshold  = round(mean(sd_y_above_threshold, na.rm=TRUE), digits = 3),
                              z_above_threshold = sum(sd_z_above_threshold, na.rm=TRUE), 
                              z_percent_above_threshold  = round(mean(sd_z_above_threshold, na.rm=TRUE), digits = 3),
                              total_minutes_observation = length(unique_min),	
                              proportion_compliance_rollmean = round(sum(compliance_rollmean, na.rm = TRUE)/60, digits = 3),
                              datetime = unique_min[1],
                              unique_24h = unique_24h[1])
  
  
  ###### CALCULATING 24-HOUR AVERAGES #####
  
  active.day.average = ddply(active.minute.average.complete, .(unique_24h), summarise, 
                             RH.Corrected.Nephelometer = mean(RH.Corrected.Nephelometer, na.rm=TRUE),
                             Temp = mean(Temp, na.rm=TRUE),
                             RH = mean(RH, na.rm=TRUE),  							
                             Battery = mean(Battery, na.rm=TRUE),
                             Inlet.Press = mean(Inlet.Press, na.rm=TRUE), 
                             Flow.Orifice.Press = mean(Flow.Orifice.Press, na.rm=TRUE),
                             Flow = mean(Flow, na.rm=TRUE),
                             count_composite_above_threshold = sum(sd_composite_above_threshold, na.rm=TRUE), 
                             percent_composite_above_threshold = mean(sd_composite_above_threshold, na.rm=TRUE), 
                             x_above_threshold = sum(sd_x_above_threshold, na.rm=TRUE), 
                             x_percent_above_threshold  = mean(sd_x_above_threshold, na.rm=TRUE),
                             y_above_threshold = sum(sd_y_above_threshold, na.rm=TRUE), 
                             y_percent_above_threshold  = mean(sd_y_above_threshold, na.rm=TRUE),
                             z_above_threshold = sum(sd_z_above_threshold, na.rm=TRUE), 
                             z_percent_above_threshold  = mean(sd_z_above_threshold, na.rm=TRUE),
                             
                             hours_compliance_rollmean = round(sum((compliance_rollmean)/60, na.rm = TRUE), digits = 2),
                             
                             total_minutes_observation = length(unique_min),
                             total_hours_observation = round(length(unique_min)/60, digits = 1),
                             datetime = unique_min[1])
  
  
  
  ####### MINUTE AVERAGE DATA ############################
  
  active.minute.average.complete$Subject <- rep(subject)
  active.minute.average.complete$Session <- rep(session)
  # active.minute.average.complete <- active.minute.average.complete[,c(25:26, 1:24)]
  
  
  ####### HOUR AVERAGE DATA ############################
  if (sum(!is.na(active.hour.average$x_percent_above_threshold))> 0) {
    active.hour.average.complete <-  active.hour.average[complete.cases(active.hour.average),] 
  } else {
    active.hour.average.complete <- active.hour.average
  }
  
  
  active.hour.average.complete$Subject <- rep(subject)
  active.hour.average.complete$Session <- rep(session)
  # active.hour.average.complete <- active.hour.average.complete[,c(21:22, 1:20)]
  
  ##### ADDING HEPA CORRECTION TO NEPHELOMETER READINGS ####
  ## NOTE: CURRENTLY ONLY SET UP FOR ONE OR TWO HEPA SESSIONS ###
  
  active.day.average$HEPA_corr_neph <- NA
  active.minute.average.complete$HEPA_corr_neph <- NA
  active.hour.average.complete$HEPA_corr_neph <- NA
  
  # new
  if (length(NEWHEPATIMES) ==1) {
    HEPA_correction <- round(HEPA1.nephelometer, digits = 2)
    active.day.average$HEPA_correction <- HEPA_correction
    active.hour.average.complete$HEPA_correction <- HEPA_correction   
    active.minute.average.complete$HEPA_correction <- HEPA_correction
    active.day.average$HEPA_corr_neph <- round(active.day.average$RH.Corrected.Nephelometer - active.day.average$HEPA_correction, digits = 2)
    active.hour.average.complete$HEPA_corr_neph <- round(active.hour.average.complete$RH.Corrected.Nephelometer - active.hour.average.complete$HEPA_correction, digits = 3)
    active.minute.average.complete$HEPA_corr_neph <- round(active.minute.average.complete$RH.Corrected.Nephelometer - active.minute.average.complete$HEPA_correction, digits = 3)
    
  } else
    
    # end new
    
    if (length(NEWHEPATIMES) ==2) {
      HEPA_correction <- seq(HEPA1.nephelometer, HEPA2.nephelometer, length.out= nrow(active.day.average)) # length = number of days sampled
      active.day.average$HEPA_correction <- round(HEPA_correction, digits = 2)
      
      for (i in 1:nrow(active.day.average)) {
        active.day.average$HEPA_corr_neph[i] <- round(active.day.average$RH.Corrected.Nephelometer[i] - HEPA_correction[i], digits = 2) # why not just subtract the vectors?
        active.minute.average.complete$HEPA_correction[active.minute.average.complete$unique_24h ==i] <- round(HEPA_correction[i], digits = 2) # sets up one HEPA correction value per 24 hours
        active.minute.average.complete$HEPA_corr_neph <- round(active.minute.average.complete$RH.Corrected.Nephelometer - active.minute.average.complete$HEPA_correction, digits = 3)
        active.hour.average.complete$HEPA_correction[active.hour.average.complete$unique_24h==i] <- round(HEPA_correction[i], digits = 2)    
        active.hour.average.complete$HEPA_corr_neph <- round(active.hour.average.complete$RH.Corrected.Nephelometer - active.hour.average.complete$HEPA_correction, digits = 3)
        
      }} else
      { active.day.average$HEPA_correction <- NA
        active.minute.average.complete$HEPA_correction <- NA 
        active.hour.average.complete$HEPA_correction <- NA
      }
  
  ### NEW ###
  
  ### CALCULATING DELTA PRESSURE ###
  if(!is.na(active.minute.average.complete$HEPA_corr_neph[1])) {
    dep_rate <- active.minute.average.complete$HEPA_corr_neph * active.minute.average.complete$Flow * 1/1000 } else 
      
    { dep_rate <- active.minute.average.complete$RH.Corrected.Nephelometer * active.minute.average.complete$Flow * 1/1000 }
  active.minute.average.complete$cumulative_dep <- cumsum(dep_rate)
  active.hour.average.complete$cumulative_dep <- round(tapply(X = active.minute.average.complete$cumulative_dep, INDEX = active.minute.average.complete$unique_hour, FUN  = mean), digits = 3)
  active.day.average$cumulative_dep <- round(tapply(X = active.minute.average.complete$cumulative_dep, INDEX = active.minute.average.complete$unique_24h, FUN  = mean), digits = 2)
  
  active.minute.average.complete$delta_pressure <- round(active.minute.average.complete$Inlet.Press - active.minute.average.complete$Inlet.Press[1], digits = 3)*2
  # note - multiplying the Inlet Pressure x2 as per RTI
  
  active.hour.average.complete$delta_pressure <- round(tapply(X = active.minute.average.complete$delta_pressure, INDEX = active.minute.average.complete$unique_hour, FUN  = mean), digits = 3)
  
  active.day.average$delta_pressure <- round(tapply(X = active.minute.average.complete$delta_pressure, INDEX = active.minute.average.complete$unique_24h, FUN  = mean), digits = 2)
  
  
  
  # active.minute.average.complete <- active.minute.average.complete[,c(1:4,28:27, 5:26, 29:30)]
  
#   active.hour.average.complete <- active.hour.average.complete[,c(1:4, 24:23, 5:22, 25:26)]
  
  active.day.average$unique_24h <- paste("Day", active.day.average$unique_24h)
  
  
  active.day.average$proportion_compliance_all <- NA
  active.day.average$proportion_compliance_all <- ifelse(active.day.average$total_hours_observation ==0, NA, round(active.day.average$hours_compliance_rollmean/active.day.average$total_hours_observation, digits = 3))
  active.day.average$proportion_compliance_all <- ifelse(is.na(active.day.average$percent_composite_above_threshold), NA, active.day.average$proportion_compliance_all)
  
  ###################    SUMMARY DATA   ###########################
  
  ##### TOTAL RUN TIME ####################
  
  total_time <- sum(active.day.average$total_hours_observation)
  total_time_minutes <- sum(active.day.average$total_minutes_observation)
  total_minutes_worn <- sum(active.minute.average.complete$compliance_rollmean, na.rm = TRUE)
  
  
  ### START DATE & TIME ###
  
  
  start_time = format(active.minute.average.complete$unique_min[1], format = "%d%b%y %H:%M:%S")
  
  ### STOP DATE & TIME ###
  stop_time = format(active.minute.average.complete$unique_min[nrow(active.minute.average.complete)], format = "%d%b%y %H:%M:%S") # due to NAs at end of rolling mean for compliance,  last minutes will be truncated
  
  #### GENERATING ID FOR FILENAMES #####
  
  
  lastdate <- substr(stop_time,1,7)
  ID <- paste(subject, lastdate, session, serialnumber, filter, sep = "_")
  
  #### AVERAGE ACTIVE SAMPLE NEPHELOMETER ####
  
  average_sample_nephelometer = round(mean(as.numeric(active.data$RH.Corrected.Nephelometer), na.rm=TRUE), digits = 2)
  
  average_sample_nephelometer_hepacorr <- round(mean(active.day.average$HEPA_corr_neph, na.rm= TRUE), digits = 2)


  ###### VOLTAGE DROP PER HOUR #####
  
  # (Vb-Ve)*1000 mV/V ÷ (hours ran/2) (adjust for 50% duty cycle)- this number should be < 30 mV/hr for best pumps
  
  voltage_b <- active.hour.average.complete$Battery[1]
  voltage_e <- active.hour.average.complete$Battery[length(active.hour.average.complete)]
  voltage_drop <- (voltage_b-voltage_e)*1000 / (total_time/2)
  
  #### DATA SUMMARY #######
  
  active.data_summary = matrix(c(as.character(filter),
                                 serialnumber_full,
                                 round(total_time), 
                                 round(total_time_minutes), 
                                 as.character(start_time), 
                                 as.character(stop_time),
                                 timezone,
                                 round(average_sample_nephelometer, digits = 2), 
                                 average_sample_nephelometer_hepacorr,
                                 HEPATIMES, 
                                 round(HEPA1.nephelometer, digits = 2),
                                 round(HEPA2.nephelometer, digits = 2),
                                 compliance_threshold,
                                 sum(active.minute.average$sd_composite_above_threshold, na.rm=TRUE), 
                                 round(total_minutes_worn/60),
                                 round(mean(active.day.average$proportion_compliance_all, na.rm = TRUE)*100, digits =1),
                                 round(voltage_drop, digits = 2)),
                               ncol = 1)
  
  
  active.data_summary = data.frame(active.data_summary, stringsAsFactors = FALSE)
  active.data_summary$variable = c("Filter", "Serialnumber", "Total Sampling Time (hrs)", "Total Sampling Time (mins)", "Start Time", "Stop Time", "Timezone", "Mean Active Nephelometer (ug/m^3)", "Mean Active Neph, HEPA corr (ug/m^3)", "HEPA1_times (start/stop)", "HEPA2_times (start/stop)",
                                   "Mean HEPA1 Nephelometer (ug/m^3)", "Mean HEPA2 Nephelometer (ug/m^3)",  "Compliance Threshold for minutewise SD",
                                   "Total Time Composite SD>Threshold (mins)", "Total Hours Worn (hrs)",  "Percent of Hours Worn (%)",  "Avg Voltage Drop per Hour (mV/hr)")  
  colnames(active.data_summary)[1] <- "V1"
  
  
  summary_24 <- as.matrix(t(active.day.average))
  summary_24[2:18,] <- round(as.numeric(summary_24[2:18,]), digits = 2) 
  summary_24 <- as.data.frame(summary_24, stringsAsFactors = FALSE)
  summary_24$variable <- as.character(colnames(active.day.average))
  
  summary_24 <- summary_24[c(1,20,2, 22,21,3:19,23),]
  summary_24$variable <-  c("Unique 24-hour period", "24-hour period start date", "RH-Corrected Nephelometer (ug/m^3)", "HEPA Correction (ug/m^3)",  "HEPA-Corrected Nephelometer (ug/m^3)", "Temp (C)", "RH (%)", "Battery (V)",
                            "Inlet.Pressure (H20)", 
                            "Flow.Orifice.Pressure (H20)",   	
                            "Flow (Lpm)",
                            "count_composite_above_threshold (mins)",
                            "percent_composite_above_threshold (%)",
                            "x_above_threshold (mins)",
                            "x_percent_above_threshold (%)",        
                            "y_above_threshold(mins)",                
                            "y_percent_above_threshold (%)", 
                            "z_above_threshold (mins)",               
                            "z_percent_above_threshold (%)",
                            "Hours of Compliance (hrs, by rolling mean)",
                            "Active Sampling Minutes (mins)",
                            "Active Sampling Hours (hrs)",
                            "Percent of Hours Worn (%)")
  
  labels_setup <- as.data.frame(t(rep("**SETUP**", 7)), stringsAsFactors = FALSE)
  colnames(labels_setup)[7] <- "variable"
  labels_summary <- as.data.frame(t(rep("**OVERALL.SUMMARY**", 7)), stringsAsFactors = FALSE)
  colnames(labels_summary)[7] <- "variable"
  labels_desc <-  as.data.frame(t(rep("**EQUIPMENT.LOG**", 7)), stringsAsFactors = FALSE)
  colnames(labels_desc)[7] <- "variable"
  labels_24 <- as.data.frame(t(rep("**24.HOUR.SUMMARY**",7)), stringsAsFactors = FALSE)
  colnames(labels_24)[7] <- "variable"
  
  
  summary <- rbind.fill( labels_summary[,c(7,1:6)], active.data_summary, labels_24,summary_24, labels_setup, data_header,labels_desc, stop.desc)
  
  # summary$Permanent_ID <- rep(permID)
  summary$Subject <- rep(subject)
  summary$Session <- rep(session)
  summary <- summary[,c(8,9, 1:7)] # IS THIS THE BEST WAY TO DO THIS (ROW-WISE)?
  
},
error = function(e) e
)

if(inherits(ErrorHandler, "error")) {
  print(paste0("Not processed due to errors: ", filter))
  next }


# save the summary
  write.csv(summary, file = paste0(ID,"_MicroPEM_Summary.csv"))

# save the minute data 
write.csv(active.minute.average.complete, file = paste0(ID, "_Data_Minute_Averages.csv"), row.names = F) 

 summary_table <- rbind(summary_table, t(active.data_summary[,1]))
}








colnames(summary_table) <- c("Filter", "Serialnumber", "Total Sampling Time (hrs)", "Total Sampling Time (mins)", "Start Time", "Stop Time", "Timezone", "Mean Active Nephelometer (ug/m^3)", "Mean Active Neph, HEPA corr (ug/m^3)", "HEPA1_times (start/stop)", "HEPA2_times (start/stop)", "Mean HEPA1 Nephelometer (ug/m^3)", "Mean HEPA2 Nephelometer (ug/m^3)",  "Compliance Threshold for minutewise SD",
"Total Time Composite SD>Threshold (mins)", "Total Hours Worn (hrs)",  "Percent of Hours Worn (%)",  "Avg Voltage Drop per Hour (mV/hr)")  

not_processed <- loginfo[!loginfo$Filterid %in% summary_table$Filter,]
not_processed$problem <- "not processed"


problem_files <- rbind(problem_files, not_processed)


### DO THESE PARTS ONLY WHEN YOU WANT TO SAVE THE TABLES, give appropriate filenames ----
# save the problem files
write.csv(problem_files, file = paste0("MicroPEM_problem_files_", Sys.Date(), ".csv"), row.names = FALSE)

# save the summary table to file
write.csv(summary_table, file = paste0("MicroPEM_summary_table_", Sys.Date(), ".csv"), row.names = FALSE)



