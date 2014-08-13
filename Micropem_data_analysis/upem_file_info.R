# creates a data file with information for each filter ID that has been logged in the database
# created August 2 by Darby 
# last updated August 12 by Darby

library(plyr)
#######################################################################
### CHUNK 1:  SCRAPE DATA FROM RTI PROCESSED FILES ####################
#######################################################################
#note that most of these can be obtained from the shiny processed files as well.

path <- "~/Dropbox/Ghana_exposure_data_SHARED_2014/Main_study_exposure_assessment/" # path to exposure data - may need to modify for your syste,
fileid <- "UGF[[:alnum:],[:punct:]]*_BM[[:alnum:],[:punct:]]*.csv" # regex for microPEM files (processed by RTI software, not by shiny app)

files <- list.files(path, recursive=T, pattern=fileid,  include.dirs=F) # this grabs the preprocessed upem files rather than the raw ones, plus the logsheets


length(files) # check how many there are

files <- as.data.frame(files) # convert to data frame



#create hhid variable
hhid_pattern <- "BM...."
hhid_match <- regexpr(hhid_pattern, files$files)
files$hhid <- regmatches(files$files, hhid_match)

#create village id variable - inefficient 2 step process, but works
vill_pattern<-"vil_.."
vill_match<-regexpr(vill_pattern, files$files, ignore.case=T)
files$vill<-regmatches(files$files, vill_match)
files$village_code<-substr(files$vill,5,6)

#create session id variable
session_pattern<-"s_.."
session_match<-regexpr(session_pattern, files$files)
files$session<-regmatches(files$files, session_match)
files$session<-substr(files$session,3,4)

# create duplicate variable
files$duplicate <- grepl("dup", files$files) # note that this assumes that files that contain "dup" are duplicates.  none seem to exist

#create date  variable
#first isolate the file name so that I can pick out the BM string

name_pattern<-"UGF[[:alnum:],[:punct:]]*_BM[[:alnum:],[:punct:]]*.csv"
name_match<-regexpr(name_pattern, files$files)
files$name<-regmatches(files$files, name_match)

# then pick out the date:
date_pattern <- "BM............."
date_match <- regexpr(date_pattern, files$name)
files$date <- regmatches(files$name, date_match)
files$date<-substr(files$date,9,17)

head(files$date) # note that the formats vary

# create microPEM SN variable
upem_pattern <- "UGF......."
upem_match <- regexpr(upem_pattern, files$files)
files$upem_sn <- regmatches(files$files, upem_match)

# create filter id variable (picks up filters starting with either KHC or GN)
filter_pattern <- "KHC....|GN..."
filter_match <- regexpr(filter_pattern, files$files)
files$filter_id <- regmatches(files$files, filter_match)

# join on arms

arms <- read.csv("~/Documents/projects/Biomass_working_group/Ghana_R01/exposure_data_assessment/arms.csv", header=T)
upem_data <- join(files,arms,by='village_code') #note that arm is numerical, arm2 is a string

#######################################################################
### CHUNK 2: SCRAPE DATA FROM SHINY PROCESSED FILES ###################
#######################################################################

#define function to import summary files from Shiny app
summary.import <- function(file) {
          # reads in summary file and extracts the following parameters into a flat file:
            # 1) volume sampled (computed as a weighted average of the daily flows)
            # 2) HEPA-corrected Nephelometer
            # 3) Percent of Hours Worn
            # 4) start and stop time
          ID <- read.csv(file, header=F,  sep=",", stringsAsFactors=F)
          M <- as.numeric(ID[4,4]) # total sampling time in minutes
          # pick flows by day
          flowrow <- grep("Flow..Lpm.", ID$V3)  # identify row with the daily flow data
              df1 <- as.numeric(ID[[flowrow,4]]) # day 1
              df2 <- as.numeric(ID[[flowrow,5]]) # day 2
                  if (is.na(df2)) {df2 <- 0} else {df2 <- df2}  # convert day 2 to zero if it's missing (otherwise average below will be NA)
              df3 <- as.numeric(ID[[flowrow,6]]) # day 3
                  if (is.na(df3)) {df3 <- 0} else {df3 <- df3}  # convert day 3 to zero if it's missing (otherwise average below will be NA)
              df4 <- as.numeric(ID[[flowrow,7]]) # day 4 (if it exists)
                  if (is.na(df4)) {df4 <- 0} else {df4 <- df4}  # convert day 4 to zero if it's missing (otherwise average below will be NA)
          # pick sampling duration by day
          timerow <- grep("Active.Sampling.Minutes..mins.", ID$V3)  # identify row with the daily flow data
              dt1 <- as.numeric(ID[[timerow,4]]) # day 1
              dt2 <- as.numeric(ID[[timerow,5]]) # day 2
                  if (is.na(dt2)) {dt2 <- 0} else {dt2 <- dt2} # convert day 2 to zero if it's missing (otherwise average below will be NA)
              dt3 <- as.numeric(ID[[timerow,6]]) # day 3
                   if (is.na(dt3)) {dt3 <- 0} else {dt3 <- dt3} # convert day 3 to zero if it's missing (otherwise average below will be NA)
              dt4 <- as.numeric(ID[[timerow,7]]) # day 4 (if it exists)
                  if (is.na(dt4)) {dt4 <- 0} else {dt4 <- dt4}   # convert day 4 to zero if it's missing (otherwise average below will be NA)
          # compute the weighted average of the flow
          ave_flow <- (df1/M*dt1) + (df2/M*dt2) + (df3/M*dt3) + (df4/M*dt4)
          # pick Average Active Sample Nephelometer (HEPA corrected)
          nephrow <- grep("Neph[[:alnum:],[:punct:],[:space:]]*HEPA", ID$V3)  # identify row with the daily flow data
          neph <- as.numeric(ID[[nephrow,4]])
          # Percent of Hours Worn
          comprow <- grep("Percent.of.Hours.Worn", ID$V3)  # identify row with the daily flow data
          compliance <- as.numeric(ID[[comprow[1],4]]) # note that this is picking the first occurence of "grep("Percent.of.Hours.Worn", ID$V3)"
          # filter id
          idrow <- grep("Filter.ID", ID$V3)
          filter_id <- ID[[idrow,4]]
          # volume
          volume = M * ave_flow 
          # start time
          startrow <- grep("Start.Time", ID$V3)
          starttime <- ID[[startrow,4]]
          # stop time
          stoprow <- grep("Stop.Time", ID$V3)
          stoptime <- ID[[stoprow,4]]
          # combine into a row 
          summary <- cbind(filter_id, ave_flow, compliance, M, volume, neph, starttime, stoptime)
          summary
}

# create list of summary files
    path <- "~/Dropbox/Ghana_exposure_data_SHARED_2014/Main_study_exposure_assessment"
    fileid <- "BM[[:alnum:],[:punct:]]*_Data_Summary.csv" # regex for microPEM files (summary files from shiny app)
    sum_file_list <- list.files(path, recursive=T, pattern=fileid, full.names=TRUE) # this grabs the preprocessed upem files rather than the raw ones, plus the logsheets
    sum_file_list <- sum_file_list[-grep("ascar", sum_file_list)] # drop any lascar files that have been shiny processed 

# apply to list of summary files
upem_summary <- ldply(sum_file_list, summary.import, .progress = "text", .inform = T )
upem_summary$filter_id <- as.character(upem_summary$filter_id)

#######################################################################
### CHUNK 3: Assemble into a single flat file #########################
#######################################################################

upem_data <- join(upem_data, upem_summary , by='filter_id', type="full") # join to upem_data

name <- paste("upem_data",Sys.Date(), sep="_") # this and the next line add the date to the file name
name <- paste(name, "csv", sep=".")

write.csv(upem_data, file=name)

reprocess <- upem_data[!complete.cases(upem_data$ave_flow), c("filter_id", "hhid", "session", "name")]


write.csv(reprocess, file="reprocess.csv")
