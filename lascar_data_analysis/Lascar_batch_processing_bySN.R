# identifies lascar files in the dropbox
# load the files once to get SN, date, lascarID, and firstline info to use in duplicate checking
# match this info to the calibration factors
# then load files in groups by SN and process and save the data in batches by SN. Apply calibration factors when available. Plot the data in the same batches. (THIS STEP TAKES A LONG TIME)
# Generate overall parameters for the processed files
# Generate forms to use in validation.
# Merge the manual validation info back into the overall parameters.



### SECTION 0: LOAD PACKAGES, FILES, and FUNCTIONS #######

require(lubridate)
require(plyr)
require(dplyr)
require(ggplot2)
require(scales)
require(reshape2)


# NOTE: path depends on computer: some dropboxes referred to as ~/Dropbox/Ghana_exposure_data_SHARED_2014 and some as ~/Dropbox/Ghana_exposure_data_SHARED_2014, do find/replace


######### Load necessary files
# Need calibration factor info (see "lascar_calibration_Jan2016.R")
# NOTE: you could temporarily process files with the mean cf and 'hi' cf confidence if there is no cf_new file, see section 1e.

cf_new <- readRDS("~/Dropbox/Ghana_exposure_data_SHARED_2014/CO_calibration_documents/Calibration Factors/Datasets/calib_factors_bymonth_interp_2016Jan31.rds")

######## Load necessary functions

# function to convert blanks to NA 
blank2na <- function(x){ 
  z <- gsub("\\s+", "", x)  #make sure it's "" and not " " etc
  x[z==""] <- NA 
  return(x)
}

# get.info function to pull out the SN
get.info <- function(x) { ####### read in first row of each data file and extract pertinent info
  
  possibleError <- tryCatch(
    dt <- read.csv(x, stringsAsFactors=F, header=T, nrows = 1)[,1:5],
    error = function(e) e
  )
  if(!inherits(possibleError, "error")) {
    
    dt$file2 <- basename(x)
    dt$lascar_infile <- names(dt)[1]
    dt$lascar_infile <- gsub("C0", "CO", dt$lascar)
    dt$lascar_infile <- gsub("-", "_", dt$lascar)
    dt$SN<- dt$Serial.Number
    dt$datetime <- dmy_hms(dt$Time, tz="GMT")
    dt$monthyear <- paste(months(dt$datetime), year(dt$datetime), sep = "_")
    dt$firstline <- paste(dt[,2:5], collapse = "/")
    dt <- dt[, c("file2", "SN", "lascar_infile", "monthyear", "firstline")]
    dt
  }
}

# Lascar.import function to calculate minutewise CO data 
lascar.import <- function(file,cf, cf_conf) { 
  dt <- read.csv(file, stringsAsFactors=F, header=T)[,1:5]
  dt$lascar <- names(dt)[1]
  dt$lascar <- gsub("C0", "CO", dt$lascar)
  dt$lascar <- gsub("-", "_", dt$lascar)
  dt$SN<- dt$Serial.Number[1]
  dt$datetime <- dmy_hms(dt$Time, tz="GMT")
  dt$rd.datetime <- as.character(round(dt$datetime, 'min'))
  dt<-dt %>% group_by(rd.datetime) %>% dplyr::summarise(mean(CO.ppm.), lascar[1], SN[1])
  names(dt) <- c('datetime','co', 'lascar', 'SN')
  dt$datetime <- ymd_hms(dt$datetime)
  dt$cf <- cf
  dt$cf_conf <- ifelse(!is.na(dt$cf[1]), cf_conf, "lo") # if CF is NA, set as low
  cfgood <- !is.na(dt$cf[1]) & dt$cf[1] >=0.2
  if(cfgood == TRUE) dt$co_corr <- dt$co/dt$cf
  if(cfgood == FALSE) dt$co_corr <- dt$co/0.85 # if CF is NA or < 0.2,  adjust with the mean
  dt
}

# CO.parameters function to calculate overall session parameters from processed files
CO.parameters <- function(x) { 
  CO_stacked_bySN <- readRDS(x)
  data <- CO_stacked_bySN %>% 
    group_by(file) %>% 
    summarise(mstudyid = mstudyid[1], 
              cstudyid = cstudyid[1], 
              session = session[1], 
              lascar = lascar[1], 
              sn = SN[1],  
              firstdate = datetime[1], 
              lastdate = datetime[n()], 
              co_mean = mean(co, na.rm = TRUE), 
              co_sd = sd(co, na.rm = TRUE), 
              co_q90 = quantile(co, probs = 0.9, na.rm = TRUE), 
              co_q98 = quantile(co, probs = 0.98, na.rm = TRUE), 
              co_cf = cf[1], 
              co_cf_conf = cf_conf[1], 
              co_mean_corr = mean(co_corr, na.rm = TRUE), 
              co_sd_corr = sd(co_corr, na.rm = TRUE), 
              co_q90_corr = quantile(co_corr, probs = 0.9, na.rm = TRUE), 
              co_q98_corr = quantile(co_corr, probs = 0.98, na.rm = TRUE), 
              
              co_day1_mean_corr = ifelse(difftime(datetime[n()], datetime[1], units = "hours") >= 24, mean(co_corr[datetime < datetime[1] + hours(24)], na.rm = TRUE), NA), 
              co_day2_mean_corr = ifelse(difftime(datetime[n()], datetime[1], units = "hours") >= 48, mean(co_corr[datetime >= datetime[1] + hours(24) & datetime < datetime[1] + hours(48)], na.rm = TRUE), NA), 
              co_day3_mean_corr = ifelse(difftime(datetime[n()], datetime[1], units = "hours") >= 72, mean(co_corr[datetime >= datetime[1] + hours(48) & datetime < datetime[1] + hours(72)], na.rm = TRUE), NA), 
              co_mean_first48_corr = ifelse(difftime(datetime[n()], datetime[1], units = "hours") >= 48, mean(co_corr[datetime < datetime[1] + hours(48)], na.rm = TRUE), NA),  
              co_mean_first72_corr = ifelse(difftime(datetime[n()], datetime[1], units = "hours") >= 72, mean(co_corr[datetime < datetime[1] + hours(72)], na.rm = TRUE), NA),
              co_hours = as.numeric(round(difftime(datetime[n()], datetime[1], units = "hours"), digits = 1)))
  data
}


############################ SECTION 1: GETTING INFO FROM RAW LASCAR FILES #####################

########## Section 1a: Make a list of the Lascar .csv files you want to process ###########
#create vectors of all file names  -----
files<-list.files("~/Dropbox/Ghana_exposure_data_SHARED_2014/Main_study_exposure_assessment",recursive=T,pattern="^(CU_CO|CU_C0|CO_USB|COL_USB|CU-CO|CU-C0|CO-USB|COL-USB)", full.names=T) 
length(files) #6656 / 6937 / Jan 29 7472 / Jan 17, 2016 11681 /Feb 1 11818

############### Section 1b: Subsetting the unvalidated files --- OPTIONAL #######
# If this is a later batch and you only want to work from  previously unvalidated files (rather than all the existing files), load the "unvalidated" file --- see SECTION 7. Otherwise, skip to Section 1c. 

unvalidated <- readRDS("/Users/Adoption/Dropbox/Ghana_exposure_data_SHARED_2014/CO_files_processed/CO_parameters_unvalidated_4734sessions_Jan30.rds")

nrow(unvalidated) # 4734

files <- files[basename(files) %in% basename(unvalidated$file_all)]
length(files) # 4735
sum(duplicated(basename(files))) # should be 0? but is 9... 
files <- files[!duplicated(basename(files))]
length(files) # 4726

######## END OPTIONAL Section 1b ########


############ Section 1c: make a data frame of the desired files ##############
Lascar_data <- data.frame(file = files, stringsAsFactors = FALSE)
Lascar_data$file2 <- basename(files)


lascar_pattern <- "(CU_CO.{4}|CU_C0.{4}|CO_USB.{4}|COL_USB.{4}|CU-CO.{4}|CU-C0.{4}|CO-USB.{4}|COL-USB.{4}|CU-CO.{4}|CU-C0.{4}|CO-USB.{4}|COL-USB.{4})" #note requires that lascar values be entered as three digits

Lascar_data$lascar<-regmatches(Lascar_data$file2, regexpr(lascar_pattern, Lascar_data$file2))

# make Lascar IDs consistent
length(unique(Lascar_data$lascar))
Lascar_data$lascar <- gsub("C0", "CO", Lascar_data$lascar)
Lascar_data$lascar <- gsub("-", "_", Lascar_data$lascar)

# get rid of hanging "_" at end of Lascar_data$lascar
Lascar_data$lascar <- ifelse(substr(Lascar_data$lascar, start = nchar(Lascar_data$lascar), stop = nchar(Lascar_data$lascar)) == "_", 
                              substr(Lascar_data$lascar, 1, nchar(Lascar_data$lascar)-1),
                              Lascar_data$lascar)



length(unique(Lascar_data$lascar)) 

############## Section 1d: load data to get SNs #################

file_data <- ldply(files, get.info, .progress = "text")

file_data$lascar_infile <- gsub("\\.", "_", file_data$lascar_infile)
#

Lascar_data <- merge(Lascar_data, file_data, by = "file2")

Lascar_data <- Lascar_data[!duplicated(Lascar_data$firstline),] # remove duplicates
nrow(Lascar_data) #6623/ Jan 29 7152/ Jan 15, 2016 5041/ Feb 1 4726

saveRDS(Lascar_data, file = paste0("Lascar_SN_data_", format(Sys.Date(), format = "%Y%b%d"), ".rds"))


############ Section 1e: match the Calibration Factor (pulls the CF from the month of the last day of monitoring) - see "lascar_calibration_Jan2016.R"

Lascar_data$monthyear <- paste0(substr(Lascar_data$monthyear, nchar(Lascar_data$monthyear)-3, nchar(Lascar_data$monthyear)), substr(Lascar_data$monthyear, 1, nchar(Lascar_data$monthyear) - 5))

##### IF there is no cf_new file, just assign the mean cf and hi confidence: ######
Lascar_data$cf <- 0.85
Lascar_data$cf_conf <- "hi"

##### BUT IDEALLY you should set the CF from a cf_new file by running the below:
Lascar_data$cf <- NA
Lascar_data$cf_conf <- NA
for (i in 1:nrow(Lascar_data)){
  cfmatch <- match(Lascar_data$SN[i], cf_new$SN) 
  Lascar_data$cf[i] <- cf_new[cfmatch, names(cf_new) == Lascar_data$monthyear[i]]
  Lascar_data$cf_conf[i] <- cf_new[cfmatch, names(cf_new) == paste0(Lascar_data$monthyear[i], "_conf")]
}
Lascar_data$cf <- as.numeric(Lascar_data$cf)





# get rid of files called "dropbox.attributes"
dropbox_pattern <- "dropbox.attributes"
Lascar_data <- Lascar_data[regexpr(dropbox_pattern, Lascar_data$file) == -1,]

summary(Lascar_data) # 66 NAs for cf
length(unique(Lascar_data$SN[is.na(Lascar_data$cf)])) # 7 unique Lascars have no calib info

# Section 1f: save Lascar_data
saveRDS(Lascar_data, file = paste0("Lascar_data_cf_", format(Sys.Date(), format = "%Y%b%d"), ".rds" ))



####### SECTION 2: IMPORT AND PROCESS THE DATA BY SN ##############

#################### START LOOP HERE if working from raw Lascar files ###################
# If working from raw files be sure to comment out section at "START HERE IF WORKING FROM SAVED .RDS DATA
## NOTE: THE FOLLOWING STEPS FROM THE RAW DATA TAKE A LONG TIME!

# load the Lascar data (from file saved in Section 1) and set directory locations to save the processed data and plots ######
Lascar_data <- readRDS("~/Dropbox/Ghana_exposure_data_SHARED_2014/CO_calibration_documents/Calibration Factors/Datasets/Lascar_data_cf_2016Feb01.rds")

# set a directory for the saved data. 
directory <- "/Users/Adoption/Dropbox/Ghana_exposure_data_SHARED_2014/CO_files_processed/2016Feb01/CO_stacked_files/"

# set a directory for the saved plots 
plotdirectory <- "~/Dropbox/Ghana_exposure_data_SHARED_2014/CO_files_processed/2016Feb01/Plots by SN/"

# set a meanCF to use for plots that have no CF
meancf <- 0.85 #(mean across all units' CFs that were between 0.6 and 1.2 between Feb and Dec 2014)

########## Process the data

ptm <- proc.time()

for (i in 1:length(unique(Lascar_data$SN))) { 
  files_bySN <- Lascar_data[Lascar_data$SN == unique(Lascar_data$SN)[i], c("file", "cf", "cf_conf")]
  
  CO_stacked <- mdply(files_bySN, lascar.import, .progress = "text") # mdply so can supply multiple arguments to lascar.import. Variable names of files_bySN must match those in lascar.import (file, cf, cf_conf)
  
  CO_stacked <- CO_stacked[!is.na(CO_stacked$co),] # removing NAs
  
  
  # grab mother and child id info
  id_pattern <- "BM....."
  CO_stacked$mstudyid <- regmatches(CO_stacked$file, regexpr(id_pattern, CO_stacked$file))
  
  child_pattern <- "BM....C"
  CO_stacked$cstudyid <- regexpr(child_pattern, CO_stacked$file)
  CO_stacked$cstudyid <- ifelse(CO_stacked$cstudyid == -1, NA, substr(x = CO_stacked$file, start = CO_stacked$cstudyid, stop = CO_stacked$cstudyid + 6))
  
  # grab session info
  # grab session info: do in multipe steps to deal with bad value propagation after NA
  session_pattern <- "(s_[0123456789]{1,2}|s[0123456789]{1,2})"
  CO_stacked$session <- regmatches(CO_stacked$file, regexpr(session_pattern, CO_stacked$file, ignore.case =TRUE))
  CO_stacked$session <- tolower(CO_stacked$session)
  
  # order from most recent to oldest
  CO_stacked <- CO_stacked[order(CO_stacked$datetime),]
  
  # simplify Lascar names
  CO_stacked$lascar <- gsub("\\.", "_", CO_stacked$lascar)
  
 
    # separate and save the data by SN, to the directory set above
    saveRDS(CO_stacked, file = paste0(directory, "CO_stacked_", CO_stacked$lascar[1], "_", CO_stacked$SN[1],".rds"))
    
    
    
    ########################## START HERE IF WORKING FROM SAVED .RDS DATA ######################
    #     #  If working from saved .rds data and ONLY redoing the plots, START HERE and run this section thru first print(i), otherwise comment it out along with the print(i) before the loop closure------
    #     
    #     
    # savedfiles <- list.files("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/29Jan2015/CO_stacked files/", full.names = TRUE)
    #     length(savedfiles)
    #     for (i in 1:length(savedfiles)) {
          #CO_stacked <- readRDS(savedfiles[i])
      #   CO_stacked <- arrange(CO_stacked, datetime) # ascending by date
    

   
    
    identifier <- paste("allplots", gsub("\\.", "_", CO_stacked$lascar[1]), CO_stacked$SN[1], sep = "_")
    
    # make and save plots
    pdf(file = paste0(plotdirectory, identifier,".pdf"), height = 10, width = 10)
    par(mfrow = c(3,3))
    for (j in 1:length(unique(CO_stacked$file))) {
      CO_bysession <- filter(CO_stacked, file == unique(file)[j])
      
      # evaluate parameters
      cfgood <- !is.na(CO_bysession$cf[1]) & CO_bysession$cf[1] >=0.2
      mean <- ifelse(cfgood ==TRUE, round(mean(CO_bysession$co_corr, na.rm = TRUE), digits = 2), round(mean(CO_bysession$co/meancf, na.rm = TRUE), digits = 2))
      q90 <-ifelse(cfgood ==TRUE, round(quantile(CO_bysession$co_corr, probs = 0.9, na.rm = TRUE), digits = 2), round(quantile(CO_bysession$co/meancf, probs = 0.9, na.rm = TRUE), digits = 2))
      q98 <- ifelse(cfgood ==TRUE, round(quantile(CO_bysession$co_corr, probs = 0.98, na.rm = TRUE), digits =2), round(quantile(CO_bysession$co/meancf, probs = 0.98, na.rm = TRUE), digits =2))
      sd <- ifelse(cfgood ==TRUE, round(sd(CO_bysession$co_corr, na.rm =TRUE), digits = 2), round(sd(CO_bysession$co/meancf, na.rm =TRUE), digits = 2))
      min <- ifelse(cfgood ==TRUE, round(min(CO_bysession$co_corr, na.rm = TRUE), digits = 2), round(min(CO_bysession$co/meancf, na.rm = TRUE), digits = 2))
      invalid <- q98==0|q90 > 20|sd > 60|min > 10
      
      
      # plot
      title <- paste(j, CO_bysession$lascar[1], CO_bysession$SN[1], "\n", format(CO_bysession$datetime[1], format = "%b %d %Y"), "CF=", round(CO_bysession$cf[1], digits = 2), "CF_conf=", CO_bysession$cf_conf[1], "\n mean=", mean, " sd=", sd, " q90=", q90, " q98=", q98, "min=", min)
      range <- ifelse(cfgood ==TRUE, max(CO_bysession$co_corr), max(CO_bysession$co)/meancf)
      
      plot(CO_bysession$datetime, CO_bysession$co, type = "l", ylim = c(0, range), main = "" , xlab = paste(CO_bysession$mstudyid[1], CO_bysession$session[1]), ylab = "", lwd = 2, col = "black")
      
      if (cfgood ==TRUE & CO_bysession$cf_conf[1] == "hi") lines(CO_bysession$datetime, CO_bysession$co_corr, col = alpha("green",0.6), lwd = 2) # green if hi confidence
      
      if (cfgood ==TRUE & CO_bysession$cf_conf[1] == "medium") lines(CO_bysession$datetime, CO_bysession$co_corr, col = alpha("plum",0.6), lwd = 2) # plum if medium confidence
      if (cfgood ==TRUE & CO_bysession$cf_conf[1] == "lo") lines(CO_bysession$datetime, CO_bysession$co_corr, col = alpha("coral",0.6), lwd = 2) # coral if low confidence
      if (cfgood ==FALSE | CO_bysession$cf_conf[1] == "none")  lines(CO_bysession$datetime, CO_bysession$co/meancf, col = alpha("darkgrey", 0.6), lwd = 2) # plot the mean-corrected values in grey if conf none or if there is no CF for this instrument
      
      mtext("CO (ppm)", side = 2, line = 2, cex = 0.7)
      legend(x="topright", legend = c(paste("CO, mean = ", round(mean(CO_bysession$co), digits=2)), paste("CO_corr, mean =", ifelse(cfgood ==TRUE, round(mean(CO_bysession$co_corr), digits = 2), round(mean(CO_bysession$co/meancf), digits =2 )))), col = c("black", ifelse((cfgood ==TRUE & CO_bysession$cf_conf[1] == "hi"), "green", ifelse((cfgood == TRUE & CO_bysession$cf_conf == "lo"), "coral", "darkgrey"))), lwd = 2, cex = 0.9)
      
      if (invalid == TRUE &!is.na(invalid)) title(main = title,  cex.main = 0.95, col.main = "red") # change title to red if any of the validity criteria are invalid
      if (invalid == FALSE | is.na(invalid)) title(main = title,  cex.main = 0.95)
      if (difftime(CO_bysession$datetime[nrow(CO_bysession)],CO_bysession$datetime[1],units = "hours") < 44) title(main = title,  cex.main = 0.95, col.main = "blue") # change title to blue if < 44 hours
      
    }
    dev.off()
    
  
  
  
  #}
  # Loop ends here if just plotting and not saving data
  
  print(i)
  print(unique(CO_stacked$lascar))
  print(unique(CO_stacked$SN))
  print(length(unique(CO_stacked$file)))
}

proc.time()-ptm


########################################### END plot saving ############

# check how many plots were saved (should equal the number of unique Lascar SNs)
allplots <- list.files("~/Dropbox/Ghana_exposure_data_SHARED_2014/CO_files_processed/2016Jan16/Plots by SN", full.names = TRUE)
length(allplots) # 174/ 264


################ SECTION 3: Calculating parameters for the saved data #############


## Load COfiles: the desired stacked files

# identify the path
path <- "~/Dropbox/Ghana_exposure_data_SHARED_2014/CO_files_processed/2016Feb01/CO_stacked_files/"
# older

# get the file names
COfiles <- list.files(path,recursive=FALSE, pattern = ".rds",full.names=TRUE) 
length(COfiles) #174 


# Calculate CO parameters
CO_parameters <- ldply(COfiles, CO.parameters, .progress = "text")

CO_parameters$lascar <- gsub("\\.", "_", CO_parameters$lascar)
CO_parameters$file_all <- CO_parameters$file
CO_parameters$file <- basename(CO_parameters$file_all)
CO_parameters$co_cf_conf[CO_parameters$co_cf_conf == "medium"] <- "lo" # had medium for the tail end if > 8 months since calibrated, change to lo

length(unique(CO_parameters$lascar)) # 210
length(unique(CO_parameters$SN)) #174
nrow(CO_parameters) #7152 / 5041

CO_parameters <- CO_parameters[!is.na(CO_parameters$co_mean),] # removing NAs
nrow(CO_parameters) #7152 / 2016 Jan17: 5041

saveRDS(CO_parameters, file = paste0("CO_parameters_", nrow(CO_parameters), "sessions_", format(Sys.Date(), format = "%Y%b%d"), ".rds"))




######## SECTION 4: Make and save forms to fill in later for validation ######

# load names of the CO_stacked files saved in Section 2 above
savedfiles <- list.files("~/Dropbox/Ghana_exposure_data_SHARED_2014/CO_files_processed/2016Feb01/CO_stacked_files", full.names = TRUE)
length(savedfiles)

# set directory where you want to save the validation forms
directory <- "~/Dropbox/Ghana_exposure_data_SHARED_2014/CO_files_processed/2016Feb01/Validation Forms 2016Feb01/"

# set mean cf
meancf <- 0.83

# make and save the validation forms
for (i in 1:length(savedfiles)){ #
  form <- data.frame()
  CO_stacked_bySN <- readRDS(savedfiles[i])
  CO_stacked_bySN <- arrange(CO_stacked_bySN, datetime)
  
  for (j in 1:length(unique(CO_stacked_bySN$file))) {
    CO_bysession <- filter(CO_stacked_bySN, file == unique(file)[j])
    CO_bysession <- arrange(CO_bysession, datetime)
    info <- CO_bysession[1, c(1,2,4:6,8:10)]
    info$file <- basename(info$file)
    info$number <- j
    info <- info[, c(9, 1,3,4,6:8,2,5)]
    info$duration_valid <- ifelse(difftime(CO_bysession$datetime[nrow(CO_bysession)], CO_bysession$datetime[1], units = "hours") >= 44, 1, 3)
    info$cf_valid <- ifelse((info$cf > 0.6 & info$cf < 1.2 &!is.na(info$cf)), 1, ifelse(((info$cf > 0.2 & info$cf < 0.6 &!is.na(info$cf))|(info$cf > 1.2 &!is.na(info$cf))), 2, 3))
    info$lascar <- gsub("\\.", "_", info$lascar)
    
    # evaluate co validity parameters
    q90 <-ifelse(!is.na(CO_bysession$cf[1]), round(quantile(CO_bysession$co_corr, probs = 0.9, na.rm = TRUE), digits = 2), round(quantile(CO_bysession$co/meancf, probs = 0.9, na.rm = TRUE), digits = 2))
    q98 <- ifelse(!is.na(CO_bysession$cf[1]), round(quantile(CO_bysession$co_corr, probs = 0.98, na.rm = TRUE), digits =2), round(quantile(CO_bysession$co/meancf, probs = 0.98, na.rm = TRUE), digits =2))
    sd <- ifelse(!is.na(CO_bysession$cf[1]), round(sd(CO_bysession$co_corr, na.rm =TRUE), digits = 2), round(sd(CO_bysession$co/meancf, na.rm =TRUE), digits = 2))
    min <- ifelse(!is.na(CO_bysession$cf[1]), round(min(CO_bysession$co_corr, na.rm = TRUE), digits = 2), round(min(CO_bysession$co/meancf, na.rm = TRUE), digits = 2))
    invalid <- q98==0|q90 > 20|sd > 60|min > 10
    info$co_valid_init <- ifelse(invalid == FALSE, 1, 2)
    info$NEW_CF <- ""
    info$CO_VALID <- ""
    info$NOTES_NOTES_NOTES <- ""
    form <- rbind(form, info)
    write.csv(form, file = paste0(directory, "ValidityForm_", form$lascar[1], "_",form$SN[1], "_",format(Sys.Date(), format = "%Y%b"), ".csv"), row.names = FALSE) # this saves a validation form
  }
  
  print(i)
}


############### SECTION 5: STOP AND DO VISUAL VALIDATION ##############

# Now go through the processed plots file by file and fill in the following columns on the validation forms: co_valid, NOTES_NOTES_NOTES, and Validated.by columns. I recommend saving the completed forms to a DIFFERENT folder so they won't accidentally get overwritten if the code above gets run again.

# When done with visual validation, proceed.

############### SECTION 6: Add validation data back in to CO_parameters #############

# load the completed validation forms
validforms <- list.files("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/Validation Forms/Reviewed Jan 2015/", full.names = TRUE)
length(validforms) #171

# load and merge CO validition forms - 
import.list <- llply(validforms, read.csv) # warnings about "incomplete final lines" are ok?
CO_validation <- Reduce(function(x, y) merge(x, y, all=T), import.list, accumulate=F)

# check row length
rows <- 0
for (i in 1:length(import.list)) { 
  a <- nrow(import.list[[i]])
  rows <- sum(rows, a)
  rows
}
rows # 6623

unique(CO_validation$CO_VALID) # check that only contains 1,2,3, and NA. If not, go back to forms and fix!


CO_all <- merge(CO_parameters, CO_validation[,c("file", "CO_VALID", "NOTES_NOTES_NOTES", "Validated.by")], all.x = TRUE, by = "file")

colnames(CO_all) <- tolower(colnames(CO_all))
colnames(CO_all)[colnames(CO_all) == "co_valid"] <- "visually_valid"
colnames(CO_all)[colnames(CO_all) == "notes_notes_notes"] <- "visual_notes"


CO_all$visual_notes <- blank2na(CO_all$visual_notes)
CO_all$validated.by <- blank2na(CO_all$validated.by)



### Calculate duration validity
## 1 if >=44 hours, 2 if between 18 and 44 hours, 3 if < 18 hours 
CO_all$duration_valid <- ifelse(CO_all$co_hours >=44, 1, ifelse(CO_all$co_hours >=18, 2, 3))

### Calculate overall validity
 ## 1 If cf_conf hi, visual validity =1 , duration =1
 ## 2 if cf_conf hi, visual validity =2, duration = 1 or 2
 ## 2 if cf_conf = hi, visual validity = 1, duration = 2
 ## 3 if cf_conf lo, visual validity =1 or 2, duration =1 or 2
 ## 4 if visual validity= 3 or duration = 3 or cf_conf = none

CO_all$overall_valid <- ifelse(CO_all$co_cf_conf == "hi" & CO_all$visually_valid == 1 & CO_all$duration_valid == 1, 1, ifelse(CO_all$co_cf_conf == "hi" & CO_all$visually_valid ==2 & (CO_all$duration_valid == 1 | CO_all$duration_valid == 2), 2, ifelse(CO_all$co_cf_conf == "hi" & CO_all$visually_valid == 1 & CO_all$duration_valid == 2, 2, ifelse(CO_all$co_cf_conf == "lo" & (CO_all$visually_valid == 1 | CO_all$visually_valid == 2) & (CO_all$duration_valid == 1 | CO_all$duration_valid == 2), 3, ifelse(CO_all$visually_valid == 3 | CO_all$duration_valid == 3 | CO_all$co_cf_conf == "none", 4, NA)))))

# Check forms
colSums(is.na(CO_all))
unique(CO_all[!is.na(CO_all$visually_valid) & is.na(CO_all$validated.by), "lascar"])

### Remove files that haven't been validated yet 
CO_all <- CO_all[!is.na(CO_all$visually_valid),]
nrow(CO_all) #6619

### save the CO_parameters with the validated info added as .RDS and .csv
saveRDS(CO_all, file = paste0("CO_parameters_validated_", nrow(CO_all), "sessions_", format(Sys.Date(), format = "%b%d"), ".rds"))

write.csv(CO_all, file = paste0("CO_parameters_validated_", nrow(CO_all), "sessions_", format(Sys.Date(), format = "%b%d"), ".csv"))

################ If you have processed only one batch of files, you can stop here. ###


# If you need to combine multiple CO_parameters files, continue.

################ SECTION 7: COMBINE SEPARATE CO_parameters files from different processing sessions (IF NEEDED) ############

## Combine CO_parameters files and remove duplicates --------------
params1 <- readRDS("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/CO_parameters_validated_6619sessions_Jan30.rds")
params2 <- readRDS("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/CO_parameters_5041sessions_2016Jan30.rds")

params1$set <- "params1"
params2$set <- "params2"
params <- rbind.fill(params1, params2) #11660 rows
params$cstudyid <- blank2na(params$cstudyid)
params$ids <- paste(as.character(params[,6]), as.character(params[,7]), as.character(params[,8]))

sum(duplicated(params)) #0
sum(duplicated(params$file)) #0

# these next lines are to look into what the issues are with the duplicated files. These have to be manually sorted out to some extent. 
sum(duplicated(params[,2:24])) # 268 just have formatting probs with file
params <- params[!duplicated(params[,2:24]),] #11392 rows

sum(duplicated(params$ids)) #39
dups <- params$ids[duplicated(params$ids)] 

duprows <- params[params$ids %in% dups,] #78
duprows <- arrange(duprows, ids)

duprows[,c("file",  "sn", "lastdate", "set", "visually_valid", "visual_notes", "overall_valid")]
# these are weird ones where the same file is assigned to two studyids. What to do - ASSUME THE ONE FROM PARAMS2 (later set) IS CORRECT? Maybe just leave these in for future appraisal.

# make the validation notes the same for the duplicated rows
for (i in seq(from = 1, to = 78, by = 2)) {
  for(j in 26:30) {
    duprows[i+1,j] <- duprows[i,j]
  }
}

duprows$is_duplicated <- TRUE

# remove duplicates and save the CO_parameters
params <- params[!params$file %in% duprows$file,]
params$is_duplicated <- FALSE

params <- rbind(params, duprows) # 11392

saveRDS(params, file = paste0(paste0("CO_parameters_all_", nrow(params), "sessions_", format(Sys.Date(), format = "%b%d"), ".rds")))

# separate out and save the unvalidated CO_parameters
unvalidated <- params[is.na(params$visually_valid),]
saveRDS(unvalidated, file = paste0(paste0("CO_parameters_unvalidated_", nrow(unvalidated), "sessions_", format(Sys.Date(), format = "%b%d"), ".rds")))

# go back to the top to process unvalidated files so they'll line up with the plots

######################################### THE END 



