# identifies lascar files in the dropbox
# load the files once to get SN, date, lascarID, and firstline info to use in duplicate checking
# match this info to the calibration factors
# then load files in groups by SN and process and save the data in batches by SN. Apply calibration factors when available. Plot the data in the same batches. (THIS STEP TAKES A LONG TIME)
# Generate overall parameters for the processed files
# Generate forms to use in validation.

require(lubridate)
require(plyr)
require(dplyr)
require(ggplot2)
require(scales)
require(reshape2)
#####################
# To load data from original Lascar .csv files
#####################


#create vectors of all file names  -----
files<-list.files("~/Dropbox/Ghana_exposure_data_SHARED (1)/Main_study_exposure_assessment",recursive=T,pattern="^(CU_CO|CU_C0|CO_USB|COL_USB|CU-CO|CU-C0|CO-USB|COL-USB)", full.names=T) 
length(files) #6656 / 6937

# make a data frame of the files
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



length(unique(Lascar_data1$lascar)) # 203

# match to SNs ----

get.info <- function(x) { ####### read in first row of each data file and extract pertinent info
  dt <- read.csv(x, stringsAsFactors=F, header=T, nrows = 1)[,1:5]
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

file_data <- ldply(files, get.info, .progress = "text")

file_data$lascar_infile <- gsub("\\.", "_", file_data$lascar_infile)
#

Lascar_data <- merge(Lascar_data, file_data, by = "file2")

Lascar_data <- Lascar_data[!duplicated(Lascar_data$firstline),] # remove duplicates
nrow(Lascar_data) #6623

# match the CF (pulls the CF from the month of the last day of monitoring)
cf_new <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/calib_factors_bymonth_interp_Dec20.rds")
for (i in 1:nrow(Lascar_data)){
  cfmatch <- match(Lascar_data$SN[i], cf_new$SN) 
  Lascar_data$cf[i] <- cf_new[cfmatch, names(cf_new) == Lascar_data$monthyear[i]]
}
Lascar_data$cf <- as.numeric(Lascar_data$cf)



# get rid of files called "dropbox.attributes"
dropbox_pattern <- "dropbox.attributes"
Lascar_data <- Lascar_data[regexpr(dropbox_pattern, Lascar_data$file) == -1,]

# save Lascar_data
saveRDS(Lascar_data, file = paste0("Lascar_data_cf_", format(Sys.Date(), format = "%b%d"), ".rds" ))





# Lascar.import function -------
lascar.import <- function(file,cf) { 
  dt <- read.csv(file, stringsAsFactors=F, header=T)[,1:5]
  dt$lascar <- names(dt)[1]
  dt$lascar <- gsub("C0", "CO", dt$lascar)
  dt$lascar <- gsub("-", "_", dt$lascar)
  dt$SN<- dt$Serial.Number[1]
  dt$datetime <- dmy_hms(dt$Time, tz="GMT")
  dt$rd.datetime <- as.character(round(dt$datetime, 'min'))
  dt<-dt %.% group_by(rd.datetime) %.% dplyr::summarise(mean(CO.ppm.), lascar[1], SN[1]) #replaced the plyr approach that ajay provided w/ ddply
  names(dt) <- c('datetime','co', 'lascar', 'SN')
  dt$datetime <- ymd_hms(dt$datetime)
  dt$cf <- cf
  dt$co_corr <- dt$co/dt$cf
  dt
}


#################### START LOOP HERE if working from raw Lascar files ###################
# If working from raw files be sure to comment out section at "START HERE IF WORKING FROM SAVED .RDS DATA
## NOTE: THE FOLLOWING STEPS FROM THE RAW DATA TAKE A LONG TIME!

Lascar_data <- readRDS("/Users/ashlinn/Dropbox/Ghana project/BP project/Baseline BP Paper/Ghana BP R Materials/Lascar_data_cf_Dec21.rds")

##################################
# set a directory for the saved data
directory <- "~/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/20Dec2014/CO_stacked files/"
##################################


ptm <- proc.time()

for (i in 1:length(unique(Lascar_data$SN))) { #
  files_bySN <- Lascar_data[Lascar_data$SN == unique(Lascar_data$SN)[i], c("file", "cf")]
  
  CO_stacked <- mdply(files_bySN, lascar.import, .progress = "text") # mdply so can supply multiple arguments to lascar.import. Variable names of files_bySN must match those in lascar.import (file, cf)
  
  CO_stacked <- CO_stacked[!is.na(CO_stacked$co),] # removing NAs
  
  
  # grab mother and child id info
  id_pattern <- "BM....."
  CO_stacked$mstudyid <- regmatches(CO_stacked$file, regexpr(id_pattern, CO_stacked$file))
  
  child_pattern <- "BM....C"
  CO_stacked$cstudyid <- regexpr(child_pattern, CO_stacked$file)
  CO_stacked$cstudyid <- ifelse(CO_stacked$cstudyid == -1, NA, substr(x = CO_stacked$file, start = CO_stacked$cstudyid, stop = CO_stacked$cstudyid + 6))
  
  # grab session info
  session_pattern <- "(s_..|S_..)"
  CO_stacked$session <- regmatches(CO_stacked$file, regexpr(session_pattern, CO_stacked$file))
  
  # order from most recent to oldest
  CO_stacked <- CO_stacked[order(CO_stacked$datetime),]
  
  # simplify Lascar names
  CO_stacked$lascar <- gsub("\\.", "_", CO_stacked$lascar)
  
 
    # separate and save the data by SN
    saveRDS(CO_stacked, file = paste0(directory, "CO_stacked_", CO_stacked$lascar[1], "_", CO_stacked$SN[1],".rds"))
    
    
    
    ########################## START HERE IF WORKING FROM SAVED .RDS DATA ######################
    #     #  If working from saved .rds data START HERE and run this section thru first print[i], otherwise comment it out along with the print[i] before the loop closure------
    #     
    #     
    #     savedfiles <- list.files("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/18Dec2014/CO_stacked files/", full.names = TRUE)
    #     length(savedfiles)
    #     for (i in 1:length(savedfiles)) {
    #       CO_stacked_bySN <- readRDS(savedfiles[i])
    #       CO_stacked_bySN <- arrange(CO_stacked_bySN, datetime) # ascending by date
    
    #########################
    
    
    #########################
    # set a directory for the saved plots & a meanCF to use for plots that have no CF
    #########################
    plotdirectory <- "~/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/20Dec2014/Plots by SN/"
    meancf <- 0.83 #(mean across all units' CFs that were between 0.3 and 1.5 between Feb and Dec 2014)
    
    
    identifier <- paste("allplots", gsub("\\.", "_", CO_stacked$lascar[1]), CO_stacked$SN[1], sep = "_")
    
    # plot
    pdf(file = paste0(plotdirectory, identifier,".pdf"), height = 10, width = 10)
    par(mfrow = c(3,3))
    for (j in 1:length(unique(CO_stacked$file))) {
      CO_bysession <- filter(CO_stacked, file == unique(file)[j])
      
      # evaluate parameters
      cfgood <- !is.na(CO_bysession$cf[1]) & CO_bysession$cf[1] > 0.3 & CO_bysession$cf[1] < 1.5
      mean <- ifelse(cfgood ==TRUE, round(mean(CO_bysession$co_corr, na.rm = TRUE), digits = 2), round(mean(CO_bysession$co/meancf, na.rm = TRUE), digits = 2))
      q90 <-ifelse(cfgood ==TRUE, round(quantile(CO_bysession$co_corr, probs = 0.9, na.rm = TRUE), digits = 2), round(quantile(CO_bysession$co/meancf, probs = 0.9, na.rm = TRUE), digits = 2))
      q98 <- ifelse(cfgood ==TRUE, round(quantile(CO_bysession$co_corr, probs = 0.98, na.rm = TRUE), digits =2), round(quantile(CO_bysession$co/meancf, probs = 0.98, na.rm = TRUE), digits =2))
      sd <- ifelse(cfgood ==TRUE, round(sd(CO_bysession$co_corr, na.rm =TRUE), digits = 2), round(sd(CO_bysession$co/meancf, na.rm =TRUE), digits = 2))
      min <- ifelse(cfgood ==TRUE, round(min(CO_bysession$co_corr, na.rm = TRUE), digits = 2), round(min(CO_bysession$co/meancf, na.rm = TRUE), digits = 2))
      invalid <- q98==0|q90 > 20|sd > 60|min > 10
      
      
      # plot
      title <- paste(j, CO_bysession$lascar[1], CO_bysession$SN[1], "\n", format(CO_bysession$datetime[1], format = "%b %d %Y"), "CF=", ifelse (cfgood ==TRUE, round(CO_bysession$cf[1], digits = 2), round(meancf, digits = 2)), "\n mean=", mean, " sd=", sd, " q90=", q90, " q98=", q98, "min=", min)
      range <- ifelse(cfgood ==TRUE, max(CO_bysession$co_corr), max(CO_bysession$co)/meancf)
      
      plot(CO_bysession$datetime, CO_bysession$co, type = "l", ylim = c(0, range), main = "" , xlab = paste(CO_bysession$mstudyid[1], CO_bysession$session[1]), ylab = "", lwd = 2, col = "green")
      
      if (cfgood ==TRUE) lines(CO_bysession$datetime, CO_bysession$co_corr, col = alpha("red",0.5), lwd = 2)
      if (cfgood ==FALSE)  lines(CO_bysession$datetime, CO_bysession$co/meancf, col = alpha("blue", 0.5)) # plot the mean correction in blue if there is no CF for this instrument
      
      mtext("CO (ppm)", side = 2, line = 2, cex = 0.7)
      legend(x="topright", legend = c(paste("CO, mean = ", round(mean(CO_bysession$co), digits=2)), paste("CO_corr, mean =", ifelse(cfgood ==TRUE, round(mean(CO_bysession$co_corr), digits = 2), round(mean(CO_bysession$co/meancf), digits =2 )))), col = c("green", ifelse(cfgood ==TRUE, "red", "blue")), lwd = 2, cex = 0.9)
      
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



allplots <- list.files("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/20Dec2014/Plots by SN/", full.names = TRUE)
length(allplots) # 171

####################################################################
### Calculating parameters for the saved data-------
####################################################################
# This is generating an equivalent list of NAs?

COfiles <- list.files("~/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/20Dec2014/CO_stacked files/",recursive=FALSE, pattern = ".rds",full.names=TRUE) 
length(COfiles) #171


CO.parameters <- function(x) {
  CO_stacked_bySN <- readRDS(x)
  data <- CO_stacked_bySN %.% group_by(file) %.% summarise(mstudyid = mstudyid[1], cstudyid = cstudyid[1], session = session[1], lascar = lascar[1], SN = SN[1],  datetime = datetime[1],  mean = mean(co, na.rm = TRUE), sd = sd(co, na.rm = TRUE), q90 = quantile(co, probs = 0.9, na.rm = TRUE), q98 = quantile(co, probs = 0.98, na.rm = TRUE), cf = cf[1], mean_corr = mean(co_corr, na.rm = TRUE), sd_corr = sd(co_corr, na.rm = TRUE), q90_corr = quantile(co_corr, probs = 0.9, na.rm = TRUE), q98_corr = quantile(co_corr, probs = 0.98, na.rm = TRUE))
  data
}

CO_parameters <- ldply(COfiles, CO.parameters, .progress = "text")

CO_parameters$lascar <- gsub("\\.", "_", CO_parameters$lascar)


length(unique(CO_parameters$lascar)) # 207
length(unique(CO_parameters$SN)) #171

saveRDS(CO_parameters, file = paste0("CO_parameters_", nrow(CO_parameters), "sessions_", format(Sys.Date(), format = "%b%d"), ".rds"))




######## Make forms to fill in later for validation ---------------
savedfiles <- list.files("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/20Dec2014/CO_stacked files/", full.names = TRUE)
length(savedfiles)

directory <- "~/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/Validation Forms/"

meancf <- 0.83
for (i in 1:length(savedfiles)){ #
  form <- data.frame()
  CO_stacked_bySN <- readRDS(savedfiles[i])
  CO_stacked_bySN <- arrange(CO_stacked_bySN, datetime)
  
  for (j in 1:length(unique(CO_stacked_bySN$file))) {
    CO_bysession <- filter(CO_stacked_bySN, file == unique(file)[j])
    CO_bysession <- arrange(CO_bysession, datetime)
    info <- CO_bysession[nrow(CO_bysession), c(1,2,4:6,8:10)]
    info$file <- basename(info$file)
    info$number <- j
    info <- info[, c(9, 1,3,4,6:8,2,5)]
    info$duration_valid <- ifelse(difftime(CO_bysession$datetime[nrow(CO_bysession)], CO_bysession$datetime[1], units = "hours") >= 44, 1, 3)
    info$cf_valid <- ifelse(info$cf > 0.3 & info$cf < 1.5 &!is.na(info$cf), 1, 3)
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
    write.csv(form, file = paste0(directory, "ValidityForm_", form$lascar[1], "_",form$SN[1], ".csv"), row.names = FALSE)
  }
  
  print(i)
}

