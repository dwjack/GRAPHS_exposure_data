require(plyr)
require(dplyr)
require(reshape)
require(lubridate)
require(foreach)

# FILE IDENTIFICATION
#create vectors of file names  -- look for both "CU_CO" and "CU_C0"
files<-list.files("~/Dropbox/Ghana_exposure_data_SHARED (1)/Main_study_exposure_assessment",recursive=T,pattern="^(CU_CO|CU_C0|CO_USB|COL_USB|CU-CO|CU-C0|CO-USB|COL-USB)", full.names=T) 
length(files) #4030 unprocessed files

s01 <- files[grep("s_01", files)] # separates out session 1

files <- files[files %in% s01] 
length(files) #1295 session 1 files

# checking how many processed files
# files3<-list.files("~/Dropbox/Ghana_exposure_data_SHARED (1)/Main_study_exposure_assessment",recursive=T,pattern="[[:alnum:]]*_[[:alnum:]]*_[[:alnum:]]*_[[:alnum:]]*_Data_Summary", full.names=T) #556 processed files

# subset the session 1 files to coincide with unique StudyIDs from the Ultrasound spreadsheet. 
Lascar_data <- as.data.frame(files[substr(gsub("^.*BM", "BM", files), 1,7) %in% StudyIDs]) # 913 files

# OR, if not doing the above,
# Lascar_data <- files

Lascar_data[,2] <- substr(gsub("^.*BM", "BM", Lascar_data[,1]), 1, 7)

Lascar_data <- Lascar_data[,c(2,1)]
colnames(Lascar_data) <- c("mstudyid", "Lascarfile")

# get rid of files that are actual duplicates (where 2 monitors were deployed simultaneously)
dupfiles_Lascar <- Lascar_data[grep("dup", Lascar_data[,2]),2] #4
Lascar_data <- Lascar_data[!Lascar_data$Lascarfile %in% dupfiles_Lascar,] 
nrow(Lascar_data) #907


length(unique(Lascar_data$mstudyid)) #905: compare to above number to see how many duplicates exist (July 14 just BM0328M and BM0583M which each have their own folders)


## sort out the duplicates before proceeding
dups <- Lascar_data$mstudyid[which(duplicated(Lascar_data$mstudyid))]
duplicates <- Lascar_data[Lascar_data$mstudyid %in% dups,]
duplicates <- duplicates[order(duplicates$mstudyid),] #sorted

## using the "duplicates" data frame, look into the folders and see if you can resolve any of the issues. Then run the above code again from the top of the page before proceeding.


# to remove duplicates that can't be resolved
Lascar_data <- Lascar_data[!Lascar_data$mstudyid %in% dups,] # remove duplicates
nrow(Lascar_data) # 903 unique observations


# check for child files 
child <- Lascar_data[grep("BM....C", Lascar_data[,2]),]  # no child files

unmatched_IDs <- StudyIDs[!StudyIDs %in% Lascar_data$mstudyid] #24 of these, including the 2 duplicates. As of Jul 14 all the rest were nonexistent for session 1
length(unmatched_IDs) #26

files <- as.character(Lascar_data[,2])

# proceed to run the 'lascar_import' function (in Lascar_data_stacker)