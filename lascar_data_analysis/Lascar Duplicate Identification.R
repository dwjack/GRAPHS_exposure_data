# Lascar file duplicate/problem identification by session


## get Lascar files
files<-list.files("~/Dropbox/Ghana_exposure_data_SHARED (1)/Main_study_exposure_assessment",recursive=T,pattern="^(CU_CO|CU_C0|CO_USB|COL_USB|CU-CO|CU-C0|CO-USB|COL-USB)", full.names=F) 
length(files)  #4894

# make a data frame of the files
Lascar_data1 <- data.frame(file = files)

# grab mother and child id info
id_pattern <- "BM....."
Lascar_data1$mstudyid <- regmatches(Lascar_data1$file, regexpr(id_pattern, Lascar_data1$file))

child_pattern <- "BM....C"
Lascar_data1$cstudyid <- regexpr(child_pattern, Lascar_data1$file)
Lascar_data1$cstudyid <- ifelse(Lascar_data1$cstudyid == -1, NA, substr(x = Lascar_data1$file, start = Lascar_data1$cstudyid, stop = Lascar_data1$cstudyid + 6))

# identify those files with ids matching neither mother nor child
Lascar_data1$problem_id <- regmatches(Lascar_data1$file, gregexpr(id_pattern, Lascar_data1$file))
Lascar_data1$problem_id <- ifelse(sapply(Lascar_data1$problem_id, unique, "[[") == Lascar_data1$mstudyid, NA, sapply(Lascar_data1$problem_id, unique, "[["))
Lascar_data1$problem_id <- ifelse(substr(Lascar_data1$problem_id, 15, 21) %in% Lascar_data1$cstudyid & nchar(Lascar_data1$problem_id) < 24, NA, Lascar_data1$problem_id)

# can't parse further

# grab session info
session_pattern <- "s_.."
Lascar_data1$session <- regmatches(Lascar_data1$file, regexpr(session_pattern, Lascar_data1$file))
Lascar_data1$session2 <- regmatches(Lascar_data1$file, gregexpr(session_pattern, Lascar_data1$file))
Lascar_data1$session2 <- ifelse(sapply(Lascar_data1$session2, unique, "[[") == Lascar_data1$session, NA, sapply(Lascar_data1$session2, unique, "[["))

# identify those paths that contain more than 1 session and there is no child
Lascar_data1$problem_session <- ifelse(nchar(as.character(Lascar_data1$session2) > 2) & is.na(Lascar_data1$cstudyid), Lascar_data1$session2, NA)



# get rid of files that are actual duplicates (where 2 monitors were deployed simultaneously)
dupfiles_Lascar <- Lascar_data1[grep("dup", Lascar_data1$file),1] #24
Lascar_data1 <- Lascar_data1[!Lascar_data1$file %in% dupfiles_Lascar,] # row numbers get added here, not sure how to avoid
nrow(Lascar_data1) # 4888

# search for duplicated files
# separate the file name from the full path
Lascar_data1$filename<-basename(as.character(Lascar_data1$file))

# id the duplicates and send to new data frame
dups <- Lascar_data1$filename[which(duplicated(Lascar_data1$filename))]

Lascar_data1$duplicated <- ifelse(Lascar_data1$filename %in% dups, TRUE, FALSE)
sum(Lascar_data1$duplicated) #78

# Lascar_duplicates <- Lascar_data1[Lascar_data1$filename %in% dups,]
# Lascar_duplicates <- Lascar_duplicates[order(Lascar_duplicates$filename), c("mstudyid", "cstudyid", "problem_id", "problem_session", "filename", "file")]


Naming_problems <- Lascar_data1[!is.na(Lascar_data1$problem_id) | !is.na(Lascar_data1$problem_session) | Lascar_data1$duplicated == TRUE, c("mstudyid", "cstudyid", "problem_id", "problem_session",  "duplicated", "filename", "file")]

Naming_problems$problem_id <- as.character(Naming_problems$problem_id)
Naming_problems$problem_session <- as.character(Naming_problems$problem_session)


# save as .csv
write.csv(Naming_problems, file = paste0("Naming_problems_", format(Sys.Date(), format = "%b%d"), ".csv"), row.names = FALSE)

############################

