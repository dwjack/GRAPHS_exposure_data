# Making summary histograms and diagnosing problems in micropem processing files


# make summary histograms
summary_table <- read.csv("~/Dropbox/Ghana project/Ghana R stuff/MicroPEM_summary_table_2014-04-07.csv", stringsAsFactors = FALSE)

par(mfrow = c(3,2))
hist(summary_table[,3], breaks  = 50, col = "red",xlab = colnames(summary_table[3]), main = "")
hist(summary_table[,8], breaks  = 50, col = "red",xlab = colnames(summary_table[8]), main = "")
hist(summary_table[,9], breaks  = 50, col = "red",xlab = colnames(summary_table[9]), main = "")
hist(summary_table[,12], breaks  = 50, col = "red",xlab = colnames(summary_table[12]), main = "")
hist(summary_table[,13], breaks  = 50, col = "red",xlab = colnames(summary_table[13]), main = "")
hist(summary_table[,17], breaks  = 50, col = "red",xlab = colnames(summary_table[17]), main = "")


# removing outliers
summary_table2 <- summary_table[!summary_table$Serialnumber == "UGF320423N",]
summary_table <- summary_table2

summary_table2 <- summary_table[!summary_table2$Filter %in% c("KHC0195", "KHC0245"),]
summary_table <- summary_table2

# Compliance per day
par(mfrow = c(3,1))
hist(as.numeric(summary_table[,19]), breaks = 50, col = "red", xlab = paste0(colnames(summary_table[19]), " (n=", sum(!is.na(summary_table[19])), ")"), main = "")
hist(as.numeric(summary_table[,20]), breaks = 50, col = "red", xlab = paste0(colnames(summary_table[20]), " (n=", sum(!is.na(summary_table[20])), ")"), main = "")
hist(as.numeric(summary_table[,21]), breaks = 50, col = "red", xlab = paste0(colnames(summary_table[21]), " (n=", sum(!is.na(summary_table[21])), ")"), main = "")

### Diagnosing problems in problem_files

for (i in 1:nrow(problem_files)) {
  data <- read.csv(as.character(hepa_problems$datafile[i]),col.names = c("Date","Time",  "RH.Corrected Nephelometer"  ,"Temp",  "RH",  "Battery",  "Inlet.Press",  "Flow.Orifice.Press",  "Flow",  "X.axis",  "Y.axis",  "Z.axis",  "Vector.Sum.Composite", "Stop.Descriptions"  ), header=F, sep=",", fill=T, stringsAsFactors=FALSE) # IMPORTING ACTUAL DATASET INTO R. 
  
  
  data <- data[25:nrow(data),]  
  
  data.withoutdesc <- data[5:nrow(data), 1:13]
  
  data2 = data.frame(sapply(data.withoutdesc, blank2na))
  for(j in 1:11){data2[,j+2] = as.numeric(levels(data2[,j+2]))[as.integer(data2[,j+2])]}
  
  data2$Date <- as.character(data2$Date)
  data2$Time <- as.character(data2$Time)
  data2$datetime <- paste(data2$Date, data2$Time)
  
  problem_files$data_start[i] <- data2$datetime[10]
  problem_files$data_end[i] <- data2$datetime[(nrow(data2)-10)]
}
write.csv(problem_files, file = paste0("fixed_problems ", Sys.Date(), ".csv"), row.names = FALSE)

# fix by hand then reimport


# format the HEPA session times as needed for xts subsetting
fixed_problems[, c(5:6, 10:11)] <- lapply(X = fixed_problems[, c(5:6, 10:11)], FUN = timeformat)
fixed_problems[, c(5:6, 10:11)] <- lapply(X = fixed_problems[, c(5:6, 10:11)], FUN = zero2na)


fixed_problems$hepatimes1 <- paste0(mdy_hm(paste(fixed_problems$Fieldsetd, fixed_problems$Thepaon1)), "/",  mdy_hm(paste(fixed_problems$Fieldsetd, fixed_problems$Thepaoff1), tz = timezone))
fixed_problems$hepatimes2 <- paste0(mdy_hm(paste(fixed_problems$Pickupdtd, fixed_problems$Thepaon2)), "/",  mdy_hm(paste(fixed_problems$Pickupdtd, fixed_problems$Thepaoff2), tz = timezone))

fixed_problems$hepatimes1[grep("NA",fixed_problems$hepatimes1)] <- NA
fixed_problems$hepatimes2[grep("NA",fixed_problems$hepatimes2)] <- NA

loginfo <- fixed_problems

# then re-run the processing script but stop before saving anything

# save and reimport the summary table so the colnames line up with the old one
write.csv(summary_table, file = paste0("summary_table_fixed_files ", Sys.Date(), ".csv"), row.names = FALSE)
summary_table_new <- read.csv("~/Dropbox/Ghana project/Ghana R stuff/summary_table_fixed_files 2014-04-06.csv", stringsAsFactors = FALSE)
  summary_table_old <- read.csv("~/Dropbox/Ghana project/Ghana R stuff/MicroPEM_summary_table_2014-04-06.csv", stringsAsFactors = FALSE)

summary_table5 <- rbind(summary_table_old, summary_table_new)
write.csv(summary_table5, file = paste0("MicroPEM_summary_table_incl_fixed ", Sys.Date(), ".csv"), row.names = FALSE)


not_processed <- loginfo[!loginfo$Filterid %in% summary_table$Filter,]
not_processed$processed <- "not processed"
write.csv(not_processed, file = paste0("MicroPEM_problem_files_not_fixed ", Sys.Date(), ".csv"), row.names = FALSE)


fixed <- loginfo[loginfo$Filterid %in% summary_table$Filter,]
write.csv(fixed, file = paste0("MicroPEM_logsheet_data_FIXED ", Sys.Date(), ".csv"), row.names = FALSE)
