#version 0
#stack the calibration data and plot it
#created 24 Feb 2014
# modified 28 Nov 2014
require(plyr)
require(dplyr)
require(ggplot2)
require(lubridate)
require(scales)

###### 
# Original runs
###############################################
#enter file to examine (calibration run)
###############################################
run <- "02Dec2014"   
###############################################
###############################################

path<-paste("~/Dropbox/Ghana_exposure_data_SHARED (1)/CO_calibration_files/", run, sep="")
path
files<-list.files(path,full.names=T)
length(files)

lascar.import <- function(x){
    dt <- read.csv(x, stringsAsFactors=F, header=T)[,c(2,3,4)]
    names(dt) <- c('datetime','co','SN')
    dt$datetime <- dmy_hms(dt$datetime, tz="GMT")
    dt$SN<-dt$SN[1]
    dt
}
names(files)<-basename(files)
calib <- ldply(files,lascar.import)


#create lascar variable
#lascar_pattern<-"CU_C._..." #note requires that lascar values be entered as three digits
lascar_pattern <- "(CU_CO_...|CU_C0_...|CO_USB_...|COL_USB_...|CU-CO_...|CU-C0_...|CO-USB_...|COL-USB_...)" #note requires that lascar values be entered as three digits
lascar_match<-regexpr(lascar_pattern, calib$.id)
calib$lascar<-regmatches(calib$.id, lascar_match)


# calib<-calib%.%filter(co<80) # why?

ggplot(calib,aes(x=datetime,y=co,colour=lascar))+geom_line()

pdf(file = paste0("Calib_plot_", run, ".pdf"))
ggplot(calib,aes(x=datetime,y=co,colour=lascar))+geom_line()
dev.off()

#check times - some of the lasars appear to be set to the wrong time.
meantime<-calib %.% group_by(lascar) %.% dplyr::summarise(datetime=mean(datetime),mean(co)) %.% arrange(desc(datetime))
meantime[,2] <- as.POSIXct(meantime[,2], origin="1970-01-01", tz='UTC')

meantime #table for inspection

################################################
# CHECK PLOT AND MEANTIME AND PROCEED VIA EYEBALL
#################################################



# ###############################################
# # drop any problem records and replot
# ###############################################
# 

calib_cleaned <- calib

calib_cleaned <- calib %.% filter(lascar!="CU_CO_112") 
# Mar 3_1: 114,  
# Mar-3_2 112, 020 (there are two called 020). 
# Mar 11: 114, 126 
# Jul 5: 114. 
# Nov 28: 114. 

calib_cleaned <- calib_cleaned %.% filter(lascar!="CU_CO_126") #repeat this row to remove additonal units from the plot
# calib_cleaned <- calib_cleaned %.% filter(lascar!="CU_CO_156") #repeat this row to remove additonal units from the plot
# 
ggplot(calib_cleaned,aes(x=datetime,y=co,colour=lascar))+geom_line()



### if needed, get rid of weird spikes & replot (set the > & < values by eyeball)-----
calib_cleaned <- calib_cleaned %.% filter(co > 38 & co <55)

ggplot(calib_cleaned,aes(x=datetime,y=co,colour=lascar))+geom_line()+scale_x_datetime(breaks = date_breaks("min"), labels = date_format("%H:%M"))

####################################
### select the "middle" 5-10 minute section of the plateau by eyeball or by referring to times on logsheets-----
###################################

calib_cleaned$datetime[1]

# Times used:
# Feb 24: "2014-02-24 16:38" / "2014-02-24 16:42"
# Feb 27: "2014-02-27 16:29"/"2014-02-27 16:39"
# Mar 03_1: "2014-03-03 09:13" / "2014-03-03 09:23"
# Mar 03_2: "2014-03-03 10:40" / "2014-03-03 10:50"
# Mar 11: "2014-03-11 12:00" / 2014-03-11 12:10" (no logsheet)
# June 17: "2014-06-17 15:20"/ "2014-06-17 15:30"
# July 5: "2014-07-05 11:25" / "2014-07-05 11:35"
# July 7: "2014-07-07 09:34" / "2014-07-07 09:44"
# July 8: "2014-07-08 09:27" / "2014-07-08 09:37"
# Dec 01_1: "2014-12-01 08:33" / "2014-12-01-08:43"
# Dec 01_2: "2014-12-01 09:03" / "2014-12-01-09:13"
# Dec 02: "2014-12-02 10:20" / "2014-12-02 10:28"

starttime <- "2014-12-02 10:20"
stoptime <- "2014-12-02 10:28"

calib_factor<- calib_cleaned %.% filter(datetime > ymd_hm(starttime, tz = "GMT") & datetime < ymd_hm(stoptime, tz = "GMT"))

ggplot(calib_factor,aes(x=datetime,y=co,colour=lascar))+geom_line()+scale_x_datetime(breaks = date_breaks("min"), labels = date_format("%H:%M"))

pdf(file = paste0("Calib_plot_trunc_", run, ".pdf"))
ggplot(calib_factor,aes(x=datetime,y=co,colour=lascar))+geom_line()+scale_x_datetime(breaks = date_breaks("min"), labels = date_format("%H:%M"))
dev.off()

# calculate the calibration factor
calib_factor <- calib_factor %.% group_by(lascar) %.% dplyr::summarise(co = mean(co), factor = round(co/50, digits = 3))


### name columns and create a date-stamped data frame ----

date <-  format(dmy(run, tz = "GMT"), format = "%b%d") # for any dates with only one run
# date <- "Mar03_2" # For March 03 and Dec 01 when there were 2 runs

names(calib_factor) <- c("lascar", paste0("co_", date), paste0("factor_", date))
calib_factor$lascar <- gsub("CU_C0", "CU_CO", calib_factor$lascar)

assign(paste0("calib_factor_", date),calib_factor)


#####################################
# After doing each folder of calibrations separately, merge the data frames--------
####################################


########################
# If doing them for the first time, without loading a calib_factor_all file
# calib_factor_all <- join_all(list(calib_factor_Feb24, calib_factor_Feb27, calib_factor_Mar03_1, calib_factor_Mar03_2, calib_factor_Mar11, calib_factor_Jun17, calib_factor_Jul05, calib_factor_Jul07, calib_factor_Jul08, calib_factor_Nov28, calib_factor_Dec01_1, calib_factor_Dec01_2), by = "lascar", type = "full")
########################


#######################
# If adding to a previously established file
calib_factor_all <- read.csv("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_calibration_files/Calibration plots/calib_factor_allDec01.csv", stringsAsFactors = FALSE)

calib_factor_all <- join_all(list(calib_factor_all, calib_factor_Dec02), by = "lascar", type = "full")

calib_factor_all$lascar <- gsub("CU_C0", "CU_CO", calib_factor_all$lascar)


########################

calib_factor_all <- calib_factor_all[order(calib_factor_all$lascar),]


write.csv(calib_factor_all, file = paste0("calib_factor_all", format(Sys.Date(), format = "%b%d"), ".csv"), row.names = FALSE)


### split out the calibration factors
calib_factor_all <- read.csv("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_calibration_files/calib_factor_allDec15.csv")
factor_variables <- regmatches(names(calib_factor_all), regexpr("factor_.*", names(calib_factor_all)))

calib_factors <- calib_factor_all[,colnames(calib_factor_all) %in% c("lascar", factor_variables)]
calib_factors$mean <- NA
calib_factors$sd <- NA
calib_factors$calibrations <- NA
calib_factors$mean_excl_0 <- NA

calib_factors$mean <- rowMeans(calib_factors[,2:(ncol(calib_factors)-4)], na.rm = TRUE)
calib_factors$calibrations <- rowSums(!is.na(calib_factors[,2:(ncol(calib_factors)-4)]))

for (i in 1:nrow(calib_factors)) {
calib_factors$sd[i] <- sd(calib_factors[i,2:(ncol(calib_factors)-4)], na.rm = TRUE)
}

for (i in 1:nrow(calib_factors)) {
  tempdata <- as.numeric(calib_factors[i, 2:(ncol(calib_factors) - 4)])
  tempdata <- tempdata[!tempdata == 0]
  calib_factors$mean_excl_0[i] <- mean(tempdata, na.rm = TRUE)
  }


calib_factors_ordered <- calib_factors[order(calib_factors$lascar),]


## Add SNs to calib_factors_ordered

files <- list.files("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/12Dec2014/CO_stacked files/")
length(files)

SN_data <- as.data.frame(files)
SN_data$SN <- regmatches(SN_data[,1], regexpr("_\\d+\\.", SN_data[,1]))
SN_data$SN <- substr(SN_data$SN, 2, nchar(SN_data$SN)-1)

lascar_pattern <- "(CO.USB....|COL.USB....|CU.CO....)"
SN_data$lascar <- regmatches(SN_data$files, regexpr(lascar_pattern, SN_data$files))
SN_data$lascar <- ifelse(substr(SN_data$lascar, start = nchar(SN_data$lascar), stop = nchar(SN_data$lascar)) == "_", substr(SN_data$lascar, 1, nchar(SN_data$lascar)-1),SN_data$lascar)

#208 files, 171 unique SNs, 206 unique lascars....
test_table <- data.frame(SN = unique(SN_data$SN))
for (i in 1:nrow(test_table)) {
  test_table$no_lascars[i] <- length(SN_data$lascar[SN_data$SN == test_table$SN[i]])
}
test_table$lascar1 <- NA
test_table$lascar2 <- NA
test_table$lascar3 <- NA
for (i in 1:nrow(test_table)) {
  test_table$lascar1[i] <- SN_data$lascar[SN_data$SN == test_table$SN[i]][1]
  test_table$lascar2[i] <- SN_data$lascar[SN_data$SN == test_table$SN[i]][2]
  test_table$lascar3[i] <- SN_data$lascar[SN_data$SN == test_table$SN[i]][3]
}

Lascar_SN_to_ID <- test_table
write.csv(Lascar_SN_to_ID, file = "Lascar_SN_to_ID.csv", row.names = FALSE)

test <- calib_factors_ordered
test$SN <- NA
for (i in 1:nrow(test)) {
  test$SN[i] <- ifelse(test$lascar[i] %in% Lascar_SN_to_ID$lascar1, as.character(Lascar_SN_to_ID$SN[Lascar_SN_to_ID$lascar1 == test$lascar[i]]), test$SN[i])
  test$SN[i] <- ifelse(is.na(test$SN[i]) & test$lascar[i] %in% Lascar_SN_to_ID$lascar2, as.character(Lascar_SN_to_ID$SN[Lascar_SN_to_ID$lascar2 == test$lascar[i] & !is.na(Lascar_SN_to_ID$lascar2)]), test$SN[i])
  test$SN[i] <- ifelse(is.na(test$SN[i]) & test$lascar[i] %in% Lascar_SN_to_ID$lascar3, as.character(Lascar_SN_to_ID$SN[Lascar_SN_to_ID$lascar3 == test$lascar[i] & !is.na(Lascar_SN_to_ID$lascar3)]), test$SN[i])
}

names(test)[2:14] <- paste0("2013", substr(names(test)[2:14],8,12))
calib_factors_ordered <- test

write.csv(calib_factors_ordered, file = paste0("calib_factors_ordered_", format(Sys.Date(), format = "%b%d"), ".csv"), row.names = FALSE)

# ### split out the co levels
# co_variables <- regmatches(names(calib_factor_all), regexpr("co_.*", names(calib_factor_all)))
# 
# calib_co <- calib_factor_all[,colnames(calib_factor_all) %in% c("lascar", co_variables)]
# 
# for (i in 1:nrow(calib_co)) {
#   calib_co$calibrations[i] <- sum(!is.na(calib_factors[i,2:9]))
#   calib_co$sd[i] <- sd(calib_factors[i,2:9], na.rm = TRUE)
# }
# 
# calib_co_ordered <- calib_co[order(calib_co$lascar),]
# 

######################
# Plots ----------
######################

# Plots: each lascar's calibration factors
pdf(file = paste0("Lascar_Calibrations_", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 7)
par(mfrow = c(3,4))
for (i in 1:nrow(calib_factors)) {
  p <-boxplot(calib_factors_ordered[i, 2:(ncol(calib_factors_ordered)-5)], ylim = c(0,2), names = names(calib_factors_ordered)[2:(ncol(calib_factors_ordered)-5)],  main = paste(calib_factors_ordered$lascar[i], calib_factors_ordered$SN[i],"\n mean(w/o zero) =", round(calib_factors_ordered$mean_excl_0[i], digits = 2), "; sd = ", round(calib_factors_ordered$sd[i], digits = 2)),  ylab = "Calibration Factor", las = 3, cex.axis = 0.8, cex.main = 0.95)
    
  text(x = 1:(ncol(calib_factors_ordered)-6), y = p$stats[1,]+0.15, labels = round(p$stats[1,],digits = 2), cex = 0.75)
}
dev.off()

require(reshape2)
df <- melt(calib_factors_ordered[,1:(ncol(calib_factors_ordered)-4)], id.vars = "lascar")

# plot of all together
pdf(file = paste0("Lascar_Calibrations_all_", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 9)
ggplot(df, aes(variable, value, color = lascar))+ geom_point(size = 4) + theme(legend.position = "none") + ylab("Calibration Factor") + xlab("Test Date") + scale_x_discrete(labels = substr(unique(df$variable), 8,length(df$variable))) + ggtitle(paste(length(unique(df$lascar)), "Lascars"))
dev.off()

mean(df$value[df$value !=0], na.rm = TRUE) # 0.81 (the mean without the zeroes)
median(df$value[df$value !=0], na.rm = TRUE) # 0.841
mean(calib_factors_ordered$calibrations) #2.3
range(calib_factors_ordered$calibrations) # 1-5
                                                
# histogram of SDs
hist(calib_factors_ordered$sd, breaks = 50, xaxp = c(0, 1, 10), col = "grey", main = "Standard Deviations of Calibration Factors", xlab = "SD")


#############
### Calibration factors by interpolation
#############
data <- calib_factors_ordered

# change 0 to NA
for (i in 2:(ncol(data)-5)) {
  data[,i] <- mapvalues(data[,i], from = 0, to = NA)
}


# set up a new data frame for the interpolated CFs
cf <- data[,c("lascar", "SN")]
cf[,3:23] <- NA
colnames(cf)[3:6] <- paste0(c("September", "October", "November", "December"), "_2013")
colnames(cf)[7:18] <- paste0(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), "_2014")
colnames(cf)[19:23] <- paste0(c("January", "February", "March", "April", "May"), "_2015")


# take the mean across the calibrations done per month, excluding 0s
colnames(cf)
cf[,1] <- data$lascar
cf[,8] <- rowMeans(data[,2:3], na.rm = TRUE)
cf[,9] <- rowMeans(data[,4:6], na.rm = TRUE)
cf[,12] <- data[,7]
cf[,13] <- rowMeans(data[8:10], na.rm = TRUE)
cf[,17] <- data[,11]
cf[,18] <- rowMeans(data[,12:14], na.rm = TRUE)
head(cf)


save(cf, file = paste0("calib_factors_bymonth_", format(Sys.Date(), format = "%b%d"), ".rda"))

# interpolate

# plot:
pdf(file = paste0("Calib_factors_bymonth_", format(Sys.Date(), format = "%b%d"), ".pdf"), height = 10, width = 10)
par(mfrow = c(3,3))
for (i in 1:nrow(cf)) {
  xax <- 1:(ncol(cf) - 2)
  yax <- cf[i, 3:ncol(cf)]
  plot(xax,yax, pch = 16, col = "red",ylim = c(0,2), main = paste(cf$lascar[i], "(", cf$SN[i], ")"), ylab = "Calibration Factor", xlab = "", xaxt = "n")
if(sum(!is.na(yax))>1) points(xax, approx(xax,yax, n = (ncol(cf)-2), rule = 2, xout = 1:(ncol(cf)-2))$y)
if(sum(!is.na(yax))==1) points(xax, rep.int(yax[!is.na(yax)], times =ncol(cf)-2)) 

xlabels <-names(cf)[3:ncol(cf)]
axis(side = 1, at = xax, labels = paste0(substr(xlabels, 1,3), substr(xlabels, nchar(xlabels)-4, nchar(xlabels))), las = 2, cex.axis = 0.9)
}
dev.off()


# save the interpolated CF factors
cf_new <- cf
xax <- 1:(ncol(cf)-2)
for (i in 1:nrow(cf_new)) {
  yax <- cf[i, 3:ncol(cf)]
  if(sum(!is.na(yax))>1) factors <- approx(xax, yax, n = (ncol(cf)-2), rule = 2, xout = 1:(ncol(cf)-2))$y
  if(sum(!is.na(yax))==1) factors <- rep.int(yax[!is.na(yax)], times = ncol(cf)-2)
  if(sum(!is.na(yax))==0) factors <- NA
  cf_new[i,3:ncol(cf)] <- factors
}


# plot to check
pdf(file = paste0("Calib_factors_interp_check", format(Sys.Date(), format = "%b%d"), ".pdf"), height = 10, width = 10)
par(mfrow = c(3,3))
for (i in 1:nrow(cf_new)) {
  xax <- 1:(ncol(cf_new) - 2)
  yax <- cf_new[i, 3:ncol(cf_new)]
  plot(xax,yax, pch = 16, col = "red",ylim = c(0,2), main = paste(cf_new$lascar[i], "(", cf_new$SN[i], ")"), ylab = "Calibration Factor", xlab = "", xaxt = "n")
#   if(sum(!is.na(yax))>1) points(xax, approx(xax,yax, n = (ncol(cf)-2), rule = 2, xout = 1:(ncol(cf)-2))$y)
#   if(sum(!is.na(yax))==1) points(xax, rep.int(cf[i, !is.na(cf[i,])][2], times =ncol(cf)-2)) 
  xlabels <-names(cf_new)[3:ncol(cf_new)]
  axis(side = 1, at = xax, labels = paste0(substr(xlabels, 1,3), substr(xlabels, nchar(xlabels)-4, nchar(xlabels))), las = 2, cex.axis = 0.9)
}
dev.off()


save(cf_new, file = paste0("calib_factors_bymonth_interp_", format(Sys.Date(), format = "%b%d"), ".rda"))


#### Calculate the mean CF to use in units with no valid CF -------
cf <- read.csv("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_calibration_files/calib_factor_allDec15.csv", stringsAsFactors = FALSE, header = TRUE)
factor_variables <- regmatches(names(cf), regexpr("factor_.*", names(cf)))
factors <- cf[, names(cf) %in% factor_variables]
cf <- cbind(cf[,1], factors)
names(cf)[1] <- "lascar"

# match to SNs
Lascar_SN_to_ID <- read.csv("/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_calibration_files/Lascar_SN_to_ID.csv", stringsAsFactors = FALSE)

Lascar_SN_long <- Lascar_SN_to_ID[,c(1,3:5)]
Lascar_SN_long <- melt(Lascar_SN_long, id = "SN", na.rm = TRUE)
Lascar_SN_long$value <- gsub("\\.", "_", Lascar_SN_long$value)
names(Lascar_SN_long)[3] <- "lascar"
cf_new <- merge(cf, Lascar_SN_long, by = "lascar", all.x = TRUE)
cf_new <- cf_new[, -16]

# make long and calculate mean over range 0.3-1.5
cf_long <- melt(cf_new[,2:15], id = "SN", na.rm = TRUE)
nrow(cf_long) #346
saveRDS(cf_long, file = "allcfs_long.rds")

cf_long <- cf_long[cf_long$value > 0.3 & cf_long$value < 1.5,]
nrow(cf_long) #311
mean(cf_long$value) # 0.829135
