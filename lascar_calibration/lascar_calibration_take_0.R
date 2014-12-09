#version 0
#stack the calibration data and plot it
#created 24 Feb 2014
# modified 28 Nov 2014
require(plyr)
require(dplyr)
require(ggplot2)
require(lubridate)
require(scales)

###############################################
#enter file to examine (calibration run)
###############################################
run <- "03Mar2014_2"   
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

 calib_cleaned <- calib %.% filter(lascar!="CU_CO_112") # Mar 3_1: 114,  #Mar-3_2 112, 020 (there are two called 020). # Mar 11: 114, 126 Jul 5: 114. Nov 28: 114. 
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
# Dec 1_1: "2014-12-01 08:33" / "2014-12-01-08:43"
# Dec 1_2: "2014-12-01 09:03" / "2014-12-01-09:13"

starttime <- "2014-03-03 10:40"
stoptime <- "2014-03-03 10:50"

calib_factor<- calib_cleaned %.% filter(datetime > ymd_hm(starttime, tz = "GMT") & datetime < ymd_hm(stoptime, tz = "GMT"))

ggplot(calib_factor,aes(x=datetime,y=co,colour=lascar))+geom_line()+scale_x_datetime(breaks = date_breaks("min"), labels = date_format("%H:%M"))

pdf(file = paste0("Calib_plot_trunc_", run, ".pdf"))
ggplot(calib_factor,aes(x=datetime,y=co,colour=lascar))+geom_line()+scale_x_datetime(breaks = date_breaks("min"), labels = date_format("%H:%M"))
dev.off()

# calculate the calibration factor
calib_factor <- calib_factor %.% group_by(lascar) %.% dplyr::summarise(co = mean(co), factor = round(co/50, digits = 3))


### name columns and create a date-stamped data frame ----
# date <-  format(dmy(run, tz = "GMT"), format = "%b%d")
date <- "Mar03_2" # For March 03 and Dec 01 when there were 2 runs

names(calib_factor) <- c("lascar", paste0("co_", date), paste0("factor_", date))

assign(paste0("calib_factor_", date),calib_factor)


#####################################
# After doing each folder of calibrations separately, merge the data frames--------
####################################



calib_factor_all <- join_all(list(calib_factor_Feb24, calib_factor_Feb27, calib_factor_Mar03_1, calib_factor_Mar03_2, calib_factor_Mar11, calib_factor_Jun17, calib_factor_Jul05, calib_factor_Jul07, calib_factor_Jul08, calib_factor_Nov28, calib_factor_Dec01_1, calib_factor_Dec01_2), by = "lascar", type = "full")


write.csv(calib_factor_all, file = paste0("calib_factor_all", format(Sys.Date(), format = "%b%d"), ".csv"))


### split out the calibration factors
calib_factor_all <- calib_factor_allDec01
factor_variables <- regmatches(names(calib_factor_all), regexpr("factor_.*", names(calib_factor_all)))

calib_factors <- calib_factor_all[,colnames(calib_factor_all) %in% c("lascar", factor_variables)]

for (i in 1:nrow(calib_factors)) {
  calib_factors$calibrations[i] <- sum(!is.na(calib_factors[i,2:13]))
}

for (i in 1:nrow(calib_factors)) {
calib_factors$sd[i] <- sd(calib_factors[i,2:13], na.rm = TRUE)
}

calib_factors_ordered <- calib_factors[order(calib_factors$lascar),]

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
pdf(file = "Lascar_Calibrations.pdf", width = 10, height = 7)
par(mfrow = c(3,4))
for (i in 1:nrow(calib_factors)) {
  boxplot(calib_factors_ordered[i, 2:13], names = substr(names(calib_factors_ordered)[2:13],8,12),  main = paste(calib_factors_ordered$lascar[i], "\n avg =", round(rowMeans(calib_factors_ordered[2:11], na.rm = TRUE)[i], digits = 2), "; sd = ", round(calib_factors_ordered$sd[i], digits = 2)),  ylab = "Calibration Factor", las = 3)
}
dev.off()

require(reshape2)
df <- melt(calib_factors_ordered[,1:(ncol(calib_factors_ordered)-2)], id.vars = "lascar")

# plot of all together
pdf(file = paste0("Lascar_Calibrations_all_", format(Sys.Date(), format = "%b%d"), ".pdf"), width = 10, height = 9)
ggplot(df, aes(variable, value, color = lascar))+ geom_point() + theme(legend.position = "none") + ylab("Calibration Factor") + xlab("Test Date") + scale_x_discrete(labels = substr(unique(df$variable), 8,length(df$variable))) + ggtitle(paste(length(unique(df$lascar)), "Lascars"))
dev.off()

mean(df$value[df$value !=0], na.rm = TRUE) # 0.81
median(df$value[df$value !=0], na.rm = TRUE) # 0.843
mean(calib_factors_ordered$calibrations) #2.1
range(calib_factors_ordered$calibrations) # 1-5
                                                

