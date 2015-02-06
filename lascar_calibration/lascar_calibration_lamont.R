
path <- "/Users/ashlinn/Dropbox/Ghana_exposure_data_SHARED (1)/CO_calibration_files/Lamont CO Calibrations 21Jan2015/"
files<-list.files(path,full.names=T)
csv <- grep(".csv", files)
files <- files[csv]

batch1_pattern <- "(USB A |USB B |USB C |USB D |USB E |USB F |USB G |USB H |USB I |USB J |USB K |USB L )"
batch2_pattern <- "(USB M |USB N |USB O |USB P |USB Q |USB R |USB S |USB T |USB U |USB V |USB W |USB X )"
batch3_pattern <- "(USB Y|USB Z|USB AA|USB BB|USB CC|USB DD|USB EE|USB I\\.2|USB J\\.2|USB K\\.2)"
batch4_pattern <-  "(USB FF|USB GG|USB HH|USB II|USB JJ|USB KK|USB M\\.2|USB P\\.2|USB Q\\.2|USB T\\.2)"

batch1 <- grep(batch1_pattern, files)
batch1_files <- files[batch1]

batch2 <- grep(batch2_pattern, files)
batch2_files <- files[batch2]

batch3 <- grep(batch3_pattern, files)
batch3_files <- files[batch3]

batch4 <- grep(batch4_pattern, files)
batch4_files <- files[batch4]

# choose/change as needed
run <- paste("Jan21 LDEO", "batch4")

files <- batch1_files
files <- batch2_files
files <- batch3_files
files <- batch4_files

##### for Lamont data
lascar.import <- function(x){
  dt <- read.csv(x, stringsAsFactors=F, header=T)
  dt$datetime <- ymd_hms(dt$Time, tz="EST")
  dt$SN<- dt$Serial.Number[1]
  names(dt$CO.ppm) <- "co"
  dt <- dt[, c("datetime", "CO.ppm", "SN")]
  names(dt) <- c("datetime", "co", "SN")
  dt
}
names(files)<-basename(files)
calib <- ldply(files,lascar.import)
lascar_pattern <- "USB..." 
lascar_match<-regexpr(lascar_pattern, calib$.id)
calib$lascar<-regmatches(calib$.id, lascar_match)

#### End of Lamont-specific code

ggplot(calib,aes(x=datetime,y=co,colour=lascar))+geom_line()

pdf(file = paste0("Calib_plot_", run, ".pdf"))
ggplot(calib,aes(x=datetime,y=co,colour=lascar))+geom_line()
dev.off()

#check times - some of the lasars appear to be set to the wrong time.
meantime<-calib %.% group_by(lascar) %.% dplyr::summarise(datetime=mean(datetime),mean(co)) %.% arrange(desc(datetime))
meantime[,2] <- as.POSIXct(meantime[,2], origin="1970-01-01", tz='EST')

meantime #table for inspection


# for batch 2 change times for lascars USB X, R, and M (subtract 12 hours)
time_pattern <- "(USB X | USB R | USB M)"
head(calib[calib$lascar == "USB M ",])
calib$datetime[calib$lascar == "USB M "] <- calib$datetime[calib$lascar == "USB M "] -dhours(12)
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
# batch 1: "2015-01-21 12:12"/ "2015-01-21 12:23"
# batch 2: "2015-01-21 00:39"/ "2015-01-21 00:43"
# batch 3: "2015-01-21 01:00"/ "2015-01-21 01:10"
# batch 4: "2015-01-21 13:25"/ "2015-01-21 13:36"
starttime <- "2015-01-21 13:25"
stoptime <-  "2015-01-21 13:36"

calib_factor<- calib_cleaned %.% filter(datetime > ymd_hm(starttime, tz = "EST") & datetime < ymd_hm(stoptime, tz = "EST"))

ggplot(calib_factor,aes(x=datetime,y=co,colour=lascar))+geom_line()+scale_x_datetime(breaks = date_breaks("min"), labels = date_format("%H:%M"))

pdf(file = paste0("Calib_plot_trunc_", run, ".pdf"))
ggplot(calib_factor,aes(x=datetime,y=co,colour=lascar))+geom_line()+scale_x_datetime(breaks = date_breaks("min"), labels = date_format("%H:%M"))
dev.off()

# calculate the calibration factor
calib_factor <- calib_factor %.% group_by(lascar) %.% dplyr::summarise(co = mean(co), factor = round(co/50, digits = 3))


### name columns and create a date-stamped data frame ----


assign(paste0("calib_factor_", "batch1"),calib_factor)
assign(paste0("calib_factor_", "batch2"),calib_factor)
assign(paste0("calib_factor_", "batch3"),calib_factor)
assign(paste0("calib_factor_", "batch4"),calib_factor)

calib_factor_LDEO <- rbind(calib_factor_batch1, calib_factor_batch2, calib_factor_batch3, calib_factor_batch4)
calib_factor_LDEO <- calib_factor_LDEO[order(calib_factor_LDEO$factor),]
calib_factor_LDEO
