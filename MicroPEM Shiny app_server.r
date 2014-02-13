############ MICROPEM SHINY APP #########
############ UPDATED DEC 23, 2013 ###########
## New items: 
# Pressure drop calculation and plot
# HEPA correction modified - calculates HEPA correction if there are one or two HEPA sessions (not three, and you will not be prompted for more than 2 sessions in the web interface)
# For plots against time, X-axes changed to be neater
# Plot titles include start-stop times
# max file size upload changed to 30MB



require(shiny)
require(ggplot2)
require(xts) 
require(zoo)
require(plyr)
require(scales)
require(grid)
require(lubridate)

### SETTING MAXIMUM SIZE UPLOAD TO 30MB ###

options(shiny.maxRequestSize=30*1024^2)

#### SETTING COMPLIANCE THRESHOLD #####

compliance_threshold <- 0.02

#### FUNCTION TO CONVERT BLANKS TO NA ####

blank2na <- function(x){ 
  z <- gsub("\\s+", "", x)  #make sure it's "" and not " " etc
  x[z==""] <- NA 
  return(x)
}



shinyServer(function(input, output) {
  
  Data <- reactive({

    ### GETTING INITIAL INFORMATION FROM USER INPUT ####
    
    subject <- input$subject
    session <- paste0("S",input$session)
    filter <- input$filter
    dir <- input$dir
    timezone <- input$timezone
    window_width <- as.numeric(input$window_width)
    
    Sys.setenv(TZ = timezone)
    
    HEPATIMES <- matrix(data = NA, nrow = 3, ncol = 1, byrow = TRUE)
    HEPA1_times <- NA # not sure these are actually necessary
    HEPA2_times <- NA
    HEPA3_times <- NA
    
    if(input$hepasessions >=1) 
      HEPATIMES[1,1] <- input$HEPA1_times
    
    
    if(input$hepasessions >=2) 
      HEPATIMES[2,1] <- input$HEPA2_times
    
    
    if(input$hepasessions == 3) 
      HEPATIMES[3,1] <- input$HEPA3_times
    
    
    
    
    #### LOADING DATA ######
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath ,col.names = c("Date","Time",  "RH.Corrected Nephelometer"  ,"Temp",  "RH",  "Battery",  "Inlet.Press",  "Flow.Orifice.Press",  "Flow",  "X.axis",  "Y.axis",	"Z.axis",	"Vector.Sum.Composite", "Stop.Descriptions"	), header=F, sep=",", fill=T, stringsAsFactors=FALSE) # IMPORTING ACTUAL DATASET INTO R. 
    
    ######### EXTRACTING STOP DESCRIPTIONS/ CREATING TIMESTAMPS/ ESTABLISHING ID FOR FILENAMES ### 
    
    data_header <- data[1:22,1:7]
    colnames(data_header) <- c("variable", "V1", "V2", "V3", "V4", "V5", "V6")
    
    serialnumber <- paste("PM", as.character(sub("^......", "", data_header[4,2])), sep = "")
    
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
    data2$datetime <- mdy_hms(data2$datetime, tz = timezone)
  
                                         
    
    data2$RH.Corrected.Nephelometer = ifelse(data2$RH <0, NA, data2$RH.Corrected.Nephelometer) # First removes RH corrected if RH < 0
    data2$RH[data2$RH < 0] = NA   
    

    data2$unique_min <- floor_date(data2$datetime, unit = "minute")
    data2$unique_hour <- floor_date(data2$datetime, unit = "hour")

   


    #############  HEPA STUFF #########################
    
    
    days.xts <- as.xts(data2, order.by = data2$datetime, .RECLASS = TRUE)
    
    data2.HEPA1 <- matrix(nrow = 0, ncol = ncol(data2))
    data2.HEPA2 <- matrix(nrow = 0, ncol = ncol(data2))
    data2.HEPA3 <- matrix(nrow = 0, ncol = ncol(data2))
    
    
    if(!is.na(HEPATIMES[1,1])) {
      
      #### INDEXING HEPA INTERVALS/ REMOVING HEPA SESSIONS FROM DATA ###
      
      NEWHEPATIMES <- HEPATIMES[!is.na(HEPATIMES)]
      
      for (i in 1:length(NEWHEPATIMES)) {
        assign(paste("data2.HEPA",i, sep = ""), days.xts[as.character(NEWHEPATIMES[i])])
        } 
      
      if (input$hepasessions ==1) {
        days.xts2 <- days.xts[!time(days.xts) %in% index(data2.HEPA1)]} else
          if (input$hepasessions ==2) {
            days.xts2 <- days.xts[!time(days.xts) %in% index(data2.HEPA1) & !time(days.xts) %in% index(data2.HEPA2)]} else
              if (input$hepasessions ==3) {
                days.xts2 <- days.xts[!time(days.xts) %in% index(data2.HEPA1) & !time(days.xts) %in% index(data2.HEPA2) & !time(days.xts) %in% index(data2.HEPA3)]}
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
        
        if(i ==3) {
          data2.HEPA3_trim = data2.HEPA3[!time(data2.HEPA3) %in%  index(first(data2.HEPA3, '1 minute')) & !time(data2.HEPA3) %in% index(last(data2.HEPA3, '1 minute'))]
          data2.HEPA3_trim = as.data.frame(data2.HEPA3_trim, stringsAsFactors = FALSE)
          HEPA3.nephelometer = round(mean(as.numeric(data2.HEPA3_trim$RH.Corrected.Nephelometer), na.rm=TRUE), digits = 2)} else (HEPA3.nephelometer <- NA)
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
    active.minute.average.complete <- active.minute.average.complete[,c(25:26, 1:24)]
  
    
    ####### HOUR AVERAGE DATA ############################
    if (sum(!is.na(active.hour.average$x_percent_above_threshold))> 0) {
      active.hour.average.complete <-  active.hour.average[complete.cases(active.hour.average),] 
    } else {
      active.hour.average.complete <- active.hour.average
    }
   

    active.hour.average.complete$Subject <- rep(subject)
    active.hour.average.complete$Session <- rep(session)
   active.hour.average.complete <- active.hour.average.complete[,c(21:22, 1:20)]
    
    
    ##### ADDING HEPA CORRECTION TO NEPHELOMETER READINGS ####
    ## NOTE: CURRENTLY ONLY SET UP FOR ONE OR TWO HEPA SESSIONS ###
    
    active.day.average$HEPA_corr_neph <- NA
    active.minute.average.complete$HEPA_corr_neph <- NA
    active.hour.average.complete$HEPA_corr_neph <- NA
    
    # new
    if (input$hepasessions ==1) {
      HEPA_correction <- round(HEPA1.nephelometer, digits = 2)
      active.day.average$HEPA_correction <- HEPA_correction
      active.hour.average.complete$HEPA_correction <- HEPA_correction   
      active.minute.average.complete$HEPA_correction <- HEPA_correction
      active.day.average$HEPA_corr_neph <- round(active.day.average$RH.Corrected.Nephelometer - active.day.average$HEPA_correction, digits = 2)
      active.hour.average.complete$HEPA_corr_neph <- round(active.hour.average.complete$RH.Corrected.Nephelometer - active.hour.average.complete$HEPA_correction, digits = 3)
      active.minute.average.complete$HEPA_corr_neph <- round(active.minute.average.complete$RH.Corrected.Nephelometer - active.minute.average.complete$HEPA_correction, digits = 3)
      
      } else
    
    # end new
    
    if (input$hepasessions ==2) {
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
    
   
    
    active.minute.average.complete <- active.minute.average.complete[,c(1:4,28:27, 5:26, 29:30)]

    active.hour.average.complete <- active.hour.average.complete[,c(1:4, 24:23, 5:22, 25:26)]
    
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
    
    ######## HEPA TIMES #####
    HEPATIMES_summary <- as.data.frame(HEPATIMES, stringsAsFactors = FALSE)
    HEPATIMES_summary$variable <- c("HEPA_session1", "HEPA_session2", "HEPA_session3")
    HEPATIMES_summary <- HEPATIMES_summary[,c(2,1)]
    
    ###### VOLTAGE DROP PER HOUR #####
    
    # (Vb-Ve)*1000 mV/V ÷ (hours ran/2) (adjust for 50% duty cycle)- this number should be < 30 mV/hr for best pumps
    
    voltage_b <- active.hour.average.complete$Battery[1]
    voltage_e <- active.hour.average.complete$Battery[length(active.hour.average.complete)]
    voltage_drop <- (voltage_b-voltage_e)*1000 / (total_time/2)
    
    #### DATA SUMMARY #######
    
    active.data_summary = matrix(c(round(total_time), 
                                   round(total_time_minutes), 
                                   as.character(start_time), 
                                   as.character(stop_time),
                                   timezone,
                                   round(average_sample_nephelometer, digits = 2), 
                                   average_sample_nephelometer_hepacorr,
                                   HEPATIMES_summary[,2],
                                   round(HEPA1.nephelometer, digits = 2),
                                   round(HEPA2.nephelometer, digits = 2),
                                   round(HEPA3.nephelometer, 	digits = 2),
                                   compliance_threshold,
                                   sum(active.minute.average$sd_composite_above_threshold, na.rm=TRUE), 
                                   round(total_minutes_worn/60),
                                   round(mean(active.day.average$proportion_compliance_all, na.rm = TRUE)*100, digits =1),
                                   round(voltage_drop, digits = 2)),
                                 ncol = 1)
    
    
    active.data_summary = data.frame(active.data_summary, stringsAsFactors = FALSE)
    active.data_summary$variable = c("Total Sampling Time (hrs)", "Total Sampling Time (mins)", "Start Time", "Stop Time", "Timezone", "Mean Active Nephelometer (ug/m^3)", "Mean Active Neph, HEPA corr (ug/m^3)", "HEPA1_times (start/stop)", "HEPA2_times (start/stop)", "HEPA3_times (start/stop)",
                                     "Mean HEPA1 Nephelometer (ug/m^3)", "Mean HEPA2 Nephelometer (ug/m^3)", "Mean HEPA3 Nephelometer (ug/m^3)",  "Compliance Threshold for minutewise SD",
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
    
    
    
    info <- list(dir = dir, ID = ID, hepainfo = hepainfo, summary = summary, short_summary = active.data_summary, minutedata = active.minute.average.complete, hourdata = active.hour.average.complete, daydata = active.day.average, no.days = no.days, window_width = window_width, start_time = start_time, stop_time = stop_time)
    return(info)
    
  })
  
  
  
  ############ OUTPUT TO WEB VIEWER #######################################
  
  mytheme <- theme_grey(16) +  theme(plot.title=element_text(size=rel(0.92)))

  
  output$summary <- renderPrint({
    short_summary <- Data()$short_summary
    print(short_summary[,2:1])
  })
  
  
  output$minute_nephelometer <- renderPlot({
    if (is.null(input$file1)) { return() }
    if(!is.na(Data()$minutedata$HEPA_corr_neph[1])) {
      print(ggplot(data=Data()$minutedata)+ geom_line(aes(unique_min,HEPA_corr_neph), color="blue")+ labs(title=paste("Minute-Averaged Nephelometer (HEPA corrected) \n", Data()$start_time, "-", Data()$stop_time))+xlab("Time")+ ylab("HEPA-corrected Nephelometer (ug/m^3)") + expand_limits(y=0)  + scale_x_datetime(labels = date_format("%H:%M")) +  mytheme) 
    } else
      print(ggplot(data=Data()$minutedata)+ geom_line(aes(unique_min,RH.Corrected.Nephelometer), color="blue")+ labs(title=paste("Minute-Averaged Nephelometer \n", Data()$start_time, "-", Data()$stop_time))+xlab("Time")+ ylab("Nephelometer (ug/m^3)") + expand_limits(y=0)  + scale_x_datetime(labels = date_format("%H:%M")) +  mytheme)                                                                                                                                                                                                                                   
  })
  
  
  output$hour_nephelometer <- renderPlot({
    if (is.null(input$file1)) { return() }
    if(!is.na(Data()$hourdata$HEPA_corr_neph[1])) {
      print(ggplot(data=Data()$hourdata)+ geom_line(aes(datetime,HEPA_corr_neph), color="blue")+ labs(title=paste("Hour-Averaged Nephelometer (HEPA corrected) \n", Data()$start_time, "-", Data()$stop_time))+xlab("Time")+ ylab("HEPA-Corrected Nephelometer (ug/m^3)") + expand_limits(y=0) + scale_x_datetime(labels = date_format("%H:%M")) +  mytheme)
    } else
      print(ggplot(data=Data()$hourdata)+ geom_line(aes(datetime,RH.Corrected.Nephelometer), color="blue")+ labs(title=paste("Hour-Averaged Nephelometer \n", Data()$start_time, "-", Data()$stop_time))+xlab("Time")+ ylab("Nephelometer (ug/m^3)") + expand_limits(y=0) + scale_x_datetime(labels = date_format("%H:%M")) +  mytheme)
  })
  

  output$nephelometer_24h <- renderPlot({
    if (is.null(input$file1)) { return() }
    if(!is.na(Data()$daydata$HEPA_corr_neph[1])) {
      print(ggplot(data=Data()$daydata) + geom_bar(aes(x = unique_24h,y = HEPA_corr_neph), stat = "identity", fill="aquamarine3") + labs(title=paste("24-Hour Averaged Nephelometer (HEPA corrected) \n", Data()$start_time, "-", Data()$stop_time)) +xlab("Day")+ ylab("HEPA-Corrected Nephelometer (ug/m^3)") + expand_limits(y=0) + geom_text(aes(x=unique_24h, y = max(HEPA_corr_neph)*4/100, label=paste("Hours:", total_hours_observation)), col = "black") +  mytheme) 
    } else
      print(ggplot(data=Data()$daydata) + geom_bar(aes(x = unique_24h,y = RH.Corrected.Nephelometer), stat = "identity", fill="aquamarine3") + labs(title=paste("24-Hour Averaged Nephelometer \n", Data()$start_time, "-", Data()$stop_time)) +xlab("Day")+ ylab("RH-Corrected Nephelometer (ug/m^3)") + expand_limits(y=0) + geom_text(aes(x=unique_24h, y = max(RH.Corrected.Nephelometer)*4/100, label=paste("Hours:", total_hours_observation)), col = "black") + mytheme) 
  })
  
  output$compliance <- renderPlot({
    if (is.null(input$file1)) { return() }
    if (sum(!is.na(Data()$minutedata$sd_composite_rollmean)) >0)
    print(ggplot(data=Data()$minutedata)+ geom_line(aes(unique_min,sd_composite_rollmean), color="blue")+ labs(title=paste("Minute-Averaged Wearing Compliance \n", Data()$start_time, "-", Data()$stop_time)) + xlab("Time")+ ylab(paste0("Mean Compliance (", Data()$window_width, " min rolling window)")) + scale_y_continuous(breaks=(c(0,1))) + scale_x_datetime(labels = date_format("%H:%M")) +  mytheme)
      })
  
  
  output$compliance24 <- renderPlot({
    if (is.null(input$file1)) { return() }
    if (sum(!is.na(Data()$minutedata$sd_composite_rollmean)) >0)
    print(ggplot(Data()$daydata, aes(x = unique_24h, y = hours_compliance_rollmean)) + geom_bar(stat = "identity", fill = "aquamarine3") + coord_cartesian(ylim = c(0,16)) + xlab ("Day") + ylab("Hours Worn") + labs(title=paste("24-Hour Averaged Wearing Compliance \n", Data()$start_time, "-", Data()$stop_time)) + geom_text(aes(x = unique_24h, y = 2, label = paste(proportion_compliance_all*100, "%", sep = "")), size = 5) + geom_text(aes(x = unique_24h, y = 1, label = paste("of", total_hours_observation, "hrs")), size = 5) + geom_hline(aes(yintercept = 8.5, col = "red"))+  mytheme)
  })
 
  
  ##### CAPTIONS #######
  output$caption1 <- renderText( {
    if (is.null(input$file1)) { return() }
    paste("Data Summary for:", Data()$ID)
  })
  
  output$caption2 <- renderText( {
    if (is.null(input$file1)) { return() }
    if(!is.na(Data()$minutedata$HEPA_corr_neph[1])) {
      paste("Minute-Averaged Nephelometer Readings, HEPA Corrected (Max =", round(max(Data()$minutedata$HEPA_corr_neph)), ")")
    } else
      paste("Minute-Averaged Nephelometer Readings (Max = ", round(max(Data()$minutedata$RH.Corrected.Nephelometer)), ")")
  })
  
  output$caption3 <- renderText({
    if (is.null(input$file1)) { return() }
    if(!is.na(Data()$hourdata$HEPA_corr_neph[1])) {
      paste("Hour-Averaged Nephelometer Readings, HEPA Corrected (Max =", round(max(Data()$hourdata$HEPA_corr_neph)), ")")
    } else
      paste("Hour-Averaged Nephelometer Readings (Max = ", round(max(Data()$hourdata$RH.Corrected.Nephelometer)), ")")
  })
  
  output$caption4 <- renderText( {
    if (is.null(input$file1)) { return() }
    ifelse (sum(!is.na(Data()$minutedata$sd_composite_rollmean)) >0,
    "Minutewise wearing compliance by rolling mean", "NA: Accelerometer was probably set to record once per minute or less")
  })
  
  output$caption5 <- renderText( {
    if (is.null(input$file1)) { return() }
    ifelse (sum(!is.na(Data()$minutedata$sd_composite_rollmean)) >0,
            "24-hour Wearing Compliance by rolling mean", "NA: Accelerometer was probably set to record once per minute or less")
  })
  
  output$caption6 <- renderText( {
    if (is.null(input$file1)) { return() }
    "Log-Probability Plot"
  })
  
  output$caption7 <- renderText( {
    if (is.null(input$file1)) { return() }
    "24-hour Average Nephelometer Readings"
  })
  
  ############ SAVE PLOTS #######################################
  
  observe({
    
    dir <- if (is.null(Data()$dir)) { return() } else dir <- Data()$dir
    
    mytheme <- theme_grey() +  theme(plot.title=element_text(size=rel(0.95)), axis.title.y = element_text(vjust=0.3), axis.title.x = element_text(vjust=0.4),plot.margin = unit(c(1,2,2,1), "lines"))
    
    pdf(file = file.path(dir, paste(Data()$ID,"_Nephelometer_minute_avg.pdf", sep = "")))
    if(!is.na(Data()$minutedata$HEPA_corr_neph[1])) {
      print(ggplot(data=Data()$minutedata)+ geom_line(aes(unique_min,HEPA_corr_neph), color="blue")+ labs(title=paste("Minute-Averaged Nephelometer (HEPA corrected) \n", Data()$ID,  "\n", Data()$start_time, "-", Data()$stop_time)) + xlab("Hour")+ ylab("HEPA-corrected Nephelometer (ug/m^3)") + expand_limits(y=0)  + scale_x_datetime(labels = date_format("%H"), breaks = pretty_breaks(n=10), minor_breaks = NULL) + mytheme)
  
    } else
      print(ggplot(data=Data()$minutedata)+ geom_line(aes(unique_min,RH.Corrected.Nephelometer), color="blue")+ labs(title=paste("Minute-Averaged Nephelometer \n", Data()$ID,  "\n", Data()$start_time, "-", Data()$stop_time)) + xlab("Hour")+ ylab("Nephelometer (ug/m^3)")+ expand_limits(y=0) + scale_x_datetime(labels = date_format("%H"), breaks = pretty_breaks(n=10), minor_breaks = NULL)  +  mytheme)
    dev.off()
    
    pdf(file = file.path(dir, paste(Data()$ID,"_Nephelometer_hour_avg.pdf", sep = "")))
    if(!is.na(Data()$hourdata$HEPA_corr_neph[1])) {
      print(ggplot(data=Data()$hourdata)+ geom_line(aes(datetime,HEPA_corr_neph), color="blue")+ labs(title=paste("Hour-Averaged Nephelometer (HEPA corrected) \n", Data()$ID,  "\n", Data()$start_time, "-", Data()$stop_time)) + xlab("Hour")+ ylab("HEPA-corrected Nephelometer (ug/m^3)") + expand_limits(y=0) + scale_x_datetime(labels = date_format("%H"), breaks = pretty_breaks(n=10), minor_breaks = NULL) +  mytheme)
    } else
      print(ggplot(data=Data()$hourdata)+ geom_line(aes(datetime,RH.Corrected.Nephelometer), color="blue")+ labs(title=paste("Hour-Averaged Nephelometer \n", Data()$ID,  "\n", Data()$start_time, "-", Data()$stop_time)) + xlab("Hour")+ ylab("Nephelometer (ug/m^3)")  + expand_limits(y=0)  + scale_x_datetime(labels = date_format("%H"), breaks = pretty_breaks(n=10), minor_breaks = NULL) +  mytheme)
    dev.off()
    
    pdf(file = file.path(dir, paste(Data()$ID,"_Nephelometer_24h_avg.pdf", sep = "")))
    if(!is.na(Data()$daydata$HEPA_corr_neph[1])) {
      print(ggplot(data=Data()$daydata) + geom_bar(aes(x = unique_24h,y = HEPA_corr_neph), stat = "identity", fill="aquamarine3") + labs(title=paste("24-Hour Averaged Nephelometer (HEPA corrected) \n", Data()$ID,  "\n", Data()$start_time, "-", Data()$stop_time)) + xlab("Day")+ ylab("HEPA-Corrected Nephelometer (ug/m^3)") + expand_limits(y=0) + geom_text(aes(x=unique_24h, y = max(HEPA_corr_neph)*4/100, label=paste("Hours:", total_hours_observation)), col = "black") +  mytheme) 
    } else
      print(ggplot(data=Data()$daydata) + geom_bar(aes(x=unique_24h, y=RH.Corrected.Nephelometer), stat = "identity", fill="aquamarine3") + labs(title=paste("24-Hour Averaged Nephelometer \n", Data()$ID,  "\n", Data()$start_time, "-", Data()$stop_time)) + xlab("Day")+ ylab("RH-Corrected Nephelometer (ug/m^3)") + expand_limits(y=0) + geom_text(aes(x=unique_24h, y = max(RH.Corrected.Nephelometer)*4/100, label=paste("Hours:", total_hours_observation)), col = "black") +  mytheme)
    dev.off()
    
    if(sum(!is.na(Data()$minutedata$sd_composite_rollmean)) >0) {
    pdf(file = file.path(dir, paste(Data()$ID,"_Compliance_min_avg.pdf", sep = "")))
    print(ggplot(data=Data()$minutedata)+ geom_line(aes(unique_min,sd_composite_rollmean), color="blue")+ labs(title=paste("Minute-Averaged Wearing Compliance \n", Data()$ID,  "\n", Data()$start_time, "-", Data()$stop_time)) + xlab("Hour")+ ylab(paste0("Mean Compliance (", Data()$window_width, " min rolling window)")) + scale_y_continuous(breaks=(c(0,1))) + scale_x_datetime(labels = date_format("%H"), breaks = pretty_breaks(n=10), minor_breaks = NULL)  +  mytheme)
    dev.off()
    }
    
    
    if(sum(!is.na(Data()$daydata$percent_composite_above_threshold)) >0) {
    pdf(file = file.path(dir, paste(Data()$ID,"_Compliance_24h_avg.pdf", sep = "")))
    print(ggplot(Data()$daydata, aes(x = unique_24h, y = hours_compliance_rollmean)) + geom_bar(stat = "identity", fill = "aquamarine3") + coord_cartesian(ylim = c(0,16)) + xlab ("Day") + ylab("Hours Worn") + labs(title=paste("24-Hour Averaged Wearing Compliance \n", Data()$ID,  "\n", Data()$start_time, "-", Data()$stop_time)) + geom_text(aes(x = unique_24h, y = 2, label = paste(proportion_compliance_all*100, "%", sep = "")), size = 5) + geom_text(aes(x = unique_24h, y = 1, label = paste("of", total_hours_observation, "hrs")), size = 5) + geom_hline(aes(yintercept = 8.5, col = "red"))  +  mytheme)
    dev.off()
    }
  
    pdf(file = file.path(dir, paste(Data()$ID,"_Log_probability.pdf", sep = "")))
    print(ggplot(data = Data()$minutedata[Data()$minutedata$RH.Corrected.Nephelometer >0,], aes(x = qnorm(ppoints(length(RH.Corrected.Nephelometer)))[order(order(RH.Corrected.Nephelometer))], y = RH.Corrected.Nephelometer)) + coord_trans(ytrans = "log") + geom_point(alpha = 0.5, color = "blue") + scale_x_continuous(breaks= qnorm(c(1e-04, 0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999, 0.9999)), labels = c(0.01, 0.1, 1, 5, 10, 25, 50, 75, 90, 95, 99, 99.9, 99.99), limits = c(qnorm(1e-04), qnorm(0.9999))) + labs(title = paste("Minute-Averaged Log Probability \n", Data()$ID,  "\n", Data()$start_time, "-", Data()$stop_time)) + xlab("Standard Normal Probability (%)") + ylab("Nephelometer (ug/m^3)") + mytheme)
    dev.off()
    

    pdf(file = file.path(dir, paste(Data()$ID,"_Battery.pdf", sep = "")))
    print(ggplot(data=Data()$minutedata)+ geom_line(aes(unique_min,Battery), color="blue")+ labs(title=paste("Minute-Averaged Battery Voltage \n", Data()$ID,  "\n", Data()$start_time, "-", Data()$stop_time)) + xlab("Hour")+ ylab("Voltage (V)") + expand_limits(y=0) + scale_x_datetime(labels = date_format("%H"), breaks = pretty_breaks(n=10), minor_breaks = NULL)   +  mytheme)
    dev.off()
    
    pdf(file = file.path(dir, paste(Data()$ID,"_Flow_Rate.pdf", sep = "")))
    print(ggplot(data=Data()$minutedata) + geom_line(aes(x = unique_min,y = Flow), color = "blue") + labs(title=paste("Minute-Averaged Flow Rate \n", Data()$ID,  "\n", Data()$start_time, "-", Data()$stop_time)) + xlab("Hour") + ylab("Lpm") + expand_limits(y=0)  + scale_x_datetime(labels = date_format("%H"), breaks = pretty_breaks(n=10), minor_breaks = NULL) +  mytheme)
    dev.off()
    
 
    pdf(file = file.path(dir, paste(Data()$ID,"_Compliance_Composite_Accel.pdf", sep = "")))
    print(ggplot(data=Data()$minutedata) + geom_line(aes(x = unique_min,y = Vector.Sum.Composite_mean), color = "blue") + labs(title=paste("Minute-Averaged Vector-Composite Accelerometer \n", Data()$ID,  "\n", Data()$start_time, "-", Data()$stop_time)) + xlab("Hour") + ylab("Vector-Sum Composite (g-unit)") + expand_limits(y=0)  + scale_x_datetime(labels = date_format("%H"), breaks = pretty_breaks(n=10), minor_breaks = NULL) +  mytheme)
    dev.off()
 

    
    pdf(file = file.path(dir, paste(Data()$ID,"_Pressure_Drop.pdf", sep = "")))
    par(oma=c(0,2,0,3))
    plot(Data()$minutedata$unique_min,Data()$minutedata$cumulative_dep, type = "l", col = "cyan3", yaxt = "n", ylab = "", xaxt = "n", xlab ="Hour", main = paste("Cumulative Deposition & Pressure Drop \n", Data()$ID,  "\n", Data()$start_time, "-", Data()$stop_time), cex.main = 1, font.main = 1)
    axis(side = 2, line =0.7, col = "cyan3", lwd = 2)
    mtext(side=2, line=3, "Cumulative Deposition (ug)")
   axis(side = 1, at = pretty(Data()$minutedata$unique_min, n=10), labels = format(pretty(Data()$minutedata$unique_min, n=10), format = "%H"))
    grid()
    par(new = T)
    plot(Data()$minutedata$unique_min,Data()$minutedata$delta_pressure, type = "l", col = "coral", yaxt = "n", xaxt = "n", ylab = "", xlab = "")
    axis(side = 4, line =0.7, col = "coral", lwd = 2, at = pretty(Data()$minutedata$delta_pressure, n=6))
    mtext(side=4, line=3, "Delta Pressure (inches H20)")
    dev.off()


    ################ INSERT UNITS TO 1 MIN, 1 HR Averages #########
    
    minute.data <- Data()$minutedata
    colnames(minute.data)[4:20] <- c("RH.Corrected.Nephelometer(ug/m^3)", "HEPA Correction(ug/m^3)", "HEPA-Corrected Nephelometer(ug/m^3)", "Temp(C)", "RH(%)", "Battery(V)",
                                     "Inlet.Press(H20)", 
                                     "Flow.Orifice.Press(H20)",     
                                     "Flow(Lpm)", 
                                     "X.axis.mean(g_unit)", 
                                     "Y.axis.mean(g_unit)",	
                                     "Z.axis.mean(g_unit)",
                                     "Vector.Sum.Composite_mean(g_unit)",
                                     "X.axis.SD(g_unit)", 
                                     "Y.axis.SD(g_unit)",	
                                     "Z.axis.SD(g_unit)",
                                     "Vector.Sum.Composite_SD(g_unit)")	
    colnames(minute.data)[29:30] <- c("cumulative_dep(ug)", "delta_pressure(H20)")
    
    hour.data <- Data()$hourdata
    colnames(hour.data)[4:20] <- c("RH.Corrected.Nephelometer(ug/m^3)", "HEPA.Correction(ug/m^3)", "HEPA.Corrected.Nephelometer(ug/m^3)",
                                   "Temp(C)", "RH(%)", "Battery(V)",
                                   "Inlet.Press(H20)", 
                                   "Flow.Orifice.Press(H20)", 		
                                   "Flow(Lpm)",
                                   "count_composite_above_threshold(mins)",
                                   "percent_composite_above_threshold",
                                   "x_above_threshold(mins)",
                                   "x_percent_above_threshold",        
                                   "y_above_threshold(mins)",                
                                   "y_percent_above_threshold", 
                                   "z_above_threshold(mins)",               
                                   "z_percent_above_threshold")
  
    colnames(hour.data)[25:26] <- c("cumulative_dep(ug)", "delta_pressure(H20)")

    ###### SAVE SUMMARY, 1 MINUTE, 1 HOUR, AND HEPA DATA TO CSV #####
    
    write.csv(Data()$summary, file = file.path(dir, paste(Data()$ID,"_Data_Summary.csv", sep = "")), row.names = F, na = "")
    write.csv(minute.data, file = file.path(dir, paste(Data()$ID, "_Data_Minute_Averages.csv", sep = "")), row.names = F) 
    write.csv(hour.data, file = file.path(dir, paste(Data()$ID,"_Data_Hour_Averages.csv", sep = "")), row.names=F)
    if(!is.na(Data()$hepainfo[1,1]))
    write.csv(Data()$hepainfo, file =  file.path(dir, paste(Data()$ID,"_HEPA_info.csv", sep = "")), row.names=F)
    
    
  })
  
})
