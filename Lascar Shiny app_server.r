########## LASCAR SHINY APP ##########
######### UPDATED NOV 7, 2013 #########

require(shiny)
require(ggplot2)
require(xts) 
require(zoo)
require(plyr)
require(scales)
require(lubridate)



shinyServer(function(input, output) {
  
  Data <- reactive({
    
    subject <- input$subject
    session <- paste0("S",input$session)
    timezone <- input$timezone
    dir <- input$dir
    
 
  
    Sys.setenv(TZ = timezone)
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #### LOADING DATA AND EXTRACTING SERIAL NUMBER ######
    
    data <- read.table(inFile$datapath, header=T, sep=",", fill=T, stringsAsFactors=FALSE) 
    
    serialnumber <- data[1,4]
    
    data2 <- data[, 1:3]
    
    ###### START & STOP DATE & TIME ####
    
    data2$Time <- dmy_hms(data2$Time, tz = timezone)
    
    if (input$entertimes ==TRUE) start_time <- ymd_hm(input$start_time, tz = timezone) else start_time <- data2$Time[1]
    if (input$entertimes ==TRUE) stop_time <- ymd_hm(input$stop_time, tz = timezone) else stop_time <- data2$Time[nrow(data2)]
    
    data2 <- (data2[data2$Time >= start_time & data2$Time <= stop_time,])
    
    firstdate <- floor_date(data2$Time[1], unit = "day")

    ID <- paste(subject, firstdate, session, serialnumber, sep = "_")
    
    
    data2$unique_min = floor_date(data2$Time, unit = "minute")  
    data2$unique_hour = floor_date(data2$Time, unit = "hour")  

    
    ###### SUBSETTING DATA INTO 24-HOUR PERIODS #####
    
    no.days <- ceiling(as.numeric(as.duration(data2$unique_min[nrow(data2)] - data2$unique_min[1]))/86400) # calculates the difference in time between last and first datetime observation (in seconds), transforms it into days and returns the ceiling of days
    
    
    dayindex <- data2$unique_min[1] + hours(seq(from = 24, to = no.days*24, by = 24))
    
    data2$unique_24h <- 1
    
    for (i in 1:no.days) { 
      data2$unique_24h <- ifelse ((data2$unique_min > dayindex[i]),  i+1, data2$unique_24h)
    } 
    

  ########### CALCULATING MINUTE, HOUR, AND 24-HOUR AVERAGES ###########
    
    minute.average = ddply(data2, .(unique_min), summarise, 
                           CO.ppm = round(mean(CO.ppm., na.rm=TRUE), digits = 2))
                           
    
    hour.average = ddply(data2, .(unique_hour), summarise, 
                         CO.ppm = round(mean(CO.ppm., na.rm=TRUE), digits = 2))
    
    day.average =  ddply(data2, .(unique_24h), summarise, 
                         CO.ppm = round(mean(CO.ppm., na.rm=TRUE),digits = 2),
                         Date_time = unique_min[1],
                         total_hours_observation = round(length(unique_min)/360, digits = 1))  
   
    ####### MINUTE AVERAGE DATA ############################
    
     # minute.average$Permanent_ID <- rep(permID)
    minute.average$Subject <- rep(subject)
    minute.average$Session <- rep(session)
    minute.average <- minute.average[,c(3:4,1:2)]
    
    ####### HOUR AVERAGE DATA ############################
    
    # hour.average$Permanent_ID <- rep(permID)
    hour.average$Subject <- rep(subject)
    hour.average$Session <- rep(session)
    hour.average <- hour.average[,c(3:4,1:2)]
    
    ####### 24-HOUR DATA ############################
    
    day.average$unique_24h <- paste("Day", day.average$unique_24h)
    
    ###### SUMMARY FILE ########
    ##### TOTAL RUN TIME ####################
    
    total_time <- sum(day.average$total_hours_observation)
    
    
    #### AVERAGE SAMPLE CO (ppm) #### 
    
    average_CO <- round(mean(data2$CO.ppm), digits = 2)
    median_CO <- median(data2$CO.ppm)
    min_CO <- min(data2$CO.ppm)
      
    
    ###### COLLATING TO SUMMARY ######
    
    Value <- c(serialnumber, round(total_time, digits = 1), as.character(start_time), as.character(stop_time), timezone, average_CO, median_CO, min_CO)
    Parameter <-  c("Lascar Serialnumber", "Total Sampling Time (Hours)", "Start Time", "Stop Time", "Timezone", "Mean Sample CO reading (ppm)", "Median Sample CO reading (ppm)", "Minimum Sample CO reading (ppm)")
    
    
    data_summary <- cbind(Parameter, Value)
    data_summary <- data.frame(data_summary, stringsAsFactors = FALSE)
    

    summary <- rbind.fill(data_summary, day.average)
    # summary$Permanent_ID <- rep(permID)
    summary$Subject <- rep(subject)
    summary$Session <- rep(session)
    summary <- summary[,c("Subject", "Session", names(data_summary), names(day.average))]
    
    info <- list(dir = dir, ID = ID, summary = summary, short_summary = data_summary, minutedata = minute.average, hourdata = hour.average, daydata = day.average)
    return(info)
  
  })
  
  
  ############## OUTPUT TO WEB VIEWER ####################
  
theme_set(theme_grey(16))
  
  output$summary <- renderPrint({
    short_summary <- Data()$short_summary
    print(short_summary)
  })
  
  
  output$minute_CO <- renderPlot({
    if (is.null(input$file1)) { return() }
    print(ggplot(data=Data()$minutedata)+ geom_line(aes(unique_min,CO.ppm), color="blue") + labs(title=paste("Minute-Averaged CO \n ", Data()$ID))+xlab("Date")+ ylab("CO (ppm)") + scale_x_datetime(labels = date_format("%a %b %d %H"))+ expand_limits(y=0) +  theme(plot.title=element_text(size=rel(0.92), color = "darkgrey")))
  })
  
  output$hour_CO <- renderPlot({
    if (is.null(input$file1)) { return() }
    print(ggplot(data=Data()$hourdata)+ geom_line(aes(unique_hour,CO.ppm), color="blue") + labs(title=paste("Hour-Averaged CO \n", Data()$ID)) + scale_x_datetime(labels = date_format("%a %b %d %H")) + xlab("Date")+ ylab("CO (ppm)") + expand_limits(y=0) +  theme(plot.title=element_text(size=rel(0.92), color = "darkgrey")))
  })
  

  output$day_CO <- renderPlot({
    if (is.null(input$file1)) { return() }
    print(ggplot(data=Data()$daydata)+ geom_bar(aes(unique_24h,CO.ppm), fill="aquamarine3", stat = "identity") + labs(title=paste("24-Hour Averaged CO \n", Data()$ID))+xlab("Day")+ ylab("CO (ppm)") + geom_text(aes(x=unique_24h, y = max(CO.ppm*4/100), label=paste("Hours:", total_hours_observation)), col = "black") +expand_limits(y=0) +  theme(plot.title=element_text(size=rel(0.92), color = "darkgrey")))
  })

  ##### CAPTIONS #######
  
  output$caption1 <- renderText( {
    if (is.null(input$file1)) { return() }
    paste("Data Summary for:", Data()$ID)
  })
  
  output$caption2 <- renderText( {
    if (is.null(input$file1)) { return() }
    paste("Minute-Averaged CO Readings (Max = ", round(max(Data()$minutedata$CO.ppm), digits = 1), ")")
  })
  
  output$caption3 <- renderText( {
    if (is.null(input$file1)) { return() }
    paste("Hour-Averaged CO Readings (Max = ", round(max(Data()$hourdata$CO.ppm), digits = 1), ")")
  })
  
  output$caption4 <- renderText( {
    if (is.null(input$file1)) { return() }
    "24-hour Average CO Readings"
  })
  
  ######### EXPORT 1-MINUTE, 1-HOUR, AND SUMMARY DATA TO CSV and SAVE PLOTS AS PDF #############
  
 
    observe({
      dir <- if (is.null(Data()$dir)) { return() } else dir <- Data()$dir
      
      write.csv(Data()$summary, file = file.path(dir, paste(Data()$ID,"_Data_Summary.csv", sep = "")), row.names = F, na = "")
      write.csv(Data()$minutedata, file = file.path(dir, paste(Data()$ID, "_Data_Minute_Averages.csv", sep = "")), row.names = F) 
      write.csv(Data()$hourdata, file = file.path(dir, paste(Data()$ID,"_Data_Hour_Averages.csv", sep = "")), row.names=F)
      
      pdf(file = file.path(dir, paste(Data()$ID,"_CO_minute_avg.pdf", sep = "")))
      print(ggplot(data=Data()$minutedata)+ geom_line(aes(unique_min,CO.ppm), color="blue") + labs(title=paste("Minute-Averaged CO \n", Data()$ID))+xlab("Date")+ ylab("CO (ppm)") + scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%a %b %d %H")) + expand_limits(y=0) +  theme(plot.title=element_text(size=rel(0.92), color = "darkgrey")))
     dev.off()
      
      pdf(file = file.path(dir, paste(Data()$ID,"_CO_hour_avg.pdf", sep = "")))
      print(ggplot(data=Data()$hourdata)+ geom_line(aes(unique_hour,CO.ppm), color="blue") + labs(title=paste("Hour-Averaged CO \n", Data()$ID))+xlab("Date")+ ylab("CO (ppm)") + scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%a %b %d %H"))+ expand_limits(y=0) +  theme(plot.title=element_text(size=rel(0.92), color = "darkgrey")))
      dev.off()
      
      pdf(file = file.path(dir, paste(Data()$ID,"_CO_24hour_avg.pdf", sep = "")))
      print(ggplot(data=Data()$daydata)+ geom_bar(aes(unique_24h,CO.ppm), fill="aquamarine3", stat = "identity") + labs(title=paste("24-Hour Averaged CO \n", Data()$ID))+xlab("Day")+ ylab("CO (ppm)") + geom_text(aes(x=unique_24h, y = max(CO.ppm*4/100), label=paste("Hours:", total_hours_observation)), col = "black") + expand_limits(y=0) +  theme(plot.title=element_text(size=rel(0.92), color = "darkgrey")))
      dev.off()

    })
  
})
