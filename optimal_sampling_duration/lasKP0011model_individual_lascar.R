

library(doBy)
setwd("")

#KP0011

## Stack 5, (overlap on 6/21/13, between 903 and 1034)
KP0011_S1_Data_Minute_Averages.copy <- read.csv("~/Desktop/Sample Optimizaiton-Rotation Work/Compiled 1 Min Avg Pilot Data/Lascar- 3 day & 6 day/KP0011_S1_Data_Minute_Averages copy.csv")
las38<-KP0011_S1_Data_Minute_Averages.copy

KP0011_S2_Data_Minute_Averages.copy <- read.csv("~/Desktop/Sample Optimizaiton-Rotation Work/Compiled 1 Min Avg Pilot Data/Lascar- 3 day & 6 day/KP0011_S2_Data_Minute_Averages copy.csv")
las39<-KP0011_S2_Data_Minute_Averages.copy

##overlap between 6/21 between 9:03 and 10:33 (90 min)
##sidebyside comparison of 90 min overlap
tail(las38,1)
head(las39,1)

##capture 90 min overlap by using indices of las38 and las 39, last and first values,
##respectively.
comp<-list(tail(las38,90), head(las39,90)) 

#chop off overlapped data in las38
las38<-las38[-(4230:4320),]

#Coerce date formats for each session data set into a new column with reformatted 
# dates by using as.Date before stacking
las38$date<-as.Date(las38$unique_min, format="%Y-%m-%d %H:%M")
las39$date<-as.Date(las39$unique_min,format= "%m/%d/%y %H:%M")


las38$datetime<-as.POSIXct(las38$unique_min, "%Y-%m-%d %H:%M", tz="UTC")
las39$datetime<-as.POSIXct(las39$unique_min, "%m/%d/%y %H:%M", tz="UTC")

lasstack.KP0011<-rbind(las38, las39)


# first date
mindate<-min(lasstack.KP0011$date) 
# last date
maxdate<-max(lasstack.KP0011$date)

mindate2<-paste(mindate, "00:00")
maxdate2<-paste(maxdate, "23:59")

#sequence from min to max and reformat
fullseq<-seq(as.POSIXct(mindate2, tz="UTC"), as.POSIXct(maxdate2, tz="UTC"), by="min")

# dataframe
dateinfo<-data.frame(datetime=fullseq)


##Create vector and format unique_min vector into POSI and tack on
##new datetime vector
##inconsistent date formats from the two sessions

lasstack.KP0011$datetime<-as.POSIXct(lasstack.KP0011$datetime, "%Y-%m-%d %H:%M", tz="UTC")


#merge date and time to date frame
lasstack.KP0011<-merge(lasstack.KP0011, dateinfo, by="datetime", all=F)
lasstack.KP0011<-lasstack.KP0011[order(lasstack.KP0011$datetime),]

## polished 6-day stacked data set for Subject KP0011!
lasstack.KP0011

# your table name is long I'll shorten for example
dat<-lasstack.KP0011
numberOfSimulations<-5000

vals<-NULL

for(i in 1:5000){
  
  ##Crude approach, but to identify the cut-off index point that precedes the last single datum
  ## by 24 hrs take the difference between "length(dat$unique_min)" and 1440 (1440 min = 1 day). 
  ## Use the difference to define lastPt object and set the cutoff
  
  
  lastPt<-(length(dat$datetime)-1439) 
  randStartPt<-sample(1:lastPt,1)
  
  ## randStartPt+1439-- used 1439 minutes as "averaging period" of interest
  ## for model to compute and calculate
  sampledVector<-as.numeric(as.character(dat$CO.ppm[randStartPt:(randStartPt+1439)])) # your values are factor!
  simMean<-mean(sampledVector, na.rm=T)
  vals<-append(vals, simMean)
  
}


las.KP0011.24hr.means<-vals
hist(las.KP0011.24hr.means)
mu<-mean(dat$CO.ppm)


# 48 hr Simulation

vals<-NULL
for(i in 1:5000){
  
  lastPt<-(length(dat$datetime)-1439) 
  randStartPt<-sample(1:lastPt,1)
  
  ## randStartPt+2879-- used 2880 minutes (48 hrs) as "averaging period" of interest
  ## for model to compute and calculate
  sampledVector<-as.numeric(as.character(dat$CO.ppm[randStartPt:(randStartPt+2839)])) # your values are factor!
  simMean<-mean(sampledVector, na.rm=T)
  vals<-append(vals, simMean)
}

las.KP0011.48hr.means<-vals
hist(las.KP0011.48hr.means)

# 72 hr Simulation

vals<-NULL
for(i in 1:5000){
  
  lastPt<-(length(dat$datetime)-1439) 
  randStartPt<-sample(1:lastPt,1)
  
  ## randStartPt+4319-- used 4319 minutes (48 hrs) as "averaging period" of interest
  ## for model to compute and calculate
  sampledVector<-as.numeric(as.character(dat$CO.ppm[randStartPt:(randStartPt+4319)])) # your values are factor!
  simMean<-mean(sampledVector, na.rm=T)
  vals<-append(vals, simMean)
}
las.KP0011.72hr.means<-vals
hist(las.KP0011.72hr.means)


##Root Mean Square Error

mu<-mean(lasstack.KP0011$CO.ppm)
mu_Vec<-seq(mu, mu, length= 5000)
SimMeans<-data.frame(mu_Vec, las.KP0011.24hr.means,las.KP0011.48hr.means,las.KP0011.72hr.means)


las.KP0011.rmse.24<-sqrt(sum((SimMeans$las.KP0011.24hr.means-SimMeans$mu_Vec)^2, na.rm=TRUE)/length(mu_Vec))
las.KP0011.rmse.48<-sqrt(sum((SimMeans$las.KP0011.48hr.means-SimMeans$mu_Vec)^2, na.rm=TRUE)/length(mu_Vec))
las.KP0011.rmse.72<-sqrt(sum((SimMeans$las.KP0011.72hr.means-SimMeans$mu_Vec)^2, na.rm=TRUE)/length(mu_Vec))

las.KP0011<-c(mu,las.KP0011.rmse.24,las.KP0011.rmse.48,las.KP0011.rmse.72)


library(hydroGOF)
rmse(SimMeans$las.BB0397.24hr.means,SimMeans$mu_Vec)


