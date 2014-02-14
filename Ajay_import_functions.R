library(lubridate)
library(plyr)
library(ggplot2)
library(reshape2)
library(scales)

bl <- '~/Desktop/HSAB0000336_2013_11_05_12_41_37.csv'
biolite.import <- function(x){
	dt <- read.csv(x, stringsAsFactors=F, header=F)
	dt <- dt[,c("V1","V8")]
	names(dt) <- c('datetime','value')	
	dt$datetime <- as.POSIXct(dt$datetime, origin="1970-01-01 12:00:00", tz='UTC')
	dt$variable <- "biolite"
	#this depends entirely on where the files are and the directory structure
	#just guessed on some potential metadata
	dt$hhid <- strsplit(strsplit(x,'_')[[1]][1],'/')[[1]][3]
	dt$id1 <- strsplit(x,'_')[[1]][5]
	dt$id2 <- strsplit(x,'_')[[1]][6]
	dt$id3 <- substring(strsplit(x,'_')[[1]][7],1,2)
	dt
}
biolite <- biolite.import(bl)

mp <- '~/Desktop/KS0072-PEM0006-PERS-2014-01-02.csv'
micropem.import <- function(x){
	dt <- read.csv(x, stringsAsFactors=F, skip=28, header=F)
	names(dt) <- c('date','time','mass','temp','rh','batt','inlet_press','flow_orifice','lpm','x','y','z','composite','status')
	dt$datetime <- paste(dt$date, dt$time,sep=" ")
	dt$datetime <- ymd_hms(dt$datetime, tz="Asia/Kathmandu")
	dt$date <- NULL
	dt$time <- NULL
	dt <- melt(dt, id.var="datetime",na.rm=T)
	#fake metadata
	dt$hhid <- "test"
	dt$id1 <- "ek"
	dt$id2 <- "do"
	dt$id3 <- "tiin"
	dt
}
micropem <- micropem.import(mp)

co <- '~/Desktop/EL_CO_4_310511_29008500_W1.csv'
lascar.import <- function(x){
	dt <- read.csv(x, stringsAsFactors=F, header=T)[,c(2,3)]
	names(dt) <- c('datetime','co')
	dt$datetime <- dmy_hms(dt$datetime, tz="Asia/Kathmandu")
	dt <- melt(dt, id.var="datetime",na.rm=T)
	dt$hhid <- "test"
	dt$id1 <- "ek"
	dt$id2 <- "do"
	dt$id3 <- "tiin"
	dt
}
co <- lascar.import(co)

combined <- rbind(co,micropem,biolite)
#the long frame created above facilitates pretty easy plotting, etc.
#however, somewhat useless for going wide, given the time stamps

#not a useful plot, but just to present an idea of what's possible
qplot(datetime,as.numeric(value),color=variable,geom=c('point','line'),data=combined[combined$variable!='status',])+facet_wrap(~ variable,scales='free')

#to get there to wide: 
#have to round to the nearest minute (or X minute interval)

#create a second, rounded, character variable for datetime -- round can take min, 10 min, etc. 
#needs to be a character or factor variable to use ddply's group variable
#this can be done in base R using aggregate -- kind of a pain.
#this can be done much more quickly (both the variable creation and the summarizing) using data.table
micropem$rd.datetime <- as.character(round(micropem$datetime, 'min'))

#use ddply to 'summarize' the value column by your new rounded datetime (and keep all ID variables)
#take mean of values with same rounded datetime stamp
micropem <- ddply(micropem,.(variable, rd.datetime, hhid, id1, id2, id3),summarize,value=mean(as.numeric(value),na.rm=T))

#rename rd.datetime to datetime
colnames(micropem)[2] <- "datetime"

#reset to proper POSIX time
micropem$datetime <- ymd_hms(micropem$datetime, tz="UTC")


#repeat for all log files
#could be turned into a function
#and sped up in other ways
#biolite
biolite$rd.datetime <- as.character(round(biolite $datetime, 'min'))
biolite <- ddply(biolite,.(variable, rd.datetime, hhid, id1, id2, id3),summarize,value=mean(as.numeric(value),na.rm=T))
colnames(biolite)[2] <- "datetime"
biolite$datetime <- ymd_hms(biolite$datetime)

#co
co$rd.datetime <- as.character(round(co $datetime, 'min'))
co <- ddply(co,.(variable, rd.datetime, hhid, id1, id2, id3),summarize,value=mean(as.numeric(value),na.rm=T))
colnames(co)[2] <- "datetime"
co$datetime <- ymd_hms(co$datetime)

#bind files again, as above
rd.combined <- rbind(micropem, biolite, co)

#make wide using recast2
combinedwide <- dcast(rd.combined,hhid+id1+id2+id3+datetime~variable,value.var='value')

#fin