#function to import biolite stove use data
#need to make sure that we import the stove ID - as of Feb 26, this is not happening
#(thanks to Ajay for the original import fuction)


wd <- "~/Documents/projects/Biomass_working_group/Ghana_R01/biolite_data_download/"
bl1 <- paste(wd, "HSAB0000428_2014_02_12_11_23_44.csv", sep="")
#bl2 <- paste(wd, "HSAB0000301_2013_12_28_13_59_06.csv", sep="")
biolite.import <- function(x){
  dt <- read.csv(x, stringsAsFactors=F, header=F)
  dt <- dt[,c("V1","V8")]
  names(dt) <- c('datetime','value')	
  dt$datetime <- as.POSIXct(dt$datetime, origin="1970-01-01 12:00:00", tz='UTC')
  dt$variable <- "biolite"
  #this depends entirely on where the files are and the directory structure
  #just guessed on some potential metadata
#   dt$hhid <- strsplit(strsplit(x,'_')[[1]][1],'/')[[1]][3]
#   dt$id1 <- strsplit(x,'_')[[1]][5]
#   dt$id2 <- strsplit(x,'_')[[1]][6]
#   dt$id3 <- substring(strsplit(x,'_')[[1]][7],1,2)
  dt
}
biolite1 <- biolite.import(bl1)
biolite1$download <- "download1"
#biolite2 <- biolite.import(bl2)
#biolite2$download <- "download2"

#biolite245 <- rbind(biolite1,biolite2)
#head(biolite245)

ggplot(biolite1,aes(x=datetime,y=value,colour=download))+geom_point()
