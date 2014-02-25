bl <- "/Users/oldmac/Documents/projects/Biomass_working_group/Ghana_R01/biolite\ data\ download/HSAB0000245_2013_12_30_10_23_56.csv"
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