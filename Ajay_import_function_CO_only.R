
co <- "/Users/oldmac/Dropbox/ghana_exposure_data_shared_2014/Main_study_exposure_assessment/Vil_AA_ms/BM0443M_Vil_AA_ms/s_01_BM0443M_AA_ms/lascar_inst_s_01_BM0443M_vil_AA_ms/CU_CO_104_BM0443M_09Nov13_s_01_pri.txt" 
lascar.import <- function(x){
  dt <- read.csv(x, stringsAsFactors=F, header=T)[,c(2,3)]
  names(dt) <- c('datetime','co')
  dt$datetime <- dmy_hms(dt$datetime, tz="Asia/Kathmandu")
  dt <- melt(dt, id.var="datetime",na.rm=T)
  dt$hhid <- "test"
  dt$id1 <- "ek"
  dt$id2 <- "do"
  dt$id3 <- "tiin"
  dt$rd.datetime <- as.character(round(dt $datetime, 'min'))
  dt <- ddply(dt,.(variable, rd.datetime, hhid, id1, id2, id3),summarize,value=mean(as.numeric(value),na.rm=T))
  colnames(dt)[2] <- "datetime"
  dt$datetime <- ymd_hms(dt$datetime)
  dt
}

co <- lascar.import(co)

