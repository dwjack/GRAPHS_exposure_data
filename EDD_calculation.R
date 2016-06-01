# Gestational Age Calculation --------
library(xlsx)
library(lubridate)


# function to change blanks to NA
blank2na <- function(x){ 
  z <- gsub("\\s+", "", x)  #make sure it's "" and not " " etc
  x[z==""] <- NA 
  return(x)
}

# (for testing)
GS <- read.xlsx2("/Users/ashlinn/Dropbox/Ghana_biomass_study (2)/Ultrasound Study Reports/Individual Gestational Age Calc Forms/Gestational age calc_BM000660S.xlsx", sheetIndex =1, startRow = 7)

# choose files
allfiles <- list.files("~/Dropbox/Ghana_biomass_study (2)/Ultrasound Study Reports/Individual Gestational Age Calc Forms/", full.names = TRUE, recursive = FALSE, include.dirs = FALSE)
length(allfiles)

# edd calculation function
edd_calc <- function(files) {
  print(basename(files))
  possibleError <- tryCatch( {
    GS <- read.xlsx2(files, sheetIndex =1, startRow = 7)

    weeks_days <- as.character(GS[9,12])
    weeks_days <- gsub( "/.*$", "",weeks_days)
    days <- as.numeric(substr(weeks_days, nchar(weeks_days), nchar(weeks_days)))
    weeks <- as.numeric(gsub(" .$", "", weeks_days))
    total_days <- weeks*7+days
    timeleft <- 280-total_days
    edd <- scandate + ddays(timeleft) # add days as duration
    }, error = function(e) e
  )
 
  
  if(!inherits(possibleError, "error")) {
    print(inherits(possibleError, "error"))
    GS <- read.xlsx2(files, sheetIndex =1, startRow = 7)
mstudyid <- as.character(GS[1,7])
scandate <- as.numeric(as.character(GS[2,7]))
# scandate <- as.Date(scandate, origin = "1899-12-30") # works

edd_initial <- ifelse(!is.na(as.numeric(as.character(GS[4,7]))), as.numeric(as.character(GS[4,7])), NA)
# edd_initial <- as.Date(edd_initial, origin = "1899-12-30") # works

weeks_days <- as.character(GS[9,12])
weeks_days <- gsub( "/.*$", "",weeks_days)
days <- as.numeric(substr(weeks_days, nchar(weeks_days), nchar(weeks_days)))
weeks <- as.numeric(gsub(" .$", "", weeks_days))
total_days <- weeks*7+days
timeleft <- 280-total_days
edd <- ifelse(!is.na(timeleft), scandate + timeleft, NA) # add days as duration
# edd <- as.Date(edd, origin = "1899-12-30")

value1 <- ""
value2 <- ""
if(nrow(GS) >11) {
  other <- GS[11:nrow(GS),1:6]
  for (i in 1:6) {
    other[,i] <- as.character(other[,i])
    other[,i] <- blank2na(other[,i])
  }
  other <- other[apply(!is.na(other), 1, any), ]
  for (i in 1:6) {
    if(!is.na(other[1,i])) value1 <- as.character(paste(value1, other[1,i]))
    if(!is.na(other[2,i])) value2 <- as.character(paste(value2, other[2,i]))
  }
  }



dt <- data.frame(file = basename(files), mstudyid = mstudyid, scandate = scandate, weeks = weeks, days = days, total_days = total_days, timeleft = timeleft, edd_initial = edd_initial, edd = edd, notes1 = as.character(value1), notes2 = as.character(value2))
dt$edd_difference <- ifelse(!is.na(edd), edd-edd_initial, NA)
  }
  
if(inherits(possibleError, "error")) { 
  dt <-data.frame(file = basename(files), mstudyid = "not run", scandate = NA, weeks = NA, days = NA, total_days = NA, timeleft = NA, edd_initial = NA, edd = NA, notes1 = "", notes2 = "", edd_difference = NA) 
}
 
dt
}

files <- allfiles
edds <- ldply(files, edd_calc, .progress = "text")

edds$scandate <- as.Date(edds$scandate, origin = "1899-12-30")
edds$edd_initial <- as.Date(edds$edd_initial, origin = "1899-12-30") # works
edds$edd <- as.Date(edds$edd, origin = "1899-12-30") # works

for (i in 10:12) {
  edds[, i] <- blank2na(edds[,i])
}
write.csv(edds, file = "edd_calculations.csv", row.names = FALSE)


# Trying to catch the ones that didn't get caught

notcaptured <- edds[is.na(edds$edd_difference) | edds$edd_difference != 0,] #736
write.csv(notcaptured, file = "edd_calcs_notcaptured.csv", row.names = FALSE)
