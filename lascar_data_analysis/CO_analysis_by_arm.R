# script to generate summary statistics from the lascar data files, by arm
# assumes that CO_stacked is in memory (run "lascar_means_by_arm.R")


require(ggplot2) 
require(dplyr)
require(plyr)
require(reshape)
require(lubridate)
require(beepr)

load("~/Documents/projects/Biomass_working_group/Ghana_R01/exposure_data_assessment/CO_stacked_july_4.Rdata")

arms <- read.csv("~/Documents/projects/Biomass_working_group/Ghana_R01/exposure_data_assessment/arms.csv", header=T)
CO_stacked <- join(CO_stacked,arms,by='village_code') #note that arm is numerical, arm2 is a string

beep()

# create a df of household by session means
CO_means_hh_session <- CO_stacked %.% group_by(arm2, session, hhid) %.% dplyr::summarise(mean(co), length(co))
names(CO_means_hh_session) <- c("arm", "session", "hhid", "co", "minutes")
CO_means_hh_session$days <- CO_means_hh_session$minutes/(24*60)
CO_means_hh_session$hours <- CO_means_hh_session$minutes/(60)
View(CO_means_hh_session)

# diagnostics
sum(is.na(CO_means_hh_session$arm))         #counts how many sessions are missing the arm identifier
sum(is.na(CO_means_hh_session$co))          #counts how many sessions had missing co data (NA)
sum(CO_means_hh_session$co==0, na.rm=TRUE)  #counts how many sessions had means of zero
sum(CO_means_hh_session$co>50, na.rm=TRUE)  #count values over 50 
sum(CO_means_hh_session$hours>78)           #count duration greater than 78 hours (the upper limit per our protocol)
sum(CO_means_hh_session$hours<66)           #count duration greater than 66 hours (the lower limit per our protocol)

# omit observations that are obviously flawed
CO_means_hh_session <- subset(CO_means_hh_session, CO_means_hh_session$co != 0)       # keep if co is nonzero
CO_means_hh_session <- subset(CO_means_hh_session, CO_means_hh_session$co != "NA")    # keep if co is non-missing
CO_means_hh_session <- subset(CO_means_hh_session, CO_means_hh_session$hours > 48)    # keep if there are at least 48 hours of data
CO_means_hh_session <- subset(CO_means_hh_session, CO_means_hh_session$hours < 100)   # keep if doesn't exceed 100 hours



# plots of duration

ggplot(CO_means_hh_session, aes(x=CO_means_hh_session$hours))+geom_histogram(binwidth=.1, fill="blue", color="black") + xlab("mean 72 hour CO")

# by arm
ggplot(CO_means_hh_session, aes(x=CO_means_hh_session$hours, fill=arm))+geom_density(alpha=.3)+geom_vline(xintercept = 72) + xlab("sampling duration in hours")
# by session
ggplot(CO_means_hh_session, aes(x=CO_means_hh_session$hours, fill=session))+geom_density(alpha=.3)+geom_vline(xintercept = 72) + xlab("sampling duration in hours")

# analysis of means by arm

CO_means_hh_session_postintervention <- subset(CO_means_hh_session , CO_means_hh_session$session != "01")
aggregate(co~arm, CO_means_hh_session_postintervention, mean) # note this is only the post-intervention obs
aggregate(co~session, CO_means_hh_session, mean)
aggregate(co~arm+session, CO_means_hh_session, mean)


# plots of CO
# by arm
# note first have to omit session 1 (pre-intervention).  
co_truncated <- subset(CO_means_hh_session, CO_means_hh_session$session != "01" & CO_means_hh_session$co < 10 & arm != "NA")

View(co_truncated)
ggplot(co_truncated , aes(x=co_truncated $co)) + geom_density(alpha=.3) + xlab("mean CO")
# by session
ggplot(co_truncated, aes(x=co_truncated$co, fill=session))+geom_density(alpha=.3)
ggplot(co_truncated, aes(x=co, fill=arm))+geom_density(alpha=.3)

# note that these plots are not taking hh averages across the sessions
# create ad dataset of household means (averaging across the sessions, and excluding the pre-intervention session)
# working off of CO_means_hh_session so problem observations have been dropped
CO_means_hh <- CO_means_hh_session %.% group_by(arm, hhid) %.% filter(session!="01") %.% dplyr::summarise(mean(co), length(co))


names(CO_means_hh) <- c("arm",  "hhid", "co", "minutes")

aggregate(co~arm, CO_means_hh, mean)
ggplot(CO_means_hh[which(CO_means_hh$co<10),], aes(x=co, fill=arm)) + geom_density(alpha=.3)
