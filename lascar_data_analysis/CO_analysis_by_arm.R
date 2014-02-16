#script to generate summary statistics from the lascar data files, by arm
#assumes that CO_stacked is in memory (run "lascar_means_by_arm_v1.R")

#merge in the arms.  Contact Darby if you need this data - only circlated as needed
arms <- read.csv("~/Desktop/arms.csv", header=T)
CO_stacked<- join(CO_stacked,arms,by='village_code') #note that arm is numerical, arm2 is a string

#dplyr syntax works nicely, and is *fast*
CO_means_arm<-CO_stacked %.% group_by(arm2) %.% dplyr::summarise(mean(value))
CO_means_hh_session<-CO_stacked %.% group_by(arm2, session, hhid) %.% dplyr::summarise(mean(value), length(value))
CO_means_hh_arm<-CO_stacked %.% group_by(arm2, hhid) %.% dplyr::summarise(mean(value), length(value))

#arm has to be a factor for plotting
CO_means_hh_arm$arm2<-factor(CO_means_hh_arm$arm2) 
CO_means_hh_session$arm<-factor(CO_means_hh_session$arm2)

#plot for durations
CO_means_hh_session<-na.omit(CO_means_hh_session)
ggplot(CO_means_hh_session, aes(x=CO_means_hh_session$length, fill=arm))+geom_density(alpha=.3)+geom_vline(xintercept = 4320)
ggplot(CO_means_hh_arm, aes(x=CO_means_hh_arm$mean, fill=arm))+geom_density(alpha=.3)

#plot for means
CO_means_hh_arm_trimmed<-subset(CO_means_hh_arm,CO_means_hh_arm$mean<10)
ggplot(CO_means_hh_arm_trimmed, aes(x=CO_means_hh_arm_trimmed$mean, fill=arm2))+geom_density(alpha=.3)

# compute the means by arm (after dropping high values)
CO_means_hh_arm_trimmed<-subset(CO_means_hh_arm,CO_means_hh_arm$mean<10)
names(CO_means_hh_arm_trimmed)<-c("arm2","hhid","value","length")
CO_means_arm_trimmed<-CO_means_hh_arm_trimmed %.% group_by(arm2) %.% dplyr::summarise(mean(value),length(value))
CO_means_arm_trimmed