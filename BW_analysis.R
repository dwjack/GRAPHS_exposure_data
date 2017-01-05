library(haven)
#library(sjPlot)
#library(sjmisc)
library(dplyr)
library(lubridate)
library(nlme)
library(sandwich)
library(lmtest)

############# DATA PREP ##################
# load original data
bw <- read_dta("/Users/ashlinn/Documents/Birthweight ER analysis/BW_data_cleaned.dta")


# names(bw_new) <- tolower(names(bw_new))
## blank2na------
blank2na <- function(x){ 
        z <- gsub("\\s+", "", x)  #make sure it's "" and not " " etc
        x[z==""] <- NA 
        return(x)
}

# remove those w no cstudyid

bw$cstudyid <- sapply(bw$cstudyid, blank2na)

sum(is.na(bw$cstudyid))
bw$mstudyid[is.na(bw$cstudyid)& !is.na(bw$b1alive1)]
bw_new <- bw[!is.na(bw$cstudyid),]

# none have multiple births

# remove stillbirths
table(bw_new$b1alive1)
bw_new<- bw_new[bw_new$b1alive1 ==1,]

# select variables
bw_new <- select(bw_new, c(mstudyid, cluster, age, medlev, numyrs, married, religion, ethnic, wownland, farmln, salary, househead, electric, comptype, Mage, wealthindex2, wealthindex, cstudyid,  datebwt, timebwt, bwtlght1, bplace, b1placbir2, bsex1, banomaly1, b1alive1, bp, anaemia2, diabetes, hiv, smokecur, smokhh, smokcc, wtkg, htcm, trialedd, age, DAYSOFFEDD, datdeliv2, GESTAGE_DAYS, GESTAGE_WEEKS, PTB, PTBCAT, BWEIGHT, BWEIGHT_GRAMS, LBW, SGA, BLENGTH, HCIRC, GESTAGE_ENROLL_WEEK, GESTAGE_ENROL_WKI, CALCPARITY, COMMUNITY))

# names to lower case
names(bw_new) <- tolower(names(bw_new))


saveRDS(bw_new, file = paste0("bw_new_", format(Sys.Date(), format = "%b%d"), ".rds"))


# merge w CO data
co <- readRDS("/Users/ashlinn/Documents/Birthweight ER analysis/FINAL_CO_parameters_withvalidation_2016Jun14.rds")
co <- co[co$mstudyid %in% bw_new$mstudyid,]


# check mstudyid and cstudyid alignment
test <- bw_new[, c("mstudyid", "cstudyid")]


for (i in 1:length(unique(co$mstudyid))) {
        data <- co[co$mstudyid == unique(co$mstudyid)[i],]
        if(length(unique(data$cstudyid[!is.na(data$cstudyid)])) > 1) {
                print(unique(data$mstudyid))
                print(unique(data$cstudyid))
        }
}
# 3 have two cstudyids - all of these accidentally have the Mstudyid as the Cstudyid. Can ignore for the pregnancy exposure.
row.names(co[co$mstudyid %in% c("BM0846M", "BM0795M", "BM1351M") &!is.na(co$cstudyid), c("mstudyid", "session", "cstudyid", "lastdate", "newid")])
co[row.names(co) %in% c(3939,7730,11041), c("mstudyid", "cstudyid")]
co <- co[!row.names(co) %in% c(3939,7730,11041),]



alignment <- data.frame(mstudyid = unique(co$mstudyid), cstudyid = NA)
for (i in 1:nrow(alignment)) {
        alignment$cstudyid[i] <- ifelse(
                !is.na(unique(co$cstudyid[co$mstudyid == alignment$mstudyid[i]])), 
                unique(co$cstudyid[co$mstudyid == alignment$mstudyid[i] &!is.na(co$cstudyid)]), 
                NA)
}


test2 <- merge(test, alignment, by = "mstudyid", all = TRUE)
test2 <- test2[!is.na(test2$cstudyid.y),]
identical(test2$cstudyid.x, test2$cstudyid.y) # TRUE, but only 244 rows???

# well at least none is misaligned....


# need to separate out CO prior to birth date, by mstudyid
str(co$lastdate)
str(bw_new$datdeliv2)
bw_new$birthdate <- ymd(bw_new$datdeliv2, tz = "UTC")

co_new <- data.frame()
allids <- unique(bw_new$mstudyid)
for(i in 1:length(allids)) {
        co_byid <- co[co$mstudyid == allids[i],]
        birthdate <- bw_new$birthdate[bw_new$mstudyid == allids[i]]
        co_byid <- co_byid[co_byid$lastdate <= birthdate,]
        co_new <- rbind(co_new, co_byid)
}

length(unique(co_new$mstudyid)) # 1302, should be 1303
bw_new$mstudyid[!bw_new$mstudyid %in% unique(co_new$mstudyid)] # BM1607M, had no co sessions during pregnancy?
unique(co_new$mstudyid[!unique(co_new$mstudyid) %in% unique(bw_new$mstudyid)])

# remove BM1607M
bw_new <- bw_new[!bw_new$mstudyid == "BM1607M",]


range(table(co_new$mstudyid)) # 1 to 7 lascar sessions

# exclude invalid CO sessions
co_new<- filter(co_new, visually_valid == 1 | visually_valid == 2,  co_hours >=18) # might want to do sens w only 1 later

# co_new<- co_new[(co_new$visually_valid == 1 | co_new$visually_valid == 2) & co_new$co_hours >=18,] # might want to do sens w only 1 later

co_new$co_dailymean_corr <- rowMeans(co_new[,c("co_day1_mean_corr", "co_day2_mean_corr", "co_day3_mean_corr")], na.rm = TRUE)



co_summarized_1and2 <- group_by(co_new, mstudyid) %>% summarise(
        co_mean_corr = mean(co_mean_corr), 
        co_dailymean_corr = mean(co_dailymean_corr),
        co_q85_corr = mean(co_q85_corr),
        co_q95_corr = mean(co_q95_corr)
)

saveRDS(co_summarized_1and2, file = "co_summarized_1and2.rds")

# with only visual validity 1

co_only1 <- filter(co_new, visually_valid ==1 & co_hours >=18)
# co_only1 <- co_new[co_new$visually_valid == 1 & co_new$co_hours >=18,] 

co_summarized_only1 <- group_by(co_only1, mstudyid) %>% summarise(
        co_mean_corr = mean(co_mean_corr), 
        co_dailymean_corr = mean(co_dailymean_corr),
        co_q85_corr = mean(co_q85_corr),
        co_q95_corr = mean(co_q95_corr)
)

saveRDS(co_summarized_only1, file = "co_summarized_only1.rds")

# merge data (including CO validated as 1 or 2) and save 
alldata <- merge(bw_new, co_summarized_1and2, by = "mstudyid", all.x = TRUE)

# add PM data
pm <- readRDS("/Users/ashlinn/Documents/Birthweight ER analysis/PM data/PMCO.rds")
# each row is a day, "CorPM" is the PM value for the day? The day is "PMday" So take the average of all CorPM values that occurred prior to the birth date. (Could also use "lastdate" from the CO data which is merged here)

pm <- pm[, c("mstudyid", "PMday", "CorPM")]

length(unique(alldata$mstudyid))
length(unique(pm$mstudyid))
length(unique(pm$mstudyid[unique(pm$mstudyid) %in% unique(alldata$mstudyid)]))

# Subset the data prior to the birth date and summarize for each participant
pmdata <- merge(pm, alldata[, c("mstudyid", "birthdate")], by = "mstudyid")
head(pmdata[which(pmdata$birthdate < pmdata$PMday),])
pmdata <- pmdata[pmdata$PMday <= pmdata$birthdate,]
pmsummary <- group_by(pmdata, mstudyid) %>% summarise(CorPM = mean(CorPM)) # the mean PM before birth

length(unique(pmsummary$mstudyid)) # 749 women with PM prior to birth

# merge with other data
alldata <- merge(alldata, pmsummary, by = "mstudyid", all.x = TRUE)

saveRDS(alldata, file = paste0("bw_er_data_1and2_", format(Sys.Date(), format = "%b%d"), ".rds"))


################### ANALYSIS ########################

alldata <- readRDS("/Users/ashlinn/Documents/Birthweight ER analysis/BW ER Analysis/bw_er_data_1and2_Jan05.rds")
# these CO measurements are averages prior to delivery

# see http://www.unige.ch/ses/sococ/cl/r/tasks/outliers.e.html

# outliers defined as values more than 3.5 times the interquartile range from the edges of the interquartile range (the box). E.g. above the 3rd quartile or below the 1st quartile.

# looking for  outliers 
pdf("bw_variable_outliers.pdf")
par(mfrow = c(2,3))
boxplot(alldata$htcm, range = 3.5, main = "height in cm")
boxplot(alldata$wtkg, range = 3.5, main = "weight in kg")
boxplot(alldata$bweight_grams, range = 3.5, main = "birth weight in grams")
boxplot(alldata$blength, range = 3.5, main = "birth length") # some outliers
boxplot(alldata$hcirc, range = 3.5, main = "head circumference") # some outliers
boxplot(alldata$gestage_weeks, range = 3.5, main = "Gest Age in weeks") # some outliers
dev.off()


# checking normality
hist(alldata$bweight_grams) # very normal
hist(alldata$co_dailymean_corr) # very right-skewed
hist(alldata$hcirc[alldata$hcirc < 150]) # normal except for outliers
hist(alldata$blength) # some outliers on low end, otherwise normal

######## BIRTH WEIGHT AS OUTCOME ########

# Cluster-robust standard errors --------
# from https://thetarzan.wordpress.com/2011/06/11/clustered-standard-errors-in-r/
# In R, you first must run a function here called cl() written by Mahmood Arai in Stockholm University – the backup can be found here and here. http://people.su.se/~ma/clustering.pdf
# http://www.ne.su.se/polopoly_fs/1.216115.1426234213!/menu/standard/file/clustering1.pdf

cl   <- function(data,fm, cluster){ # fm is the "fitted model" from a linear regression; use $ in the cluster ID
        require(sandwich, quietly = TRUE)
        require(lmtest, quietly = TRUE)
        M <- length(unique(cluster))
        N <- length(cluster)
        K <- fm$rank
        dfc <- (M/(M-1))*((N-1)/(N-K))
        uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
        vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
        coeftest(fm, vcovCL) }

# After running the code above, you can run your regression with clustered standard errors as follows:
# Run a plain linear regression
# regt = lm(nevermar ~ impdum, data = nmar)
# 
# # apply the 'cl' function by choosing a variable to cluster on.
# # here, we are clustering on state.
# cl(data = nmar, fm = regt, cluster = nmar$state)


# variables to include:
# calcparity (parity)/mage/bmi/wealthindex/bsex1 (binary 1:2)
alldata$bmi <- alldata$wtkg/((alldata$htcm*.01)^2)
alldata$bsex <- mapvalues(alldata$bsex1, from = c(1,2), to = c(0,1)) # 0 is male, 1 is female

# unadjusted models for bweight_grams ---------
# co logged
# interpretation: http://www.kenbenoit.net/courses/ME104/logmodels2.pdf
# The expected change in Y associated with a p% increase in X can be calculated as βˆ · log([100 + p]/100). So to work out the expected change associated with a 10% increase in X , therefore, multiply βˆ by log(110/100) = log(1.1) = .095. In other words, 0.095βˆ is the expected change in Y when X is multiplied by 1.1, i.e. increases by 10%.

# Subset complete data
# 10 NAs for bweight_grams, 45 NAs for co_dailymean_corr
alldata_bw <- alldata[!is.na(alldata$bweight_grams),]

results <- data.frame(model= NA, coefficient= NA, p.value = NA, per_10pct_CO = NA, per_IQR_CO = NA, n = NA)

fm <- lm(bweight_grams ~ log(co_mean_corr), data = alldata_bw)

cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community) # -55,  sig
results[1,1] <- as.character(fm$call[2])
results[1,2] <- cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community)[2,1] # coef
results[1,3] <- cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community)[2,4] # pval
results[1,4] <- cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community)[2,1] *log(1.1) # 10%
results[1,5] <- cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community)[2,1] *log(1.706/0.698) # IQR
results[1,6] <- length(fm$residuals)

# quantiles, method 1
fm <- lm(bweight_grams ~ log(co_q85_corr +0.01), data = alldata_bw)
cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community) # -32, sig
# how to adjust for the 0.01 addition in the interpretation?

fm <- lm(bweight_grams ~ log(co_q95_corr + 0.01), data = alldata_bw)
cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community) # -45, sig
plot(fm)

# quantiles, method 2
alldata_bw_quantiles <- alldata_bw[!alldata_bw$co_q85_corr == 0,]
fm <- lm(bweight_grams ~ log(co_q85_corr), data = alldata_bw_quantiles)
cl(fm = fm, data = alldata_bw_quantiles, cluster = alldata_bw_quantiles$community) # -39, sig

fm <- lm(bweight_grams ~ log(co_q95_corr), data = alldata_bw_quantiles)
cl(fm = fm, data = alldata_bw_quantiles, cluster = alldata_bw_quantiles$community) # -37, NOT sig

alldata_bw_daily <- alldata_bw[!is.na(alldata_bw$co_dailymean_corr),]
fm <- lm(bweight_grams ~ log(co_dailymean_corr), data = alldata_bw_daily) 
cl(fm = fm, data = alldata_bw_daily, cluster = alldata_bw_daily$community) # -48, sig

# unadjusted models for blength ---------
alldata_blength <- alldata[!is.na(alldata$blength),]
fm <- lm(blength ~ log(co_mean_corr), data = alldata_blength)
model <- cl(fm = fm, data = alldata_blength, cluster = alldata_blength$community) # -0.32, sig
results[2,1] <- as.character(fm$call[2])
results[2,2] <- model[2,1] # coef
results[2,3] <-model[2,4] # pval
results[2,4] <- model[2,1] *log(1.1) # 10%
results[2,5] <- model[2,1] *log(1.706/0.698) # IQR
results[2,6] <- length(fm$residuals)

fm <- lm(blength ~ log(co_q85_corr + 0.01), data = alldata_blength)
cl(fm = fm, data = alldata_blength, cluster = alldata_blength$community) # -0.22, sig

fm <- lm(blength ~ log(co_q95_corr + 0.01), data = alldata_blength)
cl(fm = fm, data = alldata_blength, cluster = alldata_blength$community) # -0.22, not sig

alldata_blength_daily <- alldata_blength[!is.na(alldata_blength$co_dailymean_corr),]
fm <- lm(blength ~ log(co_dailymean_corr), data = alldata_blength_daily)
cl(fm = fm, data = alldata_blength_daily, cluster = alldata_blength_daily$community) # -0.29, not sig


# unadjusted models for head circ ---------
alldata_hcirc <- alldata[!is.na(alldata$hcirc),]
fm <- lm(hcirc ~ log(co_mean_corr), data = alldata_hcirc)
model <- cl(fm = fm, data = alldata_hcirc, cluster = alldata_hcirc$community) # -0.19, not sig
results[3,1] <- as.character(fm$call[2])
results[3,2] <- model[2,1] # coef
results[3,3] <-model[2,4] # pval
results[3,4] <- model[2,1] *log(1.1) # 10%
results[3,5] <- model[2,1] *log(1.706/0.698) # IQR
results[3,6] <- length(fm$residuals)


fm <- lm(hcirc ~ log(co_q85_corr + 0.01), data = alldata_hcirc)
cl(fm = fm, data = alldata_hcirc, cluster = alldata_hcirc$community) # -0.07,  not sig

fm <- lm(hcirc ~ log(co_q95_corr + 0.01), data = alldata_hcirc)
cl(fm = fm, data = alldata_hcirc, cluster = alldata_hcirc$community) # -0.10, not sig

alldata_hcirc_daily <- alldata_hcirc[!is.na(alldata_hcirc$co_dailymean_corr),]
fm <- lm(hcirc ~ log(co_dailymean_corr), data = alldata_hcirc_daily)
cl(fm = fm, data = alldata_hcirc_daily, cluster = alldata_hcirc_daily$community) # -0.15, not sig

# Adjusted models ------------

# evaluate which variables are associated with exposure and outcome

for (i in c("calcparity", "bmi", "mage", "bsex", "wealthindex")) {
        fm <- lm(alldata[,i] ~ log(alldata$co_q85_corr + 0.01),na.action = na.exclude)
        print(i)
        print(summary(fm))
}
# for co variables (mean, q85, q95): parity the only one associated. 


for (i in c("calcparity", "bmi", "mage", "bsex", "wealthindex")) {
fm <- lm(alldata$bweight ~ alldata[,i],na.action = na.exclude)
print(i)
print(summary(fm))
}
# for bweight, sig: calcparity, bmi, mage, bsex
# not sig: wealthindex


for (i in c("calcparity", "bmi", "mage", "bsex", "wealthindex")) {
        fm <- lm(alldata$blength ~ alldata[,i],na.action = na.exclude)
        print(i)
        print(summary(fm))
}
# for blength, sig: calcparity, mage, bsex
# not sig: wealthindex, bmi

for (i in c("calcparity", "bmi", "mage", "bsex", "wealthindex")) {
        fm <- lm(alldata$hcirc ~ alldata[,i],na.action = na.exclude)
        print(i)
        print(summary(fm))
}
# for hcirc, sig: calcparity, mage
# not sig: wealthindex, bmi, bsex



# Adjusted
fm <- lm(bweight_grams ~ log(co_mean_corr) + calcparity, data = alldata_bw)
model <- cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community) # -63,  sig
# interp: a 10% increase in CO is associated with log(1.1)*(-63.23) = -6 grams difference in bw. 
# for IQR: 75th percentile = 1.706, 25th = 0.698. A one-IQR increase in CO is associated with log(1.706/0.698)* (-63.23) = -57 grams difference in bw. 
results[4,1] <- as.character(fm$call[2])
results[4,2] <- model[2,1] # coef
results[4,3] <-model[2,4] # pval
results[4,4] <- model[2,1] *log(1.1) # 10%
results[4,5] <- model[2,1] *log(1.706/0.698) # IQR
results[4,6] <- length(fm$residuals)

fm <- lm(blength ~ log(co_mean_corr) + calcparity, data = alldata_blength)
model <- cl(fm = fm, data = alldata_blength, cluster = alldata_blength$community) # -63,  sig
# interp: a 10% increase in CO is associated with log(1.1)*(-63.23) = -6 grams difference in bw. 
# for IQR: 75th percentile = 1.706, 25th = 0.698. A one-IQR increase in CO is associated with log(1.706/0.698)* (-63.23) = -57 grams difference in bw. 
results[5,1] <- as.character(fm$call[2])
results[5,2] <- model[2,1] # coef
results[5,3] <-model[2,4] # pval
results[5,4] <- model[2,1] *log(1.1) # 10%
results[5,5] <- model[2,1] *log(1.706/0.698) # IQR
results[5,6] <- length(fm$residuals)

fm <- lm(hcirc ~ log(co_mean_corr) + calcparity, data = alldata_hcirc)
model <- cl(fm = fm, data = alldata_hcirc, cluster = alldata_hcirc$community) # -63,  sig
# interp: a 10% increase in CO is associated with log(1.1)*(-63.23) = -6 grams difference in bw. 
# for IQR: 75th percentile = 1.706, 25th = 0.698. A one-IQR increase in CO is associated with log(1.706/0.698)* (-63.23) = -57 grams difference in bw. 
results[6,1] <- as.character(fm$call[2])
results[6,2] <- model[2,1] # coef
results[6,3] <-model[2,4] # pval
results[6,4] <- model[2,1] *log(1.1) # 10%
results[6,5] <- model[2,1] *log(1.706/0.698) # IQR
results[6,6] <- length(fm$residuals)


write.csv(results, file = "bw_er_results.csv", row.names = FALSE)



######### LBW, SGA, PTB #########

# binary outcomes... 


# Plot ------
library(ggplot2)
library(gridExtra)
p1 <- ggplot(aes(co_mean_corr, bweight_grams), data = alldata_bw)
p1 <- p1 + geom_point() + scale_x_log10(breaks = c(0, 0.1, 1, 10, 30)) + geom_smooth(method = "loess") + xlab("Mean CO (ppm)")+ ylab("Birth Weight (grams)")

p2 <- ggplot(aes(co_mean_corr, blength), data = alldata_bw)
p2 <- p2 + geom_point() + scale_x_log10(breaks = c(0, 0.1, 1, 10, 30)) + geom_smooth(method = "loess") + xlab("Mean CO (ppm)")+ ylab("Birth Length (cm)")

p3 <- ggplot(aes(co_mean_corr, hcirc), data = alldata_bw)
p3 <- p3 + geom_point() + scale_x_log10(breaks = c(0, 0.1, 1, 10, 30)) + geom_smooth(method = "loess") + xlab("Mean CO (ppm)")+ ylab("Head Circumference (cm)")

pdf(file = "BW_BL_HC_scatterplots.pdf", bg = "white")
print(p1)
print(p2)
print(p3)
dev.off()


## Analysis with PM --------------


# models
alldata_bw <- alldata[!is.na(alldata$bweight_grams) & !is.na(alldata$CorPM),]

results_pm <- data.frame(model= NA, coefficient= NA, p.value = NA, per_10pct_PM = NA, per_IQR_PM = NA, n = NA)

fm <- lm(bweight_grams ~ log(CorPM), data = alldata_bw)
quantile(alldata_bw$CorPM) # 25: 43.568; 75: 103. 558
cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community) # -33, not sig
results_pm[1,1] <- as.character(fm$call[2])
results_pm[1,2] <- cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community)[2,1] # coef
results_pm[1,3] <- cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community)[2,4] # pval
results_pm[1,4] <- cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community)[2,1] *log(1.1) # 10%
results_pm[1,5] <- cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community)[2,1] *log(103.558/43.568) # IQR
results_pm[1,6] <- length(fm$residuals)


fm <- lm(bweight_grams ~ log(CorPM) + calcparity, data = alldata_bw)
quantile(alldata_bw$CorPM) # 25: 43.568; 75: 103. 558
cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community) # -33, not sig
results_pm[2,1] <- as.character(fm$call[2])
results_pm[2,2] <- cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community)[2,1] # coef
results_pm[2,3] <- cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community)[2,4] # pval
results_pm[2,4] <- cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community)[2,1] *log(1.1) # 10%
results_pm[2,5] <- cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community)[2,1] *log(103.558/43.568) # IQR
results_pm[2,6] <- length(fm$residuals)

p1 <- ggplot(aes(CorPM, bweight_grams), data = alldata_bw)
p1 <- p1 + geom_point() + scale_x_log10() + geom_smooth(method = "loess") + xlab("Mean PM")+ ylab("Birth Weight (grams)")
p1
