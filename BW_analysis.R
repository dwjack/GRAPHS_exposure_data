library(haven)
#library(sjPlot)
#library(sjmisc)
library(plyr)
library(dplyr)
library(lubridate)
library(nlme)
library(sandwich)
library(lmtest)
library(ggplot2)
library(gridExtra)

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
bw_new <- select(bw_new, c(mstudyid, cluster, age, medlev, numyrs, married, religion, ethnic, wownland, farmln, salary, househead, electric, comptype, Mage, wealthindex2, wealthindex, cstudyid,  datebwt, timebwt, bwtlght1, bplace, b1placbir2, bsex1, banomaly1, b1alive1, bp, anaemia2, diabetes, hiv, smokecur, smokhh, smokcc, wtkg, htcm, trialedd, age, DAYSOFFEDD, datdeliv2, numancvist, sbp, dbp, urineprot, GESTAGE_DAYS, GESTAGE_WEEKS, PTB, PTBCAT, BWEIGHT, BWEIGHT_GRAMS, LBW, SGA, BLENGTH, HCIRC, GESTAGE_ENROLL_WEEK, GESTAGE_ENROL_WKI, CALCPARITY, COMMUNITY))

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

co_summarized_1and2 <- readRDS("/Users/ashlinn/Documents/Birthweight ER analysis/BW ER Analysis/co_summarized_1and2.rds")
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

# remove BM1607M with no CO data
alldata <- alldata[!alldata$mstudyid == "BM1607M",]

# variables to include:
# calcparity (parity)/mage/bmi/bsex1 (binary 1:2)/ numancvist(more vs less than 4)/ urineprot(ever yes vs always no)/ htn
alldata$bmi <- alldata$wtkg/((alldata$htcm*.01)^2)
alldata$bsex <- mapvalues(alldata$bsex1, from = c(1,2), to = c(0,1)) # 0 is male, 1 is female
alldata$numancvist_bin <- alldata$numancvist
alldata$numancvist_bin[alldata$numancvist_bin %in% c(88,99)] <- NA
alldata$numancvist_bin <- mapvalues(alldata$numancvist_bin, from = c(1:4, 5:14, NA), to = c(0,0,0,0,1,1,1,1,1,1,1,1,1, 1, NA)) # 1 is adequate (>4 visits), 0 is inadequate
alldata$urineprot_bin <- alldata$urineprot
alldata$urineprot_bin <- mapvalues(alldata$urineprot, from = c(1,2,3,8), to = c(1, 0, NA, NA)) # 1 is ever positive, 0 is always negative, NA = not done or not known
alldata$htn <- ifelse(alldata$bp == 1 | alldata$sbp > 140 | alldata$dbp > 90, 1, 0)        # 23 have either history of HTN or elevated BP (DBP > 90) at baseline

# OUTLIERS
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

# set outliers for blength, hcirc, and bweight to NA using 3.5x IQR as criterion (what about BMI, keep all values?)
for (i in c("blength", "hcirc", "bweight_grams")) {
        print(i)
        print(boxplot.stats(alldata[,i], coef = 3.5)$out)
        alldata[,i] <- ifelse(alldata[,i] %in% boxplot.stats(alldata[,i], coef = 3.5)$out | is.na(alldata[,i]), NA, alldata[,i])
}


# checking normality
hist(alldata$bweight_grams) # very normal
hist(alldata$co_dailymean_corr) # very right-skewed
hist(alldata$hcirc) # normal except for outliers
hist(alldata$blength) # some outliers on low end, otherwise normal


saveRDS(alldata, file = paste0("bw_er_data_1and2_", format(Sys.Date(), format = "%b%d"), ".rds"))


################### ANALYSIS ########################

alldata <- readRDS("/Users/ashlinn/Documents/Birthweight ER analysis/BW ER Analysis/bw_er_data_1and2_Jan06.rds")
# these CO measurements are averages prior to delivery



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

## CONFIDENCE INTERVALS-----
# 95% CI: coef +/- SE*t^alpha/2,n-k-1

# what is the critical T value: use qt()
qt(c(.025, .975), df=1291) 



# unadjusted models for continuous variables ---------
# co logged
# interpretation: http://www.kenbenoit.net/courses/ME104/logmodels2.pdf
# The expected change in Y associated with a p% increase in X can be calculated as βˆ · log([100 + p]/100). So to work out the expected change associated with a 10% increase in X , therefore, multiply βˆ by log(110/100) = log(1.1) = .095. In other words, 0.095βˆ is the expected change in Y when X is multiplied by 1.1, i.e. increases by 10%.

allresults <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(allresults) <- c("model", "coefficient", "p.value", "per_10pct", "per_IQR", "n", "outcome", "exposure", "lower_CI", "upper_CI")
for (i in c("bweight_grams", "blength", "hcirc")) {
                outcome <- i
        for (j in c("co_mean_corr", "CorPM")) {
                exposure <- j
                
labels <- data.frame(varname = c("bweight_grams", "blength", "hcirc", "co_mean_corr", "CorPM"), label = c("Birth Weight (g)", "Birth Length (cm)", "Head Circumference (cm)", "Mean CO (ppm)", "Mean PM (ug/m3)"))

results <- data.frame(model= NA, coefficient= NA, p.value = NA, per_10pct = NA, per_IQR = NA, n = NA, outcome = NA, exposure = NA, lower_CI = NA, upper_CI = NA)


pct75 <- quantile(data[!is.na(data[, exposure]),exposure], probs = 0.75)
pct25 <- quantile(data[!is.na(data[, exposure]),exposure], probs = 0.25)

data <- alldata[complete.cases(alldata[, c(outcome, exposure)]),]
fm <- lm(data[,outcome] ~ log(data[,exposure]), data = data)


cl(fm = fm, data = data, cluster = data[, "community"]) #
results[1,1] <- as.character(fm$call[2]) # model
results[1,2] <- cl(fm = fm, data = data, cluster = data[, "community"])[2,1] # coef
results[1,3] <- cl(fm = fm, data = data, cluster = data[, "community"])[2,4] # pval
results[1,4] <- cl(fm = fm, data = data, cluster = data[, "community"])[2,1] *log(1.1) # 10%
results[1,5] <- cl(fm = fm, data = data, cluster = data[, "community"])[2,1] *log(pct75/pct25) # IQR
results[1,6] <- length(fm$residuals) # n
results[1,7] <- outcome
results[1,8] <- exposure
# for confint: coef +/- SE*t^alpha/2,n-k-1
results[1,9] <-  cl(fm = fm, data = data, cluster = data[, "community"])[2,1] + cl(fm = fm, data = data, cluster = data[, "community"])[2,2]*qt(c(.025, .975), df=length(fm$residuals) - (length(fm$coefficients) - 1))[1]  # lower bound
results[1,10] <- cl(fm = fm, data = data, cluster = data[, "community"])[2,1] + cl(fm = fm, data = data, cluster = data[, "community"])[2,2]*qt(c(.025, .975), df=length(fm$residuals) - (length(fm$coefficients) - 1))[2] # upper bound

# save regression plots
pdf(file = paste0(outcome, exposure, "_regressionplots_unadj.pdf"))
par(mfrow = c(3,2))
plot(fm, which = 1:6, ask = FALSE)
dev.off()

# save scatterplot
pdf(file = paste0(i, "_scatterplot.pdf"))
p <- ggplot(aes(co_mean_corr, data[,i]), data = data)
p <- p + geom_point() + scale_x_log10(breaks = c(0, 0.1, 1, 10, 30)) + geom_smooth(method = "loess") + xlab("Mean CO (ppm)")+ ylab(labels$label[labels$varname == i])
print(p)
dev.off()

# adjusted
data <- alldata[complete.cases(alldata[, c(outcome, exposure, "bmi", "mage", "calcparity", "bsex", "numancvist_bin", "htn")]),]
fm <- lm(data[,outcome] ~ log(data[, exposure]) + bmi + mage + calcparity + bsex + numancvist_bin + htn, data = data)


cl(fm = fm, data = data, cluster = data[, "community"]) #
results[2,1] <- as.character(fm$call[2]) # model
results[2,2] <- cl(fm = fm, data = data, cluster = data[, "community"])[2,1] # coef
results[2,3] <- cl(fm = fm, data = data, cluster = data[, "community"])[2,4] # pval
results[2,4] <- cl(fm = fm, data = data, cluster = data[, "community"])[2,1] *log(1.1) # 10%
results[2,5] <- cl(fm = fm, data = data, cluster = data[, "community"])[2,1] *log(pct75/pct25) # IQR
results[2,6] <- length(fm$residuals)
results[2,7] <- outcome
results[2,8] <- exposure
results[2,9] <-  cl(fm = fm, data = data, cluster = data[, "community"])[2,1] + cl(fm = fm, data = data, cluster = data[, "community"])[2,2]*qt(c(.025, .975), df=length(fm$residuals) - (length(fm$coefficients) - 1))[1]  # lower bound
results[2,10] <- cl(fm = fm, data = data, cluster = data[, "community"])[2,1] + cl(fm = fm, data = data, cluster = data[, "community"])[2,2]*qt(c(.025, .975), df=length(fm$residuals) - (length(fm$coefficients) - 1))[2] # upper bound

# add to all results
allresults <- rbind(allresults, results)

pdf(file = paste0(outcome, exposure, "_regressionplots_adj.pdf"))
par(mfrow = c(3,2))
plot(fm, which = 1:6, ask = FALSE)
dev.off()
        }
}
write.csv(allresults, file = "bw_continuous_results.csv", row.names = FALSE)



# # quantiles, method 1
# fm <- lm(bweight_grams ~ log(co_q85_corr +0.01), data = alldata_bw)
# cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community) # -32, sig
# # how to adjust for the 0.01 addition in the interpretation?
# 
# fm <- lm(bweight_grams ~ log(co_q95_corr + 0.01), data = alldata_bw)
# cl(fm = fm, data = alldata_bw, cluster = alldata_bw$community) # -45, sig
# plot(fm)
# 
# # quantiles, method 2
# alldata_bw_quantiles <- alldata_bw[!alldata_bw$co_q85_corr == 0,]
# fm <- lm(bweight_grams ~ log(co_q85_corr), data = alldata_bw_quantiles)
# cl(fm = fm, data = alldata_bw_quantiles, cluster = alldata_bw_quantiles$community) # -39, sig
# 
# fm <- lm(bweight_grams ~ log(co_q95_corr), data = alldata_bw_quantiles)
# cl(fm = fm, data = alldata_bw_quantiles, cluster = alldata_bw_quantiles$community) # -37, NOT sig
# 
# alldata_bw_daily <- alldata_bw[!is.na(alldata_bw$co_dailymean_corr),]
# fm <- lm(bweight_grams ~ log(co_dailymean_corr), data = alldata_bw_daily) 
# cl(fm = fm, data = alldata_bw_daily, cluster = alldata_bw_daily$community) # -48, sig

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





######### LBW, SGA, PTB #########

# binary outcomes... 
# issues: how to calculate robust standard errors?
# Diagnostics?


allresults <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(allresults) <- c("model", "coefficient", "p.value", "n", "outcome", "exposure", "OR_per_doubling", "OR_lower_CI", "OR_upper_CI")

for (i in c("lbw", "sga", "ptb")) {
        outcome <- i
        for (j in c("co_mean_corr", "CorPM")) {
                exposure <- j
                
                labels <- data.frame(varname = c("lbw", "sga", "ptb", "co_mean_corr", "CorPM"), label = c("Low Birth Weight", "Small for Gestational Age", "Preterm Birth", "Mean CO (ppm)", "Mean PM (ug/m3)"))
                
                results <- data.frame(model= NA, coefficient= NA, p.value = NA, n = NA, outcome = NA, exposure = NA, OR_per_doubling = NA, OR_lower_CI = NA, OR_upper_CI = NA)
                
                
                
                data <- alldata[complete.cases(alldata[, c(outcome, exposure)]),]
                fm <- glm(data[,outcome] ~ log2(data[,exposure]), data = data, family = "binomial") # if use base 2 for the log then the exponentiated coefficient represents the odds ratio associated with a doubling in exposure
                
                
               #  cl(fm = fm, data = data, cluster = data[, "community"]) # not working for logistic models
                results[1,1] <- as.character(fm$call[2]) # model
                results[1,2] <- summary(fm)$coef[2,1] # coef
                results[1,3] <- summary(fm)$coef[2,4] # pval
                results[1,4] <- length(fm$residuals) # n
                results[1,5] <- outcome
                results[1,6] <- exposure
                results[1,7] <-exp(summary(fm)$coef[2,1]) # OR  for doubling of exposure (log2)
                results[1,8] <- exp(confint(fm))[2,1] # lower bound of OR
                results[1,9] <- exp(confint(fm))[2,2] # upper bound of OR
                
                
                # adjusted
                data <- alldata[complete.cases(alldata[, c(outcome, exposure, "bmi", "mage", "calcparity", "bsex", "numancvist_bin", "htn")]),]
                fm <- lm(data[,outcome] ~ log2(data[, exposure]) + bmi + mage + calcparity + bsex + numancvist_bin + htn, data = data)
                
                
                # cl(fm = fm, data = data, cluster = data[, "community"]) #
                results[2,1] <- as.character(fm$call[2]) # model
                results[2,2] <- summary(fm)$coef[2,1] # coef
                results[2,3] <- summary(fm)$coef[2,4] # pval
                results[2,4] <- length(fm$residuals) # n
                results[2,5] <- outcome
                results[2,6] <- exposure
                results[2,7] <-exp(summary(fm)$coef[2,1]) # OR  for doubling of exposure (log2)
                results[2,8] <- exp(confint(fm))[2,1] # lower bound of OR
                results[2,9] <- exp(confint(fm))[2,2] # upper bound of OR
                # add to all results
                allresults <- rbind(allresults, results)
                
          
        }
}
write.csv(allresults, file = "bw_logistic_results.csv", row.names = FALSE)

