# CO by arm
library(ggplot2)
library(dplyr)
library(tidyr)

library(gridExtra)

# Add arm information -------
params <- readRDS("~/Dropbox/Ghana_exposure_data_SHARED (1)/CO_files_processed/CO_parameters_validated_6619sessions_Jan30.rds")

arms <- read.csv("~/Dropbox/Ghana_exposure_data_SHARED (1)/arms.csv", stringsAsFactors = FALSE)
AC <- data.frame(village_code = "AC", arm = 2, arm2 = "biolite")
arms <- rbind(arms, AC)
params$village_code <- ifelse(!regexpr("(vil_.._ms|Vil_.._ms)", params$file_all) == -1, toupper(substr(regmatches(params$file_all, (regexpr("(vil_.._ms|Vil_.._ms)", params$file_all))), 5,6)), NA)
sum(is.na(params$village_code)) # 0
nrow(params[params$village_code == "AC",]) #13
params <- merge(params, arms, by = "village_code", all.x = TRUE)


params$session <- as.numeric(substr(params$session, 4, 4))


# subset the valid files & just those for the women, not the babies
good <- params[params$visually_valid ==1 & params$co_hours > 24 &is.na(params$cstudyid),] # 4102 obs
pre <- good[good$session == 1,]
post <- good[good$session > 1,]

range(good$lastdate) # thru dec 2014

# pre-intervention descriptives
summary(pre$co_mean_corr)
IQR(pre$co_mean_corr)
round(tapply(pre$co_mean_corr, pre$arm2, mean), digits = 2)
fit <- aov(co_mean_corr ~ arm2, data = pre)
summary(fit)

# post-intervention descriptives
summary(post$co_mean_corr)
IQR(post$co_mean_corr)
round(tapply(post$co_mean_corr, post$arm2, mean), digits = 2)
fit <- aov(co_mean_corr ~ arm2, data = post)
summary(fit)
t.test(co_mean_corr ~ arm2, data = post[post$arm2 %in% c("control", "lpg"),]) # ***
t.test(co_mean_corr ~ arm2, data = post[post$arm2 %in% c("control", "biolite"),]) # **
t.test(co_mean_corr ~ arm2, data = post[post$arm2 %in% c("lpg", "biolite"),]) #***

# pre & post-intervention boxplots
pdf(file = "CO_pre_post.pdf", width = 10, height = 8, bg = "white")
par(mfrow = c(1,2))
p1 <- ggplot(pre, aes(factor(arm2, levels = c("control", "biolite", "lpg")), co_mean_corr, fill = "red")) + geom_boxplot() + guides(fill = FALSE) + coord_cartesian(ylim = c(0,20)) + ggtitle("Pre-Intervention CO") + xlab(label = "") + ylab("CO (ppm)") + theme_bw()

p2 <- ggplot(post, aes(factor(arm2, levels = c("control", "biolite", "lpg")), co_mean_corr, fill = "blue")) + geom_boxplot() + guides(fill = FALSE) + coord_cartesian(ylim = c(0,20)) + ggtitle("Post-Intervention CO") + xlab(label = "") + ylab("CO (ppm)") + theme_bw()
df1 <- data.frame(a = c(1, 1,3, 3), b = c(18,19,19,18))
df2 <- data.frame(a = c(1.1,1.1,1.9,1.9), b = c(16,17,17,16))
df3 <- data.frame(a = c(2.1,2.1,2.9,2.9), b = c(13,14,14,13))
p2<-p2 + geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 2, y = 19.5, label = "***", size = 6) +
  geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 17.5, label = "**", size = 6) +
  geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 14.5, label = "***", size = 6)

grid.arrange(p1,p2, ncol = 2)
dev.off()


# calculate intra-individual change
library(tidyr)
good$pre_post <- ifelse(good$session == 1, "pre", "post")
change <- good %>% group_by(mstudyid, pre_post) %>% summarise(mean_co = mean(co_mean_corr),  arm2 = arm2[1])

wide <- spread(data = change, key = pre_post, value = mean_co)
wide$change <- wide$post - wide$pre
wide$percent_change <- ((wide$post - wide$pre)/wide$pre)*100

# intra-individual change descriptives
round(tapply(wide$change[complete.cases(wide)], wide$arm2[complete.cases(wide)], mean), digits = 2)
fit <- aov(change ~ arm2, data = wide)
summary(fit) # sig
t.test(change ~ arm2, data = wide[wide$arm2 %in% c("control", "lpg"),]) # ***
t.test(change ~ arm2, data = wide[wide$arm2 %in% c("control", "biolite"),]) # **
t.test(change ~ arm2, data = wide[wide$arm2 %in% c("lpg", "biolite"),]) #***

round(tapply(wide$percent_change[complete.cases(wide)], wide$arm2[complete.cases(wide)], mean), digits = 2)
fit <- aov(percent_change ~ arm2, data = wide)
summary(fit) # sig
t.test(percent_change ~ arm2, data = wide[wide$arm2 %in% c("control", "lpg"),]) # ***
t.test(percent_change ~ arm2, data = wide[wide$arm2 %in% c("control", "biolite"),]) # NS
t.test(percent_change ~ arm2, data = wide[wide$arm2 %in% c("lpg", "biolite"),]) #***

# Change Boxplots
# pre & post-intervention boxplots
# change in PPM
pdf(file = "CO_change_pre_post_ppm.pdf", width = 10, height = 8, bg = "white")
p1 <- ggplot(wide, aes(factor(arm2, levels = c("control", "biolite", "lpg")), change, fill = "red")) + geom_boxplot() + guides(fill = FALSE)  + coord_cartesian(ylim = c(-10,10)) + ggtitle("Change in personal CO Exposure") + xlab(label = "") + ylab("Change in CO after intervention (ppm)") + theme_bw()
df1 <- data.frame(a = c(1, 1,3, 3), b = c(9,10,10,9))
df2 <- data.frame(a = c(1.1,1.1,1.9,1.9), b = c(8,9,9,8))
df3 <- data.frame(a = c(2.1,2.1,2.9,2.9), b = c(7,8,8,7))
p1<-p1 + geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 2, y = 10.5, label = "***", size = 6) +
  geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 9.5, label = "**", size = 6) +
  geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 8.5, label = "***", size = 6)
print(p1)
dev.off()

# change in percent
pdf(file = "CO_change_pre_post_percent.pdf", width = 10, height = 8, bg = "white")
p2 <- ggplot(wide, aes(factor(arm2, levels = c("control", "biolite", "lpg")), percent_change, fill = "blue")) + geom_boxplot() + guides(fill = FALSE) + coord_cartesian(ylim = c(-200, 2500)) + ggtitle("Percent change in personal CO Exposure") + xlab(label = "") + ylab("Percent change in CO after intervention (ppm)") + theme_bw()
df1 <- data.frame(a = c(1, 1,3, 3), b = c(2400,2500,2500,2400))
df2 <- data.frame(a = c(1.1,1.1,1.9,1.9), b = c(2200,2300,2300,2200))
df3 <- data.frame(a = c(2.1,2.1,2.9,2.9), b = c(2000,2100,2100,2000))
p2<-p2 + geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 2, y = 2550, label = "***", size = 6) +
  geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 2350, label = "NS", size = 6) +
  geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 2150, label = "***", size = 6)

print(p2)
dev.off()

# beanplot of baseline CO exposure -----------------
library(beanplot)

pdf(file = "Baseline_CO_Beanplot_all.pdf", bg = "white")
beanplot(pre$co_mean_corr, ylab = "Mean CO (ppm)", col = c("grey", "darkgreen"), main = "Baseline CO Exposure (n = 973)")
dev.off()

pdf(file = "Baseline_CO_Beanplot_byarm.pdf", bg = "white")
beanplot(co_mean_corr ~ arm2, data = pre, ylab = "Mean CO (ppm)", col = c("grey", "darkgreen"), main = "Baseline CO Exposure by Arm (n = 973)")
dev.off()
