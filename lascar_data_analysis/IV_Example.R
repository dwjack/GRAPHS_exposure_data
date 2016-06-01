library(foreign)
sesame <- read.dta("sesame.dta")

# Wald estimate
fit.1a <- lm(regular ~ encour, data=sesame)
fit.1b <- lm(postlet ~ encour, data=sesame)
iv.est <- coef(fit.1b)["encour"]/coef(fit.1a)["encour"]
iv.est

# Two-stage least squares 
fit.2a <- lm(regular ~ encour, data=sesame)
watched.hat.2a <- fit.2a$fitted
fit.2b <- lm(postlet ~ watched.hat.2a, data=sesame)
summary(fit.2b) 

# Adjusting for covariates
fit.3a <- lm(regular ~ encour + prelet + as.factor(site) + as.factor(setting), data=sesame)
watched.hat.3a <- fit.3a$fitted
fit.3b <- lm(postlet ~ watched.hat.3a + prelet + as.factor(site) + as.factor(setting), data=sesame)
summary(fit.3b)

# Standard errors
fit.3c <- lm(postlet ~ watched.hat.3a + prelet + as.factor(site) + as.factor(setting), x=TRUE, data=sesame)
x.adj <- fit.3c$x
x.adj[,"watched.hat.3a"] <- sesame$regular
residual.sd.adj <- sd(sesame$postlet - x.adj%*%coef(fit.3c))
se.adj <- summary(fit.3c)$coefficient[,2]["watched.hat.3a"]*residual.sd.adj/summary(fit.3c)$sigma

# Using 'tsls' from 'sem' package
library(sem)
iv1 <- tsls(postlet ~ regular, ~ encour, data=sesame)
summary(iv1)

iv2 <- tsls(postlet ~ regular + prelet + as.factor(site) + as.factor(setting),
            ~ encour + prelet + as.factor(site) + as.factor(setting), data=sesame)
summary(iv2)