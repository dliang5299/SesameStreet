# Libraries
library(tidyverse)
library(gridExtra)
library(corrplot)
library(nortest)
library(car)
library(VIF)

# Loading data
ss_all <- readRDS("Data/ss_all.RDS") %>%
  mutate(viewcat = as.factor(viewcat),
         sex = as.factor(sex),
         setting = as.factor(setting),
         encour = as.factor(encour),
         site = as.factor(site)) 
  
# Max
fit.init.perclet.max_2 <- glm(perclet_max~(site+sex+age+viewcat+setting+peabody+encour)^2, data=ss_all)
fit.final.perclet.max_2 <- step(fit.init.perclet.max_2, direction='both')
summary(fit.final.perclet.max_2)

# Considering only up to three-way interactions

# Max
fit.init.perclet.max_3 <- glm(perclet_max~(site+sex+age+viewcat+setting+peabody+encour)^3, data=ss_all)
fit.final.perclet.max_3 <- step(fit.init.perclet.max_3, direction='both')
summary(fit.final.perclet.max_3)

# Three-way model has lower AIC, but not enough to justify the complicated model
c(fit.final.perclet.max_2$aic, fit.final.perclet.max_3$aic)

# save model
saveRDS(fit.final.perclet.max_2, file = 'lettermod.rds')

####################################################################################

# Check assumptions for perclet_max with two-way interaction

# Plots look good
par(mfrow = c(2, 2))
plot(fit.final.perclet.max_2)

# We may assume the data are approximately normal
ad.test(resid(fit.final.perclet.max_2))

# Histogram looks good
hist_perclet_model <- qplot(resid(fit.final.perclet.max_2),
            geom = "histogram",
            bins = 10) + labs(title = "Histogram of residuals",
       x = "residual")
hist_perclet_model

# Residuals vs. order plot looks good
par(mfrow = c(1, 1))
plot(ss_all$id, resid(fit.final.perclet.max_2), ylab="Residuals", xlab="id", main="Residuals vs order")
abline(0, 0)

# check VIFs; some predictors have VIF value of over 10; proceed with caution
car::vif(fit.final.perclet.max_2)

####################################################################################

# Model comparisons 
# Did not consider age and peabody since there are so many values for each

# Significant differences
aov_perclet <- aov(fit.final.perclet.max_2)
plot(TukeyHSD(x=aov_perclet, "viewcat", conf.level = 0.95))
plot(TukeyHSD(x=aov_perclet, "site", conf.level = 0.95))

####################################################################################
####################################################################################

# Considering only two-way interactions

# Max
fit.init.percnumb.max_2 <- glm(percnumb_max~(site+sex+age+viewcat+setting+peabody+encour)^2, data=ss_all)
fit.final.percnumb.max_2 <- step(fit.init.percnumb.max_2, direction='both')
summary(fit.final.percnumb.max_2)

# Considering only up to three-way interactions

# Max
fit.init.percnumb.max_3 <- glm(perclet_max~(site+sex+age+viewcat+setting+peabody+encour)^3, data=ss_all)
fit.final.percnumb.max_3 <- step(fit.init.perclet.max_3, direction='both')
summary(fit.final.percnumb.max_3)

# Lower AIC, but not enough to justify the complicated model
c(fit.final.percnumb.max_2$aic, fit.final.percnumb.max_3$aic)

# save model
saveRDS(fit.final.percnumb.max_2, file = 'numbmod.rds')

####################################################################################

# Check assumptions for percnumb_max with two-way interaction

# Plots look good enough
par(mfrow = c(2, 2))
plot(fit.final.percnumb.max_2)

# We may assume the data are approximately normal
ad.test(resid(fit.final.percnumb.max_2))

# Histogram looks good
hist_percnumb_model <- qplot(resid(fit.final.percnumb.max_2),
                            geom = "histogram",
                            bins = 10) + labs(title = "Histogram of residuals",
                                              x = "residual")
hist_percnumb_model

# Residuals vs. order plot looks good
par(mfrow = c(1, 1))
plot(ss_all$id, resid(fit.final.percnumb.max_2), ylab="Residuals", xlab="id", main="Residuals vs order")
abline(0, 0)

# check VIFs; one VIF value of over 10; proceed with caution
car::vif(fit.final.percnumb.max_2)

####################################################################################

# Model comparisons 
# Did not consider age and peabody since there are so many values for each

# Significant differences
aov_percnumb <- aov(fit.final.percnumb.max_2)
plot(TukeyHSD(x=aov_percnumb, "viewcat", conf.level = 0.95))
plot(TukeyHSD(x=aov_percnumb, "site", conf.level = 0.95))

####################################################################################
####################################################################################

# PERCBODY

# Considering only two-way interactions

fit.init.percbody.max_2 <- glm(percbody_max~(site+sex+age+viewcat+setting+peabody+encour)^2, data=ss_all)
fit.final.percbody.max_2 <- step(fit.init.percbody.max_2, direction='both')
summary(fit.final.percbody.max_2)

# Considering only up to three-way interactions

# Max
fit.init.percbody.max_3 <- glm(percbody_max~(site+sex+age+viewcat+setting+peabody+encour)^3, data=ss_all)
fit.final.percbody.max_3 <- step(fit.init.percbody.max_3, direction='both')
summary(fit.final.percbody.max_3)

# Slightly lower AIC, but not enough to justify the complicated model
c(fit.final.percbody.max_2$aic, fit.final.percbody.max_3$aic)

# save model
saveRDS(fit.final.percbody.max_2, file = 'bodymod.rds')

####################################################################################

# Check assumptions for percbody_max with two-way interaction

# Plots look slightly worse; check normality with Anderson-Darling to confirm the 
# data are approximately normal
par(mfrow = c(2, 2))
plot(fit.final.percbody.max_2)

# We may assume the data are approximately normal
ad.test(resid(fit.final.percbody.max_2))

# Histogram looks good
hist_percbody_model <- qplot(resid(fit.final.percbody.max_2),
                             geom = "histogram",
                             bins = 10) + labs(title = "Histogram of residuals",
                                               x = "residual")
hist_percbody_model

# Residuals vs. order plot looks good
par(mfrow = c(1, 1))
plot(ss_all$id, resid(fit.final.percbody.max_2), ylab="Residuals", xlab="id", main="Residuals vs order")
abline(0, 0)

# check VIFs; one VIF value of over 10; proceed with caution
car::vif(fit.final.percbody.max_2)

####################################################################################

# Model comparisons 
# Did not consider age and peabody since there are so many values for each

aov_percbody <- aov(fit.final.percbody.max_2)

# No significant differences
plot(TukeyHSD(x=aov_percbody, "viewcat", conf.level = 0.95))
plot(TukeyHSD(x=aov_percbody, "site", conf.level = 0.95))
plot(TukeyHSD(x=aov_percbody, "setting", conf.level = 0.95))