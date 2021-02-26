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
  

head(ss_all)
  
####################################################################################
####################################################################################

# PERCLET

# Fitting the model for original response variable perclet
fit.init.perclet <- glm(perclet~(sex*age*viewcat*setting*peabody*encour)^6, data=ss_all)
fit.final.perclet = step(fit.init.perclet, direction='backward')

# Fitting the model for new response variable perclet_max
fit.init.perclet.max <- glm(perclet_max~(sex*age*viewcat*setting*peabody*encour)^6, data=ss_all)
fit.final.perclet.max = step(fit.init.perclet.max, direction='backward')

# Comparing the two models
c(fit.final.perclet$aic, fit.final.perclet.max$aic)

####################################################################################

# Considering only two-way interactions

# Fitting the model for original response variable perclet
fit.init.perclet_2 <- glm(perclet~(site+sex+age+viewcat+setting+peabody+encour)^2, data=ss_all)
fit.final.perclet_2 <- step(fit.init.perclet_2, direction='both')
summary(fit.final.perclet_2)
plot(fit.final.perclet_2)

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

####################################################################################

# Check assumptions for perclet_max with two-way interaction

# Plots look good
plot(fit.final.perclet.max_2)

perclet_model <- lm(perclet_max~site + sex + age + viewcat + setting + peabody + 
                   encour + site:age + sex:peabody + age:viewcat + age:peabody + 
                   viewcat:setting + setting:encour + peabody:encour, data=ss_all)

# We may assume the data are approximately normal
ad.test(resid(perclet_model))

# Histogram looks good
hist_perclet_model <- qplot(resid(perclet_model),
            geom = "histogram",
            bins = 10) + labs(title = "Histogram of residuals",
       x = "residual")
hist_perclet_model

# Residuals vs. order plot looks good
plot(ss_all$id, resid(perclet_model), ylab="Residuals", xlab="id", main="Residuals vs order")
abline(0, 0)

# check VIFs; some predictors have VIF value of over 10; proceed with caution
car::vif(perclet_model)

####################################################################################

# Model comparisons 
# Did not consider age and peabody since there are so many values for each

# Significant differences
aov_perclet <- aov(fit.final.perclet.max_2)
plot(TukeyHSD(x=aov_perclet, "viewcat", conf.level = 0.95))
plot(TukeyHSD(x=aov_perclet, "site", conf.level = 0.95))

# No significant differences
plot(TukeyHSD(x=aov_perclet, "sex", conf.level = 0.95))
plot(TukeyHSD(x=aov_perclet, "setting", conf.level = 0.95))
plot(TukeyHSD(x=aov_perclet, "encour", conf.level = 0.95))

####################################################################################
####################################################################################

# PERCNUMB

# Fitting the model for original response variable perclet
fit.init.percnumb <- glm(percnumb~(sex*age*viewcat*setting*peabody*encour)^6, data=ss_all)
fit.final.percnumb = step(fit.init.percnumb, direction='backward')

# Fitting the model for new response variable perclet_max
fit.init.percnumb.max <- glm(percnumb_max~(sex*age*viewcat*setting*peabody*encour)^6, data=ss_all)
fit.final.percnumb.max = step(fit.init.percnumb.max, direction='backward')

# Comparing the two models
c(fit.final.percnumb$aic, fit.final.percnumb.max$aic)

####################################################################################

# Considering only two-way interactions

# Fitting the model for original response variable perclet
fit.init.percnumb_2 <- glm(percnumb~(site+sex+age+viewcat+setting+peabody+encour)^2, data=ss_all)
fit.final.percnumb_2 <- step(fit.init.percnumb_2, direction='both')
summary(fit.final.percnumb_2)
plot(fit.final.percnumb_2)

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

####################################################################################

# Check assumptions for percnumb_max with two-way interaction

# Plots look good
plot(fit.final.percnumb.max_2)

percnumb_model <- lm(percnumb_max~site + sex + age + viewcat + setting + 
                       peabody + site:peabody + sex:viewcat + age:setting + setting:peabody, data=ss_all)

# We may assume the data are approximately normal
ad.test(resid(percnumb_model))

# Histogram looks good
hist_percnumb_model <- qplot(resid(percnumb_model),
                            geom = "histogram",
                            bins = 10) + labs(title = "Histogram of residuals",
                                              x = "residual")
hist_percnumb_model

# Residuals vs. order plot looks good
plot(ss_all$id, resid(percnumb_model), ylab="Residuals", xlab="id", main="Residuals vs order")
abline(0, 0)

# check VIFs; one VIF value of over 10; proceed with caution
car::vif(percnumb_model)

####################################################################################

# Model comparisons 
# Did not consider age and peabody since there are so many values for each

# Significant differences
aov_percnumb <- aov(fit.final.percnumb.max_2)
plot(TukeyHSD(x=aov_percnumb, "viewcat", conf.level = 0.95))
plot(TukeyHSD(x=aov_percnumb, "site", conf.level = 0.95))

# No significant differences
plot(TukeyHSD(x=aov_perclet, "sex", conf.level = 0.95))
plot(TukeyHSD(x=aov_perclet, "setting", conf.level = 0.95))

####################################################################################
####################################################################################