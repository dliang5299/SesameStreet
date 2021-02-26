# Libraries
library(tidyverse)
library(gridExtra)
library(corrplot)
library(nortest)
library(car)

# Loading data
ss_all <- readRDS("Data/ss_all.RDS") %>%
  mutate(viewcat = as.factor(viewcat),
         site = as.factor(site)) 
  

head(ss_all)
  
# Fitting the model for original response variable perclet
fit.init.perclet <- glm(perclet~(sex*age^2*viewcat*setting*peabody*encour)^6, data=ss_all)
fit.final.perclet = step(fit.init.perclet, direction='backward')

# Fitting the model for new response variable perclet_max
fit.init.perclet.max <- glm(perclet_max~(sex*age^2*viewcat*setting*peabody*encour)^6, data=ss_all)
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
fit.final.perclet.max_3 <- step(fit.init.perclet.max_2, direction='both')
summary(fit.final.perclet.max_3)
# Lower AIC, but not enough to justify the complicated model

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
sex = as.factor(fit.final.perclet.max_2$sex)
setting = as.factor(fit.final.perclet.max_2$setting)
encour = as.factor(fit.final.perclet.max_2$encour)
site = as.factor(fit.final.perclet.max_2$site)
viewcat = as.factor(fit.final.perclet.max_2$viewcat)
aov_perclet <- aov(fit.final.perclet_2)
viewcat = as.factor(ss_all$viewcat)
TukeyHSD(x=aov_perclet, 'ss_all$viewcat', conf.level = 0.95, which="viewcat")
TukeyHSD(aov_perclet, conf.level = 0.95, which=c("sex","site", "setting", "viewcat", "encour", "site:age", "sex:peabody", "viewcat:setting", "setting:encour", "peabody:encour"))
plot(TukeyHSD(aov_perclet, conf.level = 0.95, which=c("sex","site")))
