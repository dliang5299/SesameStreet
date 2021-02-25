
# Libraries
library(tidyverse)
library(gridExtra)
library(corrplot)

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
fit.init.perclet.max_2 <- glm(perclet_max~(site+sex+age+viewcat+setting+peabody+encour)^3, data=ss_all)
fit.final.perclet.max_2 <- step(fit.init.perclet.max_2, direction='both')
summary(fit.final.perclet.max_2)
# Lower AIC, but not enough to justify the complicated model

# Plots look good
plot(fit.final.perclet.max)










