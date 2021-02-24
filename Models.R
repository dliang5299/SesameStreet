
# Libraries
library(tidyverse)
library(gridExtra)
library(corrplot)

# Loading data
ss_all <- readRDS("Data/ss_all.RDS") %>%
  mutate(viewcat = as.factor(viewcat))

head(ss_all)
  
# Fitting the model for original response variable perclet
fit.init.perclet <- glm(perclet~(sex*age^2*viewcat*setting*peabody*encour)^6, data=ss_all)
fit.final.perclet = step(fit.init.perclet, direction='backward')

# Fitting the model for new response variable perclet_max
fit.init.perclet.max <- glm(perclet_max~(sex*age^2*viewcat*setting*peabody*encour)^6, data=ss_all)
fit.final.perclet.max = step(fit.init.perclet.max, direction='backward')

# Comparing the two models
c(fit.final.perclet$aic, fit.final.perclet.max$aic)
