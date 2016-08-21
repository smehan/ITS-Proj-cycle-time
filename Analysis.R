###########################################################
## Perform general analysis to understand DW project 
## duration data
###########################################################
library(reshape2)
library(dplyr)
library(qcc)

# Load in the project duration data from disk
PDuration_P <- readRDS(file="DW_data/Project_Duration_Post.rds")
cycles2 <- readRDS(file="DW_data/Cycles2.rds")
cycles_P2 <- readRDS(file="DW_data/Cycles_P2.rds")
# end

################################################################
#  create linear models
################################################################
mod1 <- lm(mean_proj_duration ~ seq_along(QTR), data=cycles2)
summary(mod1)

mod2 <- lm(mean_proj_duration ~ seq_along(QTR), data=cycles_P2)
summary(mod2)

mod3 <- lm(prc ~ seq_along(QTR), data=cycles2)
summary(mod3)

mod4 <- lm(prc ~ seq_along(QTR), data=cycles_P2)
summary(mod4)

mod5 <- lm(prc ~ tot_projects, data=cycles2)
summary(mod5)

mod6 <- lm(prc ~ tot_projects, data=cycles_P2)
summary(mod6)

mod7 <- lm(prc ~ seq_along(QTR) + mean_proj_duration + tot_projects, data=cycles2)
summary(mod7)

mod8 <- lm(prc ~ seq_along(QTR)+ mean_proj_duration + tot_projects, data=cycles_P2)
summary(mod8)
#####################################################################

#####################################################################
# Using qcc to get a simple Individuals Chart
# Get the vector of observations in which are interested

# Create an ImR object for process capability calculation.
nq <- qcc(cycles2$prc, type="xbar.one", nsigmas=3)

# Create an ImR object for process capability calculation.
nq <- qcc(cycles_P2$prc, type="xbar.one", nsigmas=3)
#####################################################################

##########################################################
# Multiple Regression Model 1 with all predictor variables
##########################################################

model1 <- lm(prc ~ mean_proj_duration, cycles2)

summary(model1)
coef(model1)

library(coefplot)
coefplot(model1)

#######  End Model 1