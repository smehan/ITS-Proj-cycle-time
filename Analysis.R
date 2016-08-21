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
