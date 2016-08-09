###########################################################
## Perform general analysis to understand DW project 
## duration data
###########################################################
library(reshape2)
library(dplyr)
library(qcc)

# Load in the project duration data from disk
PDuration <- readRDS(file="DW_data/Project_Duration.rds")
cycles2 <- readRDS(file="DW_data/Cycles2.rds")
cycles3 <- readRDS(file="DW_data/Cycles3.rds")
# end

################################################################
#  create linear models
################################################################
mod1 <- lm(prc ~ mean_proj_duration + total_proj_duration + tot_projects,
       data=cycles3)
summary(mod1)

mod2 <- lm(prc ~ mean_proj_duration + total_proj_duration ,
           data=cycles3)
summary(mod2)
