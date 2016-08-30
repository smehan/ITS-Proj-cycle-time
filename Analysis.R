###########################################################
## Perform general analysis to understand DW project 
## duration data
###########################################################
library(reshape2)
library(dplyr)
library(qcc)

# Load in the project duration data from disk
PDuration_P <- readRDS(file="DW_data/Project_Duration_Post.rds")
cycles3_ed <- readRDS(file="DW_data/Cycles3_ed.rds")
# end

#####################################################################
# Using qcc to get a simple Individuals Chart (IMR)
# Get the voice of the process (VOP)
nq <- qcc(cycles3_ed$prc, type="xbar.one", nsigmas=3, title = "Individuals Chart - % Coverage")

# Create a process capability analysis of the xbar.one
# Set the upper, lower, and target specifications based on VOC
process.capability(nq, spec.limits = c(25,75), target = 50)
#####################################################################

################################################################
#  create linear models for cycles3_ed
################################################################
mod1 <- lm(prc ~ mean_proj_duration, data=cycles3_ed)
summary(mod1)

mod3 <- lm(prc ~ seq_along(QTR), data=cycles3_ed)
summary(mod3)

mod5 <- lm(prc ~ tot_projects, data=cycles3_ed)
summary(mod5)

mod7 <- lm(prc ~ seq_along(QTR)+ mean_proj_duration + tot_projects, data=cycles3_ed)
summary(mod7)
#####################################################################

#########################################################
### Compare the mean to a standard - T-test
### Ho: Mean of Percentage of Coverage = 50%
### H1: Mean of Percentage of Coverage <> 50%
### Significance Level = .05
########################################################

t.test (cycles3_ed$prc, mu=50)
