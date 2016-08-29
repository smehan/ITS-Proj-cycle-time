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
### 2-way anova
########################################################

model1a <- lm(prc ~ mean_proj_duration + total_proj_duration + tot_projects + 
                mean_proj_duration*total_proj_duration + mean_proj_duration*tot_projects + 
                total_proj_duration*tot_projects,
              cycles3_ed)


anova(model1a)









##########################################################
# Multiple Regression Model 1 with all predictor variables
##########################################################

model1 <- lm(prc ~ mean_proj_duration, cycles2)

summary(model1)
coef(model1)

library(coefplot)
coefplot(model1)

#######  End Model 1