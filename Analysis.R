###########################################################
## Perform general analysis to understand DW project 
## duration data
###########################################################
library(reshape2)
library(dplyr)
library(qcc)

# Load in the project duration data from disk
PDuration <- readRDS(file="DW_data/Project_Duration.rds")
# end



# calculates the mean project duration grouped by Dept
aggregate(project_duration ~ Dept, data = PDuration, mean)

# save the result as an object and calculate the mean of means
pre_means <- aggregate(project_duration ~ Dept, data = PDuration, mean)
mean(pre_means$project_duration)