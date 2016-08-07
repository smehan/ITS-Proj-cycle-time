###########################################################
## Create general plots to understand DW project 
## duration data
###########################################################
library(ggplot2)
library(reshape2)
library(dplyr)
library(qcc)

# Load in the project duration data from disk
PDuration <- readRDS(file="DW_data/Project_Duration.rds")
# end

###########################################################
## Create histogram of project duration data
###########################################################
ggplot(PDuration) +
  aes(PDuration$project_duration) +
  geom_histogram(fill="darkblue") +
  ggtitle("Histogram Plot - Project Duration") +
  labs(x="Duration", y="Count")

###########################################################
## Create scatter plot of project duration data
###########################################################

ggplot(PDuration) +
  aes(x=Key, y=project_duration) +
  geom_point() +
  ggtitle("Project Durations Histogram for all projects") + 
  labs(x="Project", y="Days")

ggplot(PDuration) +
  aes(x=Created, y=project_duration) +
  geom_point(aes(color=Dept)) +
  ggtitle("Project Duration by Project Creation Date") + 
  labs(x="Creation Date", y="Days")

ggplot(PDuration) +
  aes(x=Created, y=project_duration) +
  geom_point() + 
  facet_wrap(~Dept) +
  ggtitle("Project Duration by Project Creation Date") + 
  labs(x="Creation Date", y="Days")



