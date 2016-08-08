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
cycles2 <- readRDS(file="DW_data/Cycles2.rds")
# end

###########################################################
## Create scatter plots of project duration data
###########################################################

ggplot(PDuration) +
  aes(x=Key, y=project_duration) +
  geom_point() +
  ggtitle("Project Durations for All Projects") + 
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

ggplot(PDuration) +
  aes(x=QTR, y=Dept) +
  geom_point(aes(size = project_duration)) + 
  geom_jitter() +
  ggtitle("Projects by Department & Quarter") + 
  labs(x="Quarter", y="Department")

ggplot(cycles2) +
  aes(x=QTR, y=prc) +
  geom_point()

#Count number of projects by Dept and QTR
Proj_by_Dept <- tally(group_by(PDuration, Dept))
Proj_by_Cycle <- tally(group_by(PDuration,QTR))

