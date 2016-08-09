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
cycles3 <- readRDS(file="DW_data/Cycles3.rds")
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

###########################################################
## Create scatter plots of cycles2 data
###########################################################

ggplot(cycles2) +
  aes(x=QTR, y=prc) +
  geom_point() +
  ggtitle("Percentage of Coverage") +
  labs(x="Quarter", y="Percentage")
  
ggplot(cycles2) +
  aes(x=QTR, y=tot_projects) +
  geom_point(aes(size = total_proj_duration,
                 color = prc)) +
  geom_jitter() +
  ggtitle("Total Projects by Cycle") +
  labs(x="Quarter", y="Total # of Projects")

ggplot(cycles2) +
  aes(x=QTR, y=prc) +
  geom_point(aes(size = total_proj_duration,
                 color = mean_proj_duration)) +
  geom_jitter() +
  ggtitle("Coverage by Cycle") +
  labs(x="Quarter", y="Coverage")

ggplot(cycles2) +
  aes(x = QTR, y = total_proj_duration) +
  geom_point(aes(color = prc)) +
  scale_color_continuous_tableau()

##############################################################
# Create series of box plots from cycles2
##############################################################
boxplot(x = cycles2$total_proj_duration, notch = TRUE)
boxplot(prc ~ total_proj_duration, data = cycles2, notch = TRUE)
boxplot(total_proj_duration ~ mean_proj_duration, data = cycles2, notch = TRUE)
boxplot(total_proj_duration ~ tot_projects, data = cycles2, notch = TRUE)

###########################################################
## Create scatter plots of cycles3 data
###########################################################
ggplot(cycles3) +
  aes(x=QTR, y=prc) +
  geom_point() +
  ggtitle("Percentage of Coverage") +
  labs(x="Quarter", y="Percentage")

ggplot(cycles3) +
  aes(x=QTR, y=tot_projects) +
  geom_point(aes(size = total_proj_duration,
                 color = prc)) +
  geom_jitter() +
  ggtitle("Total Projects by Cycle") +
  labs(x="Quarter", y="Total # of Projects")

ggplot(cycles3) +
  aes(x=QTR, y=prc) +
  geom_point(aes(size = total_proj_duration,
                 color = mean_proj_duration)) +
  geom_jitter() +
  ggtitle("Coverage by Cycle") +
  labs(x="Quarter", y="Coverage")

### Project Duration by Cycle
ggplot(cycles3) +
  aes(x = QTR, y = total_proj_duration) +
  geom_point(aes(color = prc)) +
  scale_color_continuous_tableau() +
  ggtitle("Project Duration by Cycle") +
  labs(x="Quarterly Cycle", y="Total Project Duration")
  
##############################################################
# Create series of box plots from cycle3
##############################################################
boxplot(x = cycles3$total_proj_duration, notch = TRUE)
boxplot(prc ~ total_proj_duration, data = cycles3, notch = TRUE)
boxplot(total_proj_duration ~ mean_proj_duration, data = cycles3, notch = TRUE)
boxplot(total_proj_duration ~ tot_projects, data = cycles3, notch = TRUE)

