###########################################################
## Create general plots to understand DW project 
## coverage
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
## Create scatter plots of cycles2 data
###########################################################

ggplot(PDuration) +
  aes(x=QTR, y=project_duration) +
  geom_point(aes(color = Dept)) +
  geom_jitter() +
  ggtitle("Data Warehouse Projects") +
  labs(x="Quarter", y="Project Duration")

ggplot(cycles2) +
  aes(x=seq_along(QTR), y=prc) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Percentage of Coverage by Cycle") +
  labs(x="Quarter", y="% of Coverage")

###########################################################
## Experiment with creating scatter plots of cycles2 data
###########################################################

ggplot(cycles2) +
  aes(x=seq_along(QTR), y=prc) +
  geom_point(aes(size = total_proj_duration,
                 color = mean_proj_duration)) +
  geom_jitter() +
  ggtitle("Coverage by Cycle") +
  labs(x="Quarter", y="Coverage")

ggplot(cycles2) +
  aes(x = QTR, y = total_proj_duration) +
  geom_point(aes(color = prc)) +
  scale_color_continuous_tableau()

ggplot(cycles2) +
  aes(x=mean_proj_duration, y=prc) +
  geom_point(aes(color = tot_projects))

ggplot(cycles2) +
  aes(x=tot_projects, y=prc) +
  geom_point(aes(color = mean_proj_duration)) +
  scale_color_continuous_tableau()

##############################################################
# Experiment with creating series of box plots from cycles2
##############################################################
boxplot(x = cycles2$total_proj_duration, notch = TRUE)
boxplot(prc ~ total_proj_duration, data = cycles2, notch = TRUE)
boxplot(total_proj_duration ~ mean_proj_duration, data = cycles2, notch = TRUE)
boxplot(total_proj_duration ~ tot_projects, data = cycles2, notch = TRUE)

