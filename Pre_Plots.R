###########################################################
## Create general plots to understand DW project 
## coverage
###########################################################
library(ggplot2)
library(reshape2)
library(dplyr)
library(qcc)

# Load in the project duration data from disk
cycles2 <- readRDS(file="DW_data/Cycles2.rds")
# end

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

ggplot(cycles2) +
  aes(x=mean_proj_duration, y=prc) +
  geom_point(aes(color = tot_projects))

ggplot(cycles2) +
  aes(x=tot_projects, y=prc) +
  geom_point(aes(color = mean_proj_duration)) +
  scale_color_continuous_tableau()

##############################################################
# Create series of box plots from cycles2
##############################################################
boxplot(x = cycles2$total_proj_duration, notch = TRUE)
boxplot(prc ~ total_proj_duration, data = cycles2, notch = TRUE)
boxplot(total_proj_duration ~ mean_proj_duration, data = cycles2, notch = TRUE)
boxplot(total_proj_duration ~ tot_projects, data = cycles2, notch = TRUE)
