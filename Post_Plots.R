###########################################################
## Create general plots to understand DW project
## coverage - POST
###########################################################
library(ggplot2)
library(reshape2)
library(dplyr)
library(qcc)
# Load in the project duration data from disk
cycles_P2 <- readRDS(file="DW_data/Cycles_P2.rds")
# end
###########################################################
## Create scatter plots of cycles_P2 data
###########################################################
ggplot(cycles_P2) +
  aes(x=QTR, y=prc) +
  geom_point() +
  ggtitle("Percentage of Coverage - POST") +
  labs(x="Quarter", y="Percentage")

ggplot(cycles_P2) +
  aes(x=QTR, y=tot_projects) +
  geom_point(aes(size = total_proj_duration,color = prc)) +
  geom_jitter() +
  ggtitle("Total Projects by Cycle - POST") +
  labs(x="Quarter", y="Total # of Projects")

ggplot(cycles_P2) +
  aes(x=QTR, y=prc) +
  geom_point(aes(size = total_proj_duration,
  color = mean_proj_duration)) +
  geom_jitter() +
  ggtitle("Coverage by Cycle - POST") +
  labs(x="Quarter", y="Coverage")

ggplot(cycles_P2) +
  aes(x = QTR, y = total_proj_duration) +
  geom_point(aes(color = prc)) +
  scale_color_continuous_tableau()

ggplot(cycles_P2) +
  aes(x=mean_proj_duration, y=prc) +
  geom_point(aes(color = tot_projects))

ggplot(cycles_P2) +
  aes(x=tot_projects, y=prc) +
  geom_point(aes(color = mean_proj_duration)) +
  scale_color_continuous_tableau()