###########################################################
## POST - Create general POST plots to understand DW project 
## coverage - cycles edited to show # projects
## in progress during each cycle (not only started)
###########################################################
library(ggplot2)
library(reshape2)
library(dplyr)
library(qcc)

# Load in the project duration data from disk
PDuration_P <- readRDS(file="DW_data/Project_Duration_Post.rds")
cycles3P_ed <- readRDS(file="DW_data/Cycles3P_ed.rds")
# end

###########################################################
## Create scatter plots of cycles3P edited data -
## Projects in progress
###########################################################

ggplot(cycles3P_ed) +
  aes(x=seq_along(QTR), y=prc) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("POST - Percentage of Coverage by Cycle - Projects In-Progress") +
  labs(x="Quarter", y="% of Coverage")

ggplot(cycles3P_ed) +
  aes(x=tot_projects, y=prc) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("POST - % of Coverage by Total Projects In-Progress") +
  labs(x="Total Projects In-Progress", y="% of Coverage")

ggplot(cycles3P_ed) +
  aes(x=seq_along(QTR), y=prc) +
  geom_point(aes(size = total_proj_duration,
                 color = mean_proj_duration)) +
  geom_jitter() +
  ggtitle("POST - Coverage by Cycle - Projects In-Progress") +
  labs(x="Quarter", y="Coverage")

