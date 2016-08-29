###########################################################
## Create general plots to understand DW project 
## coverage - cycles edited to show # projects
## in progress during each cycle (not only started)
###########################################################
library(ggplot2)
library(reshape2)
library(dplyr)
library(qcc)

# Load in the project duration data from disk
PDuration <- readRDS(file="DW_data/Project_Duration.rds")
cycles3_ed <- readRDS(file="DW_data/Cycles3_ed.rds")
# end

###########################################################
## Create scatter plots of cycles3 edited data -
## Projects in progress
###########################################################

ggplot(PDuration) +
  aes(x=QTR, y=project_duration) +
  geom_point(aes(color = Dept)) +
  geom_jitter() +
  ggtitle("Data Warehouse Projects Started by Quarter") +
  labs(x="Quarter", y="Project Duration")

ggplot(cycles3_ed) +
  aes(x=seq_along(QTR), y=prc) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Percentage of Coverage by Cycle - Projects In-Progress") +
  labs(x="Quarter", y="% of Coverage")

ggplot(cycles3_ed) +
  aes(x=tot_projects, y=prc) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("% of Coverage by Total Projects In-Progress") +
  labs(x="Total Projects In-Progress", y="% of Coverage")

ggplot(cycles3_ed) +
  aes(x=seq_along(QTR), y=prc) +
  geom_point(aes(size = total_proj_duration,
                 color = mean_proj_duration)) +
  geom_jitter() +
  ggtitle("Coverage by Cycle - Projects In-Progress") +
  labs(x="Quarter", y="Coverage")

