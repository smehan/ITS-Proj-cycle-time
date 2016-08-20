###########################################################
## Create general plots to understand DW project 
## coverage
###########################################################
library(ggplot2)
library(reshape2)
library(dplyr)
library(qcc)

# Load in the project duration data from disk
Cycles_Edited <- readRDS(file="DW_data/Cycles_Edited.rds")
cycles3_ed <- readRDS(file="DW_data/Cycles3_ed.rds")
# end

###########################################################
## Create scatter plots of cycles_edited data
###########################################################

ggplot(Cycles_Edited) +
  aes(x=QTR, y=prc) +
  geom_point() +
  ggtitle("Percentage of Coverage") +
  labs(x="Quarter", y="Percentage")

ggplot(Cycles_Edited) +
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

ggplot(Cycles_Edited) +
  aes(x = QTR, y = total_proj_duration) +
  geom_point(aes(color = prc)) +
  scale_color_continuous_tableau()

ggplot(Cycles_Edited) +
  aes(x=mean_proj_duration, y=prc) +
  geom_point(aes(color = tot_projects))

ggplot(Cycles_Edited) +
  aes(x=tot_projects, y=prc) +
  geom_point(aes(color = mean_proj_duration)) +
  scale_color_continuous_tableau()

ggplot(cycles3_ed) +
  aes(x=QTR, y=Dept) +
  geom_point(aes(size = total_proj_duration)) + 
  geom_jitter() +
  ggtitle("Projects by Department & Quarter") + 
  labs(x="Quarter", y="Department")



##############################################################
# Create series of box plots from cycles2
##############################################################
boxplot(x = Cycles_Edited$total_proj_duration, notch = TRUE)
boxplot(prc ~ total_proj_duration, data = Cycles_Edited, notch = TRUE)
boxplot(total_proj_duration ~ mean_proj_duration, data = Cycles_Edited, notch = TRUE)
boxplot(total_proj_duration ~ tot_projects, data = Cycles_Edited, notch = TRUE)

###########################################################
## Create scatter plots of cycles3 data
###########################################################
ggplot(cycles3_ed) +
  aes(x=QTR, y=prc) +
  geom_point() +
  ggtitle("Percentage of Coverage") +
  labs(x="Quarter", y="Percentage")

ggplot(cycles3_ed) +
  aes(x=QTR, y=tot_projects) +
  geom_point(aes(size = total_proj_duration,
                 color = prc)) +
  geom_jitter() +
  ggtitle("Total Projects by Cycle") +
  labs(x="Quarter", y="Total # of Projects")

ggplot(cycles3_ed) +
  aes(x=QTR, y=prc) +
  geom_point(aes(size = total_proj_duration,
                 color = mean_proj_duration)) +
  geom_jitter() +
  ggtitle("Coverage by Cycle") +
  labs(x="Quarter", y="Coverage")

### Project Duration by Cycle
ggplot(cycles3_ed) +
  aes(x = QTR, y = total_proj_duration) +
  geom_point(aes(color = prc)) +
  scale_color_continuous_tableau() +
  ggtitle("Project Duration by Cycle") +
  labs(x="Quarterly Cycle", y="Total Project Duration")

##############################################################
# Create series of box plots from cycle3
##############################################################
boxplot(x = cycles3_ed$total_proj_duration, notch = TRUE)
boxplot(prc ~ total_proj_duration, data = cycles3_ed, notch = TRUE)
boxplot(total_proj_duration ~ mean_proj_duration, data = cycles3_ed, notch = TRUE)
boxplot(total_proj_duration ~ tot_projects, data = cycles3_ed, notch = TRUE)

