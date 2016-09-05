###########################################################
## Perform general analysis to understand DW project 
## duration data
###########################################################
library(qcc)
library(ggplot2)
library(scales)
library(ggthemes)
library(plyr)
library(dplyr)
library(stringr)
library(reshape2)
library(lubridate)

# Load in the project duration data from disk
PDuration_P <- read.csv("DW_Data/Project_Duration_Post", header=TRUE, sep = ",", stringsAsFactors = TRUE,
                      as.is = c("Summary"))
cycles3P_ed <- readRDS(file="DW_data/cycles3P_ed.rds")
# end

ggplot(PDuration_P) +
  aes(x=QTR, y=project_duration) +
  geom_point(aes(color = Dept)) +
  geom_jitter() +
  ggtitle("POST - Data Warehouse Projects") +
  labs(x="Quarter", y="Project Duration")

#####################################################################
# Using qcc to get a simple Individuals Chart (IMR)
# Get the voice of the process (VOP)
nqP <- qcc(cycles3P_ed$prc, type="xbar.one", limits = c(22.91,50.29), nsigmas=3, title = "POST - Individuals Chart - % Coverage")

# Create a process capability analysis of the xbar.one
# Set the upper, lower, and target specifications based on VOC
process.capability(nqP, spec.limits = c(25,75), target = 50)
#####################################################################

