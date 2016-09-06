###########################################################
## Compare projects pre and post 
##  using cycles3 data
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
library(xts)

# Load in the project duration data from disk
cycles3_ed <- readRDS(file="DW_data/Cycles3_ed.rds")
cycles3P_ed <- readRDS(file="DW_data/Cycles3P_ed.rds")
# end

# rename columns in POST
#cycles3P_ed <- rename(cycles3P_ed, c("QTR"="P_QTR", "mean_proj_duration"="P_mean_proj_duration",
#          "total_proj_duration"="P_total_proj_duration", "tot_projects"="P_tot_projects",
#          "prc"="P_prc"))

###  combine pre and post data frames
cycles_edited <- rbind(cycles3_ed, cycles3P_ed)

### Plot both the pre and post data - Percentage of Coverage
ggplot(cycles_edited) +
  aes(x=QTR, y=prc) +
  geom_point() +
##  geom_jitter() +
  ggtitle("Data Warehouse Projects") +
  labs(x="Quarter", y="Percentage of Coverage")

### Plot both the pre and post data - Total Projects
ggplot(cycles_edited) +
  aes(x=QTR, y=tot_projects) +
  geom_point() +
  ##  geom_jitter() +
  ggtitle("Data Warehouse Projects") +
  labs(x="Quarter", y="Total Projects")

# create a df of descriptive stats for presentation
prc_means <- c(mean(cycles3_ed$prc), mean(cycles3P_ed$prc))
tot_means <- c(mean(cycles3_ed$tot_projects), mean(cycles3P_ed$tot_projects))
dur_means <- c(mean(cycles3_ed$total_proj_duration), mean(cycles3P_ed$total_proj_duration))
tot_proj <- 
descr_df <- data.frame(prc_means, tot_means, dur_means)

ggplot(descr_df) +
  aes(x=seq_along(prc_means), y=prc_means) +
  geom_point(shape=2) +
  scale_x_continuous(breaks=c(1,2)) +
  geom_point(aes(x=seq_along(tot_means), y=tot_means))
