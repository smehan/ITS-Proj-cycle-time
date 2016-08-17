###########################################################
### Importing and cleansing DW project duration data
###########################################################
library(ggplot2)
library(scales)
library(ggthemes)
library(plyr)
library(dplyr)
library(stringr)
library(reshape2)
library(lubridate)

# Load in the project duration data from disk
CyclesEdited <- read.csv("DW_Data/cycles_edited.csv", header=TRUE, sep = ",", stringsAsFactors = TRUE)
CyclesEdited <- CyclesEdited[,2:21]
# end

### set rows to 0 or 1 to prepare for calculations
CyclesEdited$ADM <- unlist(lapply(CyclesEdited[,2], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$DRC <- unlist(lapply(CyclesEdited[,3], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$FA <- unlist(lapply(CyclesEdited[,4], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$FAC <- unlist(lapply(CyclesEdited[,5], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$FIN <- unlist(lapply(CyclesEdited[,6], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$HR <- unlist(lapply(CyclesEdited[,7], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$ITS <- unlist(lapply(CyclesEdited[,8], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$OR <- unlist(lapply(CyclesEdited[,9], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$SA <- unlist(lapply(CyclesEdited[,10], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$SF <- unlist(lapply(CyclesEdited[,11], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$UA <- unlist(lapply(CyclesEdited[,12], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$UPD <- unlist(lapply(CyclesEdited[,13], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$AP <- unlist(lapply(CyclesEdited[,14], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$Advising <- unlist(lapply(CyclesEdited[,15], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$IR <- unlist(lapply(CyclesEdited[,16], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$Int_Ctr <- unlist(lapply(CyclesEdited[,17], function(s) { ifelse (s > 0, 1, 0)}))
CyclesEdited$Security <- unlist(lapply(CyclesEdited[,18], function(s) { ifelse (s > 0, 1, 0)}))

###########################################################
### Calculate total projects and % of coverage 
###########################################################

### Calculate percentage of coverage
CyclesEdited$tot_projects <- rowSums(CyclesEdited[,2:18])

### Calculate percentage of coverage
CyclesEdited$prc <- rowSums(CyclesEdited[,2:18])/17*100

########################################################
# create cycles3 which eliminates all of the dept info
########################################################
#cycles3_ed <- data.frame(CyclesEdited)
#cycles3_ed[3:19] <- NULL
#cycles3_ed <- cycles3[-c(1,2),]

cycles3_ed<- CyclesEdited[,c(1,19:22)]

###########################################################
### save the data frame for use in other scripts
###########################################################
saveRDS(CyclesEdited, "DW_data/Cycles_Edited.rds")
saveRDS(cycles3_ed, "DW_data/Cycles3_ed.rds")
# end

