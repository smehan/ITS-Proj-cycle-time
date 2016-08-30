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
Cycles_P_Ed <- read.csv("DW_Data/cyclesP_ed.csv", header=TRUE, sep = ",", stringsAsFactors = TRUE)
Cycles_P_Ed <- Cycles_P_Ed[1:12,]
# end

### set rows to 0 or 1 to prepare for calculations
Cycles_P_Ed$ADM <- unlist(lapply(Cycles_P_Ed[,2], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$ADV <- unlist(lapply(Cycles_P_Ed[,3], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$AP <- unlist(lapply(Cycles_P_Ed[,4], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$FA <- unlist(lapply(Cycles_P_Ed[,5], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$FAC <- unlist(lapply(Cycles_P_Ed[,6], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$FIN <- unlist(lapply(Cycles_P_Ed[,7], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$INT_CTR <- unlist(lapply(Cycles_P_Ed[,8], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$IR <- unlist(lapply(Cycles_P_Ed[,9], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$ITS <- unlist(lapply(Cycles_P_Ed[,10], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$OR <- unlist(lapply(Cycles_P_Ed[,11], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$SEC <- unlist(lapply(Cycles_P_Ed[,12], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$UA <- unlist(lapply(Cycles_P_Ed[,13], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$HR <- unlist(lapply(Cycles_P_Ed[,14], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$SF <- unlist(lapply(Cycles_P_Ed[,15], function(s) { ifelse (s > 0, 1, 0)}))
Cycles_P_Ed$UPD <- unlist(lapply(Cycles_P_Ed[,16], function(s) { ifelse (s > 0, 1, 0)}))

###########################################################
### Calculate total projects and % of coverage 
###########################################################

### Calculate percentage of coverage
Cycles_P_Ed$tot_projects <- rowSums(Cycles_P_Ed[,2:16])
### Calculate percentage of coverage
Cycles_P_Ed$prc <- round((rowSums(Cycles_P_Ed[,2:16])/15),2)

########################################################
# create cycles3 which eliminates all of the dept info
########################################################
cycles3P_ed <- data.frame(Cycles_P_Ed)
cycles3P_ed[2:16] <- NULL
cycles3P_ed <- cycles3P_ed[-c(1,2),]

###########################################################
### save the data frame for use in other scripts
###########################################################
saveRDS(Cycles_P_Ed, "DW_data/Cycles_P_Ed.rds")
saveRDS(cycles3P_ed, "DW_data/Cycles3P_ed.rds")
# end
