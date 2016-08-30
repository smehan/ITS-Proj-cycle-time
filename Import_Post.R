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

# assemble the main datafile
PDuration_P <- read.csv("DW_Data/DW_Demand_Mgmt_Post.csv", header=TRUE, sep = ",", stringsAsFactors = TRUE,
                      as.is = c("Summary"))

#rownames(PDuration) <- PDuration$Key
# end

# Convert empty values to NA
PDuration_P[PDuration_P == ""] <- NA
# end

# Convert dates to posix date objects
PDuration_P$Created <- mdy(PDuration_P$Create_Date)
PDuration_P$Resolved <- mdy(PDuration_P$Resolved_Date)
PDuration_P$Due.Date <- mdy(PDuration_P$Due_Date)
# end

###########################################################
### perform date calculations
###########################################################

# Year request created into year and month, with a numerical and text month
PDuration_P$year_created <- year(PDuration_P$Created)
PDuration_P$month_created <- month(PDuration_P$Created, label = TRUE)
PDuration_P$month_num_created <- month(PDuration_P$Created)
PDuration_P$year_resolved <- year(PDuration_P$Resolved)
PDuration_P$month_resolved <- month(PDuration_P$Resolved, label = TRUE)
PDuration_P$month_num_resolved <- month(PDuration_P$Resolved)
PDuration_P$year_due <- year(PDuration_P$Due.Date)
PDuration_P$month_due <- month(PDuration_P$Due.Date, label = TRUE)
PDuration_P$month_num_due <- month(PDuration_P$Due.Date)

# Create quarterly cycles for each project
# This will be used to see which customers had a project in each cycle and
# to calculate the coverage through each cycle (percentge of customers starting projects)

# First create the QTR - prepare to concatenate with Year to create cycle
# A list with month and year is passed to the function to create the 
# cycle (year concatenated with month)
determine_QTR <- function(data){
  month <- data[1]
  year <- data[2]
  if ((month >= 1) & (month <= 3)) {
    return(paste(year, "1", sep = "-"))
  } else if ((month >= 4) & (month <= 6)) {
    return(paste(year, "2", sep = "-"))
  } else if ((month >= 7) & (month <= 9)) {
    return(paste(year, "3", sep = "-"))
  } else if ((month >= 10) & (month <= 12)) {
    return(paste(year, "4", sep = "-"))
  }
}

# Using the determine_QTR function, iterate over every row and pull the month and year created
# to calculate the new QTR value.
PDuration_P$QTR <- apply(PDuration_P[,c('month_num_created', 'year_created')], 1,
                       function(x) determine_QTR(x))

# Calendar duration of project, cast as an integer
PDuration_P$project_duration <- as.integer(round((PDuration_P$Resolved - PDuration_P$Created), 3))
# end

###########################################################
# Add column for total number of cycles that a project ran
# Use 92 as the total days that equals one quarter
# Divide the total duration by 92 to get the total cycles
###########################################################
PDuration_P$tot_cycles <- as.integer(ceiling(PDuration_P$project_duration/92))

###########################################################
### Recast long data into a wide format that
### groups particpating departments by cycles
###########################################################

cycles_P <- dcast(PDuration_P, QTR ~ Dept)
cycles_P$HR <- as.integer(0)
cycles_P$SF <- as.integer(0)
cycles_P$UPD <- as.integer(0)

# calculate the mean project duration of each set of projects by cycle
tmp <- aggregate(PDuration_P[, c('project_duration')], list(PDuration_P$QTR), mean)
# outputs mean project_duration to $Group.1, so join tables
cycles_P <- left_join(cycles_P, tmp, by = c("QTR" = "Group.1"))
colnames(cycles_P)[17] <- "mean_proj_duration"
rm(tmp)

# calculate the total project duration for all projects by cycle
tmp2 <- aggregate(PDuration_P[, c('project_duration')], list(PDuration_P$QTR), sum)
# outputs total project_duration to $Group.1, so join tables
cycles_P <- left_join(cycles_P, tmp2, by = c("QTR" = "Group.1"))
colnames(cycles_P)[18] <- "total_proj_duration"
rm(tmp2)

##############################################################
# calculate the total number of projects by cycle
##############################################################
cycles_P$tot_projects <- rowSums(cycles_P[,2:16])

###########################################################
### Calculate % of coverage based on projects started by
###  dept by cycle
###########################################################
### sum the rows to prepare for % calculation
cycles_P2 <- cycles_P
cycles_P2$ADM <- unlist(lapply(cycles_P2[,2], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$ADV <- unlist(lapply(cycles_P2[,3], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$AP <- unlist(lapply(cycles_P2[,4], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$FA <- unlist(lapply(cycles_P2[,5], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$FAC <- unlist(lapply(cycles_P2[,6], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$FIN <- unlist(lapply(cycles_P2[,7], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$INT_CTR <- unlist(lapply(cycles_P2[,8], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$IR <- unlist(lapply(cycles_P2[,9], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$ITS <- unlist(lapply(cycles_P2[,10], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$OR <- unlist(lapply(cycles_P2[,11], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$SEC <- unlist(lapply(cycles_P2[,12], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$UA <- unlist(lapply(cycles_P2[,13], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$HR <- unlist(lapply(cycles_P2[14], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$SF <- unlist(lapply(cycles_P2[,15], function(s) { ifelse (s > 0, 1, 0)}))
cycles_P2$UPD <- unlist(lapply(cycles_P2[,16], function(s) { ifelse (s > 0, 1, 0)}))
#cycles2$SA <- unlist(lapply(cycles2[,9], function(s) { ifelse (s > 0, 1, 0)}))
#cycles2$DRC <- unlist(lapply(cycles2[,18], function(s) { ifelse (s > 0, 1, 0)}))

### Calculate percentage of coverage
cycles_P2$prc <- round((rowSums(cycles_P2[,2:16])/15),2)

########################################################
# create cycles3 which eliminates all of the dept info
########################################################
cycles_P3 <- data.frame(cycles_P2)
cycles_P3[2:16] <- NULL
cycles_P3 <- cycles_P3[-c(1,2),]

###########################################################
### save the data frame for use in other scripts
###########################################################
saveRDS(PDuration_P, "DW_data/Project_Duration_Post.rds")
saveRDS(cycles_P2, "DW_data/Cycles_P2.rds")
saveRDS(cycles_P3, "DW_data/Cycles_P3.rds")
###  create csv file for exporting
write.csv(PDuration_P, file = 'DW_data/PDurationP.csv')
write.csv(cycles_P, file = 'DW_data/Cycles_P.csv')
#####  End of csv creation

# end

