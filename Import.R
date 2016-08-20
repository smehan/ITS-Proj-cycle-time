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
PDuration <- read.csv("DW_Data/DW_Demand_Mgmt.csv", header=TRUE, sep = ",", stringsAsFactors = TRUE,
                    as.is = c("Summary"))
#rownames(PDuration) <- PDuration$Key
# end

# Convert empty values to NA
PDuration[PDuration == ""] <- NA
# end

# Convert dates to posix date objects
PDuration$Created <- mdy(PDuration$Create_Date)
PDuration$Resolved <- mdy(PDuration$Resolved_Date)
PDuration$Due.Date <- mdy(PDuration$Due_Date)
# end

###########################################################
### perform date calculations
###########################################################

# Year request created into year and month, with a numerical and text month
PDuration$year_created <- year(PDuration$Created)
PDuration$month_created <- month(PDuration$Created, label = TRUE)
PDuration$month_num_created <- month(PDuration$Created)
PDuration$year_resolved <- year(PDuration$Resolved)
PDuration$month_resolved <- month(PDuration$Resolved, label = TRUE)
PDuration$month_num_resolved <- month(PDuration$Resolved)
PDuration$year_due <- year(PDuration$Due.Date)
PDuration$month_due <- month(PDuration$Due.Date, label = TRUE)
PDuration$month_num_due <- month(PDuration$Due.Date)

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
PDuration$QTR <- apply(PDuration[,c('month_num_created', 'year_created')], 1,
                               function(x) determine_QTR(x))
      
# Calendar duration of project, cast as an integer
PDuration$project_duration <- as.integer(round((PDuration$Resolved - PDuration$Created), 3))
# end

###########################################################
# Add column for total number of cycles that a project ran
# Use 92 as the total days that equals one quarter
# Divide the total duration by 92 to get the total cycles
###########################################################
PDuration$tot_cycles <- as.integer(ceiling(PDuration$project_duration/92))

###########################################################
### Recast long data into a wide format that
### groups particpating departments by cycles
###########################################################

cycles <- dcast(PDuration, QTR ~ Dept)
cycles$AP <- 0
cycles$ADV <- 0
cycles$IR <- 0
cycles$INT_CTR <- 0
cycles$SEC <- 0

# calculate the mean project duration of each set of projects by cycle
tmp <- aggregate(PDuration[, c('project_duration')], list(PDuration$QTR), mean)
# outputs mean project_duration to $Group.1, so join tables
cycles <- left_join(cycles, tmp, by = c("QTR" = "Group.1"))
colnames(cycles)[18] <- "mean_proj_duration"
rm(tmp)

# calculate the total project duration for all projects by cycle
tmp2 <- aggregate(PDuration[, c('project_duration')], list(PDuration$QTR), sum)
# outputs total project_duration to $Group.1, so join tables
cycles <- left_join(cycles, tmp2, by = c("QTR" = "Group.1"))
colnames(cycles)[19] <- "total_proj_duration"
rm(tmp2)

##############################################################
# calculate the total number of projects by cycle
##############################################################
cycles$tot_projects <- rowSums(cycles[,2:17])

###########################################################
### Calculate % of coverage based on projects started by
###  dept by cycle
###########################################################
### sum the rows to prepare for % calculation
cycles2 <- cycles
cycles2$ADM <- unlist(lapply(cycles2[,2], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$DRC <- unlist(lapply(cycles2[,3], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$FA <- unlist(lapply(cycles2[,4], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$FAC <- unlist(lapply(cycles2[,5], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$FIN <- unlist(lapply(cycles2[,6], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$HR <- unlist(lapply(cycles2[,7], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$ITS <- unlist(lapply(cycles2[,8], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$OR <- unlist(lapply(cycles2[,9], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$SA <- unlist(lapply(cycles2[,10], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$SF <- unlist(lapply(cycles2[,11], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$UA <- unlist(lapply(cycles2[,12], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$UPD <- unlist(lapply(cycles2[,13], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$AP <- unlist(lapply(cycles2[,14], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$ADV <- unlist(lapply(cycles2[,15], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$IR <- unlist(lapply(cycles2[,16], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$INT_CTR <- unlist(lapply(cycles2[,17], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$SEC <- unlist(lapply(cycles2[,18], function(s) { ifelse (s > 0, 1, 0)}))



### Calculate percentage of coverage
cycles2$prc <- rowSums(cycles2[,2:18])/17*100

########################################################
# create cycles3 which eliminates all of the dept info
########################################################
cycles3 <- data.frame(cycles2)
cycles3[2:18] <- NULL
cycles3 <- cycles3[-c(1,2),]

###########################################################
### save the data frame for use in other scripts
###########################################################
saveRDS(PDuration, "DW_data/Project_Duration.rds")
saveRDS(cycles2, "DW_data/Cycles2.rds")
saveRDS(cycles3, "DW_data/Cycles3.rds")
# end

