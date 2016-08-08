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
PDuration <- read.csv("DW_Data/DW_Projects.csv", header=TRUE, sep = ",", stringsAsFactors = TRUE,
                    as.is = c("Summary"))
rownames(PDuration) <- PDuration$Key
# end

# Convert empty values to NA
PDuration[PDuration == ""] <- NA
# end

# Convert dates to posix date objects
PDuration$Created <- mdy(PDuration$Created)
PDuration$Resolved <- mdy(PDuration$Resolved)
PDuration$Due.Date <- mdy(PDuration$Due.Date)
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
### Recast long data into a wide format that
### groups particpating departments by cycles
###########################################################

cycles <- dcast(PDuration, QTR ~ Dept)

# calculate the mean project duration of each set of projects by cycle
tmp <- aggregate(PDuration[, c('project_duration')], list(PDuration$QTR), mean)

# outputs mean project_duration to $Group.1, so join tables
cycles <- left_join(cycles, tmp, by = c("QTR" = "Group.1"))
colnames(cycles)[13] <- "mean_proj_duration"
rm(tmp)

###########################################################
### Calculate % of coverage based on projects started by
###  dept by cycle
###########################################################
### sum the rows to prepare for % calculation
cycles2 <- cycles
cycles2$ADM <- unlist(lapply(cycles2[,2], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$FA <- unlist(lapply(cycles2[,3], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$FAC <- unlist(lapply(cycles2[,4], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$FIN <- unlist(lapply(cycles2[,5], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$HR <- unlist(lapply(cycles2[,6], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$ITS <- unlist(lapply(cycles2[,7], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$OR <- unlist(lapply(cycles2[,8], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$Security <- unlist(lapply(cycles2[,9], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$SF <- unlist(lapply(cycles2[,10], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$UA<- unlist(lapply(cycles2[,11], function(s) { ifelse (s > 0, 1, 0)}))
cycles2$UPD <- unlist(lapply(cycles2[,12], function(s) { ifelse (s > 0, 1, 0)}))
### Calculate percentage of coverage
cycles2$prc <- rowSums(cycles2[,2:12])/10*100

###########################################################
### save the data frame for use in other scripts
###########################################################
saveRDS(PDuration, "DW_data/Project_Duration.rds")
saveRDS(cycles2, "DW_data/Cycles2.rds")
# end

