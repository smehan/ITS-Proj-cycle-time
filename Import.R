###########################################################
### Importing and cleansing DW data
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
DWData <- read.csv("DW_Data/DW_Projects.csv", header=TRUE, sep = ",", stringsAsFactors = TRUE,
                    as.is = c("Summary"))
rownames(DWData) <- DWData$Key
# end

# Convert empty values to NA
DWData[DWData == ""] <- NA
# end

# Convert dates to posix date objects
DWData$Created <- mdy(DWData$Created)
DWData$Resolved <- mdy(DWData$Resolved)
DWData$Due.Date <- mdy(DWData$Due.Date)
# end

###########################################################
### perform date calculations
###########################################################

# Year request created into year and month, with a numerical and text month
DWData$year_created <- year(DWData$Created)
DWData$month_created <- month(DWData$Created, label = TRUE)
DWData$month_num_created <- month(DWData$Created)
DWData$year_resolved <- year(DWData$Resolved)
DWData$month_resolved <- month(DWData$Resolved, label = TRUE)
DWData$month_num_resolved <- month(DWData$Resolved)
DWData$year_due <- year(DWData$Due.Date)
DWData$month_due <- month(DWData$Due.Date, label = TRUE)
DWData$month_num_due <- month(DWData$Due.Date)

# Calendar duration of project
DWData$project_duration <- round((DWData$Resolved - DWData$Created), 3)
# end

