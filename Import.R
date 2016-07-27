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

# Calendar duration of project
PDuration$project_duration <- round((PDuration$Resolved - PDuration$Created), 3)
# end

