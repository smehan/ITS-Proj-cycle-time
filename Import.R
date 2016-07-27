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
#DWData[DWData == " "] <- NA # there are still some rows ending with an extra space
# end

# Convert dates to posix date objects
DWData$Created <- mdy(DWData$Created)
DWData$Resolved <- mdy(DWData$Resolved)
DWData$Due.Date <- mdy(DWData$Due.Date)
