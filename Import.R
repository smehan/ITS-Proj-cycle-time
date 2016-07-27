###########################################################
### Importing and cleansing DW data
###########################################################

# assemble the main datafile
DWData <- read.csv("DW_Data/DW_Projects.csv", header=TRUE, sep = ",", stringsAsFactors = TRUE,
                    as.is = c("Summary"))
rownames(DWData) <- DWData$Key
# end

# Convert empty values to NA
DWData[DWData == ""] <- NA
#DWData[DWData == " "] <- NA # there are still some rows ending with an extra space
# end
