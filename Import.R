###########################################################
### Importing and cleansing DW data
###########################################################

# assemble the main df
DWData <- read.csv("DW_Data/DW_Projects.csv", header=TRUE, sep = ",", stringsAsFactors = TRUE,
                    as.is = c("Summary"))
rownames(DWData) <- DWData$Key

