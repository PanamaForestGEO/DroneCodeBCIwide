##########################################################
## Purpose: Create cloud and QAQC masks
##
## Input: whole-island BCI DSMs
## Output: cloud- and QAQC-masked DSMs (primarily for flights < 2020)
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## Contact: 
## System: R Version 4.2.2, Aug 2023 (edited)
## Last modified: Aug 2023
##########################################################
library(terra)
library(data.table)

source("scripts/1_funs.R")

# Define metadata
folderPath <- "Data_QAQC/"
years <- c(2015, 2018, 2020)
thresh <- data.table(year = c(2015, 2018, 2020), 
                     albedo = c(475, 375, 375),
                     redblu = c(20, 20, 15))

# Cloud masks
bufferPath <- "Data_Ancillary/BCI_Outline_Minus25/BCI_Outline_Minus25.shp"
cloud <- lapply(years, applyCloudMask, folderPath, bufferPath, thresh, 
                plotMask=TRUE)

lapply(1:length(cloud), function(X){
  writeRaster(X, paste0(folderPath, "CloudMasks/CloudMask_", X, ".tif"))
})

# QAQC masks
qaqc <- lapply(years, applyQAQCMask, folderPath, plotMask=TRUE)
lapply(1:length(cloud), function(X){
  writeRaster(maskRaster, paste0(folderPath, "QAQCMask_", X, ".tif"))
})
