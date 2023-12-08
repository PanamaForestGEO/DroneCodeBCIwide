##########################################################
## Purpose: Time series visualization of gap data
##
## Creator: Ian McGregor
## Contact: mcgregori@caryinstitute.org
## System: R Version 4.3.1, Sep 2023
## Last modified: Sep 2023
##########################################################
library(terra)
library(data.table)
library(parallel)
script <- "orthoTS"
flight <- as.Date("2023-06-19")

# 0a. Define functions
source("scripts/3_funs.R")

# 0b. Define variables
source("scripts/0_args.R", local=TRUE)

# id = gapID for the `flight` date above. To see gaps, check each flight's 
## metrics at `Data_HeightRasters/canopyGaps/metrics/`
# x, y = coordinates (lon, lat) in UTM 32617 if don't have id
# pngFolder, targetDates, dataPath, and bufferN are defined in `0_args.R`

## Serial runs
checkTimeSeries(id=2340, xCoord=NULL, yCoord=NULL, flight, nImg=5, 
                showPlots=TRUE, savePNG=TRUE, pngFolder, targetDates, dataPath, 
                bufferN)

checkTimeSeries(id=NULL, xCoord=626599, yCoord=1010609, flight, nImg=5, 
                showPlots=TRUE, savePNG=TRUE, pngFolder, targetDates, dataPath, 
                bufferN)

## Parallel runs
## If you want to run all the gap ids for a single flight date (and only save,
## not plot them all in the console)
metricsPath <- list.files(paste0(dataPath, "metrics/"), 
                          pattern=paste0(".*_", flight), full.names=TRUE)
metrics <- fread(metricsPath)
vars <- ls()

cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, library(terra))
clusterEvalQ(cl, library(data.table))
clusterExport(cl, c(vars))
parSapply(cl, 1:nrow(metrics), checkTimeSeries, xCoord=NULL, yCoord=NULL, 
          flight, nImg=5, showPlots=FALSE, savePNG=TRUE, pngFolder, targetDates, 
          dataPath, bufferN)
stopCluster(cl)
