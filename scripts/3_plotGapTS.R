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
script <- "orthoTS"
flight <- as.Date("2023-03-17")

# 0a. Define functions
source("scripts/3_funs.R")

# 0b. Define variables
source("scripts/0_args.R", local=TRUE)

# id = gapID for the `flight` date above. To see gaps, check each flight's 
## metrics at `Data_HeightRasters/canopyGaps/metrics/`
# x, y = coordinates (lon, lat) in UTM 32617 if don't have id
# pngFolder, targetDates, dataPath, and bufferN are defined in `0_args.R`

## Examples
checkTimeSeries(id=2340, x=NULL, y=NULL, flight, nImg=5, savePNG=TRUE, 
                pngFolder, targetDates, dataPath, bufferN)

checkTimeSeries(id=NULL, x=626599, y=1010609, flight, nImg=5, savePNG=TRUE, 
                pngFolder, targetDates, dataPath, bufferN)


