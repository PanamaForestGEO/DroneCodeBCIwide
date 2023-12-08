##########################################################
## Purpose: Define canopy gaps from drone flights
##
## Input: whole-island DSMs
## Output: 3 separate files
##        - raster of gaps between two flight dates
##        - shapefile of gap polygons (derived from raster)
##        - metrics csv with mean canopy height, perim, area, and poly centroid
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## Contact: Ian McGregor, mcgregori@caryinstitute.org
## System: R Version 4.2.2, Sep 2023 (edited)
## Last modified: Sep 2023
##########################################################
library(terra)
library(data.table)
script <- "changeGaps"

# 0. Set and define functions and variables
changeType <- "structural" # "structural" or "ortho"

# 0a. Define functions
source("scripts/S5_funs.R")

# 0b. Define variables
source("scripts/args.R", local=TRUE)

## ------------------------------------------------- ##
# A. Process difference between flights
## for "chm" changeType, create canopy height models and calculate height 
## change btwn flights
## for "ortho" changeType, calculate change btwn RGB index of flights
### Please see comments in the function for applyBufferMask argument
changeRasters <- processFlightDiff(targetDates, changeType, pathData, demBCI, crsProj, 
                                      saveChange=TRUE, savePath, resN, indexName,
                                   validated=FALSE, applyBufferMask=FALSE)

## ------------------------------------------------- ##
# B. Inspect height change rasters (for CHMs only!)
## may need to play around with colors a bit
for(i in 1:length(changeRasters)){
  if(i==1) pal <- colorRampPalette(c("black", "black", "black", "lightgrey", 
                                     "white"))
  if(i %in% c(2,3)) pal <- colorRampPalette(c("black", "grey", "white"))
  plot(changeRasters[[i]], col=pal(500))
}

## example case where we remove a section of the raster due to anomalies
vecRemove <- draw("polygon")
vecRemoveList <- list(poly1, poly2, poly3)
vecRemove <- vect(vecRemoveList)
crs(vecRemove) <- "epsg: 32617"
writePath <- "droneData/droneOrthomosaics/shapefiles/anomalyPolygons/structural/"
writeVector(vecRemove, paste0(writePath,"anom_2023-10-12.shp"), overwrite=TRUE)

## ------------------------------------------------- ##
# C. Identify gaps from CHMs or numeric gaps from differenced RGB index
## **NB**: Identifying the gaps takes ~20-30 mins per raster
vecRemove <- vect(paste0(writePath, "anom_2023-10-12.shp"))
vecRemove <- NULL
gapRasters <- lapply(2:length(targetDates), identifyGaps, targetDates, 
                     changeRasters, saveChangePath, thresholds, 
                     returnGaps=TRUE, saveGaps=TRUE, saveGapsPath, 
                     vecRemove)

## ------------------------------------------------- ##
# 3. Create gap polygons and calculate metrics
## if returnAll=TRUE, a list is returned. Otherwise, nothing is returned.
gapOutputs <- lapply(2:length(targetDates), gapPolyMetrics, targetDates,
                     saveGapsPath, saveChangePath, returnAll=FALSE)

listNames <- sapply(2:length(targetDates), function(X){
  return(paste0("gaps", targetDates[X-1], "_", targetDates[X]))
})
names(gapOutputs) <- listNames
