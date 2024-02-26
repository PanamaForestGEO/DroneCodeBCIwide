##########################################################
## Purpose: Define structural or spectral gaps from drone flights
##
## Input: DSMs or orthomosaics
## Output: 2 separate files
##        - shapefile of gap polygons
##        - metrics csv with mean canopy height, perim, area, and poly centroid
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## Contact: Ian McGregor, mcgregori@caryinstitute.org
## System: R Version 4.2.2, Sep 2023 (edited)
##########################################################
library(terra)
library(data.table)
script <- "changeGaps"

# 0. Set and define functions and variables
changeType <- "structural" # "structural"

# 0a. Define functions
source("scripts/mod2_funs.R")

# 0b. Define variables
source("scripts/args.R", local=TRUE)

## ------------------------------------------------- ##
# A. Run main processing functions
## if validated is FALSE, the function will stop to let you draw and save anomaly polygons
## as in Part B below. Otherwise, if validated is TRUE then the full function will run.
validated <- TRUE
runType <- "all" # either "change" rasters only, "gaps" outputs only, or "all"

defineGapsWrap(targetDates, changeType, pathData, demBCI, crsProj, 
                saveChange=TRUE, savePath, resN, indexName, validated, 
                saveChangePath, thresholds, gdalOutDir, maskPath, 
                buildingPath, saveGapFiles, saveGapsPath, runType)

## ------------------------------------------------- ##
# B. Inspect height change rasters
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
# C. Identify gaps, calculate metrics, and save outputs
vecRemove <- vect(paste0(writePath, "anom_2023-10-12.shp"))
vecRemove <- NULL

gapOutputs <- sapply(2:length(targetDates), identifyGapsMetrics, targetDates, 
                    saveChangePath, thresholds, gdalOutDir, vecRemove, saveGapFiles, 
                    saveGapsPath)
