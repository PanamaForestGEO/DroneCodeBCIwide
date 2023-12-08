##########################################################
## Purpose: Define canopy gaps from drone flights
##
## Input: whole-island DSMs (masked or not)
## Output: 3 separate files
##        - raster of gaps between two flight dates (Step 2)
##        - shapefile of gap polygons (derived from raster)
##        - metrics csv with mean canopy height, perim, area, and poly centroid
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## Contact: Ian McGregor, mcgregori@caryinstitute.org
## System: R Version 4.2.2, Aug 2023 (edited)
## Last modified: Aug 2023
##########################################################
library(terra)
library(data.table)
script <- "defineGaps"

# 0a. Define functions
source("scripts/2_funs.R")

# 0b. Define variables
source("scripts/0_args.R")

## NOTE
### Steps 1-2 are run together
### Step 3 is run alone

## Data is automatically saved in separate files. Each one has the same gap IDs
### - gap raster (Step 2)
### - gap polygon shapefile (derived from raster)
### - metrics table with mean canopy height, perimeter, area, polygon centroid

## ------------------------------------------------- ##
# 1. Create canopy height models and calculate height change btwn flights
changeRasters <- processCanopyHeights(targetDates, pathData, pathAge, 
                                      bufferPath, demBCI, maskPath, crsProj, 
                                      saveHeightChange=TRUE,
                                      saveHeightPath, validated=FALSE)

# 2. Inspect height change rasters
## may need to play around with colors a bit
for(i in 1:length(changeRasters)){
  if(i==1) pal <- colorRampPalette(c("black", "black", "black", "lightgrey", 
                                     "white"))
  if(i %in% c(2,3)) pal <- colorRampPalette(c("black", "grey", "white"))
  plot(changeRasters[[i]], col=pal(500))
}

## example case where we remove a section of the raster due to anomalies
vecRemove <- draw("polygon", col="black")
crs(vecRemove) <- crs(changeRasters[[1]])
writeVector(vecRemove, "Data_HeightRasters/anomalyRemove.shp")

# 2. Identify gaps
## **NB**: Identifying the gaps takes ~20-30 mins per raster
gapRasters <- lapply(2:length(targetDates), identifyGaps, targetDates, 
                     changeRasters, saveHeightPath, shortThresh, 
                     returnGaps=TRUE, saveGaps=TRUE, saveGapsPath, vecRemove)

## ------------------------------------------------- ##
# 3. Create gap polygons and calculate metrics
## if returnAll=TRUE, a list is returned. Otherwise, nothing is returned.
gapOutputs <- lapply(2:length(targetDates), gapPolyMetrics, targetDates,
                     saveGapsPath, saveHeightPath, returnAll=TRUE)

listNames <- sapply(2:length(targetDates), function(X){
  return(paste0("gaps", targetDates[X-1], "_", targetDates[X]))
})
names(gapOutputs) <- listNames
