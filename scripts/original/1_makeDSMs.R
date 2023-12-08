##########################################################
## Purpose: Create digital surface models from aligned orthomosaics
##
## Input: aligned point tiles for BCI
## Output: whole-island DSMs
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## Contact: 
## System: R Version 4.2.2, Aug 2023 (edited)
## Last modified: Aug 2023
##########################################################
library(terra)
library(lidR)
source("scripts/1_funs.R")

## Before running for the first time yourself, please ensure the folder
## and file names in the `prepMakeDSM()` function and `args.R` are set 
## correctly.

# 1. Define variables and function arguments
script <- "makeDSMs"
source("scripts/0_args.R", local=TRUE)

# 2. Run the main function and save DSMs if wanted
outputList <- prepMakeDSM(targetType, targetDates, qaqcType, pathList, crsProj,
                          plotDSM=TRUE, saveDSM=TRUE, returnDSM=FALSE)

## plot DSMs a posteriori
sapply(targetDates, function(X){
  r <- rast(paste0(pathList$pathDSM, "DSM_", X, "_corrected.tif"))
  plot(r)
})

# 2a. If necessary, crop lidar data to extent of photogrammetry data and save
cropLidar <- function(dsmLPath, dsmPPath){
  dsmLidar <- rast(dsmLPath)
  dsmPhtgrm <- rast(dsmPPath)
  dsmLidar <- crop(dsmLidar, dsmPhtgrm)
  
  writeRaster(dsmLidar, dsmLPath)
}
dsmLPath <- "Data_HeightRasters/DSM_2009.tif"
dsmPPath <- "Data_HeightRasters/DSM_2015_corrected.tif"
cropLidar(dsmLPath, dsmPPath)
