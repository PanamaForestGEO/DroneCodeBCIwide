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
## System: R Version 4.2.2
##########################################################
library(terra)
library(data.table)

processGaps <- function(){
  script <- "changeGaps"

  # 0a. Define functions
  source("scripts/mod3_funs.R")

  # 0b. Define variables
  source("scripts/args.R", local=TRUE)

  ## ------------------------------------------------- ##
  # A. Run main processing functions
  ## if validated is FALSE, the function will stop to let you draw and save 
  ## anomaly polygons as in Part B below. Otherwise, if validated is TRUE then 
  ## the full function will run.

  out <- defineGapsWrap(targetDates, changeType, pathInput, demBCI, crsProj, 
                  saveChange, savePath, resN, indexName, validated, 
                  saveChangePath, thresholds, gdalOutDir, maskPath, 
                  buildingPath, saveGapFiles, saveGapsPath, runType)
  print(paste0("Done! Files saved to ", savePath, " directory."))

  ## ------------------------------------------------- ##
  # B. Inspect height change rasters
  ## play around with colors as needed
  if(!validated){
    for(i in 1:length(changeRasters)){
      if(i==1) pal <- colorRampPalette(c("black", "black", "black", "lightgrey", "white"))
      if(i %in% c(2,3)) pal <- colorRampPalette(c("black", "grey", "white"))
      plot(changeRasters[[i]], col=pal(500))
    }

    ## example case where we remove a section of the raster due to anomalies
    vecRemove <- draw("polygon")
    vecRemoveList <- list(poly1, poly2, poly3)
    vecRemove <- vect(vecRemoveList)
    crs(vecRemove) <- "epsg: 32617"
    writeVector(vecRemove, paste0(maskPath,"mask_2023-10-12.shp"), overwrite=TRUE)
  }
}

# DSMs
processGaps()
