##########################################################
## Purpose: Define variables and parameters for gap identification scripts
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## Contact: Ian McGregor, mcgregori@caryinstitute.org
## System: R Version 4.2.2, Sep 2023 (edited)
##########################################################

# dates and resolutions of whole island flights
flightInfo <- read.csv("droneData/metadata/metaIslandFlights.csv")
flightDates <- as.Date(paste0(flightInfo$id, "-01"))

crsProj <- "epsg: 32617"
resN <- resMin <- 0.2

if(script=="makeDSM"){
  targetDates <- flightDates
  pointCloudPath <- "droneData/pointClouds/4_aligned/tilesAlignedBCI_DD"
  pathSave <- gsub("4_.*", "5_dsm/", pointCloudPath)
  pathBuffer <- "spatialData/bci/BCI_Outline_Minus25/BCI_Outline_Minus25.shp"
  plotDSM <- TRUE
  saveDSM <- TRUE
}
if(script=="metadata"){
  flightPath <- paste0("droneData/droneFlights/", flightMonth, "/", 
                       flightDate, "/mission", nMission, "/ulgDroneGPS")
}
if(script=="standardize"){
  targetDates <- flightInfo$id[flightDates > as.Date("2020-01-01")]
  path <- "droneData/droneOrthomosaics/"
  pathExt <- paste0(path, "shapefiles/minCommonExtent/")
  shpName <- "minCommonExtent.shp"

  pathInput <- paste0(path, "1_original")
  pathStandard <- paste0(path, "2_standardized/")
  pathMask <- "3_masked"
  pathSpatial <- "spatialData/bci/"
  pathBuffer <- paste0(pathSpatial, 
                       "BCI_Outline_Minus25/BCI_Outline_Minus25.shp")

  ## filter the age shapefile (only run if file doesn't exist)
  ## NOTE this is irrelevant as of Jan 2024, potential to remove
  pathAge <- paste0(pathSpatial, 
                    "Ender_Forest_Age_1935/Ender_Forest_Age_1935.shp")
  pathAgeFilt <- gsub(".shp", "_filtered.shp", pathAge)
  
  filterAge(pathAge, pathAgeFilt)
}
if(script=="changeGaps"){
  targetDates <- flightInfo$id[flightDates > as.Date("2020-01-01")]
  # targetDates <- c("2023-06", "2023-11")
  savePath <- "droneData/processedChange/"
  vecRemovePath <- "droneData/droneOrthomosaics/shapefiles/anomalyPolygons/"
  saveGapsPath <- paste0(savePath, "gapsSl/fileType/gapsD1_D2.ext")
  
  if(changeType=="structural"){
    resN <- 1
    indexName <- ""
    pathHeight <- "droneData/pointClouds/"
    pathData <- paste0(pathHeight, "6_dsmMasked")
    maskPath <- paste0(maskPath, "structural")
    demBCI <- "spatialData/bci/LidarDEM_BCI.tif"

    saveGapsPath <- gsub("Sl", "Structural", saveGapsPath)
    gdalOutDir <- gsub("fileType.*", "gdalOut", saveGapsPath)
    if(!dir.exists(gdalOutDir)) dir.create(gdalOutDir)

    saveGapFiles <- TRUE
    saveChangePath <- paste0(savePath, "changeStructural/changeD1_D2.tif")
    applyBufferMask <- FALSE
    thresholds <- list(shortThreshMin = -9999, shortThreshMax = -5, 
                       gapSizeMin = 25, gapSizeMax = 10^6,
                       directions = 4)
  } 
}
if(script == "vis"){
  dataType <- c("rasters", "polygons", "metrics")
  dataPath <- "processedChange/gapsCanopy"
  targetDates <- flightDates[flightDates > as.Date("2021-01-01")]
}
