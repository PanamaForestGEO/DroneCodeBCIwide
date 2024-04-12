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
flightDates <- as.Date(flightInfo$flightID)

crsProj <- "epsg: 32617"
resN <- resMin <- 0.2

if(script=="alignPC"){
  nCores <- 9
  # targetDate <- "2015-06"

  ## find Z-adjustment if necessary
  findZ <- FALSE

  ## run shell scripts for height adjustment and alignment in cloudCompare
  shell_heightAdjust <- FALSE
  shell_align <- TRUE

  ## path definitions
  pathPointCloud <- paste0("droneData/pointClouds/1_raw/", targetDate)
  pathSoils <- "spatialData/bci/BCI_Soils/BCI_Soils.shp"
  pathGrid <- "droneData/pointClouds/gridInfo.csv"
  crsProj <- "epsg:32617"

  dirPath <- gsub("1_raw", "2_standardized", pathPointCloud)

  outPathFull <- gsub(targetDate, paste0("1_fullResolution/", targetDate), dirPath)
  outPathDec <- gsub(targetDate, paste0("2_decimated/", targetDate), dirPath)
  outPathAligned <- gsub(targetDate, paste0("tilesAlignedBCI_", targetDate),
                      gsub("2_standardized", "4_aligned/decimated", dirPath))
  if(!dir.exists(outPathAligned)) dir.create(outPathAligned)

  savePaths <- c(outPathFull, outPathDec, outPathAligned)
  saveConditions <- c(TRUE, TRUE, TRUE)
  sapply(savePaths, function(X) if(!dir.exists(X)) dir.create(X))
}
if(script=="makeDSM"){
  # targetDates <- flightDates
  pointCloudPath <- "droneData/pointClouds/4_aligned/decimated/tilesAlignedBCI_DD"
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
  # targetDates <- flightInfo$id[flightDates > as.Date("2020-01-01")]
  targetDates <- c("2023-06-19", "2024-03-06")
  savePath <- "droneData/processedChange/"
  vecRemovePath <- "droneData/droneOrthomosaics/shapefiles/anomalyPolygons/"
  saveGapsPath <- paste0(savePath, "gapsSl/fileType/gapsD1_D2.ext")

  maskPath <- "droneData/anomalyPolygons/"
  buildingPath <- "spatialData/bci/Barro_Colorado_Island_Buildings/Buildings.shp"
  
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
  } else if(changeType=="spectral"){
    pathOrtho <- "droneData/droneOrthomosaics/"
    pathData <- paste0(pathOrtho, "3_masked")
    maskPath <- paste0(maskPath, "spectral")

    saveGapsPath <- gsub("Sl", "Spectral", 
                      gsub(".ext", paste0("_res", resN*100, ".ext"), saveGapsPath))
    gdalOutDir <- gsub("fileType.*", "gdalOut", saveGapsPath)
    if(!dir.exists(gdalOutDir)) dir.create(gdalOutDir)
    
    saveGapFiles <- TRUE
    saveChangePath <- paste0(savePath, "changeSpectral/changeD1_D2_res", resN*100, ".tif")
    
    # NOTE - change thresholds based on index
    indexName <- "exgr"
    thresholds <- list(shortThreshMin = -9999, shortThreshMax = -75, 
                       gapSizeMin = 25, gapSizeMax = 10^6,
                       directions = 4)
  }
}
if(script == "vis"){
  dataType <- c("rasters", "polygons", "metrics")
  
  if(changeType=="ortho"){
    dataPath <- "processedChange/gapsIndex/"
  } else if(changeType=="chm"){
    dataPath <- "processedChange/gapsCanopy"
  }
  
  targetDates <- flightDates[flightDates > as.Date("2021-01-01")]
}