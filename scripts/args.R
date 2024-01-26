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
flightDates <- as.Date(flightInfo$date, format="%m/%d/%y")

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
  targetDates <- flightDates[flightDates > as.Date("2021-01-01")]
  path <- "droneData/droneOrthomosaics/"
  pathExt <- paste0(path, "shapefiles/minCommonExtent/")
  shpName <- "minCommonExtent.shp"

  if(changeType=="ortho"){
    pathInput <- paste0(path, "1_original")
    pathStandard <- paste0(path, "2_standardized/")
    pathMask <- "3_masked"
  }

  if(changeType=="structural"){
    pathInput <- pathStandard <-  "droneData/pointClouds/5_dsm/"
    pathMask <- "6_dsmMasked"
    resMin <- 1
  }
  
  pathSpatial <- "spatialData/bci/"
  pathBuffer <- paste0(pathSpatial, 
                       "BCI_Outline_Minus25/BCI_Outline_Minus25.shp")
  pathAge <- paste0(pathSpatial, 
                    "Ender_Forest_Age_1935/Ender_Forest_Age_1935.shp")
}
if(script=="changeGaps"){
  # targetDates <- flightDates[flightDates > as.Date("2021-01-01")]
  targetDates <- c("2023-10", "2023-11")
  savePath <- "droneData/processedChange/"
  vecRemovePath <- "droneData/droneOrthomosaics/shapefiles/anomalyPolygons/"
  saveGapsPath <- paste0(savePath, "gapsSl/fileType/gapsD1_D2.ext")
  
  if(changeType=="structural"){
    resN <- 1
    indexName <- ""
    pathHeight <- "droneData/pointClouds/"
    pathData <- paste0(pathHeight, "6_dsmMasked")
    vecRemoveShp <- paste0(vecRemovePath, "lidar/anomalyRemove.shp")
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
  } else if(changeType=="ortho"){
    pathOrtho <- "droneData/droneOrthomosaics/"
    pathData <- paste0(pathOrtho, "3_masked")
    vecRemoveShp <- paste0(vecRemovePath, "spectral/clouds_2023-09.shp")
    ## for above, 2022-07 or 2023-09

    saveGapsPath <- gsub("Sl", "Spectral", 
                      gsub(".ext", paste0("_res", resN*100, ".ext", saveGapsPath)))
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