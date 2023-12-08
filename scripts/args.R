
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
  targetDates <- c("2023-06-19", "2023-10-31")
  savePath <- "droneData/processedChange/"
  vecRemovePath <- "droneData/droneOrthomosaics/shapefiles/anomalyPolygons/"
  
  if(changeType=="structural"){
    resN <- 1
    indexName <- ""
    pathHeight <- "droneData/pointClouds/"
    pathData <- paste0(pathHeight, "6_dsmMasked")
    vecRemoveShp <- paste0(vecRemovePath, "lidar/anomalyRemove.shp")
    demBCI <- "spatialData/bci/LidarDEM_BCI.tif"
    saveChangePath <- paste0(savePath, "changeHeight/changeD1_D2.tif")
    saveGapsPath <- paste0(savePath, "gapsCanopy/rasters/gapsD1_D2.tif")
    applyBufferMask <- FALSE
    thresholds <- list(shortThreshMin = -9999, shortThreshMax = -5, 
                       gapSizeMin = 25, gapSizeMax = 10^6,
                       directions = 4)
  } else if(changeType=="ortho"){
    pathOrtho <- "droneData/droneOrthomosaics/"
    pathData <- paste0(pathOrtho, "3_masked")
    vecRemoveShp <- paste0(vecRemovePath, "spectral/clouds_2023-09-21.shp")
    ## for above, 2022-07-21 or 2023-09-21
    
    saveChangePath <- paste0(savePath, "changeIndex/changeD1_D2_res", resN*100, ".tif")
    saveGapsPath <- paste0(savePath, "gapsIndex/rasters/gapsD1_D2_res", resN*100, ".tif")
    
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