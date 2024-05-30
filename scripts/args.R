##########################################################
## Purpose: Define variables and parameters for gap analysis scripts
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## Contact: Ian McGregor, mcgregori@caryinstitute.org
## System: R Version 4.3.1
##########################################################

# "siteName" is defined before sourcing this file

# STOP! Have you run `createDirStructure.R` already??

siteName <- "bci"

##----------------------------------------##
# Top-level paths
##----------------------------------------##
pathData <- paste0("droneData/", siteName, "/")
pathSpatial <- paste0("spatialData/", siteName, "/")
if(!dir.exists(pathData)){
  stop(paste0("Please run `createDirStructure.R`, otherwise this code will ",
              "fail. If you did run the script and are still getting this ",
              "error, please check your working directory path."))
}

##----------------------------------------##
# General variables
##----------------------------------------##
nCores <- 10 #cores available for parallelization

# dates and resolutions of whole study site flights
flightInfo <- read.csv(paste0(pathData, "metadata/metadataFlights.csv"))
flightDates <- as.Date(flightInfo$flightID, format=c("%m/%d/%y"))

targetDate <- "2023-10-31" #for what date should we process the point clouds?
targetDates <- c("2022-07-21", "2023-03-16") #for what dates should we do the gap comparisons?
targetDates <- sort(as.Date(targetDates)) #dates will be put in order (if >1)

# td <- list.files(paste0(pathData, "pointClouds/6_dsmMasked/"))
# targetDates <- gsub(".tif", "", gsub("DSM_", "", td[!grepl("orig", td)]))

# Gap comparisons
changeType <- "structural" # "structural" or "ortho"
validated <- TRUE # see mod3 for description
runType <- "all" # either "change" rasters only, "gaps" outputs only, or "all"
saveChange <- TRUE

##----------------------------------------##
# Site-specific paths
##----------------------------------------##
if(siteName=="bci"){
  pathBuffer <- paste0(pathSpatial, "BCI_Outline_Minus25/BCI_Outline_Minus25.shp")
  buildingPath <- paste0(pathSpatial, "Barro_Colorado_Island_Buildings/Buildings.shp")
  pathBorder <- paste0(pathSpatial, "BCI_Outline/BCI_Outline.shp")
  demBCI <- paste0(pathSpatial, "LidarDEM_BCI.tif")
}

##----------------------------------------##
# Site-specific variables
##----------------------------------------##
if(siteName=="bci"){
  crsProj <- "epsg: 32617"
  resN <- resMin <- 0.2
}

##----------------------------------------##
# Site-specific formatting
##----------------------------------------##
if(siteName=="bci"){
  ## filter the age shapefile (only run if file doesn't exist)
  ## NOTE this is irrelevant as of Jan 2024, potential to remove
  pathAge <- paste0(pathSpatial, 
                    "Ender_Forest_Age_1935/Ender_Forest_Age_1935.shp")
  pathAgeFilt <- gsub(".shp", "_filtered.shp", pathAge)

  # filterAge(pathAge, pathAgeFilt)

  if(script=="alignPC"){
    ## code to create the BCI border shapefile
    # pathSoils <- paste0(pathSpatial, "BCI_Soils/BCI_Soils.shp")
    # bciBorder <- aggregate(vect(pathSoils), by=NULL, dissolve=TRUE)
    # bciBorder <- fillHoles(bciBorder, inverse=FALSE)
    # bciBorder <- project(bciBorder, crsProj)
    # writeVector(bciBorder, paste0(pathSpatial, "BCI_Outline/BCI_Outline.shp"))

    bciBorder <- vect(pathBorder)
    ROI <- st_as_sf(bciBorder)
  }
}

##----------------------------------------##
# Task-specific variables
##----------------------------------------##
if(script=="alignPC"){
  ## find Z-adjustment if necessary
  findZ <- FALSE

  ## run shell scripts for height adjustment and alignment in cloudCompare
  shell_heightAdjust <- FALSE #should we find a height adjustment using batch script?
  shell_align <- TRUE #should we align our target cloud to a lidar cloud?
  python <- TRUE #should we use pythonAPI? if FALSE, will use command-line script

  ## path definitions
  pathPointCloud <- paste0(pathData, "pointClouds/1_raw/", targetDate, "/")
  pathGrid <- paste0(pathData, "pointClouds/gridInfo.csv")
  pathTile <- paste0("cloud_tileN_", siteName, "_type.las")

  outPathStand <- paste0(gsub("1_raw", "2_standardized", pathPointCloud), 
                        gsub("type", "2stand", pathTile))
  outPathCC <- gsub("2_standardized", "3_cloudCompare", dirname(outPathStand))
  outPathAligned <- paste0(gsub("2_standardized/.*", "4_aligned/", outPathStand),
                        targetDate, "/",
                        gsub("type", "4aligned", pathTile))
  outPathTrans <- gsub("2_standardized", "7_transformationMatrices", 
                        dirname(outPathStand))
  
  pathVector <- c(outPathStand, outPathCC, outPathAligned, outPathTrans)
  for(i in pathVector){
    if(grepl("2_|4_", i)){
      if(!dir.exists(dirname(i))) dir.create(dirname(i))
    } else {
      if(!dir.exists(i)) dir.create(i)
    }
  }

  # outPathFull <- gsub(targetDate, paste0("1_fullResolution/", targetDate), dirPath)
  # outPathDec <- gsub(targetDate, paste0("3_decimated/", targetDate), dirPath)

  # savePaths <- c(outPathFull, outPathDec, outPathAligned)
  savePaths <- c(outPathStand, outPathAligned)
  saveConditions <- c(TRUE, TRUE)
}

if(script=="makeDSM"){
  pointCloudPath <- paste0(pathData, "pointClouds/4_aligned/DD")
  pathSave <- gsub("4_.*", "5_dsm/", pointCloudPath)
  targetDates <- targetDate
  plotDSM <- TRUE
  saveDSM <- TRUE
}

if(script=="metadata"){
  flightPath <- paste0("droneData/droneFlights/", flightMonth, "/", 
                       flightDate, "/mission", nMission, "/ulgDroneGPS")
}

if(script=="standardize"){
  path <- paste0(pathData, "droneOrthomosaics/")
  pathExt <- paste0(path, "shapefiles/minCommonExtent/")
  shpName <- "minCommonExtent.shp"

  pathInput <- paste0(path, "1_original")
  pathStandard <- paste0(path, "2_standardized/")
  pathMask <- "3_masked"

}

if(script=="changeGaps"){
  savePath <- paste0(pathData, "processedChange/")
  vecRemovePath <- paste0(pathData, "droneOrthomosaics/shapefiles/maskPolygons/")
  saveChangePath <- paste0(savePath, "changeSl/changeCT_D1_D2.tif")
  saveGapsPath <- paste0(savePath, "gapsSl/fileType/gapsCT_D1_D2.ext")
  maskPath <- paste0(pathData, "maskPolygons/")
  
  if(changeType=="structural"){
    resN <- 1
    indexName <- ""
    pathHeight <- paste0(pathData, "pointClouds/")
    pathInput <- paste0(pathHeight, "6_dsmMasked")
    maskPath <- paste0(maskPath, "structural")
    saveChangePath <- gsub("Sl", "Structural", 
                            gsub("CT", "Stru", saveChangePath))
    saveGapsPath <- gsub("Sl", "Structural", saveGapsPath)
    gdalOutDir <- gsub("fileType.*", "gdalOut", saveGapsPath)

    saveGapFiles <- TRUE
    
    saveGapsPath <- gsub("CT", "Stru", saveGapsPath)
    applyBufferMask <- FALSE
    thresholds <- list(shortThreshMin = -9999, shortThreshMax = -5, 
                       gapSizeMin = 25, gapSizeMax = 10^6,
                       directions = 4)
  } else if(changeType=="spectral"){
    pathOrtho <- paste0(pathData, "droneOrthomosaics/")
    pathInput <- paste0(pathOrtho, "3_masked")
    maskPath <- paste0(maskPath, "spectral")

    saveGapsPath <- gsub("Sl", "Spectral", 
                      gsub("CT", "Spec", 
                            gsub(".ext", paste0("_res", resN*100, ".ext"), 
                                  saveGapsPath)))
    gdalOutDir <- gsub("fileType.*", "gdalOut", saveGapsPath)
    
    saveGapFiles <- TRUE
    saveChangePath <- gsub("Sl", "Spectral",
                          gsub("CT", "Spec",
                              gsub(".tif", paste0("_res", resN*100, ".tif"),
                                    saveChangePath)))
    
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