##########################################################
## Purpose: Define paths, variables, and arguments for main functions
##
## Creator: Ian McGregor (adapted from Cushman et al 2020)
## Contact: 
## System: R Version 4.2.2, Aug 2023 (edited)
## Last modified: Aug 2023
##########################################################

# *NB*: "script" variable is defined in each respective script

# dates and resolutions of whole island flights
flightInfo <- read.csv("Data_Ancillary/metaIslandFlights.csv")
flightDates <- as.Date(flightInfo$date, format="%d/%m/%Y")

# common variables
crsProj <- "epsg: 32617"

# specific variables
if(script=="makeDSMs"){
  pathList <- list(
    "pathDrone" = "PointClouds/Processed/",
    "pathTrim" = "Lidar/BCIXXTiles_trim/",
    "pathPhtQA" = "DronePhotogrammetry/tilesAlignedBCI_DD/",
    "pathRaw" = "DronePhotogrammetry/BCIXXTiles/",
    "pathDSM" = "Data_HeightRasters/dsm/",
    "pathDSMqa" = "Data_QAQC/"
  )
  
  ## targetType = "trim" (lidar), 
  ###             "phtgrm" (corrected DSM), 
  ###             "raw" (uncorrected DSMs for 2015, 2018, 2020), 
  ###             "qaqc" (QAQC DSM or height range metrics)
  ## targetYears = whatever years there is data for
  ###             for Cushman et al 2020, this is either 
  ###             - c(2009) for targetType = "trim" or
  ###             - c(2015, 2018, 2020) for all other targetTypes
  ## qaqcType = "1m" (1 m pixels with no points (no gap fill)), 
  ###           "0.2m" (0.2 m pixels with height range of points)
  
  targetType <- "phtgrm"
  targetDates <- flightDates[flightDates > as.Date("2021-01-01")]
  qaqcType=""
}
if(script == "defineGaps"){
  pathHeight <- "Data_HeightRasters/"
  pathData <- paste0(pathHeight, "dsm/DSM_Y.tif")
  pathAge <- "Data_Ancillary/Enders_Forest_Age_1935/Ender_Forest_Age_1935.shp"
  bufferPath <- "Data_Ancillary/BCI_Outline_Minus25/BCI_Outline_Minus25.shp"
  demBCI <- paste0(pathHeight, "LidarDEM_BCI.tif")
  maskPath <-"Data_QAQC/"
  targetDates <- flightDates[flightDates > as.Date("2021-01-01")]
  
  shortMask <- FALSE
  shortThresh <- ""
  saveGapsPath <- paste0(pathHeight, "canopyGaps/rasters/gapsD1_D2.tif")
  saveHeightPath <- paste0(pathHeight, "heightChange/changeD1_D2.tif")
}
if(script == "orthoTS"){
  dataPath <- "Data_HeightRasters/canopyGaps/"
  orthoPath <- "DroneOrthomosaics"
  pngFolder <- paste0(orthoPath, "/gapTimeseries")
  dIndex <- which(grepl(flight, flightDates))
  targetDates <- flightDates[(dIndex-1):dIndex]
  bufferN <- 10 # length of buffer (m) for individual points (i.e. no gap ID)
}