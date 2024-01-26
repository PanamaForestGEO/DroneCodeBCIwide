##########################################################
## Purpose: Functions for post-alignment script S5_DefineGaps.R
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## System: R Version 4.2.2, Aug 2023 
##########################################################

# --------------------------------------------------------------------#
## createCHM = create canopy height models from DSMs
## timeChange = difference the CHMs or index rasters from two flights
## processFlightDiff = calculate differences between two flights
## identifyGapsMetrics = create gap polygons and calculate metrics
# --------------------------------------------------------------------#
createCHM <- function(X, pathData, demBCI, crsProj, changeType){
  print(paste0("Creating chm for ", X))
  
  ## Bring in dsm
  path <- list.files(pathData, full.names=TRUE)
  dsm <- rast(path[grepl(paste0(X, ".tif"), path)])

  ## bring in BCI dem and resample to match dsm
  dem <- rast(demBCI)
  # demC <- resample(dem, dsm, method="cubic")
  
  ## create canopy height model by subtracting ground elevation
  ### note there may be some negative values - this is ok because it's relative
  ### to the prior lidar data
  chm <- dsm - dem # (i.e. dsm - dem = chm)
  
  return(chm)
}
timeChange <- function(X, flightTifs, changeType, saveChange, savePath, 
                       indexName=""){
  dateStart <- gsub("^.*_", "", names(flightTifs)[X-1])
  dateEnd <- gsub("^.*_", "", names(flightTifs)[X])
  
  if(changeType=="structural"){
    change <- flightTifs[[X]] - flightTifs[[(X-1)]]
  }
  
  if(saveChange){
    outPath <- gsub("D1", dateStart, saveChangePath)
    outPath <- gsub("D2", dateEnd, outPath)
    writeRaster(change, outPath, overwrite=TRUE)
  }
  
  return(change)
}
processFlightDiff <- function(targetDates, changeType, pathData,
                              demBCI, crsProj, saveChange, 
                              saveChangePath,
                              validated=FALSE, applyBufferMask=FALSE){
  
  if(changeType=="structural"){
    # 1. Create canopy height models for each flight DSM
    ## ~10 sec per DSM 
    flightTifs <- lapply(targetDates, createCHM, pathData, demBCI, crsProj, 
                        changeType)
  }
  names(flightTifs) <- paste0(targetDates)
  
  # 2. Create change rasters between successive flights
  ## if CHMs, this is a simple subtraction of previous from current flight
  ## if orthos, you first calculate a spectral RGB index and then difference
  ## the previous and current photos.
  changeFlights <- lapply(2:length(targetDates), timeChange, flightTifs,
                          changeType, saveChange, saveChangePath, indexName)
  
  if(!validated){
    warning("
    STOP! Please evaluate each height change raster and run through this 
    checklist to ensure there are no data issues BEFORE identifying gaps:
    
    Problems:
    1. Can you see straight, square lines? These are tiling issues.
    2. Do you see long straight lines with a lot of change on one side? 
    This is probably due to cloud or flight artefacts.
    
    Solutions:
    - The fastest solution is to create a mask and remove the problematic areas
    if possible (e.g. one bad tile, one thing long tiling offset). 
    - However, the best solution is to fix the original point cloud processing.
    
    Next steps:
    - Once you have ensured the height change rasters are fine or you have
    fixed the issues, re-run this function with `validated=TRUE`.")
  }
  return(list(changeFlights=changeFlights, flightTifs=flightTifs))
}
identifyGapsMetrics <- function(X, targetDates, saveChangePath, thresholds, gdalOutDir, 
                                vecRemove=NULL, saveGapFiles, saveGapsPath){
  dateStart <- targetDates[X-1]
  dateEnd <- targetDates[X]
  print(paste0("Processing change between ", dateStart, " and ", dateEnd, 
              " at ", format(Sys.time())))

  ## load flight change rasters
  outPath <- gsub("D1", dateStart, saveChangePath)
  outPath <- gsub("D2", dateEnd, outPath)
  change <- rast(outPath)

  if(!is.null(vecRemove)) change <- mask(change, vecRemove, inverse=TRUE)

  # Step 1: Filter change raster to only retain cells that have experienced no greater
  ## than a `threshold` depth (height) change, then save
  print("Step 1/5 - Mask change raster based on gap magnitude threshold")
  rclMat <- matrix(c(-9999, thresholds$shortThreshMin, NA,
                      thresholdMin = thresholds$shortThreshMin, 
                          thresholdMax = thresholds$shortThreshMax, 1,
                      thresholdMax = thresholds$shortThreshMax, 9999, NA), 
                  nrow=3, byrow=TRUE)
  changeTif <- classify(change, rclMat, right=TRUE, include.lowest=TRUE)

  fileLab <- paste0(gdalOutDir, "/class", dateStart, "_", dateEnd, ".tif")
  writeRaster(changeTif, fileLab, overwrite=TRUE)

  # Step 2: Create patches by grouping cells that are surrounded by NA
  #         aka convert all continuous pixels into separate polygons
  ## using gdal is much faster than terra::patches()
  ### NOTE for future we can do 8-directions instead of 4 (default), just have to
  ### add in a "-8" flag in the gdal command
  print("Step 2/5 - Identify gap clusters using gdal_polygonize")
  outputFile <- gsub("class", "poly", fileLab)
  outputFile <- gsub(".tif", ".shp", outputFile)
  cmd <- paste0("gdal_polygonize.py ", fileLab, " ", outputFile, " -overwrite")
  system(cmd)

  # Step 3: Read in polygons, calculate area, and remove any that are below our min size threshold
  print("Step 3/5 - Remove gaps below size threshold")
  v <- vect(outputFile)
  v$area_m2 <- expanse(v, unit="m")
  v <- v[v$area_m2 >= thresholds$gapSizeMin]

  # Step 4: Calculate other gap metrics and build output table
  print("Step 4/5 - Calculate other gap metrics and build metrics table")
  ## polygon centroids + perim and area
  polyM <- as.data.table(crds(centroids(v)))
  colnames(polyM) <- c("centroidX", "centroidY")
  polyM$area_m2 <- v$area_m2
  polyM$perim_m <- perim(v)

  ## quantiles of spectral / structural values
  dt <- as.data.table(extract(change, v))
  colnames(dt) <- c("gapID", "gapChange")
  meanChange <- dt[, .(quant05_m=quantile(gapChange, 0.05, na.rm=TRUE),
                      quant25_m=quantile(gapChange, 0.25, na.rm=TRUE),
                      quant75_m=quantile(gapChange, 0.75, na.rm=TRUE),
                      quant95_m=quantile(gapChange, 0.95, na.rm=TRUE),
                      median_m = median(gapChange, 0.50, na.rm=TRUE),
                      mean_m = mean(gapChange, na.rm=TRUE)), 
                  by=.(gapID)]

  ## construct full table
  out <- cbind(meanChange, polyM, 
              data.table(dateStart=dateStart, dateEnd=dateEnd))

  # Step 5: Save files
  print("Step 5/5 - Save gap files if instructed")
  if(saveGapFiles){
      saveGapsPath <- gsub("D2", dateEnd, gsub("D1", dateStart, saveGapsPath))
      polyPath <- gsub("fileType", "polygons", gsub(".ext", ".shp", saveGapsPath))
      metricsPath <- gsub("fileType", "metrics", gsub(".ext", ".csv", saveGapsPath))

      writeVector(v, polyPath, overwrite=TRUE)
      fwrite(out, metricsPath)
  }
}
