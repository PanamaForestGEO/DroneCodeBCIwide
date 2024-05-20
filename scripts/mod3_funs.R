##########################################################
## Purpose: Functions for post-alignment script S5_DefineGaps.R
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## System: R Version 4.2.2, Aug 2023 
##########################################################

# --------------------------------------------------------------------#
## loadOrtho = load orthomosaics as tif files in list
## createCHM = create canopy height models from DSMs
## indexFun = function for calculating a spectral index of the orthomosaics
## timeChange = difference the CHMs or index rasters from two flights
## processFlightDiff = calculate differences between two flights
## loadMasks = load mask polygon shapefiles
## identifyGapsMetrics = create gap polygons and calculate metrics
## defineGapsWrap = wrapper for the main workflow
# --------------------------------------------------------------------#
loadOrtho <- function(X, pathInput, resN){
  f <- list.files(pathInput, full.names=TRUE,
                         pattern=paste0(X, ".*Whole.*_crop.tif"))
  return(rast(f[grepl(paste0("stand", resN*100), f)])) 
  # return(rast(list.files(pathInput, full.names=TRUE,
  #                        pattern=paste0(X, ".*Subset.*masked.tif"))))
}
createCHM <- function(X, pathInput, demBCI, crsProj, changeType){
  print(paste0("Creating chm for ", X))
  
  ## Bring in dsm
  path <- list.files(pathInput, full.names=TRUE)
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
indexFun <- function(i, r, g, b){
  ## best results
  if(i=="exg") index <- (2*g)-r-b
  if(i=="exgr") index <- ((2*g)-r-b) - ((1.4*r)-g)
  if(i=="tgi") index <- 0.5*((670-480)*(r-g)-(670-550)*(r-b))
  
  ## tried indices, good results, still some noise
  if(i=="gcc") index <- g / (r+g+b)
  if(i=="gli") index <- ((2*g)-r-b) / ((2*g)+r+b)
  if(i=="rgri") index <- r/g
  if(i=="ngbdi") index <- (g-b) / (g+b)
  if(i=="mgrvi") index <- (g^2 - r^2) / (g^2 + r^2)
  
  ## tried indices, not great results
  if(i=="vari") index <- (g-r) / (g+r-b)
  if(i=="ngrdi") index <- (g-r) / (g+r)
  
  if(i=="cos"){
    ## rgb for target color green
    ### see `gapIndex.R` for how target was calculated
    R <- 182
    G <- 203
    B <- 106
    index <- (R*r+G*g+B*b)*((R*r+G*g+B*b)) / ((R*R+G*G+B*B)*(r*r+g*g+b*b))
  }
  
  return(index)
}
timeChange <- function(X, flightTifs, changeType, saveChange, savePath, 
                       indexName=""){
  dateStart <- gsub("^.*_", "", names(flightTifs)[X-1])
  dateEnd <- gsub("^.*_", "", names(flightTifs)[X])
  
  if(changeType=="structural"){
    change <- flightTifs[[X]] - flightTifs[[(X-1)]]
  } else if(changeType=="spectral"){
    print(paste0("Calculating spectral index ", indexName, " for ", dateStart, 
                 " versus ", dateEnd, " at ", format(Sys.time())))

    # tic()
    focal <- lapply(1:2, function(w){
      if(w==1) tif <- flightTifs[[(X-1)]] else tif <- flightTifs[[X]]

      r <- tif[[1]]
      g <- tif[[2]]
      b <- tif[[3]]
      tif <- indexFun(indexName, r, g, b)
      return(tif)
    })
    # toc()
    
    # print*********************************************************
    change <- focal[[2]] - focal[[1]]
  }
  
  if(saveChange){
    outPath <- gsub("D1", dateStart, saveChangePath)
    outPath <- gsub("D2", dateEnd, outPath)
    writeRaster(change, outPath, overwrite=TRUE)
  }
  
  return(change)
}
processFlightDiff <- function(targetDates, changeType, pathInput,
                              demBCI, crsProj, saveChange, 
                              saveChangePath, resN, indexName,
                              validated=FALSE, applyBufferMask=FALSE){
  
  if(changeType=="structural"){
    # 1. Create canopy height models for each flight DSM
    ## ~10 sec per DSM 
    flightTifs <- lapply(targetDates, createCHM, pathInput, demBCI, crsProj, 
                        changeType)
  } else if(changeType=="spectral"){
    flightTifs <- lapply(targetDates, loadOrtho, pathInput, resN)

    # 1. if applyBufferMask=TRUE, crop the full orthophotos in the same way as 
    #   for the point cloud prior to calculating change (and then saved to 
    #   the `path` directory). Note that this takes a long time, but (in 
    #   addition to standardizing both processes) can save some time when 
    #   identifying gaps later.
    ##  If it's FALSE, it means they are already saved, and thus are just 
    ##  loaded as in below.
    # if(applyBufferMask){
    #   files <- list.files(pathInput, pattern="BCI.tif", full.names=TRUE)
    #   flightTifs <- lapply(files, bufferMask, pathAge, bufferPath, crsProj,
    #                        changeType)
    # }
  }
  names(flightTifs) <- paste0("res", resN*100, "_", targetDates)
  
  # 2. Create change rasters between successive flights
  ## if CHMs (structural), this is a simple subtraction of previous from current flight
  ## if orthos (spectral), you first calculate a spectral RGB index and then difference
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
loadMasks <- function(dateStart, dateEnd, maskPath, buildingPath){
  f <- list.files(maskPath, pattern=".shp", full.names=TRUE)
  f <- f[grepl(paste0(c(dateStart, dateEnd), collapse="|"), f)]

  maskList <- lapply(f, vect)

  if(length(maskList) > 0){
    names(maskList) <- paste0("x", gsub(".*mask_", "", gsub(".shp", "", f)))
  }

  ## BCI building outlines
  maskList$buildings <- vect(buildingPath)

  return(maskList)
}
identifyGapsMetrics <- function(X, targetDates, saveChangePath, thresholds, gdalOutDir, 
                                maskPath, buildingPath, saveGapFiles, saveGapsPath){
  dateStart <- targetDates[X-1]
  dateEnd <- targetDates[X]
  print(paste0("Processing change between ", dateStart, " and ", dateEnd, 
              " at ", format(Sys.time())))

  ## load flight change rasters
  outPath <- gsub("D1", dateStart, saveChangePath)
  outPath <- gsub("D2", dateEnd, outPath)
  change <- rast(outPath)

  threshInfo <- paste0("_a", thresholds$gapSizeMin, 
                        "d", thresholds$shortThreshMax*-1)

  # Step 1: Mask the polygons by buildings and anomalous areas if present
  print("Step 1/6 - Mask change raster based on ortho/lidar anomalies")
  masks <- loadMasks(dateStart, dateEnd, maskPath, buildingPath)
  masksAll <- vect(masks)
  change <- mask(change, masksAll, inverse=TRUE)

  # Step 2: Filter change raster to only retain cells that have experienced no greater
  ## than a `threshold` depth (height) change, then save
  print("Step 2/6 - Mask change raster based on gap magnitude threshold")
  rclMat <- matrix(c(-9999, thresholds$shortThreshMin, NA,
                      thresholds$shortThreshMin, thresholds$shortThreshMax, 1,
                      thresholds$shortThreshMax, 9999, NA), 
                  nrow=3, byrow=TRUE)
  changeTif <- classify(change, rclMat, right=TRUE, include.lowest=TRUE)

  fileLab <- paste0(gdalOutDir, "/class", dateStart, "_", dateEnd, ".tif")
  writeRaster(changeTif, fileLab, overwrite=TRUE)

  # Step 2: Create patches by grouping cells that are surrounded by NA
  #         aka convert all continuous pixels into separate polygons
  ## using gdal is much faster than terra::patches()
  ### NOTE for future we can do 8-directions instead of 4 (default), just have to
  ### add in a "-8" flag in the gdal command
  print("Step 3/6 - Identify gap clusters using gdal_polygonize")
  outputFile <- gsub("class", "poly", fileLab)
  outputFile <- gsub(".tif", ".shp", outputFile)

  outputFile <- gsub(".shp", paste0(threshInfo, ".shp"), outputFile)

  cmd <- paste0("gdal_polygonize.py ", fileLab, " ", outputFile, " -overwrite")
  system(cmd)

  # Step 3: Read in polygons, calculate area, and remove any that are below our min size threshold
  print("Step 4/6 - Remove gaps below size threshold")
  v <- vect(outputFile)
  v$area_m2 <- expanse(v, unit="m")
  v <- v[v$area_m2 >= thresholds$gapSizeMin]

  # Step 4: Calculate other gap metrics and build output table
  print("Step 5/6 - Calculate other gap metrics and build metrics table")
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

  ## add gap ID back to the polygon shapefile as a column
  v$gapID <- out$gapID

  # Step 5: Save files
  print("Step 6/6 - Save gap files if instructed")
  if(saveGapFiles){
    ## carry through the threshold info to the file names
    saveGapsPath <- gsub(".ext", paste0(threshInfo, ".ext"), saveGapsPath)
    saveGapsPath <- gsub("D2", dateEnd, gsub("D1", dateStart, saveGapsPath))
    polyPath <- gsub("fileType", "polygons", gsub(".ext", ".shp", saveGapsPath))
    metricsPath <- gsub("fileType", "metrics", gsub(".ext", ".csv", saveGapsPath))

    writeVector(v, polyPath, overwrite=TRUE)
    fwrite(out, metricsPath)
  }

  print(paste0("Finished processing change between ", dateStart, " and ", dateEnd, 
              " at ", format(Sys.time())))

  print("Finished with gaps hoo-rah")
  return(list(gapPoly = v, gapMetrics = out))
}
defineGapsWrap <- function(targetDates, changeType, pathInput, demBCI, crsProj, 
                          saveChange=TRUE, savePath, resN, indexName, validated, 
                          saveChangePath, thresholds, gdalOutDir, maskPath, 
                          buildingPath, saveGapFiles, saveGapsPath, runType){
  ## ------------------------------------------------- ##
  # A. Process difference between flights
  ## for "chm" changeType, create canopy height models and calculate height 
  ## change btwn flights
  ## for "ortho" changeType, calculate change btwn RGB index of flights
  ### Please see comments in the function for applyBufferMask argument

  if(runType %in% c("change", "all")){
    changeRasters <- processFlightDiff(targetDates, changeType, pathInput, demBCI, crsProj, 
                                      saveChange=TRUE, savePath, resN, indexName,
                                      validated, applyBufferMask=FALSE)
    if(!validated){
      print("Inspect height change rasters and draw polygons as needed")
      return(changeRasters)
    }
  }

  ## ------------------------------------------------- ##
  # C. Identify gaps, calculate metrics, and save outputs
  if(runType %in% c("gaps", "all")){
    gapOutputs <- sapply(2:length(targetDates), identifyGapsMetrics, targetDates, 
                      saveChangePath, thresholds, gdalOutDir, maskPath, buildingPath, saveGapFiles, 
                      saveGapsPath)
    return(gapOutputs)
  }
}