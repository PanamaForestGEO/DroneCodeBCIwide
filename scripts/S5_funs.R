##########################################################
## Purpose: Functions for post-alignment script 2_DefineGaps.R
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## Contact: 
## System: R Version 4.2.2, Aug 2023 (edited)
## Last modified: Aug 2023
##########################################################

# --------------------------------------------------------------------#
## applyQAQC = apply QAQC checks (primarily for flights < 2020)
## createCHM = create canopy height models from DSMs
## getForestGaps = adapted from ForestGapR::getForestGaps() for terra package
## identifyGaps = calculate canopy height change and identify where gaps are
## loadGapFiles = load necessary gap files
## gapPolyMetrics = create gap polygons and calculate metrics
# --------------------------------------------------------------------#
loadOrtho <- function(X, pathData, resN){
  f <- list.files(pathData, full.names=TRUE,
                         pattern=paste0(X, ".*Whole.*_crop.tif"))
  return(rast(f[grepl(paste0("stand", resN*100), f)])) 
  # return(rast(list.files(pathData, full.names=TRUE,
  #                        pattern=paste0(X, ".*Subset.*masked.tif"))))
}
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
  } else if(changeType=="ortho"){
    print(paste0("Calculating spectral index ", indexName, " for date ", X, 
                 " versus date ", X-1))
    focal <- lapply(1:2, function(w){
      if(w==1) tif <- flightTifs[[(X-1)]] else tif <- flightTifs[[X]]
      tif <- indexFun(indexName, r=tif[[1]], g=tif[[2]], b=tif[[3]])
      return(tif)
    })
    
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
processFlightDiff <- function(targetDates, changeType, pathData,
                              demBCI, crsProj, saveChange, 
                              saveChangePath, resN, indexName,
                              validated=FALSE, applyBufferMask=FALSE){
  
  if(changeType=="structural"){
    # 1. Create canopy height models for each flight DSM
    ## ~10 sec per DSM 
    flightTifs <- lapply(targetDates, createCHM, pathData, demBCI, crsProj, 
                        changeType)
  } else if(changeType=="ortho"){
    # 1. if applyBufferMask=TRUE, crop the full orthophotos in the same way as 
    #   for the point cloud prior to calculating change (and then saved to 
    #   the `path` directory). Note that this takes a long time, but (in 
    #   addition to standardizing both processes) can save some time when 
    #   identifying gaps later.
    ##  If it's FALSE, it means they are already saved, and thus are just 
    ##  loaded as in below.
    if(applyBufferMask){
      files <- list.files(pathData, pattern="BCI.tif", full.names=TRUE)
      flightTifs <- lapply(files, bufferMask, pathAge, bufferPath, crsProj,
                           changeType)
    } else {
      flightTifs <- lapply(targetDates, loadOrtho, pathData, resN)
    }
  }
  names(flightTifs) <- paste0("res", resN*100, "_", targetDates)
  
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
getForestGaps <- function(changeTif, thresholdMin, thresholdMax, size, 
                          directions){
  ## this function comes from ForestGapR package. The annotations below are
  ## added and not part of the original code.
  
  # Step 1: Filter CHM to only retain cells that have experienced no greater
  ## than a `threshold` depth (height) change
  print("Step 1/5 - Mask change raster based on gap magnitude threshold")
  rclMat <- matrix(c(-9999, thresholdMin, NA,
                     thresholdMin, thresholdMax, 1,
                     thresholdMax, 9999, NA), nrow=3, byrow=TRUE)
  changeTif <- classify(changeTif, rclMat, right=TRUE, include.lowest=TRUE)
  
  # changeTif <- crop(changeTif, p) # from earlier code
  
  # Step 2: Create patches by grouping cells that are surrounded by NA
  print("Step 2/5 - Creating initial patch grouping")
  gaps <- patches(changeTif, directions, allowGaps = FALSE)
  
  # Step 3: Combine the cells with the same patch number into a single block
  ## indicating the area of the patch. We first get the frequency of each patch
  ## number, then multiply that frequency by the area of a single cell. Finally,
  ## we assign that value to those cells.
  print("Step 3/5 - Combining cells with same patch # into single block.")
  rclMat <- freq(gaps)[, 2:3]
  rclMat[, 2] <- rclMat[, 2] * res(changeTif)[1]^2
  z <- classify(gaps, rcl = rclMat, right = NA)
  
  # Step 4: We now use the gap area raster to cross-reference with the first
  ## gap raster in 2 sub-steps:
  ## - mask so that both have the same NA
  ## - filter the original gap raster to exclude gaps whose areas are not
  ### within the desired range
  
  print(paste0("Step 4/5 - Masking gap raster to avoid patches outside ", 
               "defined threshold"))
  
  ## then change cells to NA if outside of defined size range
  gapsMask <- ifel(z > size[2] | z < size[1], NA, z)
  
  
  # Step 5: Now create the patches again, but this time acting on only the
  ## masked and filtered pixels by gap area
  print("Step 5/5 - Creating updated patches with the filtered and masked rast")
  gapsMask <- patches(gapsMask, directions, allowGaps = FALSE)
  names(gapsMask) <- "gaps"
  return(gapsMask)
}
identifyGaps <- function(X, targetDates, changeRasters, saveChangePath, 
                         thresholds, returnGaps, saveGaps, 
                         saveGapsPath, vecRemove=NULL){
  dateStart <- targetDates[X-1]
  dateEnd <- targetDates[X]
  print(paste0("Processing change between ", dateStart, " and ", dateEnd, 
               " at ", format(Sys.time())))
  
  ## load flight change rasters
  outPath <- gsub("D1", dateStart, saveChangePath)
  outPath <- gsub("D2", dateEnd, outPath)
  change <- rast(outPath)
  
  if(!is.null(vecRemove)) change <- mask(change, vecRemove, inverse=TRUE)

  # Identify gaps (takes a bit of time) that were at least as big of a 
  ## change as shortThresh
  print(paste0("Identifying forest gaps at ", format(Sys.time())))
  gaps <- getForestGaps(change, thresholdMin = thresholds$shortThreshMin,
                        thresholdMax = thresholds$shortThreshMax,
                        size=c(thresholds$gapSizeMin, thresholds$gapSizeMax), 
                        directions = thresholds$directions)
  
  print(paste0("Finished processing change between ", dateStart, " and ", 
               dateEnd, " at ", format(Sys.time())))
  
  if(saveGaps){
    outPath <- gsub("D1", dateStart, saveGapsPath)
    outPath <- gsub("D2", dateEnd, outPath)
    
    writeRaster(gaps, outPath, overwrite=TRUE)
  }
  
  if(returnGaps) return(gaps) else return(print("Done!"))
}
loadGapFiles <- function(dateStart, dateEnd, saveGapsPath, saveChangePath){
  saveGapsPath <- gsub("D1", dateStart, gsub("D2", dateEnd, saveGapsPath))
  saveChangePath <- gsub("D1", dateStart, gsub("D2", dateEnd, saveChangePath))
  
  gaps <- rast(saveGapsPath)
  change <- rast(saveChangePath)
  return(list(rastChange = change, canopyGaps = gaps, gapsPath=saveGapsPath))
}
gapPolyMetrics <- function(X, targetDates, saveGapsPath, saveChangePath,
                           returnAll){
  # Load gap and CHM change files for the selected date comparison
  dateStart <- targetDates[X-1]
  dateEnd <- targetDates[X]
  
  print(paste0("Calculating metrics for gaps between ", dateStart, " and ",
               dateEnd))
  rastFiles <- loadGapFiles(dateStart, dateEnd, saveGapsPath, saveChangePath)
  
  # Calculate mean gap change only across the gaps.
  ## i.e., don't look at change over non-gaps (where gapID is NaN)
  ## because the rasters are so large, the code below is a workaround to just
  ## doing values(r)[!is.na(values(r))].
  rastFiles$rastChange <- mask(rastFiles$rastChange, rastFiles$canopyGaps)
  changeVals <- rastFiles$rastChange[cells(rastFiles$rastChange)]
  gapIDs <- rastFiles$canopyGaps[cells(rastFiles$canopyGaps)]
  
  dt <- as.data.table(cbind(gapIDs, changeVals)); rm(changeVals, gapIDs)
  colnames(dt) <- c("gapID", "gapChange")
  meanChange <- dt[, .(quant05_m=quantile(gapChange, 0.05, na.rm=TRUE),
                     quant25_m=quantile(gapChange, 0.25, na.rm=TRUE),
                     quant75_m=quantile(gapChange, 0.75, na.rm=TRUE),
                     quant95_m=quantile(gapChange, 0.95, na.rm=TRUE),
                     median_m = median(gapChange, 0.50, na.rm=TRUE),
                     mean_m = mean(gapChange, na.rm=TRUE)), 
                   by=.(gapID)]
  
  # Convert gap IDs to separate polygons
  ## adapted from GapSPDF in ForestGapR package
  gaps_poly <- terra::as.polygons(x=rastFiles$canopyGaps, dissolve=TRUE, 
                                  na.rm=TRUE, values=TRUE)
  
  # Get metadata metrics
  ## get centroids and add to mean height change
  gapsData <- as.data.table(crds(centroids(gaps_poly)))
  gapsData <- cbind(data.table(gapID=1:nrow(gapsData)), gapsData)
  setkeyv(meanChange, "gapID")
  setkeyv(gapsData, "gapID")
  gapsData <- gapsData[meanChange]
  
  ## redo colnames, add perimeter, add area
  setnames(gapsData, old=c("x", "y"), new=c("centroidX", "centroidY"))
  gapsData[, `:=` (perim_m = perim(gaps_poly),
                   area_m2 = expanse(gaps_poly),
                   dateStart = as.numeric(as.Date(dateStart, 
                                                  origin="1970-01-01")),
                   dateEnd = as.numeric(as.Date(dateEnd, origin="1970-01-01")))]
  
  # Save polygon vector and metrics table
  outPath <- rastFiles$gapsPath
  writeVector(gaps_poly, gsub("rasters", "polygons", 
                              gsub(".tif", ".shp", outPath)), overwrite=TRUE)
  fwrite(gapsData, gsub("rasters", "metrics", gsub(".tif", ".csv", outPath)))
  
  print(paste0("Done! Data has been saved to ", 
               gsub("rasters.*", "", saveGapsPath)))
  
  if(returnAll){
    return(list(gapsRaster = rastFiles$canopyGaps,
                gapsPolygons = gaps_poly,
                gapsMetrics = gapsData))
  } else {
    return(print("yay!"))
  }
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

  fileLab <- gsub(paste0("change", dateStart), paste0("class", dateStart), outPath)
  fileLab <- gsub("changeIndex", gdalOutDir, fileLab)
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
              data.table(dateStart=as.numeric(as.Date(dateStart)), 
                          dateEnd=as.numeric(as.Date(dateEnd))))

  # Step 5: Save files
  print("Step 5/5 - Save gap files if instructed")
  if(saveGapFiles){
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
