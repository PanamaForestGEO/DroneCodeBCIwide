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

applyQAQC <- function(qaqcType, maskPath, X, chm){
  if(qaqcType=="cloud"){
    rMask <- rast(paste0(maskPath, "CloudMasks/CloudMask_", X, ".tif"))
  } else if(qaqcType=="qaqc"){
    rMask <- rast(paste0(maskPath, "QAQCMask_", X, ".tif"))
  }
  
  rMask <- resample(rMask, chm)
  chm[!(rMask > 0.99)] <- NA
  
  if(X==2015){
    stop("Need to update code specifically for 2015 if want to run this")
    vals09 <- raster::values(chm09)
    vals15 <- raster::values(chm15)
    vals18 <- raster::values(chm18)
    
    toChange <- which((vals15-vals09) > 5 & (vals18-vals15) < -1)
    newVals <- vals09[toChange]
    chm15c <- chm15
    raster::values(chm15c)[toChange] <- newVals
    
    raster::writeRaster(chm15, "Data_HeightRasters/CHM_2015_QAQC_wBias.tif")
    raster::writeRaster(chm15c, "Data_HeightRasters/CHM_2015_QAQC.tif")
    raster::writeRaster(chm18, "Data_HeightRasters/CHM_2018_QAQC.tif")
    raster::writeRaster(chm20, "Data_HeightRasters/CHM_2020_QAQC.tif")
  }
  
  return(chm)
}
createCHM <- function(X, pathData, pathAge, bufferPath, demBCI, maskPath, 
                      crsProj){
  print(paste0("Creating chm for ", X))
  
  path <- gsub("Y", X, pathData)
  path <- ifelse(X != 2009, gsub(".tif", "_corrected.tif", path), path)
  
  dsm <- rast(path)
  if(length(crs(dsm))==0) crs(dsm) <- "epsg: 32617"
  age <- vect(pathAge)
  
  ## add new data column to shapefile
  age$AgeClass <- ifelse(age$Mascaro_Co == "> 400", "OldGrowth",
                         ifelse(age$Mascaro_Co %in% c("80-110", "120-130"),
                                "Secondary", "Other"))
  ageUse <- age[!(age$AgeClass=="Other"),]
  
  ## bring in buffer
  bufferFile <- project(vect(bufferPath), crsProj) #UTM zone 17 WGS84
  
  ## for DSM, mask out buffer and clearings, then crop
  dsm <- mask(dsm, bufferFile)
  dsm <- mask(dsm, ageUse)
  dsm <- crop(dsm, ageUse)
  
  ## bring in BCI dem, crop to age shapefile, and resample based on DSM
  dem <- rast(demBCI)
  dem <- crop(dem, ageUse)
  dem <- resample(dem, dsm)
  
  ## create canopy height model by subtracting ground elevation
  ### note there may be some negative values - this is ok because it's relative
  ### to the prior lidar data
  chm <- dsm - dem
  
  ## bring in QAQC masks if needed
  # if(X %in% c(2015, 2018, 2020)){
  #   for(i in c("cloud", "qaqc")){
  #     chm <- applyQAQC(i, maskPath, X, chm)
  #   }
  # }
  
  return(chm)
}
getForestGaps <- function(chm_layer, threshold = 10, size = c(1, 10^4), 
                          directions = 8){
  chm_layer[chm_layer > threshold] <- NA
  chm_layer[chm_layer <= threshold] <- 1
  gaps <- terra::patches(chm_layer, directions, allowGaps = F)
  rcl <- terra::freq(gaps)
  rcl[, 3] <- rcl[, 3] * terra::res(chm_layer)[1]^2
  z <- terra::classify(gaps, rcl = rcl, right = NA)
  z[is.na(gaps)] <- NA
  gaps[z > size[2]] <- NA
  gaps[z < size[1]] <- NA
  gaps <- terra::patches(gaps, directions, allowGaps = F)
  names(gaps) <- "gaps"
  return(gaps)
}
identifyGaps <- function(X, chmFlights, shortMask, shortThresh, 
                         saveHeightChange, saveHeightPath, returnGaps,
                         saveGaps, saveGapsPath){
  dateStart <- gsub("x", "", names(chmFlights)[X-1])
  dateEnd <- gsub("x", "", names(chmFlights)[X])
  print(paste0("Processing change between ", dateStart, " and ", dateEnd, 
               " at ", format(Sys.time())))
  
  ## create canopy height change rasters.
  print(paste0("Making delta height rasters at ", format(Sys.time())))
  change <- chmFlights[[X]] - chmFlights[[(X-1)]]
  
  if(saveHeightChange){
    outPath <- gsub("D1", dateStart, saveHeightPath)
    outPath <- gsub("D2", dateEnd, outPath)
    writeRaster(change, outPath, overwrite=TRUE)
  }
  
  ## mask out any pixels that were not initially >= a threshold
  if(shortMask){
    rcl <- matrix(c(-9999, shortThresh, NA,
                    shortThresh, 9999, 1),
                  ncol=3, byrow=TRUE)
    pixRetain <- classify(chmYears[[X]], rcl, right=TRUE)
    deltaCHM <- mask(deltaCHM, pixRetain)
  }
  
  # Define gap height threshold, min gap size, and max gap size
  gapSzMin <- 25
  gapSzMax <- 10^6
  gapHtThresh <- -5
  directions <- 4
  
  # Identify gaps (takes a bit of time)
  print(paste0("Identifying forest gaps at ", format(Sys.time())))
  gaps <- getForestGaps(change, threshold = gapHtThresh, 
                        size=c(gapSzMin,gapSzMax), directions)
  
  print(paste0("Finished processing chqnge between ", dateStart, " and ", 
               dateEnd, " at ", format(Sys.time())))
  
  if(saveGaps){
    outPath <- gsub("D1", dateStart, saveGapsPath)
    outPath <- gsub("D2", dateEnd, outPath)
    if(shortMask){
      outPath <- gsub("gaps", "gapsShortMask", outPath)
    }
    
    writeRaster(gaps, outPath, overwrite=TRUE)
  }
  
  if(returnGaps) return(gaps) else return(print("Done!"))
}
loadGapFiles <- function(dateStart, dateEnd, saveGapsPath, saveHeightPath){
  saveGapsPath <- gsub("D1", dateStart, gsub("D2", dateEnd, saveGapsPath))
  saveHeightPath <- gsub("D1", dateStart, gsub("D2", dateEnd, saveHeightPath))
  
  gaps <- rast(saveGapsPath)
  change <- rast(saveHeightPath)
  return(list(chmChange = change, canopyGaps = gaps, gapsPath=saveGapsPath))
}
gapPolyMetrics <- function(X, targetDates, saveGapsPath, saveHeightPath,
                           returnAll){
  # Load gap and CHM change files for the selected date comparison
  dateStart <- targetDates[X-1]
  dateEnd <- targetDates[X]
  
  print(paste0("Calculating metrics for gaps between ", dateStart, " and ",
               dateEnd))
  rastFiles <- loadGapFiles(dateStart, dateEnd, saveGapsPath, saveHeightPath)
  
  # Calculate mean height change only across the gaps.
  ## i.e., don't look at height change over non-gaps (where gapID is NaN)
  dt <- data.table(values(rastFiles$canopyGaps),
                   values(rastFiles$chmChange))
  colnames(dt) <- c("gapID", "heightChange")
  meanChange <- dt[!is.na(gapID), 
                   .(quant05_m=quantile(heightChange, 0.05, na.rm=TRUE),
                     quant25_m=quantile(heightChange, 0.25, na.rm=TRUE),
                     quant75_m=quantile(heightChange, 0.75, na.rm=TRUE),
                     quant95_m=quantile(heightChange, 0.95, na.rm=TRUE),
                     median_m = median(heightChange, 0.50, na.rm=TRUE),
                     mean_m = mean(heightChange, na.rm=TRUE)), 
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
                   dateStart = as.numeric(dateStart),
                   dateEnd = as.numeric(dateEnd))]
  
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