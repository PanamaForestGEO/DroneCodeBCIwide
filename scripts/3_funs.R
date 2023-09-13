##########################################################
## Purpose: Functions for post-alignment script 3_plotGapTS.R
##
## Creator: Ian McGregor
## Contact: mcgregori@caryinstitute.org
## System: R Version 4.3.1, Sep 2023
## Last modified: Sep 2023
##########################################################
loadGapData <- function(dateN, targetDates, dataType, dataPath){
  if(any(!(dataType %in% c("rasters", "polygons", "metrics")))){
    stop("dataType must be one or a combination of 'rasters', 'polygons', or 
         'metrics'")
  }
  
  dateStart <- targetDates[dateN-1]
  dateEnd <- targetDates[dateN]
  dateDesc <- paste0(dateStart, "_", dateEnd)
  
  if(any(grepl("rasters", dataType))){
    rPath <- list.files(paste0(dataPath, "rasters"), pattern=dateDesc)
    r <- rast(file.path(dataPath, "rasters", rPath))
  } else {
    r <- NA
  }
  
  if(any(grepl("polygons", dataType))){
    vPath <- list.files(paste0(dataPath, "polygons"), 
                        pattern=paste0(dateDesc, ".shp"))
    v <- vect(file.path(dataPath, "polygons", vPath))
  } else {
    v <- NA
  }
  
  if(any(grepl("metrics", dataType))){
    dtPath <- list.files(paste0(dataPath, "metrics"), pattern=dateDesc)
    dt <- fread(file.path(dataPath, "metrics", dtPath))
  } else {
    dt <- NA
  }
  
  return(list(gapsRaster = r, gapsPolygons = v, gapsMetrics = dt))
}
cropPlotOrtho <- function(ort, poly, vBuff){
  r <- rast(ort)
  vBuff <- project(vBuff, r)
  rCrop <- crop(r[[1:3]], vBuff)
  
  plotRGB(rCrop, mar=NA, axes=TRUE, main=gsub("_.*", "", names(rCrop)[1]))
  lines(poly, col="#66FFFF", lwd=1)
  return(rCrop)
}
checkTimeSeries <- function(id=NULL, xCoord=NULL, yCoord=NULL, flight, nImg, 
                            showPlots, savePNG, pngFolder, targetDates, 
                            dataPath, bufferN){
  if((!is.null(id) & (!is.null(xCoord) & !is.null(yCoord))) |
     (is.null(id) & (is.null(xCoord)|is.null(yCoord)))){
    stop("Must provide either id OR xy coords")
  }
  
  # 1. Load polygons and metrics
  gapData <- lapply(2:length(targetDates), loadGapData, targetDates,
                    dataType=c("polygons", "metrics"), dataPath)
  names(gapData[[1]]) <- c("empty", "poly", "dt")
  list2env(gapData[[1]], envir=.GlobalEnv)
  
  # 2. Define specific gap polygon for time series
  if(!is.null(id)){
    if(!id %in% dt$gapID) stop("Must choose valid gapID from metadata")
    
    ## if gapID defined, crop gap poly to that ID and put buffer of 3 meters
    v <- poly[values(poly)==id]
    vBuff <- buffer(v, 3) 
    
  }
  if(is.null(id)){
    v <- vect(matrix(c(xCoord, yCoord), ncol=2), crs=crsProj)
    b <- relate(ext(poly), v, "contains")
    
    if(!(b[,1])) stop("Provided xy point is not within extent of gap shpfile")
    
    # if xy defined, define a point buffer to be 5 m (may need to change later)
    vBuff <- buffer(v, 10)
  }
  
  # 3. Loop over each orthophoto, load, crop, plot, and save to png if wanted
  orthoPhotos <- list.files(orthoPath, pattern=".tif", full.names=TRUE)
  datesOrtho <- gsub("_.*", "", orthoPhotos)
  datesOrthoIndex <- which(grepl(as.character(flight), datesOrtho))
  
  if(datesOrthoIndex <= nImg){
    orthoPhotos <- orthoPhotos[1:datesOrthoIndex]
    datesOrtho <- datesOrtho[1:datesOrthoIndex]
  } else {
    orthoPhotos <- orthoPhotos[(datesOrthoIndex - nImg):datesOrthoIndex]
    datesOrtho <- datesOrtho[(datesOrthoIndex - nImg):datesOrthoIndex]
  }
  
  
  ## if you do NOT want to show axes, add `xaxt="n", yaxt="n"` to `par`
  ### to add back in after removing, need to run `par(xaxt="s", yaxt="s")`
  
  if(showPlots){
    par(mfrow=c(2,3), mar=rep(1,4), oma=rep(1,4))
    orthoClips <- lapply(orthoPhotos, cropPlotOrtho, poly, vBuff)
    names(orthoClips) <- paste0("x", datesOrtho) #necessary only if returned
  }
  
  if(savePNG){
    pngFolder <- paste0(pngFolder, "/", flight)
    if(!dir.exists(pngFolder)) dir.create(pngFolder)
    
    pngFile <- ifelse(!is.null(id), 
                      paste0(pngFolder, "/gap", id, "_", flight, ".png"),
                      paste0(pngFolder, "/gap_", "x", xCoord, "_y", yCoord, "_", 
                             flight, ".png"))
    png(pngFile, width=16, height=12, units="cm", res=350)
    
    par(mfrow=c(2,3), mar=rep(1,4), oma=rep(1,4))
    orthoClips <- lapply(orthoPhotos, cropPlotOrtho, poly, vBuff)
    dev.off()
    
    print(paste0("Image sequence saved to ", pngFile))
  }
  
  return(print("Done!"))
}
