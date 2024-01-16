##########################################################
## Purpose: Functions for processing point clouds
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## Contact: 
## System: R Version 4.2.2, Aug 2023 (edited)
## Last modified: Aug 2023
##########################################################
# --------------------------------------------------------------------#
## makeDSMs = make DSM from aligned lidar data using the lidR package
## createGrid = create grid for standardization
## standardizePC = standardize the point clouds
# --------------------------------------------------------------------#
createGrid <- function(tileSz){
  xmin <- 623400
  xmax <- 630100
  ymin <- 1009700
  ymax <- 1015200
  
  xmins <- seq(xmin,(xmax-tileSz),tileSz)
  xmaxs <- seq((xmin+tileSz),xmax,tileSz)
  ymins <- seq(ymin,(ymax-tileSz),tileSz)
  ymaxs <- seq((ymin+tileSz),ymax,tileSz)
  
  gridInfo <- data.frame(xmin=rep(xmins, length(ymins)),
                         xmax=rep(xmaxs, length(ymins)),
                         ymin=rep(ymins, each=length(xmins)),
                         ymax=rep(ymaxs, each=length(xmins)))
  
  gridInfo$Use <- NA
  return(gridInfo)
}
standardizePC <- function(gridN, gridInfo, catObj, overlap, dirPath, type, ROI=NULL){
  print(paste0("Processing tile ", gridN))
  data <- clip_rectangle(catObj, 
                         xleft = gridInfo$xmin[gridN] - overlap,
                         ybottom = gridInfo$ymin[gridN] - overlap,
                         xright = gridInfo$xmax[gridN] + overlap,
                         ytop = gridInfo$ymax[gridN] + overlap)

  if(type=="align"){
    data <- decimate_points(data, algorithm=highest(res=0.5))
  }
  
  if(!is.null(ROI)){
    data <- clip_roi(data,ROI)
  }

  if(length(data@data$X)>0){
    writeLAS(data, file=paste0(dirPath, "/cloud_", gridN,".laz"))
  }
  
  return(print(paste0("Finished tile ", gridN, " at ", 
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"))))
}
makeDSMs <- function(X, pointCloudPath, pathSave, crsProj, pathBuffer, plotDSM, 
                    saveDSM, returnDSM){
  
  print(paste0("Creating DSM for ", targetDates[X], " at ", format(Sys.time())))
  
  ## format the folder path and read in the catalog
  path <- gsub("DD", targetDates[X], pointCloudPath)
  
  catObj <- readLAScatalog(path)
  
  ## make the basic DSMs
  pathSave <- paste0(pathSave, "DSM_", targetDates[X], ".tif")
  outFile <- rasterize_canopy(catObj, res = 1,
                              algorithm = p2r(subcircle=0.01, 
                                              na.fill = tin()))

  ## need to assign CRS
  crs(outFile) <- crsProj

  ## resample to DEM to standardize
  dem <- rast("spatialData/bci/LidarDEM_BCI.tif")
  outFile <- resample(outFile, dem, method="cubic")
  
  if(plotDSM) plot(outFile, main=targetDates[[X]])
  if(saveDSM) writeRaster(outFile, filename=pathSave, overwrite=TRUE)

  ## mask DSM to shapefile boundary
  v <- project(vect(pathBuffer), crsProj)
  outFile <- mask(outFile, v)
  if(saveDSM){
    writeRaster(outFile, filename=gsub("5_dsm", "6_dsmMasked", pathSave),
                overwrite=TRUE)
  }

  print(paste0("Finished DSM for ", targetDates[X], " at ", format(Sys.time())))
  
  if(returnDSM){
    return(list(outFile = outFile, pathSave = pathSave))
  } else {
    return(print("Done!"))  
  }
}
