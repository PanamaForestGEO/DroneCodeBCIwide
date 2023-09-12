##########################################################
## Purpose: Functions for post-alignment script 1_MakeDMS.R
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## Contact: 
## System: R Version 4.2.2, Aug 2023 (edited)
## Last modified: Aug 2023
##########################################################
# --------------------------------------------------------------------#
## prepMakeDSM = wrapper to prep and format paths, then run makeDSMs
## makeDSMs = make DSM from aligned lidar data using the lidR package
## applyCloudMask = apply cloud masks to filter rasters
## applyQAQCMask = apply qaqc filtering
# --------------------------------------------------------------------#

prepMakeDSM <- function(targetType, targetDates, qaqcType=NULL, pathList,
                        crsProj, plotDSM, saveDSM, returnDSM){
  dronePath <- pathList$pathDrone
  
  if(targetType=="trim"){
    folderPath <- paste0(dronePath, pathList$pathTrim)
  } else if(targetType %in% c("phtgrm", "qaqc")){
    folderPath <- paste0(dronePath, pathList$pathPhtQA)
  } else if(targetType=="raw"){
    folderPath <- paste0(dronePath, pathList$pathRaw)
  }
  
  if(targetType=="qaqc" & is.null(qaqcType)){
    stop("Must define qaqcType! Value is either '1m' or '0.2m'.")
  }
  
  print(paste0("Started processing at ", format(Sys.time())))
  
  dsmFiles <- lapply(1:length(targetDates), makeDSMs, targetType, 
                     targetDates, qaqcType, folderPath, pathList, crsProj,
                     plotDSM, saveDSM)
  
  print(paste0("Finished processing at ", format(Sys.time())))
  
  if(returnDSM) return(dsmFiles) else return(print("Done!"))
}
makeDSMs <- function(X, targetType, targetDates, qaqcType, folderPath, 
                     pathList, crsProj, plotDSM, saveDSM){
  
  print(paste0("Creating DSM for type ", targetType, " for ", targetDates[X],
               " at ", format(Sys.time())))
  
  ## format the folder path and read in the catalog
  # yr <- sprintf("%02d", targetDates[X] %% 100) #for original folder paths
  path <- gsub("DD", targetDates[X], folderPath)
  
  ## for original folder paths
  # if(targetType %in% c("phtgrm", "raw", "qaqc")){
  #   prevYr <- ifelse(targetDates[X]==2018, 3,
  #                    ifelse(targetDates[X]==2020, 2, 1))
  #   if(targetDates[X]!=2015){
  #     path <- gsub("alignedTrim", 
  #                  paste0("alignedto", as.numeric(yr)-as.numeric(prevYr), 
  #                         "Trim"),
  #                  path)
  #   }
  # }
  
  catObj <- readLAScatalog(path)
  
  ## make the basic DSMs
  if(targetType %in% c("trim", "phtgrm", "raw")){
    pathSave <- paste0(pathList$pathDSM, "DSM_", targetDates[X], ".tif")
    
    if(targetType=="trim"){
      outFile <- rasterize_canopy(catObj, res = 1,
                              algorithm = p2r(subcircle=0.01))
    } else {
      outFile <- rasterize_canopy(catObj, res = 1,
                              algorithm = p2r(subcircle=0.01, 
                                              na.fill = tin()))
      pathSave <- ifelse(targetType=="phtgrm", 
                         gsub(".tif", "_corrected.tif", pathSave),
                         gsub(".tif", "_raw.tif", pathSave))
    }
  }
  
  ## make QAQC DSMs and save (if called for)
  if(targetType=="qaqc"){
    pathSave <- paste0(pathList$pathsDSMqa, "desc_", targetDates[X], ".tif")
    
    if(qaqcType=="1m"){
      outFile <- rasterize_canopy(catObj, res = 1,
                                   algorithm = p2r(subcircle=0.01))
      pathSave <- gsub("desc", "DSM_noGapFill", pathSave)
    } else if(qaqcType=="0.2m"){
      outFile <- pixel_metrics(catObj, res = 0.2, func = ~max(Z)-min(Z))
      pathSave <- gsub("desc", "htRangeRaster", pathSave)
    }
  }
  
  ## need to assign CRS
  crs(outFile) <- crsProj
  
  if(plotDSM) plot(outFile, main=targetDates[[X]])
  if(saveDSM) writeRaster(outFile, filename=pathSave, overwrite=TRUE)
  
  return(list(outFile = outFile, pathSave = pathSave))
}
applyCloudMask <- function(X, folderPath, bufferPath, thresh, plotMask){
  # 1. Read in and define data
  bufferFile <- project(vect(bufferPath), "epsg: 32617") #UTM zone 17 WGS84
  
  tifAlbedo <- rast(paste0(folderPath, "CloudMasks/Min_Albedo_", X, ".tif"))
  tifAlbedo <- mask(tifAlbedo, bufferFile)
  
  tifRedBlu <- rast(paste0(folderPath, "CloudMasks/Max_RBdiff_", X, ".tif"))
  tifRedBlu <- mask(tifRedBlu, bufferFile)
  
  thresh_albedo <- thresh[year==X, albedo]
  thresh_redblu <- thresh[year==X, redblu]
  
  # 2. Define the pixels that should be masked with different values
  ### these cells should have values of 0 only if both conditions are met
  cond1 <- cells(tifAlbedo >= thresh_albedo, 1)$Band_1
  cond2 <- cells(tifRedBlu <= thresh_redblu, 1)$Band_1
  pix0 <- cond2[cond2 %in% cond1]
  
  ### these cells should have values of 1 in the mask (all)
  pix1 <- c(cells(tifAlbedo < thresh_albedo, 1)$Band_1,
            cells(tifRedBlu > thresh_redblu, 1)$Band_1)
  pix1 <- unique(pix1)
  
  # 3. Create empty raster and assign values
  maskRaster <- rast(ext(tifAlbedo), resolution=res(tifAlbedo), 
                     crs=crs(tifAlbedo), vals=NA)
  maskRaster[pix0] <- 0
  maskRaster[pix1] <- 1
  
  if(plotMask) plot(maskRaster, main=X)
  
  return(maskRaster)
}
applyQAQCMask <- function(X, folderPath, plotMask){
  
  ## Criteria 1: range of heights within 0.2 m pixels
  r_htRange <- rast(paste0(folderPath, "htRangeRaster_", X, ".tif"))
  r_htRange_ag <- aggregate(r_htRange, fact = 50, fun=median, na.rm=TRUE)
  
  ## Criteria 2: number of non-empty cells
  r_DSM <- rast(paste0(folderPath, "DSM_noGapFill_", X, ".tif"))
  r_DSM[!is.na(r_DSM)] <- 1
  r_DSM[is.na(r_DSM)] <- 0
  r_DSM_ag1 <- aggregate(r_DSM, fact=10, fun=sum, na.rm=TRUE)
  
  ## Make binary map
  thresh1 <- 1.5
  thresh2 <- 90
  
  ## Bad pixels, value = 0
  pix0 <- c(cells(r_htRange_ag >= thresh1),
            cells(r_DSM_ag <= thresh2))
  pix0 <- unique(pix0)
  pix0 <- pix0[pix0 >= 1] #randomly, r_DSM_ag returns cell # of 0
  
  ## Good pixels, value = 1
  cond1 <- cells(r_htRange_ag < thresh1)
  cond2 <- cells(r_DSM_ag > thresh2)
  pix1 <- cond2[cond2 %in% cond1]
  pix1 <- pix1[pix1 >= 1] #consistency with above
  
  # 3. Create empty raster and assign values
  maskRaster <- rast(ext(r_htRange_ag), resolution=res(r_htRange_ag), 
                     crs=crs(r_htRange_ag), vals=NA)
  maskRaster[pix0] <- 0
  maskRaster[pix1] <- 1
  
  if(plotMask){
    layout(matrix(1:3, nrow=1))
    plot(r_htRange_ag, main="Height", breaks=c(0,1.5,50), col=c("grey","red"))
    plot(r_DSM_ag, main="Binary", breaks=c(0,90,100), col=c("red","grey"))
    plot(maskRaster)
    layout(matrix(1:1))
  }
  
  return(maskRaster)
}