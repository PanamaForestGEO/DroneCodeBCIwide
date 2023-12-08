##########################################################
## Purpose: Functions for 3_standardizeOrtho.R
##
## Creator: Ian McGregor
## Contact: mcgregori@caryinstitute.org
## System: R Version 4.3.1, Oct 2023 (edited)
## Last modified: ~ Oct 2023
##########################################################

# --------------------------------------------------------------------#
## createComonExt = create common min extent for orthophotos
## standardizeOrthos = align orthos to common ext, origin, and resolution
## bufferMask = mask orthos to BCI boundary and forest age classes
# --------------------------------------------------------------------#
createCommonExt <- function(pathExt, shpName, pathBuffer, overwrite){
  # We are simply using the immediate bounds of the contiguous island. To do
  # so, we need to bring in the buffered boundary, and add 25m back to the 
  # extent on all sides, then save as a minimum common extent.
  
  fullPath <- paste0(pathExt, shpName)
  if(file.exists(fullPath) & !overwrite){
    return(vect(fullPath))
  } else {
    bound <- project(vect(pathBuffer), crsProj)
    extCommon <- ext(bound) + 25 #add 25m to all sides
    pol <- as.polygons(extCommon)
    writeVector(pol, paste0(pathExt, shpName), 
                overwrite=TRUE)
    return(pol)
  }
}
loadData <- function(targetDates, pathInput, changeType){
  # Step 1. Read in the orthomosaics
  f <- list.files(pathInput, full.names=TRUE)

  if(changeType=="ortho") f <- f[grepl(".*Whole.*", f)]
  f <- f[grepl(paste0(targetDates, collapse="|"), f)]
  inputs <- lapply(f, function(X) return(rast(X)))
  names(inputs) <- paste0("x", targetDates)

  return(list(f=f, inputs=inputs))
}
filterAge <- function(pathAge, pathAgeFilt){
  
  if(file.exists(pathAgeFilt)){
    return(print("File already exists, no updates made."))
  } else {
    age <- vect(pathAge)
    
    ## add new data column to shapefile
    age$AgeClass <- ifelse(age$Mascaro_Co == "> 400", "OldGrowth",
                           ifelse(age$Mascaro_Co %in% c("80-110", "120-130"),
                                  "Secondary", "Other"))
    ageUse <- age[!(age$AgeClass=="Other"),]
    
    writeVector(ageUse, pathAgeFilt)
    return(print("Done! Age classes filtered and new shapefile saved"))
  }
}
standardizeOrthos <- function(X, f, orthos, pathInput, pathStandard, 
                              extCommon, resMin){
  print(paste0("Transforming orthophoto for ", names(orthos)[X]))
  
  outPath <- gsub(paste0(pathInput, "/"), pathStandard, f[X])
  outPath <- gsub(".tif", paste0("_stand", resMin*100, ".vrt"), outPath)
  script <- gdalwarp(srcfile = f[X], 
           dstfile=outPath, t_srs="EPSG:32617",
           te=c(extCommon[1], extCommon[3], extCommon[2], extCommon[4]),
           tr=c(resMin, resMin), tap=TRUE, r="cubic", overwrite=TRUE,
           dryrun=TRUE)
  system(script)
  
  return(print("Done"))
}
bufferMask <- function(p, pathMask, pathAgeFilt, pathBuffer, crsProj, 
                       changeType){
  print(paste0("Masking and cropping ", p))
  
  # if(length(crs(r))==0) crs(dsm) <- "epsg: 32617"

  ## for both ortho and DSM, clip to buffered boundary (output = tif file)
  if(changeType=="ortho"){
    outPathBuffer <- gsub(".vrt", "_crop.tif", p)
    outPath <- gsub("2_standardized", pathMask, outPathBuffer)
  } else if(changeType=="structural"){
    outPath <- gsub(".tif", "_crop.tif", p)
    outPath <- gsub("5_dsm", pathMask, outPath)
  }

  script <- gdalwarp(srcfile=p, dstfile=outPath, cutline=pathBuffer, dstnodata=0,
             overwrite=TRUE, dryrun=TRUE)

  system(script)

  ## then, clip again (mask) to filtered age classes.
  ### NOTE we are not using this for now because with the orthos, this clipping
  ### causes inconsistencies that ultimately cause gdal to fail
  # outVRTAge <- gsub(".vrt", "Mask.vrt", outVRT)
  # gdalwarp(srcfile=outVRT, dstfile=outVRTAge, cutline=pathAgeFilt, 
  #          overwrite=TRUE, dryrun=FALSE)
  
  return(print("Done with masking and cropping"))
}