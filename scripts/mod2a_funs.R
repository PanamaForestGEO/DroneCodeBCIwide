##########################################################
## Purpose: Functions for processing point clouds
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## Contact: 
## System: R Version 4.2.2, Aug 2023 (edited)
##########################################################
# --------------------------------------------------------------------#
## createGrid = create grid for standardization
## writeStdGrid = wrapper for createGrid, clips based on soils and saves to csv
## standardizePC = standardize the point clouds
## reTilePC = wrapper for standardizePC function that brings in variables
## makeZAdjust = create text file with z-adjustments needed for cloudCompare alignment
## checkZAdj = exploratory check to see if height adjustment is needed
## zAdjustWrap = wrapper for makeZAdjust
## runCloudCompare = interactive function to run shell scripts with cloudCompare software
## reformatOutputs = reformat the files output from retiling with 0 overlap
## makeDSMs = make DSM from aligned lidar data using the lidR package
## makeDSMwrap = wrapper for applying makeDSMs function
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
writeStdGrid <- function(tileSize, pathSoils, crsProj, pathGrid){
  gridInfo <- createGrid(tileSz=150)

  soils <- vect(pathSoils)
  soils <- project(soils, crsProj)

  for(i in 1:nrow(gridInfo)){
    bounds <- ext(as.numeric(gridInfo[i, 1:4]))
    poly <- as.polygons(bounds, crs=crsProj)
    
    each <- relate(poly, soils, relation="intersects")
    gridInfo[i, "Use"] <- ifelse(any(each), 1, 0)
  }

  gridInfo <- gridInfo[gridInfo$Use==1,]
  gridInfo$ID <- 1:nrow(gridInfo)

  write.csv(gridInfo, pathGrid)
  return(print(paste0("Done! Saved grid to ", pathGrid)))
}
standardizePC <- function(gridN, gridInfo, catObj, overlap, type,
                          savePaths, saveConditions, siteName, ROI=NULL){
  print(paste0("Processing tile ", gridN))
  data <- clip_rectangle(catObj, 
                         xleft = gridInfo$xmin[gridN] - overlap,
                         ybottom = gridInfo$ymin[gridN] - overlap,
                         xright = gridInfo$xmax[gridN] + overlap,
                         ytop = gridInfo$ymax[gridN] + overlap)

  ## decimate the point cloud to only keep the highest points at a certain resolution
  ### smaller resolution in `highest()` = larger file size.
  ### e.g. 0.5 = 1 GB cloud, 0.05 = 10 GB cloud
  if(type=="align"){
    data <- decimate_points(data, algorithm=highest(res=0.5))
  }
  
  # trim point cloud using ROI
  if(!is.null(ROI)) data <- clip_roi(data, ROI)

  ## TEMP 
  ##      from before updating dir structure, probs can delete
  ## Pre-CloudCompare, save the standardized and decimated cloud tiles
  ## Post-CloudCompare, save the new point cloud with no overlap
  # if(length(data@data$X)>0 & type=="align" & saveConditions[2]){
  #   writeLAS(data, file=paste0(savePaths[2], "/cloud_", gridN,".las"))
  # } else if(length(data@data$X)>0 & saveConditions[3]){
  #   writeLAS(data, file=paste0(savePaths[3], "/cloud_", gridN,".las"))
  # }

  ## Save point cloud either preCloudCompare (type=="align") or post
  if(type=="align" & length(data@data$X)>0 & saveConditions[1]){
    writeLAS(data, file=gsub("tileN", gridN, savePaths[1]))
  } else if(type=="" & length(data@data$X)>0 & saveConditions[2]){
    writeLAS(data, file=gsub("tileN", gridN, savePaths[2]))
  } else {
    stop(paste0("Point cloud tile ", gridN, " is corrupted. Please fix."))
  }
  
  return(print(paste0("Finished tile ", gridN, " at ", 
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"))))
}
reTilePC <- function(overlap, nCores, pathSoils, crsProj, pathGrid, 
                    pathPointCloud, savePaths, saveConditions, ROI=NULL){
  print(paste0("Started retiling point cloud at ", format(Sys.time())))
  gridInfo <- read.csv(pathGrid)
  
  cl <- makeCluster(nCores)
  clusterEvalQ(cl, library(terra))
  clusterEvalQ(cl, library(lidR))

  if(overlap != 0){
    catObj <- readLAScatalog(pathPointCloud)
    if(is.na(st_crs(catObj))) st_crs(catObj) <- crsProj

    w <- st_crs(as.numeric(gsub("epsg:", "", crsProj)))
    if(st_crs(catObj) != w) stop("Point cloud CRS is not correct. Please change.")

    clusterExport(cl, c("gridInfo", "catObj", "overlap", "savePaths",
                      "saveConditions"), envir=environment())
    parSapply(cl, 1:nrow(gridInfo), standardizePC, gridInfo, catObj, overlap, 
              type="align", savePaths, saveConditions, ROI)
    # for(i in 1:nrow(gridInfo)){
    #   print(i)
    #   standardizePC(gridN=i, gridInfo, catObj, overlap, type="align", savePaths, 
    #   saveConditions, ROI)
    # }
  } else {
    catObj <- catalog(gsub("1_raw", "3_cloudCompare", pathPointCloud))
    clusterExport(cl, c("gridInfo", "catObj", "savePaths", "saveConditions"), 
                  envir=environment())
    parSapply(cl, 1:nrow(gridInfo), standardizePC, gridInfo, catObj, overlap=0, 
              type="", savePaths, saveConditions)
  }
  
  stopCluster(cl)

  print(paste0("Finished retiling point cloud at ", format(Sys.time())))
}
makeZAdjust <- function(targetPath, refPath, matPath){
  # Make catalog items  
  ctgTarget <- catalog(targetPath)
  ctgRef <- catalog(refPath)
  
  # Create coarse scale chm from both datasets
  print(paste0("Creating coarse CHM from target at ", format(Sys.time())))
  dsmTarget <- rasterize_canopy(ctgTarget, res = 10, algorithm = p2r())

  print(paste0("Creating coarse CHM from lidar reference at ", format(Sys.time())))
  dsmRef <- rasterize_canopy(ctgRef, res = 10, algorithm = p2r())
  
  # Difference the value
  print(paste0("Calculating z-adjustment and saving output at ", format(Sys.time())))
  meanHtTarget <- mean(values(dsmTarget),na.rm=T)
  meanHtRef <- mean(values(dsmRef),na.rm=T)
  zShift <- round(meanHtRef - meanHtTarget)
  
  # Make transformation matrix
  zShiftMat <- diag(4)
  zShiftMat[3,4] <- zShift

  # Save output
  write.table(zShiftMat, file = matPath, row.names = F, col.names = F)

  return(print("Done and saved to text file"))
}
checkZAdj <- function(targetDate, outPathFull, targetPath){
  ## A height adjustment is needed if the height of the point cloud tiles are not
  ## close to the height of the reference cloud
  ### e.g. differences of 10s of m are fine but diffs of > several hundred m are not

  ## FIRST
  ## check if you need to do a height adjustment in the first place:
  catNew <- catalog(outPathFull)
  catRef <- catalog(gsub(targetDate, "lidar_2023", targetPath))

  maxHeight <- data.frame(heightNew = catNew$Max.Z, heightRef = catRef$Max.Z)
  diff <- maxHeight$heightNew - maxHeight$heightRef
  print(paste0("Median height difference = ", median(diff)))
  print(paste0("Range of height difference = ", range(diff)))
  hist(diff)

  readline(prompt=paste0("Look over the differences. If you don't need to",
                        "perform a height adjustment, press Enter then quit",
                        "the function and change the parameters. Otherwise,
                        press Enter and continue to run."))
}
zAdjustWrap <- function(targetDate, pathPointCloud){
  # From KC: 
  # "Note I chose the "standardized" so that edges have been removed, will throw 
  # an overlapping tile warning but not important. It is a little slow though, 
  # maybe due to this."

  # targetPath = choose target cloud (to be aligned)
  # refPath = choose reference cloud
  # output file = matPath
  targetPath <- gsub("1_raw", "2_standardized", pathPointCloud)
  refPath <- gsub(targetDate, "lidar_2023", targetPath)
  matPath <- "scripts/cloudCompare/zAdjust.txt"

  ## run the function (note that this takes a long time)
  makeZAdjust(targetPath, refPath, matPath)
}
runCloudCompare <- function(shell_heightAdjust, shell_align, python){
  ## you can manually edit the shell scripts via R by running 
  ## file.edit(scriptName), then type "i" and change the date in the file.
  ## then press "Esc", then ":wq".
  ## However, it is easier to tell people to just open the file themselves.

  if(shell_heightAdjust){
    scriptName <- "scripts/cloudCompare/zAdjust.sh"
    txtHt <- paste0("Whoa there. Running height adjustment requires a ",
                    "shell script with the correct parameters. ",
                    "Please open `", scriptName, "` and ensure the parameters ", 
                    "are correct. Then press [Enter].")
    readline(prompt=txtHt)

    cmd <- paste0("sh ", scriptName)
    print(paste0("Running the command `", cmd, "` in terminal at ", 
                  format(Sys.time())))
    system(cmd)
    print(paste0("Finished running the shell script at ", format(Sys.time())))
  }
  
  if(shell_align){
    if(python){
      scriptName <- "scripts/cloudCompare/2_runCloudComPy.sh"
      cmd <- paste0("zsh ", scriptName, " bci ", targetDate)
    } else {
      scriptName <- "scripts/cloudCompare/2_batchAlign.sh"
    cmd <- paste0("sh ", scriptName)
    }
    
    print(paste0("Running the command `", cmd, "` in terminal at ",
                format(Sys.time())))
    system(cmd)
    print(paste0("Finished registering via cloudCompare at ", format(Sys.time())))

  }
}
reformatOutputs <- function(outPathDec, pathPointCloud){
  outPathShift <- gsub("3_decimated", "2_fullShifted", outPathDec)

  if(dir.exists(outPathShift)) n <- 2 else n <- 1

  ## Delete the .bin files in both the decimated and decimated shifted folders
  for(i in 1:n){
    binPath <- ifelse(i==2, outPathDecShift, outPathDec)
    binFiles <- list.files(binPath, full.names = TRUE, pattern = ".bin")
    file.remove(binFiles)
  }

  ## for the adjusted AND shifted files, move the transformation matrices
  ## to a different folder
  matPath <- gsub("1_raw", "7_transformationMatrices", pathPointCloud)
  if(!dir.exists(matPath)) dir.create(matPath, recursive=TRUE)

  matFiles <- list.files(outPathDec, full.names = TRUE, pattern = ".txt")
  matFilesShort <- list.files(outPathDec, full.names = FALSE, pattern = ".txt")

  if(length(matFiles) > 0){
    for(i in 1:length(matFiles)){
      nameNew <- gsub("MATRIX_.*", "MATRIX.txt", matFilesShort[i])
      pathNew <- gsub(matFilesShort[i], nameNew, matFiles[1])
      file.rename(from = matFiles[i], to = paste0(matPath, "/", nameNew))
    }
  }
}
makeDSMs <- function(X, targetDates, pointCloudPath, pathSave, crsProj, 
                    pathBuffer, plotDSM, saveDSM, returnDSM){
  
  print(paste0("Creating DSM for ", targetDates[X], " at ", format(Sys.time())))
  
  ## format the folder path and read in the catalog
  path <- gsub("DD", targetDates[X], pointCloudPath)
  
  catObj <- readLAScatalog(path)

  ## if we need to increase processing speed, might be worth looking into
  ## the following (see https://cran.r-project.org/web/packages/lidR/vignettes/lidR-computation-speed-LAScatalog.html)
  # lidR:::catalog_laxindex()
  
  ## make the basic DSMs
  pathSave <- paste0(pathSave, "DSM_", targetDates[X], ".tif")
  outFile <- rasterize_canopy(catObj, res = 1,
                              algorithm = p2r(subcircle=0.01, 
                                              na.fill = tin()))

  ## need to assign CRS
  if(terra::crs(outFile) != terra::crs(crsProj)){
    terra::crs(outFile) <- terra::crs(crsProj)
  }

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
makeDSMwrap <- function(targetDate, returnDSM){
  # 1. Define variables and function arguments
  script <- "makeDSM"
  source("scripts/args.R", local=TRUE)

  # NOTE
  ## you have the option to run multiple dates at once; just need to update
  ## the args.R file for this script name. Default is just using the main
  ## targetDate since that's the most common usage.

  # 2. Run the main function and save DSMs if wanted
  outputList <- lapply(1:length(targetDates), makeDSMs, targetDates, 
                        pointCloudPath, pathSave, crsProj, pathBuffer, plotDSM, 
                        saveDSM, returnDSM)
  
  if(returnDSM) return(outputList) else return(print("Done!"))
}

getTileLabs <- function(catObj){
  w <- as.data.table(st_coordinates(catObj))

  polyC <- lapply(unique(w$L2), function(q){
    tab <- w[L2==q]
    p <- as.polygons(ext(c(min(tab$X), max(tab$X), min(tab$Y), max(tab$Y))))
    crs(p) <- "EPSG:32617"
    p$ID <- q
    return(p)
  })

  polys <- vect(polyC)
  writeVector(polys, "droneData/pointClouds/grid.shp")

  plot(dsmList[[1]][[1]])
  lines(polys, labels=polys$ID)
  text(polys, labels=polys$ID)
}

pathLidar <- "droneData/pointClouds/2_standardized/lidar_2023"
clipLidar <- function(pathLidar, ROI){
  ctl <- catalog(pathLidar)
}