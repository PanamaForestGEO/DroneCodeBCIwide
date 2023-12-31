##########################################################
## Purpose: Aligning point clouds
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## Contact: 
## System: R Version 4.2.2, Aug 2023 (edited)
## Last modified: Aug 2023
##########################################################
library(terra)
library(lidR)
library(parallel)
library(sf)
source("scripts/L1_funs.R")

## path definitions
pathPointCloud <- "droneData/pointClouds/1_raw/2023-10-12"
pathSoils <- "spatialData/bci/BCI_Soils/BCI_Soils.shp"
pathGrid <- "droneData/pointClouds/gridInfo.csv"
crsProj <- "epsg:32617"

##-------------------------------------------------------##
# How to align point clouds with previous ones.
## 1. First, create a standardized grid to align to, and only keep the cells
##    that overlap with BCI
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

##-------------------------------------------------------##
## 2. Re-tile the point cloud using the new grid with a certain overlap amount
##    and save the new laz files

# trim point cloud using border of soils polygon
bciBorder <- aggregate(vect(pathSoils),by=NULL, dissolve=TRUE)
bciBorder <- fillHoles(bciBorder,inverse=FALSE)
bciBorder <- project(bciBorder, crsProj)
bciBorder <- st_as_sf(bciBorder)

gridInfo <- read.csv(pathGrid)
catObj <- catalog(pathPointCloud)
dirPath <- gsub("1_raw", "2_standardized", pathPointCloud)
if(!dir.exists(dirPath)) dir.create(dirPath)

cl <- makeCluster(10)
clusterEvalQ(cl, library(terra))
clusterEvalQ(cl, library(lidR))
clusterExport(cl, c("gridInfo", "catObj", "dirPath"))
parSapply(cl, 1:nrow(gridInfo), standardizePC, gridInfo, catObj, overlap=30, 
           dirPath, type="align", ROI=bciBorder)
stopCluster(cl)

##-------------------------------------------------------##
## 3. Now download CloudCompare software and run batch scripts to fully
##    align the previous and current point clouds.
##    See the protocol alignPointClouds.md in github for the differences
##    between Mac and Windows.

# to edit, run the line below, type "i", then change the date in the file.
# then press "Esc", then ":wq"
file.edit("cloudCompareBatch.sh")

# now run the line below
system("sh cloudCompareBatch.sh")

##-------------------------------------------------------##
## 4a. Re-tile the point cloud using the new grid with no overlap amount
##    and save the new laz files
catObj <- catalog(gsub("1_raw", "3_cloudCompare", pathPointCloud))
dirPath <- gsub("1_raw/", "4_aligned/tilesAlignedBCI_", pathPointCloud)
if(!dir.exists(dirPath)) dir.create(dirPath)

cl <- makeCluster(10)
clusterEvalQ(cl, library(terra))
clusterEvalQ(cl, library(lidR))
clusterExport(cl, c("gridInfo", "catObj", "dirPath"))
parSapply(cl, 1:nrow(gridInfo), standardizePC, gridInfo, catObj, overlap=0, 
          dirPath, type="")
stopCluster(cl)

## 4b. Delete .bin files and move transformation matrices to a separate 
##      folder from the Cloud Compare default (original target folder)

binFiles <- list.files(gsub("1_raw", "2_standardized", pathPointCloud),
                       full.names = T, pattern = ".bin")
file.remove(binFiles)

matPath <- gsub("1_raw", "7_transformationMatrices", pathPointCloud)
if(!dir.exists(matPath)) dir.create(matPath)

matFiles <- list.files(gsub("1_raw", "2_standardized", pathPointCloud),
                       full.names = T, pattern = ".txt")
for(i in 1:length(matFiles)){
  file.rename(from = matFiles[i],
              to = gsub("2_standardized","7_transformationMatrices",matFiles[i]))
}

##-------------------------------------------------------##
## 5. Create a DSM from the aligned point cloud
### Save 2 things: original DSM and masked to BCI boundary.
source("scripts/L1_funs.R")

## Before running for the first time yourself, please ensure the folder
## and file names in the `prepMakeDSM()` function and `args.R` are set 
## correctly.

# 1. Define variables and function arguments
script <- "makeDSM"
source("scripts/args.R", local=TRUE)

# 2. Run the main function and save DSMs if wanted
targetDates <- "2023-10-12"
outputList <- lapply(1:length(targetDates), makeDSMs, pointCloudPath, pathSave,
                      crsProj, pathBuffer, plotDSM, saveDSM, returnDSM=TRUE)

## plot DSMs a posteriori
sapply(targetDates, function(X){
  r <- rast(paste0(pathList$pathDSM, "DSM_", X, "_corrected.tif"))
  plot(r)
})

# 2a. If necessary, crop lidar data to extent of photogrammetry data and save
# cropLidar <- function(dsmLPath, dsmPPath){
#   dsmLidar <- rast(dsmLPath)
#   dsmPhtgrm <- rast(dsmPPath)
#   dsmLidar <- crop(dsmLidar, dsmPhtgrm)
  
#   writeRaster(dsmLidar, dsmLPath)
# }
# dsmLPath <- "Data_HeightRasters/DSM_2009.tif"
# dsmPPath <- "Data_HeightRasters/DSM_2015_corrected.tif"
# cropLidar(dsmLPath, dsmPPath)
