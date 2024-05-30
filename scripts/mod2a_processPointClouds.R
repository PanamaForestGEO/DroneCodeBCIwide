##########################################################
## Purpose: Aligning point clouds
##
## Creator: KC Cushman, 2020
## Edited: Mia Mitchell, Ian McGregor
## Contact: 
## System: R Version 4.2.2, Aug 2023 (edited)
##########################################################
library(terra)
library(lidR)
library(parallel)
library(sf) # for reTilePC function
source("scripts/mod2a_funs.R")

# NOTE: if this workflow involves an interactive step in the internal `runCloudCompare`
#       function. The script will NOT finish without your input.
# steps
## 1 = makeGrid
## 2 = reTile with overlap
## 3 = findZAdjust (if needed)
## 4 = runCloudCompare
## 5 = reTile with no overlap
## 6 = createDSM

siteName <- "bci"

processPointClouds <- function(){
  script <- "alignPC"
  source("scripts/args.R", local=TRUE)

  ##-------------------------------------------------------##
  # How to align point clouds with previous ones.
  ## 1. First, create a standardized grid to align to, and only keep the cells
  ##    that overlap with BCI
  print("Creating standardized grid if it doesn't exist.")
  if(!file.exists(pathGrid)) writeStdGrid(tileSize, pathSoils, crsProj, pathGrid)

  ##-------------------------------------------------------##
  ## 2. Re-tile the point cloud using the new grid with a certain overlap amount
  ##    and save the new laz files
  print("Retiling the point cloud to the grid with overlap.")
  reTilePC(overlap=30, nCores, pathSoils, crsProj, pathGrid, 
            pathPointCloud, savePaths, saveConditions, ROI)
  
  ##-------------------------------------------------------##
  ## 3. Find height adjustment needed before ICP alignment (code author = KC)
  if(findZ){
    # differences of 10s of m are fine but diffs of > several hundred m are not
    print("Checking what the height difference from the reference point cloud.")
    checkZAdj(targetDate, outPathFull, targetPath)

    print("Finding the height adjustment matrix necessary for CloudCompare.")
    zAdjustWrap(targetDate, pathPointCloud)
  }

  ##-------------------------------------------------------##
  ## 4. Now download CloudCompare software and run batch scripts to fully
  ##    align the previous and current point clouds.
  ##    See the protocol alignPointClouds.md in github for the differences
  ##    between Mac and Windows.
  print("Running CloudCompare for height adjustment and/or alignment.")
  runCloudCompare(shell_heightAdjust, shell_align, python)

  ##-------------------------------------------------------##
  ## 4a. Re-tile the point cloud using the grid with no overlap amount
  ##    and save the new laz files
  print("Retiling the point cloud again, this time with no overlap.")
  reTilePC(overlap=0, nCores, pathSoils, crsProj, pathGrid, 
            pathPointCloud, savePaths, saveConditions)

  ## 4b. Arrange outputs from CloudCompare
  ### only necessary when using cloudcompare software directly
  if(shell_align & !python){
    reformatOutputs(outPathDec, pathPointCloud)
  }

  ##-------------------------------------------------------##
  ## 7. Create a DSM from the aligned point cloud
  ### Save 2 things: original DSM and masked to BCI boundary.
  print("Creating full and masked DSM.")
  dsmList <- makeDSMwrap(targetDate, returnDSM=TRUE)

  print("Done!")
}
processPointClouds()