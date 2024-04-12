#!/bin/bash

##########################################################
## Purpose: Apply transformation matrix to full resolution point cloud
##
## Creator: KC Cushman 2020
## Edited: Ian McGregor
## Contact: 
## System: R Version 4.2.2, Jan 2024 (edited)
##########################################################

# run this code using the following command in terminal
## sh scripts/cloudCompare/transform.sh

# 1. Tile (standardize) the point cloud from metashape
# 2. Decimate to only keep highest points
# 3. Apply z-adjustment to decimated cloud
# 4. Align to 2023 lidar cloud. A transformation matrix is automatically made for each tile.
# 5. THIS SCRIPT: Apply each tile's transformation matrix to each tile of the original, non-decimated point cloud.

# General variables
totalN=844
baseDir="$PWD"
targetDate="2023-10"
filePath="${baseDir}/droneData/pointClouds/"

targetCloud="${filePath}2_standardized/1_fullResolution/${targetDate}/cloud_"
transMatrix="${filePath}7_transformationMatrices/${targetDate}/cloud_"
alignedDir="${filePath}2_standardized/4_fullTransformed/${targetDate}"

# create output dir if it doesn't exist
mkdir -p $alignedDir

## add on file prefix
alignedCloud="${alignedDir}/cloud_"

echo "Target cloud to transform = $targetCloud"
echo "Transformation matrix path = $transMatrix"
echo "Output path = $alignedCloud"

# there are a couple parameters we've set using the Mac `open` command:
## -W = do not move to the next part of the code (next loop iter) until
##      the application closes. This is because CloudCompare can only run
##      one thing at a time.
## -g = keep the application in the background. This means your Mac won't
##      automatically try to change the active window every time the app opens.
## -a = indicates you want to open an application
## --args = everything after this is the CloudCompare-specific command line script


# for tileN in {1..844}; do
for tileN in {790..793}; do
    echo "Shifting tile $tileN out of $totalN"
    
    targetCloudFull="${targetCloud}$tileN.laz"
    transMatrixFull="${transMatrix}${tileN}_REGISTRATION_MATRIX.txt"
    alignedCloudFull="${alignedCloud}$tileN.las"

    open -W -g -a CloudCompare.app --args -SILENT -AUTO_SAVE OFF -O -GLOBAL_SHIFT -620000.00 -1000000.00 0 "$targetCloudFull" -APPLY_TRANS "$transMatrixFull"  -C_EXPORT_FMT LAS -SAVE_CLOUDS FILE "$alignedCloudFull" -CLEAR

done