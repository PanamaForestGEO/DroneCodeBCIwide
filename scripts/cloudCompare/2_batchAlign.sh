#!/bin/bash

##########################################################
## Purpose: Aligning point clouds with CloudCompare software
##
## Creator: KC Cushman, 2020
## Edited: Ian McGregor
## Contact: 
## System: R Version 4.2.2, Nov 2023 (edited)
##########################################################

# run this code using the following command in terminal
## sh scripts/cloudCompare/2_batchAlign.sh

# General variables
totalN=844
baseDir="$PWD"
filePath=${baseDir}"/droneData/pointClouds/"
flightDate="2024-03-25"

# Reference point cloud
refPath="lidar_2023"
refFilePrefix="/cloud_"

# Target point cloud
targetPath="3_decimatedShifted/${flightDate}"
targetFilePrefix="/cloud_"

# Aligned point cloud
alignPath="${filePath}3_cloudCompare/${flightDate}"
alignFilePrefix="/cloud_"

## Build the full file paths
refPathCC="${filePath}2_standardized/${refPath}${refFilePrefix}"
targetPathCC="${filePath}2_standardized/${targetPath}${targetFilePrefix}"
alignPathCC="${alignPath}${alignFilePrefix}"

echo "Reference point cloud path = $refPathCC"
echo "Target point cloud path = $targetPathCC"
echo "Output path = $alignPathCC"

# create output dir if it doesn't exist
mkdir -p $alignPath

# there are a couple parameters we've set using the Mac `open` command:
## -W = do not move to the next part of the code (next loop iter) until
##      the application closes. This is because CloudCompare can only run
##      one thing at a time.
## -g = keep the application in the background. This means your Mac won't
##      automatically try to change the active window every time the app opens.
## -a = indicates you want to open an application
## --args = everything after this is the CloudCompare-specific command line script

for tileN in {1..844}; do
# for tileN in {803..805}; do
    echo "Aligning tile $tileN out of $totalN"
    
    path1="${targetPathCC}$tileN.laz"
    path3="${alignPathCC}$tileN.las"

    path2="${refPathCC}$tileN.laz"
    path4="${refPathCC}$tileN.las"

    open -W -g -a CloudCompare.app --args -SILENT -O -GLOBAL_SHIFT -620000.00 -1010000.00 0 "$path1" -O -GLOBAL_SHIFT -620000.00 -1010000.00 0 "$path2" -ICP -OVERLAP 98 -RANDOM_SAMPLING_LIMIT 100000 -C_EXPORT_FMT LAS -SAVE_CLOUDS FILE "$path3 $path4" -CLEAR

    ## either hold the code for 15 seconds or until "Return" is pressed,
    ## whichever is first. To troubleshoot and only advance when "Return"
    ## is pressed, remove the `-t 15`.
    # read -t 15
done
