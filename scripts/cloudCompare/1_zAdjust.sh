#!/bin/bash

##########################################################
## Purpose: Apply z-adjustment to point cloud prior to alignment
##          in CloudCompare software
##
## Creator: KC Cushman 2024
## Edited: Ian McGregor
## Contact: 
## System: R Version 4.2.2, Jan 2024 (edited)
##########################################################

# to run this script, type the following into terminal
# sh scripts/cloudCompare/zAdjust.sh

# General variables
totalN=844
baseDir="$PWD"
filePath="${baseDir}/droneData/pointClouds/"

# Path to transformation matrix
matrixPath="${baseDir}/scripts/cloudCompare/zAdjust.txt"

# Target point cloud
targetDate="2023-11"
targetPath="2_decimated/${targetDate}"
targetOutPath="3_decimatedShifted/${targetDate}"
targetFilePrefix="/cloud_"

## Build the full file paths
targetPathCC="${filePath}2_standardized/${targetPath}${targetFilePrefix}"
targetOutPathCC="${filePath}2_standardized/${targetOutPath}"

echo "Target point cloud path = $targetPathCC"
echo "Output path = $targetOutPathCC"
echo "Matrix path = $matrixPath"

# create output dir if it doesn't exist
mkdir -p $targetOutPathCC

# add file prefix to output path
targetOutPathCC="${targetOutPathCC}${targetFilePrefix}"

# there are a couple parameters we've set using the Mac `open` command:
## -W = do not move to the next part of the code (next loop iter) until
##      the application closes. This is because CloudCompare can only run
##      one thing at a time.
## -g = keep the application in the background. This means your Mac won't
##      automatically try to change the active window every time the app opens.
## -a = indicates you want to open an application
## --args = everything after this is the CloudCompare-specific command line script

for tileN in {1..844}; do
# for tileN in {388}; do
    echo "Shifting tile $tileN out of $totalN"
    
    path1="${targetPathCC}$tileN.laz"
    path3="${targetOutPathCC}$tileN.las"

    open -W -g -a CloudCompare.app --args -SILENT -O -GLOBAL_SHIFT -620000.00 -1010000.00 0 "$path1" -APPLY_TRANS "$matrixPath" -C_EXPORT_FMT LAS -SAVE_CLOUDS FILE "$path3" -CLEAR

    ## either hold the code for 15 seconds or until "Return" is pressed,
    ## whichever is first. To troubleshoot and only advance when "Return"
    ## is pressed, remove the `-t 15`.
    # read -t 15
done