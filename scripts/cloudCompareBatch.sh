#!/bin/bash

# General variables
totalN=844
filePath="/Users/ianmcgregor/Documents/labData/droneData/pointClouds/"

# Reference point cloud
refDate="2023-06-19"
refFilePrefix="/BCI23fat_"

# Target point cloud
targetDate="2023-10-12"
targetFilePrefix="/cloud_"
targetPath="/Users/ianmcgregor/Documents/labData/droneData/pointClouds/"

## Build the full file paths
alignPath="${filePath}4_aligned/tilesAlignedBCI_"
refPathCC="${alignPath}${refDate}${refFilePrefix}"
targetPathCC="${filePath}2_standardized/${targetDate}${targetFilePrefix}"
targetOutPathCC="${filePath}3_cloudCompare/${targetDate}"

echo "Reference point cloud path = $refPathCC"
echo "Target point cloud path = $targetPathCC"
echo "Output path = $targetOutPathCC"

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
# for tileN in {649,650}; do
    echo "Aligning tile $tileN out of $totalN"
    
    path1="${targetPathCC}$tileN.laz"
    path3="${targetOutPathCC}$tileN.las"

    path2="${refPathCC}$tileN.laz"
    path4="${refPathCC}$tileN.las"

    open -W -g -a CloudCompare.app --args -SILENT -O -GLOBAL_SHIFT -620000.00 -1000000.00 0 "$path1" -O -GLOBAL_SHIFT -620000.00 -1000000.00 0 "$path2" -ICP -ITER 800 -OVERLAP 80 -C_EXPORT_FMT LAS -SAVE_CLOUDS FILE "$path3 $path4" -CLEAR

    ## either hold the code for 15 seconds or until "Return" is pressed,
    ## whichever is first. To troubleshoot and only advance when "Return"
    ## is pressed, remove the `-t 15`.
    # read -t 15
done
