#!/bin/bash
# test to make ortho

# run this code using the following command in terminal
## sh scripts/cloudCompare/ortho.sh

## Step 1: First need to merge all the point clouds into 1

# General variables
totalN=844
baseDir="$PWD"
cloudDir="${baseDir}/droneData/pointClouds/4_aligned/"
targetDate="2023-10"
filePath="${cloudDir}full/tilesAlignedBCI_${targetDate}/cloud_"
outDir="${cloudDir}singleCloud"

## create output dir if it doesn't exist, then finish defining output file
# mkdir -p $outDir
# outFile="${outDir}/${targetDate}.las"
# echo "Save dir = $outFile"


args=""
for tileN in {1..844}; do
    file="${filePath}$tileN.laz"
    args+=" -O -GLOBAL_SHIFT -620000.00 -1000000.00 0 "${file}""

done

# echo "$args"
echo "Target dir = $filePath"
cmdFull="open -W -g -a CloudCompare.app --args -SILENT "$args" -RASTERIZE -GRID_STEP 0.2 -OUTPUT_RASTER_RGB -CLEAR"

# echo "$cmdFull"

eval "$cmdFull"