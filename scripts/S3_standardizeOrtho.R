##########################################################
## Purpose: Standardize the orthomosaics to be same resolution, same origin, 
##          and same number of rows and columns
##
## Input: original orthomosaics from drone flights
## Output: standardized orthophotos
##
## Creator: Ian McGregor 2023
## Contact: mcgregori@caryinstitute.org
## System: R Version 4.3.1, Sep 2023
## Last modified: ~ Sep 2023
##########################################################
# Standardize orthos
library(terra)
library(gdalUtilities)

script <- "standardize"
changeType <- "ortho" # "structural" or "ortho"
source("scripts/args.R", local=TRUE)
source("scripts/S3_funs.R")

# Three steps in total:
## 1. Load orthos, define min common extent from them, and save to shapefile
## 2. Crop, align, and resample the orthos so that each one is the same
##    resolution, has same origin, and has same number of rows/cols
## 3. Mask the standardized orthos to BCI boundary and age class of forests

# Step 1: Prep data and files

# Step 1a. Define min common extent, and save to shapefile
extCommon <- createCommonExt(pathExt, shpName, pathBuffer, overwrite=FALSE)
extCommon <- ext(extCommon)

# Step 1b. Load input files and data
targetDates <- c("2023-06-19", "2023-10-12")
rastFiles <- loadData(targetDates, pathInput, changeType)

# Step 1c. Filter age class shapefile to use later (only run once)
filterAge(pathAge, pathAgeFilt)

# Step 2. Crop, align, and resample the orthos so that each one is the same
#         resolution, has same origin, and has same number of rows/cols

## Define a minimum resolution to use for processing. Note that this only
## affects downstream products. We are retaining the original orthomosaics in 
## their native resolution and extents.
if(changeType=="ortho"){
    orthosAligned <- sapply(1:length(rastFiles$f), standardizeOrthos, 
                        f=rastFiles$f, orthos=rastFiles$inputs, 
                        pathInput, pathStandard, extCommon, resMin)
}

# Step 3. Mask the standardized orthos to BCI boundary and age class of forests
if(changeType=="ortho"){
    files <- list.files(gsub("/$", "", pathStandard), pattern="WholeBCI.*vrt", 
                    full.names=TRUE)
} else if(changeType=="structural"){
    files <- list.files(gsub("/$", "", pathStandard), full.names=TRUE)
}

files <- files[grepl(paste0(targetDates, collapse="|"), files)]
if(changeType=="ortho") files <- files[grepl(paste0(resMin*100, ".vrt"), files)]
flightTifs <- sapply(files, bufferMask, pathMask, pathAgeFilt, pathBuffer, 
                     crsProj, changeType)

