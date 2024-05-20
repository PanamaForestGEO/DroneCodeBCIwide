##########################################################
## Purpose: Format files and directories for gap analysis
##
## Creator: Ian McGregor, 2024
## System: R Version 4.3.1
##########################################################

## Create the directory structure needed to run the analysis
createDirectories <- function(siteName){
    top <- paste0("droneData/", siteName, "/")
    mid <- c("droneFlights", "droneOrthomosaics", "pointClouds", 
            "processedChange", "maskPolygons")
    orthos <- c("1_original", "2_standardized", "3_masked")
    pointClouds <- c("1_raw", "2_standardized", "3_cloudCompare", "4_aligned", 
                    "5_dsm", "6_dsmMasked")
    change <- c(paste0(rep(c("change", "gaps"), each=2), 
                        c("Spectral", "Structural")))
    gaps <- c("gdalOut", "metrics", "polygons")

    ##----------------------------------------##
    # Spatial data
    ##----------------------------------------##
    pathSpat <- paste0("spatialData/", siteName)
    if(!dir.exists(pathSpat)) dir.create(pathSpat, recursive=TRUE)

    ##----------------------------------------##
    # Mask polygons
    ##----------------------------------------##
    pathMask <- paste0(top, mid[5], "/", c("spectral", "structural"))
    for(i in pathMask) if(!dir.exists(i)) dir.create(i, recursive = TRUE)

    ##----------------------------------------##
    # Drone data
    ##----------------------------------------##
    ## Folder for drone images
    imgPath <- paste0(top, mid[1])
    if(!dir.exists(imgPath)) dir.create(imgPath)

    ## Folder for orthomosaics
    ### Direct outputs from Metashape go in ~./1_original
    sapply(orthos, function(X){
        orthoPath <- paste0(top, mid[2], "/", X)
        if(!dir.exists(orthoPath)) dir.create(orthoPath, recursive = TRUE)
    })

    ## Folder for point clouds and DSMs
    sapply(pointClouds, function(X){
        pcPath <- paste0(top, mid[3], "/", X)
        if(!dir.exists(pcPath)) dir.create(pcPath, recursive = TRUE)
    })

    ## Folder for processed change and gaps
    sapply(change, function(X){
        changePath <- paste0(top, mid[4], "/", X)
        if(!dir.exists(changePath)) dir.create(changePath, recursive=TRUE)
    })

    for(i in change[3:4]){
        sapply(gaps, function(X){
            gapsPath <- paste0(top, mid[4], "/", i, "/", X)
            if(!dir.exists(gapsPath)) dir.create(gapsPath, recursive=TRUE)
        })
    }

    return(print("Done! All directories were created."))
}

createDirectories(siteName="test")

###############################################################################
## Convert .las to .laz (or vice versa) after processing is finished
convertLASObj <- function(objPath, targetFileExt){
    if(targetFileExt=="las"){
        currentExt <- "laz"
    } else if(targetFileExt=="laz"){
        currentExt <- "las"
    } else {
        stop(paste0("Invalid file extension. Must be `las` or `laz`.",
                    " If you want to change this, then please modify ",
                    "the `convertLASObj` function."))
    }

    ## perform the conversion
    obj <- readLAS(objPath)
    writeLAS(obj, gsub(currentExt, targetFileExt, objPath))

    ## remove the original
    file.remove(objPath)

    invisible() #silent return
}
convertDirs <- function(X, d, siteName, nCores, targetFileExt){
    dirPath <- paste0(X, "/", d)
    f <- list.files(dirPath)

    if(any(grepl(".las", f)) & any(grepl(".laz", f))){
        stop(paste0("Multiple file types are present in ", dirPath, 
                    ". Please address or change this function."))
    }

    if(grepl(targetFileExt, f[1])){
        warning(paste0("No conversion needed. Current and target extensions ",
                    " are the same. Files in ", objPath, " are unchanged."))
    } else {
        if(length(f) > 1){
            cl <- makeCluster(nCores)
            clusterEvalQ(cl, library(lidR))
            parSapply(f, convertLASObj, targetFileExt)
            stopCluster(cl)
        } else {
            convertLASObj(f, targetFileExt)
        }
    } 
}
changeLidarFiles <- function(siteName, nCores, targetFileExt, targetDate){
    path <- "droneData/bci/pointClouds"
    dirs <- list.files(path, full.names=TRUE)
    dirs <- dirs[grepl(paste0(1:4, collapse="|"), dirs)]

    for(d in targetDate){
        sapply(dirs, convertDirs, d, siteName, nCores, targetFileExt)
    }
}

#! as of 2024-05-16, this is written but not fully tested!
library(lidR)
library(parallel)

targetDate <- "2018-06-14"
targetFileExt <- "laz"
nCores <- 9
siteName <- "bci"

changeLidarFiles(siteName, nCores, targetFileExt, targetDate)