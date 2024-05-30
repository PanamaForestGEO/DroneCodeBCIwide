##########################################################
## Purpose: Aligning point clouds with CloudCompare software
##          using CloudComPy module
##
## Author: Ian McGregor, 2024
## Contact: 
## System: Python3.10, CloudComPy310 environment
##########################################################
# See this website for documentation
# https://www.simulation.openfields.fr/documentation/CloudComPy/html/

## cmd line scripting

##Windows
# cd <path install>\CloudComPy310
# conda activate CloudComPy310
# envCloudComPy.bat

##Mac
# . CloudComPy310/bin/condaCloud.zsh activate CloudComPy310
# python

import os
import sys
import re
from functools import partial
from multiprocessing import Pool
import cloudComPy as cc

name_of_script = sys.argv[0]
siteName = sys.argv[1]
targetDate = sys.argv[2]

# siteName = "bci"
# targetDate = "2024-03-06"

## functions
def extractNumList(st):
        nList = re.findall(r'\d+', st)
        n = int(''.join(str(x) for x in nList))
        return n

def definePaths(type, targetDate):
    pathDir = "droneData/" + siteName + "/pointClouds/type/dateFolder" 

    if type=="reference":
        pat = ".laz"
        cloudPathDir = re.sub("type/dateFolder", 
                              "0_reference/lidar_2023", pathDir)
    elif type == "input":
        pat = ".las"
        cloudPathDir = re.sub("type/dateFolder",
                              "2_standardized/" + targetDate, pathDir)
    
    cloudPath = os.listdir(cloudPathDir)
    paths = [x for x in cloudPath if pat in x]

    ## put the tiles in order so that way both lists are consistent
    tileIDs = [extractNumList(st) for st in paths]
    order = sorted(range(len(tileIDs)), key=lambda i: tileIDs[i])
    paths = [paths[i] for i in order]

    pathsFull = [os.path.join(cloudPathDir, x) for x in paths]

    return pathsFull

def alignCloud(cloudN, cloudPaths, shiftX, shiftY, shiftZ, totalN):
    print("Aligning tile " + str(cloudN) + " out of " + str(totalN))

    pathRef = cloudPaths[0][cloudN]
    pathInput = cloudPaths[1][cloudN]
    
    clouds = [cc.loadPointCloud(w, mode = cc.CC_SHIFT_MODE.XYZ, 
                                x = shiftX, y = shiftY, z = shiftZ)
                for w in [pathInput, pathRef]]

    # res = cc.ICP(data = clouds[0],
    #             model = clouds[1], 
    #             finalOverlapRatio = 0.98,
    #             randomSamplingLimit = 100000)
    
    res = cc.ICP(data = clouds[0], model = clouds[1], 
                minRMSDecrease=1.e-5, maxIterationCount=0,          # defaults
                removeFarthestPoints=False,                         # defaults
                method=cc.CONVERGENCE_TYPE.MAX_ERROR_CONVERGENCE,   # default when maxIter=0
                adjustScale=False,                                  # defaults
                finalOverlapRatio = 0.98,
                randomSamplingLimit = 100000)

    trans = res.transMat
    cloudAligned = res.aligned
    cloudAligned.applyRigidTransformation(trans)

    ## save the aligned cloud
    cloudPathOutput = re.sub("2_standardized", "3_cloudCompare", pathInput)
    cloudPathOutput = re.sub("2stand", "3cloudComp", cloudPathOutput)
    cc.SavePointCloud(cloudAligned, cloudPathOutput)

    ## save the transformation matrix to text file
    transPath = re.sub("2_standardized", "7_transformationMatrices", 
                       re.sub("2stand.las", "7transMat.txt", pathInput))
    text_file = open(transPath, "w")
    text_file.write(trans.toString())
    text_file.close()

if __name__ == '__main__':
    ## Define variables
    shiftX = -620000.00
    shiftY = -1010000.00
    shiftZ = 0

    ## Define the file paths
    ### cloudPaths[0] = Lidar reference paths
    ### cloudPaths[1] = Input target date paths
    cloudPaths = [definePaths(x, targetDate) for x in ["reference", "input"]]

    ## run the loop
    totalN = len(cloudPaths[0])

    ## parallel (unique to python, can't do this in command line because it 
    ##           "physically" opens the application for each alignment)
    funPartial = partial(alignCloud, cloudPaths=cloudPaths, shiftX=shiftX, 
                        shiftY=shiftY, shiftZ=shiftZ, totalN=totalN)
    bl = list(range(totalN))

    p = Pool(processes=10)
    p.map(funPartial, bl)
    p.close()

# **NOTE** If you need to troubleshoot, run CloudCompare sequentially
# [alignCloud(x, cloudPaths, shiftX, shiftY, shiftZ, totalN) for x in range(totalN)]