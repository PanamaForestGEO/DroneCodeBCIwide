# DroneCodeBCIwide
Code for processing drone imagery for all of Barro Colorado Island

## Post-alignment code

### Code
The scripts are labeled to run in order as below. Note that the information here is also printed at the top of each script.
- `0_args.R` = arguments for each main function per script to be set *a priori*.
- `<N>_funs.R` = the functions used for each main script
- `0.5_annualQualityMasks.R`
  - Task: create cloud and QAQC masks to apply to DSMs for use in script 1. This is an updated version of KC's `AnnualQualityMasks.R` with terra compatibility and works on the original data. However, note that the remaining scripts below do not use this.
  - Input: manually-created tifs for albedo, red-blue difference, and height rasters
  - Output: corrected DSMs for only certain years
- `1_makeDSMs.R`
  - Task: create digital surface models from aligned point clouds for each flight date. Note that there is functionality in the function to do individualized processing for "raw", "qaqc", and "photogrammetry". However, the rest of the scripts (and directory structure below) only apply to "photogrammetry" (i.e. aligned point clouds).
  - Input: aligned point clouds
  - Output: individual DSMs per flight date. Each one is saved before moving on to the next one because the processing takes such a long time.
- `2_defineGaps.R`
  - Task: create canopy height models, define gaps
  - Input: DSMs from script 1
  - Output: raster / polygon shapefile and calculated metrics for each gap
- `3_plotGapTS.R`
  - Task: for a specific gap ID (or a supplied coordinate point), plot a time series of the orthophotos cropped to that specific location.
  - Input: gap data from script 2
  - Output: time series of orthophotos, optional png

### Directory structure
The code as written is dependent on the following directory structure:
- `Data_Ancillary`
  - `BCI_Outline_Minus25/BCI_Outline_Minus25.shp` = shapefile of BCI border with inverted buffer
  - `metaIslandFlights.csv` = metadata of flights, namely flight dates and date each flight was aligned to
  - `Data_HeightRasters`
    - `canopyGaps` = 3 folders of `metrics`, `polygons`, and `rasters`. These are the outputs from `2_defineGaps.R`
    - `dsm` = digital surface models (rasters) from point clouds. This is an output from `1_MakeDSMs.R`
    - `heightChange` = raster of canopy height change, output from step 1 of `2_defineGaps.R`
    - `LidarDEM_BCI.tif`
- `DroneOrthomosaics`
  - one file for each orthomosaic, labeled as `YYYY-MM-DD_orthoWholeBCI.tif`
- `gapTimeseries` = png files of ortho time series for a specific (gap) location
- `PointClouds/Processed/DronePhotogrammetry`= folders of aligned point clouds (844 `.laz` files)for each flight date. Naming convention for each folder is `tilesAlignedBCI_YYYY-MM-DD`.
- `scripts`
