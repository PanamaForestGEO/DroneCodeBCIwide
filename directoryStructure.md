# Directory structure
This document contains metadata explaining the directory structure for the photogrammetry-based gap analysis protocol. The descriptions of the end-level folders are first, followed by a text tree of the structure at the bottom.

Note that these are the base folders. There may be additional ones at different sites but they are not directly necessary for the analysis.

## Folder descriptions
`CloudComPy310` = This is the Python module that allows us to use CloudCompare without needing to open and close the application for each call, thus allowing us to parallelize.

`scripts` = code used for analyses

`spatialData` = any spatial data pertaining to the study site, e.g. a boundary shapefile to crop the orthomosaics and DSMs to

`droneData`
- `anomalyPolygons` = polygons of masked areas due to anomalies from photogrammetry reconstruction or clouds. These are often manually drawn and saved as shapefiles.
- `droneFlights` = the drone images from each flight are saved in a separate folder per mission (i.e. one mission is 1 full flight of the study area, potentially completed over multiple days). 
- `droneOrthomosaics`
  - `1_original` = these are the original orthomosaics as output by Metashape
  - `2_standardized` = Because each orthomosaic has slightly different dimensions and resolutions, we want to ensure we have a method to directly compare them (necessary for spectral analysis). This folder contains virtual raster tiles (VRTs), which are essentially a pointer to say "when you load this file, load the original and then do the resampling / transformation". Using VRTs allows us to cut back on memory usage.
  - `3_masked` = These orthomosaics are those that have been standardized and then masked to the exact study area boundary polygon.
- `metadata` = This folder contains metadata for all the flights for the study site.
- `pointClouds`
  - `0_reference` = The lidar point cloud that is used as the reference coud to which all others are registered to when using CloudCompare
  - `1_raw` = The original point cloud as output by Metashape
  - `2_standardized` = Output from taking the raw point cloud, splitting it into a standardized grid with 30% overlap between tiles, and decimating to only keep the highest points per tile.
  - `3_cloudCompare` = Register each tile from folder2 to the corresponding tile in the folder0 reference point cloud
  - `4_aligned` = Re-tile the CloudCompare output using the same grid as before, but this time wiht 0% overlap. This creates a smooth, cohesive point cloud.
  - `5_dsm` & `6_dsmMasked` = The re-tiled point cloud is used to directly create a DSM (5), and is then masked to the study site boundary (6)
  - `7_transformationMatrices` = This folder contains the transformation matrices that were created by CloudCompare when registering each tile. This acts as a record of what processing each tile underwent during registration.
- `processedChange`
  - `changeSpectral` = This is the difference between the spectral indexed orthomosaics for two supplied target dates. The most recent date is subtracted from the earlier date.
  - `changeStructural`
    - This is the difference between the canopy height models of the two supplied target dates. The most recent date is subtracted from the earlier date.
  - `gapsSpectral`
    - `gdalOut` = rasters of the gaps (cluster of adjacent pixels surrounded by NA) as defined by gdal
    - `metrics` = csv's of gap metadata between two target dates, including gap perimeter, area, and quantiles of the differenced index values present in the gap pixels. GapID is provided to cross-reference with the polygon shapefiles
    - `polygons` = shapefiles of the gaps between two target dates
  - `gapsStructural`
    - `gdalOut` = rasters of the gaps (cluster of adjacent pixels surrounded by NA) as defined by gdal
    - `metrics` = csv's of gap metadata between two target dates, including gap perimeter, area, and quantiles of depth in the gap pixels. GapID is provided to cross-reference with the polygon shapefiles
    - `polygons` = shapefiles of the gaps between two target dates

## Overall structure
Here is a text representation of the directory structure that is repeatable across sites. A `\` indicates it's the last folder or file in that organization level. To make this tree, can run `tree -d <dir>` in the main folder. On Mac if you don't have `tree`, download with `brew install tree`.

```
.
├── CloudComPy310
│   └── folders necessary for software
├── droneData
|   ├── siteName [e.g. bci]
|   │   ├── anomalyPolygons
|   │   │   ├── spectral
|   │   │   └── structural
|   │   ├── droneFlights
|   │   │   └── yyyy/mm/dd_droneImages
|   │   ├── droneOrthomosaics
|   │   │   ├── 1_original
|   │   │   ├── 2_standardized
|   │   │   ├── 3_masked
|   │   │   └── shapefiles
|   │   │   |    └── minCommonExtent
|   │   ├── metadata
|   │   ├── pointClouds
|   │   │   ├── 0_reference
|   │   │   │   └── lidar_2023
|   │   │   ├── 1_raw
|   │   │   │   └── yyyy/mm/dd
|   │   │   │   |  └── yyyy/mm/dd
|   │   │   ├── 2_standardized
|   │   │   │   └── yyyy/mm/dd
|   │   │   │   |  └── cloud_N_<siteName>_2stand.las
|   │   │   ├── 3_cloudCompare
|   │   │   │   └── yyyy/mm/dd
|   │   │   │   |  └── cloud_N_<siteName>_3cloudComp.las
|   │   │   ├── 4_aligned
|   │   │   │   └── yyyy/mm/dd
|   │   │   │   |  └── cloud_N_<siteName>_4aligned.las
|   │   │   ├── 5_dsm
|   │   │   │   └── yyyy/mm/dd
|   │   │   │   |  └── yyyy/mm/dd_<siteName>_5dsm.tif
|   │   │   ├── 6_dsmMasked
|   │   │   │   └── yyyy/mm/dd
|   │   │   │   |  └── yyyy/mm/dd_<siteName>_6dsmMask.tif
|   │   │   └── 7_transformationMatrices
|   │   │   │   └── yyyy/mm/dd
|   │   │   │   |  └── yyyy/mm/dd_<siteName>_7transMat.txt
|   │   └── processedChange
|   │   │   ├── changeSpectral
|   │   │   ├── changeStructural
|   │   │   ├── gapsSpectral
|   │   │   │   ├── gdalOut
|   │   │   │   ├── metrics
|   │   │   │   └── polygons
|   │   │   └── gapsStructural
|   │   │   |   ├── gdalOut
|   │   │   |   ├── metrics
|   │   │   |   ├── plots
|   │   │   |   └── polygons
├── scripts
└── spatialData
|   └── siteName [e.g. bci]
|   │   └── folders [as necessary]
```
