# Full workflow
Below is the complete list of steps for processing drone data as part of a spectral and structural analysis. Currently (Dec 2023) the workflow below is run using a mix of coding languages and software applications. Eventually, our goal is to convert this to a single coding language.

|Step # | Task | Protocol or script name | Software 
|---|---|---|---|
**1** | **Prepare the data for analyses**
1a | Process the data in Metashape: align the images, then build and export a point cloud. | see below | Metashape
1b-i | Structural analysis: Standardize the point cloud to a common grid. | `L1_processPointClouds.R` | R
1b-ii | Structural analysis: Align the standardized point cloud to previous ones. | `5_processPointCloud.md` | CloudCompare 
1b-iii | Structural analysis: Use the aligned point cloud to create a DSM. | `L1_processPointClouds.R` | R
| | | | |
**2** | **Run the analyses**
2a | Calculate a CHM from each DSM, then difference the two to see changes | `S5_defineGaps.R` | R
2b | Identify gaps | `S5_defineGaps.R` | R
2c | Calculate gap metrics and save 3 things: raster of polygons, shapefile of polygons, and metrics table. | `S5_defineGaps.R` | R

## Metashape workflow
Using the application itself, the workflow is
1. Align photos (Generic preselection, Reference preselection, Highest accuracy, Source, 40,000 key point limit, 4000 tie point limit, exclude stationary tie points)
2. Build Point Cloud (Medium quality, aggressive filtering)
3. Export point cloud (UTM)
