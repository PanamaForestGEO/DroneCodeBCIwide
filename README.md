# DroneCodeBCIwide
Code for processing drone-collected aerial RGB data products for all of Barro Colorado Island, Panama.  Specifically, this code spatially aligns the time-series of 3D point clouds, and differences these to detect canopy disturbances (drops in canopy height due to branchfalls and canopy tree death).    

## Workflow
The primary code with the alignment and gap processing follows the workflow in `workflow.md`. The original script files (with converted `terra` functionality) from Cushman et al. are in `scripts/original/` and follow the `postAlignWorkflow.md` file within the same folder.

## Contributors to the code
KC Cushman wrote the original set of code which was used for analyses in Cushman et al. (2022) and is pubslihed at the Smithsonian Figshare repository cited below.  This code was subsequently modified by KC Cushman, Ian McGregor, Mia Mitchell, and Vicente Vasquez.    

## References

Cushman, K. C., M. Detto, M. García, and H. C. Muller-Landau. 2022. Soils and topography control natural disturbance rates and thereby forest structure in a lowland tropical landscape. Ecology Letters, 25: 1126-1138. https://doi.org/https://doi.org/10.1111/ele.13978

Cushman, K. C., H. C. Muller-Landau, M. Detto, and M. Garcia. 2022. Datasets for “Soils and topography control natural disturbance rates and thereby forest structure in a lowland tropical landscape". Smithsonian Tropical Research Institute. Smithsonian Figshare.  https://doi.org/10.25573/data.17102600.v1. In Smithsonian Figshare, edited by S. T. R. Institute. https://doi.org/10.25573/data.17102600.v1


