# Full workflow
Below is the complete list of steps for processing drone data as part of a spectral and structural analysis. 

|Step # | Task | Protocol or script name | Software 
|---|---|---|---|
**0** | **Prepare for analyses**
0a | Create directory structure necessary for the processing code to run | `mod0_createDirStructure.R` | R
0b | Manually set function arguments for the processing pipeline | `args.R` | R
| | | | |
**1** | **Initial data processing**
1a | Process the data in Metashape: align the images, then build and export a point cloud. | see below. This is `mod1`.| Metashape
1b-i | Structural analysis: Standardize the point cloud to a common grid. | `mod2a_processPointClouds.R` | R
1b-ii | Structural analysis: Align the standardized point cloud to previous ones. | `mod2a_processPointCloud.md` | CloudCompare 
1b-iii | Structural analysis: Use the aligned point cloud to create a DSM. | `mod2a_processPointClouds.R` | R
| | | | |
**2** | **Run the analyses**
2a | Calculate a CHM from each DSM, then difference the two to see changes | `mod3_defineGaps.R` | R
2b | Identify gaps | `mod3_defineGaps.R` | R
2c | Calculate gap metrics and save 2 things: shapefile of polygons and metrics table. | `mod3_defineGaps.R` | R

## Function arguments
To facilitate an automatic workflow, the function arguments are set once in `args.R`.
- `siteName` is the site for the analysis
- General path arguments should be kept as-is (they correspond to the directory structure built in Step 0a above). However, changes will need to be made for any site-specific paths, formatting, and variables.
  - **NB**: Everything is set up for BCI, but the functionality is there for the code to be applied for another site's data. Note that if this is the case, you will potentially need to change some of the internal code (e.g. if you have to do some spatial processing before you have a usable study site border).

## Metashape
Using the application itself, the workflow is
1. Align photos (Generic preselection, Reference preselection, Highest accuracy, Source, 40,000 key point limit, 4000 tie point limit, exclude stationary tie points)
2. Build Point Cloud (Medium quality, aggressive filtering)
3. Export point cloud (UTM)

## CloudCompare
This is the software we use to align the point cloud to a lidar-derived point cloud. There are two ways to batch apply the software, either using Python API or a command-line interface (CLI). For consistency's sake between operating systems, we recommend running the Python API.

### Python API (CloudComPy)
In order to run the python script, we have to use R to activate a conda environment specifically for the CloudComPy module, then run python there. The easiest way to do this is to simply source a shell (Mac) or batch file (Windows) that then calls the python code. Instructions for setting up the module are below.

#### Mac
1. Download the application binary using [this link](https://gitlab.com/openfields1/CloudComPy/-/blob/master/doc/UseMacOSCondaBinary.md).
   1. Click on `CloudComPy binaries`, then choose `MacOS CloudComPy binaries`, then download the most recent version. Save it to the parent directory of the project (i.e. at the same level as your `scripts` folder).
2. Create the conda environment called `CloudComPy310` using the necessary python version.
3. Add the packages using the code they provide.
4. Create a `.sh` file using the content in `scripts/CloudCompare/2_runCloudComPy.sh`. Make sure to change the path to the `.zsh` file as you need.
5. You should be good to go! The code should now be able to be called from R.
   1. On Mac you may get a warning during the use of the python code that says the following. This is a [known bug](https://bugreports.qt.io/browse/QTBUG-87267) from the QT application that was fixed in subsequent versions, but it does not affect the outcome of the code. It is probably here because CloudComPy relies on Python3.10 (as of May 2024), which then probably uses the older version of QT.
     ```
     qt.qpa.fonts: Populating font family aliases took 51 ms. Replace uses of missing font family ".AppleSystemUIFont" with one that exists to avoid this cost.
     ```
#### Windows
tbd

### CloudCompare CLI
The specific process is dependent on whether you're running in Windows or Mac.
- The software can be downloaded [here](https://www.danielgm.net/cc/release/).
- The command line arguments can be found [here](https://www.cloudcompare.org/doc/wiki/index.php/Command_line_mode).

#### Windows
Download the software with the default set-up. To run on a PC, the batch script is a `.bat` file as created by KC [here](https://github.com/kccushman/BCI_Photogrammetry/blob/master/Code_AlignDroneData/do2020to2018.bat). The main command for the CloudCompare application is written after the words `CloudCompare`.
- Note from Nov 2023: In trying to run the script I (Ian) got the message that CloudCompare on Windows requires Python 3.10.X. The normal installer for this is gone because it's an old version, so I downloaded the `.tgz` file and found a way to install it myself. But! When trying to run the basic installation file (`setup.py`), it fails. So...I have no idea what's going on but it appears to be un-runnable in Windows.

#### Mac
1. Download the software with default set-up.
2. Scripts to call and run CloudCompare are in `.sh` files as seen in the `scripts` folder. This script can be either run directly in terminal (e.g. `path/to/script.sh`) or called from R using `system(path/to/script.sh)`.
3. Update the paths to be what they should be. **NOTE** the paths need to be the *full* paths, otherwise CloudCompare won't find it. This can potentially be altered if you move the CloudCompare application to the folder with the other data.
4. Open a terminal and run `sh path/to/cloudCompare.sh`
