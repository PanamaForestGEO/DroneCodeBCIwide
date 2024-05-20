#!/bin/zsh

# this script MUST be sourced as "zsh /runCloudComPy.sh" due to the header,
# otherwise won't work

# some notes
## - the "." is the source command
## - "&&\" says to run the next line immediately (not totally necessary)
## - using "python" calls the python version in the conda environment. Using
##      "python3" would use the system python version.
## - "$N" indicates that the arguments used when calling this .sh script should
##      be passed on as arguments to the python script. In this case,
##      - $1 = siteName
##      - $2 = targetDate

. CloudComPy310/bin/condaCloud.zsh activate CloudComPy310 &&\
python scripts/cloudCompare/2_runCC.py $1 $2
