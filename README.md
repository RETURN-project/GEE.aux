# Calculate recovery metrics from sampled time series (GEE)
This repository contains JavaScript, python and R code and follows this workflow: 
- Generate an image with pixels of interest (/Javascript/SampleArea)
- Sample point locations using a stratified random sampling from the pixels of interest (/JavaScript/SamplePoints)
- Extract time series and auxiliary information from the sampled locations to a csv file (/python/dllLandsat.py)

The JavaScript code should be pasted in a Google Earth Engine (GEE) [code editor](https://code.earthengine.google.com/) (registration is needed to get access the first time). After clicking on 'run' button, a new 'run' button should appear for the specific task under the tasks tab. The task will start after clicking this button.

The python code can be executed after installing the Earth Engine Python API. More information about the GEE can be found [here](https://developers.google.com/earth-engine) and information about the python installation of the GEE can be found [here](https://developers.google.com/earth-engine/guides/python_install).
