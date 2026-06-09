This research was performed with R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"

Folders in this repository:
- data
contains subfolders
	- raw data: raw input - Excel files, registered species at fieldwork
	- environmental maps: raw input - maps of enviromental variables (vector and/or raster) on climate, geography and land use
	- species richness data: intermediate data - calculated species richness data files from raw data 
	- model input data: intermediate data - csv files with extracted environmental variables, GPS coordinates and species richness counts

- analysis
all R files for doing analysis
- analysis-output
final .rds files of machine learning models, shapviz data, and the factor levels of categorical input variables used for training and testing


Steps:

1. Extract environmental variable values at plot locations
Use Extract_env_vars.R to extract the environmental variable values at the locations of the plots. 
Depending on the extent and resolution of the region of interest, this can be all at once, or per region. 
In the latter case, concatenate the output CSV files before continuing with step 2.

2. Investigate data
Investigate Species Richness Data.R

3. Perform VIF analysis
VIF_1m.R

4. Train ML models
BRT_dismo.R - part 1

5. SHAP analysis
BRT_dismo.R - part 2

5. Create species richness prediction maps
MapPredictionTerraRaster.R

7. Analyse patterns in predicted species richness across scales
Analyse Prediction Patterns.R
e.g. hotspots, density plots.

---- 100m scales ----
8. Create new 100m locations
R file: AverageAndTotal100mSpeciesRichness.R
CSV file: small scale plot combinations.csv

9. Perform steps 1-7 for the 100m scales