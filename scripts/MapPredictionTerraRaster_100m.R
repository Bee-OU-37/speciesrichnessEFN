#############################################
# Packages 
#############################################
library(raster)  # raster handling
library(terra)   # terra package for raster handling)
library(sf)      # vector handling
library(gbm)     # gbm models
library(ggplot2) # plotting
library(dplyr)   # data manipulation

#############################################
# User‑defined paths & constants
#############################################
folder      <- "C:/Users/Johanneke/Maps/Prediction maps/final"   

#############################################
# List raster & vector files
#############################################
raster_pat  <- "\\.(tif|img|nc|grd)$"
vector_pat  <- "\\.(shp|gpkg|geojson)$"

raster_files <- list.files(folder,
                           pattern = raster_pat,
                           full.names = TRUE,
                           ignore.case = TRUE)

vector_files <- list.files(folder,
                           pattern = vector_pat,
                           full.names = TRUE,
                           ignore.case = TRUE)

# -------------------------------------------------------------
# Build a RasterStack that points to the raster files
# -------------------------------------------------------------

stacked_rasters <- stack(raster_files)  

#load stacked_rasters entirely, not just as pointers
stacked_rasters <- readAll(stacked_rasters)

#Check layer names
print(names(stacked_rasters))

# Keep only Infrared_1, rename to Infrared, remove other Infrared_*
infrared_indices <- grep("Infrared_", names(stacked_rasters))
if (length(infrared_indices) > 1) {
  # Keep only the first Infrared band
  stacked_rasters <- stacked_rasters[[-infrared_indices[-1]]]
  # Rename the first Infrared band to "Infrared"
  names(stacked_rasters)[infrared_indices[1]] <- "Infrared"
}

#Check layer names
print(names(stacked_rasters))

# -------------------------------------------------------------
# Prepare Categorical Data from Vector Layers
# -------------------------------------------------------------
lc_file <- vector_files[grepl("Landcover", vector_files, ignore.case = TRUE)]
sc_file <- vector_files[grepl("Soilcode", vector_files, ignore.case = TRUE)]

# Load vector files as Spatial*DataFrame
landcover_sf <- st_read(lc_file)
soilcode_sf <- st_read(sc_file)

# Make sure vectors have desired factor levels
desired_lc_levels <- readRDS("BRT_dismo_100m_levels_lc.rds")
desired_soil_levels <- readRDS("BRT_dismo_100m_levels_soil.rds")
landcover_sf$category <- factor(landcover_sf$category, levels = desired_lc_levels)
soilcode_sf$soilcode <- factor(soilcode_sf$soilcode, levels = desired_soil_levels)

# 
# remove NAs, as they cause issues during rasterization, and this also establishes a correct AOA
landcover_sf <- landcover_sf[!is.na(landcover_sf$category), ]
soilcode_sf <- soilcode_sf[!is.na(soilcode_sf$soilcode), ]

# -------------------------------------------------------------
# Remove overlapping features in landcover_sf
# -------------------------------------------------------------
# Add a unique ID to each feature
#landcover_sf$id <- seq_len(nrow(landcover_sf))

plot(landcover_sf, main = "Landcover Features with Groups")

# Create the mode function for overlapping polygons
mode_function <- function(values, na.rm = TRUE) {
  # Handle NA values explicitly if na.rm = TRUE
  if (na.rm) values <- na.omit(values)
  # Return the most frequent value
  unique_value <- names(which.max(table(values)))
  return(as.numeric(unique_value))
}

# Rasterize vector layers to match raster stack
landcover_raster <- terra::rasterize(
  landcover_sf, 
  stacked_rasters, 
  field = "category",
  fun = mode_function,       # Custom mode function
  overwrite = TRUE, # overwrite existing raster
  progress = TRUE # show progress bar
)

print(names(landcover_raster))
plot(landcover_raster, main = "Landcover Raster")  # Plot the landcover raster
# save to file
writeRaster(landcover_raster, "landcover_raster_USP_Q_Prediction.tif", overwrite = TRUE)


soilcode_raster <- terra::rasterize(
  soilcode_sf, 
  stacked_rasters, 
  field = "soilcode",
  overwrite = TRUE # overwrite existing raster
)

plot(soilcode_raster, main = "Soilcode Raster")    # Plot the soilcode raster

# save to file
writeRaster(soilcode_raster, "soilcode_raster_USP_Q_Prediction.tif", overwrite = TRUE)

# Check their alignment with rasters
if (!compareRaster(landcover_raster, stacked_rasters[[1]], extent = TRUE, crs = TRUE, res = TRUE)) {
  stop("Align the extents, resolution, or CRS")
}
if (!compareRaster(soilcode_raster, stacked_rasters[[1]], extent = TRUE, crs = TRUE, res = TRUE)) {
  stop("Align the extents, resolution, or CRS")
}
# Convert to dataframe
raster_df <- as.data.frame(stacked_rasters, xy = TRUE)
landcover_raster_df <- as.data.frame(landcover_raster, xy = FALSE)
soilcode_raster_df <- as.data.frame(soilcode_raster, xy = FALSE)
final_df <- cbind(raster_df, Landcover = landcover_raster_df[[1]], Soilcode = soilcode_raster_df[[1]])
final_df <- na.omit(final_df)

# -------------------------------------------------------------
# Predict with GBM 100mean model
# -------------------------------------------------------------

# Load trained gbm model
gbm_100m_mean_model <- readRDS("BRT_100total.rds")

# Predict with the GBM model
final_df$prediction <- predict(
  gbm_100m_mean_model, 
  newdata = final_df[, -c(1, 2)],
  n.trees = gbm_model$n.trees,
  type = "response"
)

# Convert predictions to raster
prediction_raster <- rasterFromXYZ(final_df[, c("x", "y", "prediction")])
plot(prediction_raster)
writeRaster(prediction_raster, "predictions_USP_100m_mean.tif", overwrite = TRUE)


# -------------------------------------------------------------
# Predict with GBM 100m total model
# -------------------------------------------------------------

# Load trained gbm model
gbm_100m_total_model <- readRDS("BRT_100mean.rds")

# Predict with the GBM model
final_df$prediction <- predict(
  gbm_100m_total_model, 
  newdata = final_df[, -c(1, 2)],
  n.trees = gbm_model$n.trees,
  type = "response"
)

# Convert predictions to raster
prediction_raster <- rasterFromXYZ(final_df[, c("x", "y", "prediction")])
plot(prediction_raster)
writeRaster(prediction_raster, "predictions_USP_100m_total.tif", overwrite = TRUE)


