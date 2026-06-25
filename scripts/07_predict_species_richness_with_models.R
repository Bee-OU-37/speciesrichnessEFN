# This script uses the trained GBM models to predict species richness across the landscape based on the input raster and vector data.

init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()

# Load configuration
input_maps <- config$paths$prediction_maps  # location of the prediction input maps
models_dir <- config$paths$models_dir       # location of the trained models
output_maps <- config$paths$output_maps     # location to save the prediction output maps

# -------------------------------------------------------
# functions
# -------------------------------------------------------

# Function to prepare categorical levels for landcover and soilcode vector layers
function_prepare_levels <- function(scale, lc_sf, sc_sf) {
  lc_level_file_name <- paste0("BRT_", scale, "_levels_lc.rds")
  soil_level_file_name <- paste0("BRT_", scale, "_levels_soil.rds")

  # Make sure vectors have desired factor levels, read them from the models dir
  desired_lc_levels <- readRDS(file.path(models_dir, lc_level_file_name))
  desired_soil_levels <- readRDS(file.path(models_dir, soil_level_file_name))
  
  lc_sf$category <- factor(lc_sf$category, levels = desired_lc_levels)
  sc_sf$soilcode <- factor(sc_sf$soilcode, levels = desired_soil_levels)
  
  plot(lc_sf, main = "Landcover Features with Groups")  
  # return files as list
  return(list(landcover_sf = lc_sf, soilcode_sf = sc_sf))
}

# Function to rasterize vector layers
function_rasterize_vector <- function(scale, lc_sf, sc_sf) {
  # Remove overlapping features in landcover_sf by dissolving them based on the 'category' field,
  # this is necessary for rasterization to work properly, and this also establishes a correct AOA
  
  message("rasterizing")

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
    lc_sf, 
    stacked_rasters, 
    field = "category",
    fun = mode_function,       # Custom mode function
    overwrite = TRUE, # overwrite existing raster
    progress = TRUE # show progress bar
  )
  
  print(names(landcover_raster))
  plot(landcover_raster, main = "Landcover Raster")  # Plot the landcover raster
  # save to file
  #file_name <- paste0("landcover_raster_", scale, "_Prediction.tif")
  #writeRaster(landcover_raster, file.path(output, file_name), overwrite = TRUE)
  
  soilcode_raster <- terra::rasterize(
    sc_sf, 
    stacked_rasters, 
    field = "soilcode",
    overwrite = TRUE # overwrite existing raster
  )
  
  plot(soilcode_raster, main = "Soilcode Raster")    # Plot the soilcode raster
  
  # save to file
  #file_name <- paste0("soilcode_raster_", scale, "_Prediction.tif")
  #writeRaster(soilcode_raster, file.path(output, file_name), overwrite = TRUE)
  
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
  return_df <- cbind(raster_df, Landcover = landcover_raster_df[[1]], Soilcode = soilcode_raster_df[[1]])
  return_df <- na.omit(return_df)
  
  return(return_df)
}

function_predict_with_models <- function(df, scale) {
  # Load trained gbm model
  gbm_model <- readRDS(file.path(models_dir, paste0("BRT_", scale, "_model.rds")))
  
  # Predict with the GBM model
  df$prediction <- predict(
    gbm_model, 
    newdata = df[, -c(1, 2)],
    n.trees = gbm_model$n.trees,
    type = "response"
  )
  
  # Convert predictions to raster
  prediction_raster <- rasterFromXYZ(df[, c("x", "y", "prediction")])
  plot(prediction_raster)
  
  # write tif file to output folder
  file_name <- paste0("predictions_", scale, ".tif")
  writeRaster(prediction_raster, file.path(output_maps, file_name), overwrite = TRUE)  
}

# ---------------------------------------------
# 1. List raster & vector files
# ---------------------------------------------
raster_pat  <- "\\.(tif|img|nc|grd)$"
vector_pat  <- "\\.(shp|gpkg|geojson)$"

raster_files <- list.files(input_maps,
                           pattern = raster_pat,
                           full.names = TRUE,
                           ignore.case = TRUE)

vector_files <- list.files(input_maps,
                           pattern = vector_pat,
                           full.names = TRUE,
                           ignore.case = TRUE)

# -------------------------------------------------------------
# 2. Build a RasterStack that points to the raster files
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
# 3. Prepare Categorical Data from Vector Layers
# -------------------------------------------------------------
lc_file <- vector_files[grepl("Landcover", vector_files, ignore.case = TRUE)]
sc_file <- vector_files[grepl("Soilcode", vector_files, ignore.case = TRUE)]

# Load vector files as Spatial*DataFrame
landcover_sf <- st_read(lc_file)
soilcode_sf <- st_read(sc_file)

# ---------------------------------------------
# 4. Start with 0.25m2 (Q) and 1m2 scales
# ---------------------------------------------

return_list <- function_prepare_levels("Q", landcover_sf, soilcode_sf)    # categorical levels for Q and 1m2 scale are the same
landcover_sf_Q <- return_list$landcover_sf
soilcode_sf_Q <- return_list$soilcode_sf

# remove NAs, as they cause issues during rasterization, and this also establishes a correct AOA
landcover_sf_Q <- landcover_sf_Q[!is.na(landcover_sf_Q$category), ]
soilcode_sf_Q <- soilcode_sf_Q[!is.na(soilcode_sf_Q$soilcode), ]

# Q scale
final_Q_df <- function_rasterize_vector("Q", landcover_sf_Q, soilcode_sf_Q)  # rasterize vector layers and prepare final_df for predictions
function_predict_with_models(final_Q_df, "Q")

# 1m scale
final_1m_df <- function_rasterize_vector("1m", landcover_sf_Q, soilcode_sf_Q)  # rasterize vector layers and prepare final_df for predictions
function_predict_with_models(final_1m_df, "1m")

# ---------------------------------------------
# 5. 100m mean and total scales
# ---------------------------------------------
return_list <- function_prepare_levels("100m", landcover_sf, soilcode_sf)    # categorical levels for 100m scales are the same
landcover_sf_100m <- return_list$landcover_sf
soilcode_sf_100m <- return_list$soilcode_sf

# remove NAs, as they cause issues during rasterization, and this also establishes a correct AOA
landcover_sf_100m <- landcover_sf_100m[!is.na(landcover_sf_100m$category), ]
soilcode_sf_100m <- soilcode_sf_100m[!is.na(soilcode_sf_100m$soilcode), ]

# 100m mean scale
final_100mmean_df <- function_rasterize_vector("100mean", landcover_sf_100m, soilcode_sf_100m)  # rasterize vector layers and prepare final_df for predictions

# 100m GBM models expect Canopy_Height, not Canopy_height, will change with new model training. For now, this changes the raster name so the prediction will work
# Change Canopy_height to Canopy_Height in final_100mmean_df (same with Soil_Organic_Matter, Soilcode, Landcover) 
names(final_100mmean_df)[names(final_100mmean_df) == "Canopy_height"] <- "Canopy_Height"
names(final_100mmean_df)[names(final_100mmean_df) == "Soil_organic_matter"] <- "Soil_Organic_Matter"
names(final_100mmean_df)[names(final_100mmean_df) == "Soilcode"] <- "soilcode"
names(final_100mmean_df)[names(final_100mmean_df) == "Landcover"] <- "landcover"

function_predict_with_models(final_100mmean_df, "100mean")

# 100m total scale
final_100mtotal_df <- function_rasterize_vector("100total", landcover_sf_100m, soilcode_sf_100m)  # rasterize vector layers and prepare final_df for predictions
# 100m GBM models expect Canopy_Height, not Canopy_height, will change with new model training. For now, this changes the raster name so the prediction will work
# Change Canopy_height to Canopy_Height in final_100mmean_df (same with Soil_Organic_Matter, Soilcode, Landcover) 
names(final_100mtotal_df)[names(final_100mtotal_df) == "Canopy_height"] <- "Canopy_Height"
names(final_100mtotal_df)[names(final_100mtotal_df) == "Soil_organic_matter"] <- "Soil_Organic_Matter"
names(final_100mtotal_df)[names(final_100mtotal_df) == "Soilcode"] <- "soilcode"
names(final_100mtotal_df)[names(final_100mtotal_df) == "Landcover"] <- "landcover"

function_predict_with_models(final_100mtotal_df, "100total")

