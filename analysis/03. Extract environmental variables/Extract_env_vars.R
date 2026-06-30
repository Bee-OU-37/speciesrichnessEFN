# ---------------------------------
# Purpose: Extract environmental variables (raster and vector) at plot locations,
# ---------------------------------


extract_env_vars <- function(loc, input_maps_dir, processed_data_dir, output_dir_name) {
  
  # Construct full file paths
  loc_map_dir <- file.path(input_maps_dir, loc)
  output_path <- file.path(processed_data_dir, output_dir_name)
  # check if dir exists
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  
  # Find species richness file for the location
  species_richness_file <- file.path(processed_data_dir, pattern = paste0(loc, " species richness.xlsx"))
  
  # ========== 1. Read plot locations (as sf object) =========
  plots_df <- read_xlsx(species_richness_file, sheet = "Species_count_Plotcode")   
  plots_sf <- st_as_sf(plots_df, coords = c("Longitude_GIS", "Latitude_GIS"), crs = 4326)
  plots_sf <- st_transform(plots_sf, st_crs(28992))  # Transform to match raster CRS if needed
  
  # ========== 2. Extract raster values at points ==========
  # 2a. get location-specific raster files from the location's map directory
  raster_files <- list.files(loc_map_dir, pattern = "\\.tif$", full.names = TRUE)
  # 2b. add the general raster files from the general map directory
  raster_files <- c(raster_files, list.files(input_maps_dir, pattern = "\\.tif$", full.names = TRUE))
  
  #Loop through raster files, extract values at plot locations
  raster_vals_list <- list()
  for (f in raster_files) {
    r <- rast(f)
    # Reproject points if needed
    if (!st_crs(plots_sf) == crs(r)) {
      pts <- st_transform(plots_sf, crs(r))
    } else {
      pts <- plots_sf
    }
    vals <- terra::extract(r, plots_sf, ID = FALSE)[, 1]
    nm <- tools::file_path_sans_ext(basename(f))
    raster_vals_list[[nm]] <- vals
  }
  
  # ========== 3. Extract vector attribute: soilcode from Soilmap shapefile from general map dir ==========
  soil_sf <- st_read(file.path(input_maps_dir, "Soilmap.shp"), quiet = TRUE)
  plots_sf_soil <- st_transform(plots_sf, st_crs(soil_sf)) # Ensure CRS match
  soilcode_vals <- st_join(plots_sf_soil, soil_sf)$soilcode
  
  # ========== 4. Extract vector attribute: landcover from Landcover shapefile from location_specific map dir ==========
  landcover_sf <- st_read(file.path(loc_map_dir, "Landcover.shp"), quiet = TRUE)
  plots_sf_lc <- st_transform(plots_sf, st_crs(landcover_sf)) # Ensure CRS match
  landcover_vals <- st_join(plots_sf_lc, landcover_sf)$category
  
  # inspect values
  head(soilcode_vals)
  head(landcover_vals)
  
  # ========== 5. Combine all extracted variables into a data frame ==========
  env_data <- data.frame(
    plot_code = plots_df$Plotcode,
    plot_species_richness = plots_df$unique_species_count,
    do.call(cbind, raster_vals_list),
    soilcode = as.factor(soilcode_vals),
    landcover = as.factor(landcover_vals)
  )
  
  summary(env_data)
  
  # Remove rows with NA values
  any(rowSums(is.na(env_data)) > 0)        # Are there NA rows?
  #Remove rows with NA values
  env_data <- na.omit(env_data)
  
  summary(env_data)
  output_filename <- paste0("env_data_", loc, ".csv")
  
  # =========== 6. write to CSV file =============
  #adjust filename to your needs
  write.csv(env_data, file.path(output_path, output_filename), row.names = FALSE)
  
  return(env_data)
  
}
