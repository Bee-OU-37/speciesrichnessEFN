# ---------------------------------
# Purpose: Extract environmental variables (raster and vector) at plot locations,
# ---------------------------------

# Load required packages
library(terra)
library(sf)
library(car)
library(readr)

# ---- USER SETTINGS ----
csv_file <- "Location species richness.csv"        # Species richness count data file of *Location*
plot_name_col <- "Plotcode"                        # Column with plot names
lon_col <- "Longitude_GIS"                         # Longitude column
lat_col <- "Latitude_GIS"                          # Latitude column
map_dir <- "..."                                   # Local directory with raster files

# 1. Read plot locations (as sf object)
plots_df <- read.csv(csv_file)
plots_sf <- st_as_sf(plots_df, coords = c("Longitude_GIS", "Latitude_GIS"), crs = 4326)

plots_sf <- st_transform(plots_sf, st_crs(28992))  # Transform to match raster CRS if needed

# ========== 2. Extract raster values at points ==========
raster_files <- list.files(map_dir, pattern = "\\.tif$", full.names = TRUE)

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

# ========== 3. Extract vector attribute: soilcode from Soilmap shapefile ==========
soil_sf <- st_read(file.path(map_dir, "Soilmap.shp"), quiet = TRUE)
plots_sf_soil <- st_transform(plots_sf, st_crs(soil_sf)) # Ensure CRS match
soilcode_vals <- st_join(plots_sf_soil, soil_sf)$soilcode

# ========== 4. Extract vector attribute: landcover from Landcover shapefile ==========
landcover_sf <- st_read(file.path(map_dir, "Landcover.shp"), quiet = TRUE)
plots_sf_lc <- st_transform(plots_sf, st_crs(landcover_sf)) # Ensure CRS match
landcover_vals <- st_join(plots_sf_lc, landcover_sf)$category

# inspect values
head(soilcode_vals)
head(landcover_vals)

# ========== 5. Combine all extracted variables into a data frame ==========
env_data <- data.frame(
  plot_code = plots_df$Plotcode,
  plot_alphadiv = plots_df$Unique_species_count,
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

#write to CSV file
#adjust filename to your needs
write.csv(env_data, "env_data_Location.csv", row.names = FALSE)
