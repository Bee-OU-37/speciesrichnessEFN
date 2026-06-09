# Load required packages
library(terra)
library(sf)
library(car)
library(readxl)
library(dplyr)

# ---- USER SETTINGS ----
excel_file <- "mean_and_total_alpha_diversity_100m.xlsx"   # Path to Excel file
sheet <- "Fv"                                     # Sheet number or name
plot_name_col <- "Triangle"                        # Column with plot (triangle) names
lon_col <- "triangle_longitude"                         # Longitude column
lat_col <- "triangle_latitude"                          # Latitude column
map_dir <- "C:/Users/Johanneke/Maps/VIF maps"      # Directory with raster files

# 1. Read plot locations (as sf object)
plots_df <- read_excel(excel_file, sheet = sheet)
plots_sf <- st_as_sf(plots_df, coords = c(lon_col, lat_col), crs = 4326)

plots_sf <- st_transform(plots_sf, st_crs(28992))  # Transform to match raster CRS if needed

#####Extract average values for a buffer around your points#####
raster_files <- list.files(map_dir, pattern = "\\.tif$", full.names = TRUE)

# create a 100m buffer around each point
buffer_100m <- st_buffer(plots_sf, dist = 100)

# Loop through raster files, calculate mean value within each buffer around plot locations
raster_vals_list <- list()
for (f in raster_files) {
  r <- rast(f)
  # extract values at the buffered locations
  mean_vals <- terra::extract(r, buffer_100m, fun = mean, na.rm = TRUE, ID = FALSE)
  print(mean_vals)
  nm <- tools::file_path_sans_ext(basename(f))
  raster_vals_list[[nm]] <- mean_vals
}

# ========== 3. Extract vector attribute: soilcode from soilmap shapefile ==========
soil_sf <- st_read(file.path(map_dir, "soilmap.shp"), quiet = TRUE)
buffer_sf_soil <- st_transform(buffer_100m, st_crs(soil_sf)) # Ensure CRS match
# take the soilcode that covers the largest area under the buffer. Soilcode is in soil_sf$soilcode
buffer_100m_with_soil <- st_join(
  buffer_sf_soil,
  soil_sf,
  join = st_intersects,
  largest = TRUE               # keep the polygon with the largest overlap
) %>% rename(soilcode = soilcode)   # keep the original column name
soilcode_vals <- buffer_100m_with_soil$soilcode

head(soilcode_vals)

# ========== 4. Extract vector attribute: landcover from BGT shapefile ==========
landcover_sf <- st_read(file.path(map_dir, "BGT_Fv.shp"), quiet = TRUE)
buffer_sf_lc <- st_transform(buffer_100m, st_crs(landcover_sf)) # Ensure CRS match
# take the soilcode that covers the largest area under the buffer. Soilcode is in soil_sf$soilcode
landcover_vals <- st_join(
  buffer_sf_lc,
  landcover_sf,
  join = st_intersects,
  largest = TRUE               # keep the polygon with the largest overlap
)$category

head(landcover_vals)

# ========== 5. Combine all extracted variables into a data.frame ==========
env_data <- data.frame(
  plot_code = plots_df$Triangle,
  plot_mean_alphadiv = plots_df$mean_unique_Species_count,
  plot_total_alphadiv = plots_df$total_species_richness,
  location = sheet,
  Latitude_GIS = plots_df$triangle_latitude,
  Longitude_GIS = plots_df$triangle_longitude,
  do.call(cbind, raster_vals_list),
  soilcode = as.factor(soilcode_vals),
  landcover = as.factor(landcover_vals),
  USP_location = plots_df$Location
)

summary(env_data)

filename <- paste0("env_data_100m_buffer_", sheet, ".csv")

#write to Excel file
write.csv(env_data, filename, row.names = FALSE)


any(rowSums(is.na(env_data)) > 0)        # Are there NA rows?
#Remove rows with NA values
env_data <- na.omit(env_data)

summary(env_data)




