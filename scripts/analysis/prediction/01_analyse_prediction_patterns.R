# --------------------------------------------------------------------------------
# Title: Patterns of Species Richness Predictions Across Spatial Scales
# Analyze patterns of species richness predictions across different spatial scales
# --------------------------------------------------------------------------------

# Load necessary libraries
library(terra)
library(ggplot2)
library(reshape2)
library(spdep)

# Load the four rasters representing species richness at different spatial scales
richness_scale1 <- rast("predictions_Q.tif")
richness_scale2 <- rast("predictions_1m.tif")
richness_scale3 <- rast("predictions_100m_mean.tif")
richness_scale4 <- rast("predictions_100m_total.tif")

# set all CRS to EPSG:28992
crs(richness_scale1) <- "EPSG:28992"
crs(richness_scale2) <- "EPSG:28992"
crs(richness_scale3) <- "EPSG:28992"
crs(richness_scale4) <- "EPSG:28992"

# print extents
print(ext(richness_scale1))
print(ext(richness_scale2))
print(ext(richness_scale3))
print(ext(richness_scale4))

# Align extents and resolutions if necessary
richness_scale2 <- resample(richness_scale2, richness_scale1, method = "bilinear")
richness_scale3 <- resample(richness_scale3, richness_scale1, method = "bilinear")
richness_scale4 <- resample(richness_scale4, richness_scale1, method = "bilinear")

# Load raster datasets representing species richness at different scales
richness_stack <- c(richness_scale1, richness_scale2, richness_scale3, richness_scale4)
names(richness_stack) <- c("0.25m","1m","100mean","100total")

# Normalize the stacked raster to a 0-1 scale
richness_norm <- (richness_stack - min(richness_stack)) / (max(richness_stack) - min(richness_stack))

# ------------------------------------------------------------------
# 1. Histograms and Density Plots
# ------------------------------------------------------------------
# Examine the distribution of species richness values at each scale
richness_df <- data.frame(
  Scale1 = values(richness_stack[[1]]),
  Scale2 = values(richness_stack[[2]]),
  Scale3 = values(richness_stack[[3]]),
  Scale4 = values(richness_stack[[4]])
)

richness_long <- melt(richness_df, variable.name = "Scale", value.name = "Richness")  # Reshape for ggplot

ggplot(richness_long, aes(x = Richness, fill = Scale)) +
  geom_density(alpha = 0.4) +
  labs(title = "Density of Predictions Across Scales") +
  theme_minimal()

# To reduce overplotting, we can sample a subset of the data for visualization
# Sample a subset of the data
set.seed(123)  # For reproducibility
subset_richness <- richness_long[sample(nrow(richness_long), size = 100000), ]

# Re-plot with the subset
ggplot(subset_richness, aes(x = Richness, fill = Scale)) +
  geom_density(alpha = 0.4) +
  labs(title = "Density of Predictions Across Scales") +
  theme_minimal()


# ------------------------------------------------------------------
# 2. Spatial Vizualization
# ------------------------------------------------------------------
# Visualize the normalized richness predictions across scales
plot(richness_stack, main = "Species Richness Predictions at Different Scales") 

# ------------------------------------------------------------------
# 3. Variation Across Scales
# ------------------------------------------------------------------
# Calculate and plot the coefficient of variation across scales
# This can help identify areas where values change significantly across scales.
cv_raster <- app(richness_stack, fun = function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
plot(cv_raster, main = "Coefficient of Variation Across Scales")


# ------------------------------------------------------------------
# 4. Hotspot Analysis
# ------------------------------------------------------------------
# Identify areas of high or low richness (hotspots) at each scale and compare them:
# Extract values from the first layer of the raster stack
values_richness <- values(richness_stack[[1]])  # Extract raster values as a vector

# Compute the 90th percentile (exclude NA values)
q90 <- quantile(values_richness, 0.9, na.rm = TRUE)

# Create a logical raster of hotspots (areas above the 90th percentile)
hotspots <- richness_stack[[1]] > q90

# Plot the hotspots
plot(hotspots, col = c("lightgrey", "red"), legend = FALSE, main = "90th Percentile Hotspots")

# Identify hotpots occuring both in smallest and largest scale
hotspots_scale1 <- richness_stack[[1]] > quantile(values(richness_stack[[1]]), 0.9, na.rm = TRUE)
hotspots_scale4 <- richness_stack[[4]] > quantile(values(richness_stack[[4]]), 0.9, na.rm = TRUE)
common_hotspots <- hotspots_scale1 & hotspots_scale4

# Plot common hotspots, use colouring normal for ecological hotspots.
plot(common_hotspots, col = c("lightgrey", "red"), legend = FALSE, main = "Common hotspots in 0.25m² and 100m total scales")


# calculate how much of the hotspots in hotspots_scale1 are also in common_hotspots
hotspot_overlap <- sum(values(hotspots_scale1) & values(common_hotspots), na.rm = TRUE) /
  sum(values(hotspots_scale1), na.rm = TRUE)
# calculate how much of the hotspots in hotspots_scale4 are also in common_hotspots
hotspot_overlap2 <- sum(values(hotspots_scale4) & values(common_hotspots), na.rm = TRUE) /
  sum(values(hotspots_scale4), na.rm = TRUE)

print(paste("Proportion of 0.25m² hotspots also in common hotspots:", round(hotspot_overlap * 100, 2), "%"))
print(paste("Proportion of 100m total hotspots also in common hotspots:", round(hotspot_overlap2 * 100, 2), "%"))

# ------------------------------------------------------------------
# 5. Spatial Cross-Scale Comparison
# ------------------------------------------------------------------
# 5.2 Differences Across Scales
#Compute the difference between richness predictions across scales (absolute or relative changes):
diff_scale1 <- richness_stack[[2]] - richness_stack[[1]]

# Convert the terra raster to a data frame
raster_df <- as.data.frame(diff_scale1, xy = TRUE, na.rm = TRUE)

# Check the structure of the resulting data frame
head(raster_df) # Should contain columns: x, y, layer (value)
#rename column "1m" to "layer"
raster_df <- dplyr::rename(raster_df, layer = names(diff_scale1)[1])

# Plot using ggplot2
ggplot(raster_df, aes(x = x, y = y, fill = layer)) +
  geom_raster() +  # Use raster tiles for the plot
  scale_fill_gradient2(
    low = "red",     # Color for negative values
    mid = "white",   # Color for zero
    high = "blue",   # Color for positive values
    midpoint = 0,    # Set zero as the neutral midpoint
    name = "Difference"  # Legend title
  ) +
  theme_minimal() +  # Use a clean theme
  labs(
    title = "Difference Between 1m² and 0.25m²",
    x = "Longitude",
    y = "Latitude"
  )

# Compute difference between 100m total and 0.25m2
diff_scale2 <- richness_stack[[4]] - richness_stack[[1]]

# Convert the terra raster to a data frame
raster_df <- as.data.frame(diff_scale2, xy = TRUE, na.rm = TRUE)

# Check the structure of the resulting data frame
head(raster_df) # Should contain columns: x, y, layer (value)
#rename column "1m" to "layer"
raster_df <- dplyr::rename(raster_df, layer = names(diff_scale2)[1])

# Plot using ggplot2
ggplot(raster_df, aes(x = x, y = y, fill = layer)) +
  geom_raster() +  # Use raster tiles for the plot
  scale_fill_gradient2(
    low = "red",     # Color for negative values
    mid = "white",   # Color for zero
    high = "blue",   # Color for positive values
    midpoint = 0,    # Set zero as the neutral midpoint
    name = "Difference"  # Legend title
  ) +
  theme_minimal() +  # Use a clean theme
  labs(
    title = "Difference Between 100m total and 0.25m2",
    x = "Longitude",
    y = "Latitude"
  )


# ------------------------------------------------------------------
# 6. Statistical Modeling
# ------------------------------------------------------------------
# Use statistical tests or models to formally test hypotheses about scale effects.

# 6.1 Analysis of Variance (ANOVA)
# Determine whether richness differs significantly between scales:
anova_result <- aov(Richness ~ Scale, data = richness_long)
summary(anova_result)

posthoc_result <- TukeyHSD(anova_result)
print(posthoc_result)

plot(posthoc_result)



