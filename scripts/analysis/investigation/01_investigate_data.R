# -------------------------------------------------
# Investigate ecological data
# Author: J. Siljee
# Date: January 2026
# -------------------------------------------------

#  Load necessary libraries
library(dplyr)
library(moments)

# -------------------------------------------------
#  Load ecological data
# -------------------------------------------------
df_raw <- read.csv("env_data_total_1m.csv")  

# -------------------------------------------------
# Distribution of alpha diversity with a histogram
hist(df_raw$alpha_diversity, main = "Histogram of species richness", xlab = "Species Richness", ylab = "Frequency", breaks = 20)

# Distribution of alpha diversity with a density plot
plot(density(df_raw$alpha_diversity, na.rm = TRUE), main = "Density Plot of Alpha Diversity", xlab = "Alpha Diversity", ylab = "Density")

# Summary statistics of alpha diversity
summary(df_raw$alpha_diversity)
sd(df_raw$alpha_diversity)

# Boxplot of alpha diversity
boxplot(df_raw$alpha_diversity, main = "Boxplot of Alpha Diversity", ylab = "Alpha Diversity")

# Are there negative values in alpha diversity?
any(df_raw$alpha_diversity < 0)

# Minimum, mean, median, maximum, standard deviation, CV, skewness, and kurtosis of alpha diversity?
min_alpha <- min(df_raw$alpha_diversity, na.rm = TRUE)
mean_alpha <- mean(df_raw$alpha_diversity, na.rm = TRUE)
median_alpha <- median(df_raw$alpha_diversity, na.rm = TRUE)
max_alpha <- max(df_raw$alpha_diversity, na.rm = TRUE)
sd_alpha <- sd(df_raw$alpha_diversity, na.rm = TRUE)
cv_alpha <- sd_alpha / mean_alpha
skewness_alpha <- skewness(df_raw$alpha_diversity, na.rm = TRUE)
kurtosis_alpha <- kurtosis(df_raw$alpha_diversity, na.rm = TRUE)
cat("Minimum alpha diversity:", min_alpha, "\n")
cat("Mean alpha diversity:", mean_alpha, "\n")
cat("Median alpha diversity:", median_alpha, "\n")
cat("Maximum alpha diversity:", max_alpha, "\n")
cat("Standard deviation of alpha diversity:", sd_alpha, "\n")
cat("Coefficient of variation of alpha diversity:", cv_alpha, "\n")
cat("Skewness of alpha diversity:", skewness_alpha, "\n")  
cat("Kurtosis of alpha diversity:", kurtosis_alpha, "\n")

# -------------------------------------------------
# Investigate categorical variables against alpha diversity
# Translate category names from Dutch to English
# -------------------------------------------------
df_raw$Landcover <- recode(df_raw$Landcover,
                            "naaldbos" = "coniferous forest",
                            "loofbos" = "deciduous forest",  
                            "gemengd bos" = "mixed forest",  
                            "BGT pand" = "building",
                            "heide" = "heathland",
                            "erf" = "yard",
                            "onverhard" = "unsealed surface",
                            "gesloten verharding" = "impervious surface",
                            "open verharding" = "pervious surface",
                            "grasland agrarisch" = "agricultural grassland",
                            "grasland overig" = "other grassland",
                            "groenvoorziening" = "green urban area",
                            "water" = "water",
                            "oever, slootkant" = "bank, ditch side",
                            "bouwland" = "arable land",
                            "waterloop" = "watercourse")

# plot Landcover values against alpha diversity, make x-axis labels smaller font to fit in the image, remove x-axis label for clarity
# reduce font size of x-axis labels
boxplot(alpha_diversity ~ Landcover, data = df_raw, main = "Boxplot of Species Richness by Landcover", xlab = "", ylab = "species richness", cex.axis=0.5, las=2)

#plot soilcode values against alpha diversity, make x-axis labels smaller font to fit in the image, remove x-axis label for clarity
boxplot(alpha_diversity ~ Soilcode, data = df_raw, main = "Boxplot of Species Richness by Soilcode", xlab = "", ylab = "species richness", las=2)

