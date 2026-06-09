library(dplyr)
library(moments)

# -------------------------------------------------
#  Load ecological data
# -------------------------------------------------
df_raw <- read.csv("mean_and_total_alpha_diversity_100mQ_buffer.csv")  

# -------------------------------------------------
# look at distribution of alpha diversity
hist(df_raw$plot_mean_alphadiv, main = "Histogram of mean species count", xlab = "Mean species richness", ylab = "Frequency", breaks = 20)

#look at the distribution of alpha diversity with density plot
plot(density(df_raw$plot_mean_alphadiv, na.rm = TRUE), main = "Density Plot of mean Alpha Diversity", xlab = "Alpha Diversity", ylab = "Density")

# look at summary statistics of alpha diversity
summary(df_raw$plot_mean_alphadiv)
sd(df_raw$plot_mean_alphadiv)

# look at the boxplot of alpha diversity
boxplot(df_raw$plot_mean_alphadiv, main = "Boxplot of Alpha Diversity", ylab = "Alpha Diversity")

#are there negative values in alpha diversity?
any(df_raw$plot_mean_alphadiv < 0)

#what are the minimum, mean, median, maximum, standard deviation, CV, skewness, and kurtosis of alpha diversity?
min_alpha <- min(df_raw$plot_mean_alphadiv, na.rm = TRUE)
mean_alpha <- mean(df_raw$plot_mean_alphadiv, na.rm = TRUE)
median_alpha <- median(df_raw$plot_mean_alphadiv, na.rm = TRUE)
max_alpha <- max(df_raw$plot_mean_alphadiv, na.rm = TRUE)
sd_alpha <- sd(df_raw$plot_mean_alphadiv, na.rm = TRUE)
cv_alpha <- sd_alpha / mean_alpha
skewness_alpha <- skewness(df_raw$plot_mean_alphadiv, na.rm = TRUE)
kurtosis_alpha <- kurtosis(df_raw$plot_mean_alphadiv, na.rm = TRUE)
cat("Minimum alpha diversity:", min_alpha, "\n")
cat("Mean alpha diversity:", mean_alpha, "\n")
cat("Median alpha diversity:", median_alpha, "\n")
cat("Maximum alpha diversity:", max_alpha, "\n")
cat("Standard deviation of alpha diversity:", sd_alpha, "\n")
cat("Coefficient of variation of alpha diversity:", cv_alpha, "\n")
cat("Skewness of alpha diversity:", skewness_alpha, "\n")  
cat("Kurtosis of alpha diversity:", kurtosis_alpha, "\n")

# plot Landcover values against alpha diversity, make x-axis labels smaller font to fit in the image, remove x-axis label for clarity
# reduce font size of x-axis labels
boxplot(plot_mean_alphadiv ~ landcover, data = df_raw, main = "Boxplot of Alpha Diversity by Landcover", xlab = "", ylab = "alpha diversity", cex.axis=0.6, las=2)

#plot soilcode values against alpha diversity, make x-axis labels smaller font to fit in the image, remove x-axis label for clarity
boxplot(plot_mean_alphadiv ~ soilcode, data = df_raw, main = "Boxplot of Alpha Diversity by Soilcode", xlab = "", ylab = "alpha diversity", las=2)

