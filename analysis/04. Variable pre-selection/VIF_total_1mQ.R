# Load required packages
library(terra)
library(sf)
library(car)
library(dplyr)
library(readxl)
library(ggplot2)
library(tibble)
library(Hmisc)      # rcorr() gives r and p together
library(corrplot)

# ---- USER SETTINGS ----
excel_file <- "env_data_total.xlsx"   # Path to Excel file

# 1. Read environmental variables data (as df)
env_data <- read_excel(excel_file)
env_data <- as.data.frame(env_data)

summary(env_data)

any(rowSums(is.na(env_data)) > 0)        # Are there NA rows?
#Remove rows with NA values
env_data <- na.omit(env_data)

summary(env_data)

# ========== Remove response variable ==========
response = env_data[,1]  # Save response variable
env_data <- env_data[, -1]  # Remove response variable from env_data for VIF analysis

summary(env_data)


# ========== Remove categorical variables to be able to do VIF analysis ==========
# Which columns are numeric?
num_vars <- env_data %>% select(where(is.numeric)) %>% names()

# Which columns are categorical? (factor or character)
cat_vars <- env_data %>% select(where(~ is.factor(.) | is.character(.))) %>% names()

#remove categorical variables for VIF analysis
env_data <- env_data %>% select(all_of(num_vars))

# ========== 8. Standardize (scale) all variables ==========
#scale env_data
env_data_vif_scaled <- env_data %>%
  mutate(across(all_of(num_vars), ~ scale(.)[, 1]))   # [,1] drops the matrix class

env_data_vif_scaled <- as.data.frame(scale(env_data))

summary(env_data_vif_scaled)

#remove Temp_min as it is highly correlated with Temp_mean and Temp_max 1
#env_data_vif_scaled <- env_data_vif_scaled %>% select(-Temp_min)
#remove Temp_mean as it is highly correlated with Temp_max 2
#env_data_vif_scaled <- env_data_vif_scaled %>% select(-Temp_mean)
# #remove Temp_max as the VIF value is still too high
#env_data_vif_scaled <- env_data_vif_scaled %>% select(-Temp_max)
#remove Precitipation as it is highly correlated with DTM 5
#env_data_vif_scaled <- env_data_vif_scaled %>% select(-Precipitation)
#remove DTM as it is highly correlated with Precipitation and pH 3
#env_data_vif_scaled <- env_data_vif_scaled %>% select(-DTM)
#remove Soil organic matter as it is highly correlated with distance to path and pH 6
#env_data_vif_scaled <- env_data_vif_scaled %>% select(-Soil_organic_matter)
# #remove Groundwater as it is highly correlated with distance to path
# env_data_vif_scaled <- env_data_vif_scaled %>% select(-Groundwater)
#remove pH as it has the highest VIF score 4
#env_data_vif_scaled <- env_data_vif_scaled %>% select(-pH)

#cor(env_data_vif_scaled)

# ========== 9. VIF Analysis ==========
lm_vif <- lm(response ~ . , data = env_data_vif_scaled)

#are there aliased coefficients?
#alias(lm_vif)
#which are the aliased coefficients
summary(lm_vif)

#remove aliased coefficients from the data
#env_data_vif_scaled <- env_data_vif_scaled %>% select(-c(SoilcodezWpx))

vif_values <- vif(lm_vif)
print(vif_values)

#create barplot showing horizontal line where VIF values are above √5 or √10 (2.2 or 3.2)

#convert vif_values to data frame
df <- as.data.frame(vif_values)
print(df)
df_long <- tibble::rownames_to_column(df, var = "Bar")

ggplot(df_long, aes(x = Bar, y = `vif_values`)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_hline(yintercept = 5, colour = "darkorange", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 10, colour = "darkred",    linetype = "dashed", linewidth = 1) +
  labs(title = "VIF values",
       x = "Predictors", y = "VIF") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Creating a correlation matrix
cor_matrix <- cor(env_data_vif_scaled)
 
# #cor_matrix <- cor(env_data[c("Distance_to_path", "DTM", "Precitipation", "Temp_mean" "Infrared", "NDVI", "pH", "Canopy_height")])
# Visualizing the correlation matrix
library(corrplot)
abs_cor_matrix <- abs(cor_matrix)    # Take absolute value
corrplot(abs_cor_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8)

summary(env_data_vif_scaled)
#image(cor_matrix, main = "Correlation Matrix", col = colorRampPalette(c("blue", "white", "red"))(20), axes = )
print(cor_matrix)

# # 10. Optionally, remove variables with VIF > 5 or 10, and repeat
# high_vif_vars <- names(vif_values[vif_values > 5])
# print("Variables with VIF > 5:")
# print(high_vif_vars)
#env_data_vif_scaled_final <- env_data_vif_scaled[, !(names(env_data_vif_scaled) %in% high_vif_vars)]

# ======= Additional: Correlation matrix with p-values =======
res <- rcorr(as.matrix(env_data_vif_scaled), type = "pearson")   # or "spearman"

# Extract the matrices
cor_mat <- res$r   # correlation coefficients
p_mat   <- res$P   # p‑values

print(p_mat)
# Helper: convert p‑values to significance symbols
sig_symbols <- function(p) {
  ifelse(p < .001, "***",
         ifelse(p < .01, "**",
                ifelse(p < .05, "*", "")))
}

# Build a matrix of symbols matching the correlation matrix
sym_mat <- matrix(sig_symbols(p_mat), nrow = nrow(p_mat))
rownames(sym_mat) <- rownames(p_mat)
colnames(sym_mat) <- colnames(p_mat)

# Plot
corrplot(cor_mat,
         method = "color",        # fill colour by magnitude
         type = "upper",          # show only upper triangle (optional)
         tl.col = "black",        # variable labels colour
         addCoef.col = "black",   # show correlation coefficient numbers
         p.mat = p_mat,           # supply p‑values
         sig.level = 0.05,        # threshold for shading non‑significant cells
         insig = "blank",         # hide non‑significant correlations
         cl.pos = "b",            # colour legend position
         number.cex = 0.7)        # size of the coefficient text

corrplot(cor_mat,
         method = "circle",
         type = "lower",
         addCoef.col = "black",
         p.mat = p_mat,
         sig.level = 0.05,
         insig = "label_sig",      # prints the significance symbol
         pch.cex = 1.2,
         pch.col = "red")          # colour of the star/label



