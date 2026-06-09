# Load required packages
library(terra)
library(sf)
library(car)
library(dplyr)
library(ggplot2)
library(tibble)

# 1. Read environmental variables data (as df)
env_data <- read.csv("env_data_total_100m_buffer.csv")
env_data <- as.data.frame(env_data)

summary(env_data)

any(rowSums(is.na(env_data)) > 0)        # Are there NA rows?
#Remove rows with NA values
env_data <- na.omit(env_data)

#remove columns not required for analysis
env_data <- env_data %>%
  dplyr::select(-plot_code, , -location, -USP_location, -Latitude_GIS, -Longitude_GIS, -plot_total_alphadiv)

summary(env_data)


response = env_data[,1]  # Save response variable
env_data <- env_data[, -1]  # Remove response variable from env_data for VIF analysis

summary(env_data)


# ========== 6. Remove response variable and one-hot encode categorical variables (soilcode, landcover) ==========
#env_data_vif <- as.data.frame(model.matrix(~ . - 1, data = env_data[,-1]))
#any(rowSums(is.na(env_data_vif)) > 0)        # Are there NA rows?

# ========== 7. Remove rows with missing data ==========
#env_data_vif <- env_data_vif[complete.cases(env_data_vif), ]

# ========== 8. Standardize (scale) all variables ==========
# Which columns are numeric?
num_vars <- env_data %>% select(where(is.numeric)) %>% names()

# Which columns are categorical? (factor or character)
cat_vars <- env_data %>% select(where(~ is.factor(.) | is.character(.))) %>% names()

#env_data_vif_scaled <- env_data %>%
#  mutate(across(all_of(num_vars), ~ scale(.)[, 1]))   # [,1] drops the matrix class

env_data_vif_scaled <- env_data

summary(env_data_vif_scaled)

#remove Temp_min as it is highly correlated with Temp_mean and Temp_max
env_data_vif_scaled <- env_data_vif_scaled %>% select(-Temp_min)
# #remove Temp_mean as it is highly correlated with Temp_max
env_data_vif_scaled <- env_data_vif_scaled %>% select(-Temp_mean)
# #remove Temp_max as the VIF value is still too high
env_data_vif_scaled <- env_data_vif_scaled %>% select(-Temp_max)
# remove Precitipation as it is highly correlated with DTM
env_data_vif_scaled <- env_data_vif_scaled %>% select(-Precipitation)
# remove DTM as it is highly correlated with Precipitation and pH
env_data_vif_scaled <- env_data_vif_scaled %>% select(-DTM)
# # #remove Soil organic matter as it is highly correlated with distance to path and pH
# # env_data_vif_scaled <- env_data_vif_scaled %>% select(-Soil_organic_matter)
# # #remove Groundwater as it is highly correlated with distance to path
# env_data_vif_scaled <- env_data_vif_scaled %>% select(-Groundwater)
# #remove pH as it has the highest VIF score
# env_data_vif_scaled <- env_data_vif_scaled %>% select(-pH)
# #remove soilcode 
env_data_vif_scaled <- env_data_vif_scaled %>% select(-soilcode)


#cor(env_data_vif_scaled)

# ========== 9. VIF Analysis ==========
lm_vif <- lm(response ~ . , data = env_data_vif_scaled)

# -------------------------------------------------
# Identify aliased coefficients
# -----------------------------------------------
aliased_vec   <- summary(lm_vif)$aliased          # logical vector
aliased_names <- names(aliased_vec)[aliased_vec]

cat("Aliased coefficients (dropped because of singularities):\n")
print(aliased_names)

# # -------------------------------------------------
# # Optional: refit without the offending term(s)
# # -------------------------------------------------
# if (length(aliased_names) > 0) {
#   new_formula <- reformulate(
#     termlabels = setdiff(attr(terms(lm_vif), "term.labels"), aliased_names),
#     response   = as.character(formula(lm_vif))[2]
#   )
#   lm_fit_refit <- lm(new_formula, data = env_data_vif_scaled)
#   cat("\nRefitted model (without aliased terms):\n")
#   print(summary(lm_fit_refit))
# }



#are there aliased coefficients?
alias(lm_vif)
#which are the aliased coefficients
summary(lm_vif)$aliased

#remove rows with aliased coefficients from the data
#env_data_vif_scaled <- env_data_vif_scaled %>% select(-c("landcover"))

lm_vif2 <- update(lm_vif, . ~ . - landcover)


vif_values <- vif(lm_vif)
alias(lm_vif)
print(vif_values)

#create barplot showing horizontal line where VIF values are above √5 or √10 (2.2 or 3.2)

#convert vif_values to data frame
df <- as.data.frame(vif_values)
df_long <- tibble::rownames_to_column(df, var = "Bar")

ggplot(df_long, aes(x = Bar, y = `GVIF^(1/(2*Df))`)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_hline(yintercept = 2.2, colour = "darkorange", linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 3.2, colour = "darkred",    linetype = "dashed", linewidth = 1) +
  labs(title = "aGSIF values",
       x = "Predictors", y = "aGSIF") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# # Creating a correlation matrix
cor_matrix <- cor(env_data_vif_scaled[c("Distance_to_path", "Infrared", "NDVI", "pH", "Canopy_height")])
# Visualizing the correlation matrix
library(corrplot)
abs_cor_matrix <- abs(cor_matrix)    # Take absolute value
corrplot(abs_cor_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8)

#image(cor_matrix, main = "Correlation Matrix", col = colorRampPalette(c("blue", "white", "red"))(20), axes = )


# # 10. Optionally, remove variables with VIF > 5 or 10, and repeat
# high_vif_vars <- names(vif_values[vif_values > 5])
# print("Variables with VIF > 5:")
# print(high_vif_vars)
#env_data_vif_scaled_final <- env_data_vif_scaled[, !(names(env_data_vif_scaled) %in% high_vif_vars)]







