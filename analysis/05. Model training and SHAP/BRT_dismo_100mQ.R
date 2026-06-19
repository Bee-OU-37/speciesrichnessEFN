library(dismo)   # provides gbm.step
library(gbm)     # underlying gbm implementation
library(dplyr)
library(kernelshap)
library(shapviz)
library(readr)
library(ggplot2)


# -------------------------------------------------
#  Load ecological data
# -------------------------------------------------
df_raw <- read.csv("mean_and_total_alpha_diversity_100mQ_buffer.csv")  

#remove columns not required for training
df_raw <- df_raw %>%
  dplyr::select(-Triangle, -Quadrant_code, -n_plots, -Latitude_GIS, -Longitude_GIS)

#temporary line for residual creation
# df_raw <- df_raw %>%
#   dplyr::select(-Triangle, -Quadrant_code, -n_plots)


head(df_raw)
# Convert the location column to a factor
df_raw$Location <- factor(df_raw$Location)

# -------------------------------------------------
# Keep only required features
# -------------------------------------------------

# remove variables Temp_min, Temp_mean, Precipitation, and DTM because of VIF analysis
df_selection <- df_raw %>%
  dplyr::select(-Temp_min, -Temp_mean, -Precipitation, -DTM)

# remove one of the two responses: keep plot_total_alphadiv, remove plot_mean_alphadiv
response_col <- "plot_total_alphadiv"
df_selection <- df_selection %>%
  dplyr::select(-plot_mean_alphadiv)

# View the structure of the selected data frame
str(df_selection)

# -------------------------------------------------
#  Create percentage‑based balanced sampling
# -------------------------------------------------
# reproducibility
set.seed(2025)   

pct_per_loc <- 0.80   # <-- 80% of each site's records go to the training set

# create unique id for each row
df_selection <- df_selection %>% mutate(row_id = row_number())

cat("selected data set size     :", nrow(df_selection), "\n")

train_balanced <- df_selection %>%
  group_by(Location) %>%                     # work within each location
  slice_sample(prop = pct_per_loc) %>%       # sample the given proportion
  ungroup()

# Remaining rows become the external test set
test_set <- anti_join(df_selection, train_balanced, by = "row_id")

# Remove the temporary row_id columns
train_balanced <- train_balanced %>% dplyr::select(-row_id)
test_set       <- test_set %>% dplyr::select(-row_id)

cat("data set size     :", nrow(df_selection), "\n")
cat("Training set size :", nrow(train_balanced), "\n")
cat("Test set size     :", nrow(test_set), "\n")

# -------------------------------------------------
# Identify all character columns that are categorical
# -------------------------------------------------
char_cols <- names(train_balanced)[sapply(train_balanced, is.character)]

# -------------------------------------------------
# Turn them into factors (this gives  control over the levels)
# -------------------------------------------------
for (cn in char_cols) {
  train_balanced[[cn]] <- factor(train_balanced[[cn]])
  test_set[[cn]]       <- factor(test_set[[cn]])
}

# -------------------------------------------------
# Unify the levels across train and test for each of those factors
# -------------------------------------------------
for (cn in char_cols) {
  all_lev <- dplyr::union(levels(train_balanced[[cn]]),
                   levels(test_set[[cn]]))
  train_balanced[[cn]] <- factor(train_balanced[[cn]], levels = all_lev)
  test_set[[cn]]       <- factor(test_set[[cn]],   levels = all_lev)
}

# extract the factor variables for later use
my_factor_soil <- train_balanced[[char_cols[1]]]
my_factor_lc <- train_balanced[[char_cols[2]]]

#save levels of categorical variables
saveRDS(levels(my_factor_lc), "BRT_dismo_100m_levels_lc.rds")
saveRDS(levels(my_factor_soil), "BRT_dismo_100m_levels_soil.rds")

# -------------------------------------------------
# Run gbm.step
# -------------------------------------------------
set.seed(2025)

end_column <- ncol(train_balanced)  # predictor columns
print(end_column)

# #checks
# class(train_balanced[[end_column]])
# sum(is.na(train_balanced[[end_column]]))
# is.vector(train_balanced[[end_column]])
# which(names(train_balanced) == end_column)
# sapply(train_balanced[2:15], class)
# colSums(is.na(train_balanced[2:15]))
# str(train_balanced[[end_column]])
#train_balanced[[end_column]] <- as.numeric(train_balanced[[end_column]])

gbm_x <- 3:end_column   
print(gbm_x)
print(names(train_balanced)[gbm_x])
colSums(is.na(train_balanced[gbm_x]))


#change train_balanced from tibble to data.frame to prevent errors
train_balanced <- as.data.frame(train_balanced)

any(train_balanced[[end_column]] < 0)

# alpha_div.tc5.lr1 <- gbm.step(data=train_balanced, gbm.x = 3:end_column, gbm.y = 2,
#                              family = "gaussian", tree.complexity = 2,
#                              learning.rate = 0.1, bag.fraction = 0.75,
#                              silent = F)

#summary(alpha_div.tc5.lr1)

 # alpha_div.tc5.lr01 <- gbm.step(data=train_balanced, gbm.x = 3:end_column, gbm.y = 2,
 #                                 family = "gaussian", tree.complexity = 4,
 #                                 learning.rate = 0.01, bag.fraction = 0.75,
 #                                 n.trees = 50, step.size = 1, silent = F)

#  alpha_div.tc4.lr05 <- gbm.step(data=train_balanced, gbm.x = 3:end_column, gbm.y = 2,
#                                   family = "gaussian", tree.complexity = 2,
#                                   learning.rate = 0.05, bag.fraction = 0.75,
#                                   silent = F)
# 
alpha_div.tc4.lr01 <- gbm.step(data=train_balanced, gbm.x = 3:end_column, gbm.y = 2,
                               family = "gaussian", tree.complexity = 4,
                               learning.rate = 0.01, bag.fraction = 0.75,
                               silent = F)

# alpha_div.fixed <- gbm(data=train_balanced, gbm.x = 2:15, gbm.y = 1,
#                          learning.rate=0.005, tree.complexity=2, n.trees=3400)
# 
#summary(alpha_div.tc4.lr01)
# 
# names(alpha_div.tc2.lr01)

#create linear regression model for comparison
#alpha_div.lm <- lm(plot_mean_alphadiv ~ ., data = train_balanced %>% dplyr::select(-Location))

# -------------------------------------------------
#  External test performance
# -------------------------------------------------
pred_test <- predict(alpha_div.tc4.lr01, newdata = test_set)
#pred_test <- predict(alpha_div.tc4.lr05, newdata = test_set)
#pred_test <- predict(alpha_div.tc5.lr01, newdata = test_set)
#pred_test <- predict(alpha_div.lm, newdata = test_set)

test_r2   <- cor(test_set[[response_col]], pred_test)^2
test_RMSE <- sqrt(mean((test_set[[response_col]] - pred_test)^2))

cat("\nExternal test performance:\n")
cat("  R²   =", round(test_r2, 3), "\n")
cat("  RMSE =", round(test_RMSE, 3), "\n")

# Compare the CV‑(R^{2}) to the naïve baseline (predict the mean of the training set)
train_mean <- mean(train_balanced[[response_col]])
print(train_mean)
#print CV R2

# Plot observed vs predicted
plot(test_set[[response_col]], pred_test,
     xlab = "Observed species richness",
     ylab = "Predicted species richness",
     main = "Observed vs Predicted Species Richness on Test Set")
abline(0, 1, col = "red")  # 1:1 line

alpha_div.simp <- gbm.simplify(alpha_div.tc2.lr05)

alpha_div.tc2.lr05.simp <- gbm.step(train_balanced,
                                  gbm.x=alpha_div.simp$pred.list[[3]], gbm.y=2,
                                  family = "gaussian", tree.complexity=4, 
                                  learning.rate=0.01)

summary(alpha_div.tc2.lr05.simp)

pred_test <- predict(alpha_div.tc2.lr05.simp, newdata = test_set)

test_rmse <- sqrt(mean((test_set[[response_col]] - pred_test)^2))
test_r2   <- cor(test_set[[response_col]], pred_test)^2

cat("\nSimplified BRT - External test performance:\n")
cat("  RMSE =", round(test_rmse, 3), "\n")
cat("  R²   =", round(test_r2, 3), "\n")

plot(test_set[[response_col]], pred_test,
     xlab = "Observed species richness",
     ylab = "Predicted species richness",
     main = "Observed vs Predicted Species Richness on Test Set")
abline(0, 1, col = "red")  # 1:1 line


# CV statistics:
alpha_div.tc.lr01[34]

cat("\nCross-validated performance (on training set):\n")
cat("  CV R²   =", round(alpha_div.tc4.lr01$cv.statistics$correlation.mean^2, 3), "\n")
cat("  CV RMSE =", round(alpha_div.tc4.lr01$cv.statistics$deviance.mean^0.5, 3), "\n")
cat("  CV Deviance =", round(alpha_div.tc4.lr01$cv.statistics$deviance.mean, 3), "\n")
cat("  CV Correlation =", round(alpha_div.tc4.lr01$cv.statistics$correlation.mean, 3), "\n")

#save model to file
saveRDS(alpha_div.tc4.lr01, file = "BRT_100total_model.rds")

# -------------------------------------------------
#  SHAP values
# -------------------------------------------------
#lm_mod <- alpha_div.lm
gbm_mod <- alpha_div.tc4.lr01
#gbm_mod <- alpha_div.tc5.lr01
gbm_mod <- readRDS("BRT_100mean.rds")

shap_data <- test_set %>%
  dplyr::select(-all_of(response_col), -Location)

shap_values <- kernelshap(gbm_mod, shap_data, nsim = 100)

sv <- shapviz(shap_values)

sv_importance(sv, kind = "bee")
sv_importance(sv, kind = "bar")
sv_importance(sv, kind = "no")
sv_importance(sv, kind = "both")
sv_importance(sv, kind = "bar", show_numbers= TRUE)

#plot depence plots of all columns
sv_dependence(sv, v = colnames(shap_data), color_var = NULL)
#save to file
ggsave("shap_dependence_all_vars_100total.png", width = 12, height = 8)

sv_i <- shapviz(kernelshap(gbm_mod, shap_data, nsim = 100, interactions = TRUE))
sv_interaction(sv_i) #only with sv <- shapviz(kernelshap(gbm_mod, shap_data, nsim = 100, interactions = TRUE))

# plot dependence for Landcover, with x labels rotated to keep them readable
sv_dependence(sv, v = "landcover") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot dependence for pH
sv_dependence(sv, v = "NDVI")

#save shapviz to file
#saveRDS(sv, file = "shapviz_100mmean.rds")

# -------------------------------------------------
# Percentage contribution of each variable to variance explained
# -------------------------------------------------
test_df <- test_set %>%
  dplyr::select(-Location)

library(tidyverse)

# sv$S is a matrix: rows = observations, cols = predictors
shap_long <- as_tibble(sv$S) %>%
  mutate(row = row_number()) %>%               # keep observation ID
  pivot_longer(
    cols   = -row,
    names_to = "variable",
    values_to = "shap_value"
  )

# Wide format
shap_wide <- shap_long %>%
  pivot_wider(names_from = variable, values_from = shap_value)

# Observed variance of the response_col
var_y <- var(test_df[[response_col]])

contrib_vec <- sapply(dplyr::select(shap_wide, -row),
                      function(col) var(col, na.rm = TRUE) / var_y)

contrib_tbl <- tibble(variable = names(contrib_vec),
                      perc_explained = 100 * contrib_vec) %>%
  arrange(desc(perc_explained))

print(contrib_tbl)

# -------------------------------------------------
# Save the results to a CSV file
# -------------------------------------------------

# Save the trained model
#saveRDS(alpha_div.tc5.lr05, file = "Q_alpha_div_tc5_lr05_model.rds")

# To load the model back in future sessions, use:
# brt_dismo_Q_loaded <- readRDS("Q_alpha_div_tc5_lr05_model.rds")


# -------------------------------------------------
# Pairwise interactions
# -------------------------------------------------
find.int <- gbm.interactions(gbm_mod)
find.int$interactions

find.int$rank.list
gbm.perspec(gbm_mod, 10, 5)
gbm.perspec(gbm_mod, 6, 4)

# -------------------------------------------------
# plot larger fonts
# -------------------------------------------------

library(ggplot2)      # sv_dependence returns a ggplot object
library(patchwork)    # or gridExtra / cowplot – any layout works

# Translate Landcover category names from Dutch to English
sv[["X"]][["landcover"]] <- recode(sv[["X"]][["landcover"]],
                                   "naaldbos" = "coniferous forest",
                                   "gemengd bos" = "mixed forest",  
                                   "BGT pand" = "building",
                                   "erf" = "yard",
                                   "onverhard" = "unsealed surface",
                                   "gesloten verharding" = "impervious surface",
                                   "open verharding" = "pervious surface",
                                   "grasland agrarisch" = "agricultural grassland",
                                   "grasland overig" = "other grassland",
                                   "groenvoorziening" = "green urban area",
                                   "bouwland" = "arable land")

# plot importance bar with larger and black font on y-axis
sv_importance(sv, kind = "bar") +
  theme(axis.text.y = element_text(size = 12, color = "black"))

# plot importance bar with larger and black font on y-axis
sv_importance(sv, kind = "bar", show_numbers= TRUE) +
  theme(axis.text.y = element_text(size = 12, color = "black"))

# plot dependence for soilcode, with x labels rotated to keep them readable and x-axis font size increased, plot soilcode with pH as colouring
sv_dependence(sv, v = "soilcode", color_var = "pH") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
sv_dependence(sv, v = "soilcode") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

sv_dependence(sv, v = "Infrared") +
  theme(axis.text.x = element_text(hjust = 1, size = 12))

# SHAP dependence plots for all 10 variables, without interactions colouring (color_var = NULL)
p1 <- sv_dependence(sv, v = colnames(shap_data)[1], color_var = NULL)
p2 <- sv_dependence(sv, v = colnames(shap_data)[2], color_var = NULL)
p3 <- sv_dependence(sv, v = colnames(shap_data)[3], color_var = NULL)
p4 <- sv_dependence(sv, v = colnames(shap_data)[4], color_var = NULL)
p5 <- sv_dependence(sv, v = colnames(shap_data)[5], color_var = NULL)
p6 <- sv_dependence(sv, v = colnames(shap_data)[6], color_var = NULL)
p7 <- sv_dependence(sv, v = colnames(shap_data)[7], color_var = NULL)
p8 <- sv_dependence(sv, v = colnames(shap_data)[8], color_var = NULL)
p9 <- sv_dependence(sv, v = colnames(shap_data)[9], color_var = NULL)
p10 <- sv_dependence(sv, v = colnames(shap_data)[10], color_var = NULL)

# Rotate x‑axis on **all** of them
p9 <- rotate_x_axis(p9, angle = 45, hjust = 1)
p10 <- rotate_x_axis(p10, angle = 45, hjust = 1)


(p1 | p2 | p3 | p4) /
  (p5 | p6 | p7 | p8) /
  (p9 | p10)   

# -----------------------------------------------------------------
# prepare data for testing spatial autocorrelation in the residuals of the BRT model
# -----------------------------------------------------------------

#load model from file
gbm_mod <- readRDS("BRT_100total.rds")

#remove Latitude_GIS and Longitude_GIS from test set for prediction
noLoc_test_set <- test_set %>% dplyr::select(-Latitude_GIS, -Longitude_GIS)

pred_test <- predict(gbm_mod, newdata = noLoc_test_set)

print(test_set$plot_total_alphadiv)

residuals <- test_set$plot_total_alphadiv - pred_test
library(spdep)
# Create spatial weights based on the locations of the test set

# Assuming test_set has columns 'longitude' and 'latitude' for spatial coordinates
coordinates <- test_set %>% select(Latitude_GIS, Longitude_GIS)
# perform Moran's I for spatial autocorrelation in the residuals for dataset with identical points

#create .csv with residuals and latitude and longitude for each point from test set
residuals_df <- data.frame(residuals = residuals,
                           Latitude_GIS = test_set$Latitude_GIS,
                           Longitude_GIS = test_set$Longitude_GIS)
write.csv(residuals_df, "100total_residuals_with_coordinates.csv", row.names = FALSE)

