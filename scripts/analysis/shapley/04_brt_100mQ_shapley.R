# SHAPley analysis split from legacy combined model script.
source(file.path("..", "model_training", "04_brt_100mQ_train.R"))
# -------------------------------------------------
#  SHAP values
# -------------------------------------------------
#lm_mod <- alpha_div.lm
gbm_mod <- alpha_div.tc4.lr01
#gbm_mod <- alpha_div.tc5.lr01
if (file.exists("BRT_100mean.rds")) gbm_mod <- readRDS("BRT_100mean.rds")

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
if (file.exists("BRT_100total.rds")) gbm_mod <- readRDS("BRT_100total.rds")

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

