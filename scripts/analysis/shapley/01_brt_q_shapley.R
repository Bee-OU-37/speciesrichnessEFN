# SHAPley analysis split from legacy combined model script.
source(file.path("..", "model_training", "01_brt_q_train.R"))
# -------------------------------------------------
#  SHAP values
# -------------------------------------------------

gbm_mod <- alpha_div.tc5.lr05
#gbm_mod <- alpha_div.tc2.lr001
#gbm_mod <- alpha_div.tc5.lr05.simp

shap_data <- test_set %>%
  dplyr::select(-alpha_diversity, -location)

shap_values <- kernelshap(gbm_mod, shap_data, nsim = 100)

sv <- shapviz(shap_values)

sv_importance(sv, kind = "bee")
sv_importance(sv, kind = "bar")
sv_importance(sv, kind = "no")
sv_importance(sv, kind = "both")
sv_importance(sv, kind = "bar", show_numbers= TRUE)

#plot dependence plots of all columns
sv_dependence(sv, v = colnames(shap_data), color_var = NULL)
#save to file
ggsave("shap_dependence_all_vars_Q.png", width = 12, height = 8)

# plot dependence for Landcover, with x labels rotated to keep them readable
sv_dependence(sv, v = "Landcover") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot dependence for pH
sv_dependence(sv, v = "Infrared")

# plot dependence for pH, without interactions colouring
sv_dependence(sv, v = "pH", color_var = NULL)

#save shapviz to file
#saveRDS(sv, file = "shapviz_Q.rds")


# -------------------------------------------------
# Analyse SHAP importance values for one Soilcode class only
# -------------------------------------------------

# -------------------------------------------------
# 1. Identify the Soilcode SHAP column(s)
# -------------------------------------------------
soil_shap_cols <- grep("Soilcode", colnames(sv$S), value = TRUE, ignore.case = TRUE)
# Use the single factor column
soil_shap_col <- "Soilcode"

if (is.matrix(sv$X)) sv$X <- as.data.frame(sv$X)   # ensure data.frame
# -------------------------------------------------
# 2. Subset to rows where Soilcode == "urban_soil"
# -------------------------------------------------

idx_soil <- which(sv$X$Soilcode == "urban_soil")
cat("Rows with urban soil:", length(idx_soil), "\n")

# -------------------------------------------------
# 2. Subset to rows where Soilcode = AVo, Hn23x, zWpx, vWpx
# -------------------------------------------------
soil_classes <- c("AVo", "Hn23x", "zWpx", "vWpx")
if (is.matrix(sv$X)) sv$X <- as.data.frame(sv$X)   # ensure data.frame
idx_soil <- which(sv$X$Soilcode %in% soil_classes)
cat("Rows with selected soil classes:", length(idx_soil), "\n")
sv_soil <- sv[idx_soil, ]   # new shapviz object (only those rows)


# -------------------------------------------------
# 3. Custom beeswarm plot without Soilcode SHAP values
# -------------------------------------------------

if (is.matrix(sv_soil$S)) sv_soil$S <- as.data.frame(sv_soil$S)
# 7a) Drop the Soilcode SHAP column we want to hide
shap_df <- sv_soil$S %>% select(-all_of(soil_shap_col))

# 7b) Long‑format tidy data
shap_long <- shap_df %>%
  mutate(obs_id = row_number()) %>%      # identifier for each observation
  pivot_longer(cols = -obs_id,
               names_to = "Feature",
               values_to = "Shapley")

# 7c) Order features by mean absolute SHAP (nice ordering)
feature_order <- shap_long %>%
  group_by(Feature) %>%
  summarise(mean_abs = mean(abs(Shapley), na.rm = TRUE)) %>%
  arrange(mean_abs) %>%
  pull(Feature)

shap_long$Feature <- factor(shap_long$Feature, levels = feature_order)

# 7d) Plot
p_custom <- ggplot(shap_long,
                   aes(x = Shapley, y = Feature, colour = Feature)) +
  geom_beeswarm(priority = "density", size = 1.5, alpha = 0.6) +
  scale_color_viridis_d(option = "C") +
  geom_vline(xintercept = sv_soil$baseline,
             linetype = "dashed", colour = "gray40") +
  labs(
    title = "Beeswarm of SHAP values (Fochteloërveen rows only)",
    subtitle = "All features except Soilcode",
    x = "SHAP value (model output units)",
    y = "Feature"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p_custom)

#7e) Plot a bar plot of mean absolute SHAP values without Soilcode
mean_abs_shap <- shap_long %>%
  group_by(Feature) %>%
  summarise(mean_abs = mean(abs(Shapley), na.rm = TRUE)) %>%
  arrange(desc(mean_abs))
# bar plot with features on the y-axis
p_bar <- ggplot(mean_abs_shap, aes(x = mean_abs, y = reorder(Feature, mean_abs), fill = Feature)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "C") +
  labs(
    title = "Mean Absolute SHAP values (urban soil rows only)",
    subtitle = "All features except Soilcode",
    x = "Mean Absolute SHAP value",
    y = "Feature"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p_bar)


# -------------------------------------------------
# Save the results to a CSV file
# -------------------------------------------------
# Save the Shap values to a CSV file
#saveRDS(sv, file = "shapviz_Q_alpha_div_tc5_lr05.rds")

# Save the trained model
#saveRDS(alpha_div.tc5.lr05, file = "Q_alpha_div_tc5_lr05_model.rds")

# To load the model back in future sessions, use:
# brt_dismo_Q_loaded <- readRDS("Q_alpha_div_tc5_lr05_model.rds")


# -------------------------------------------------
#  Load saved SHAP viz object and translate category names
# -------------------------------------------------

# load saved shapviz object
if (file.exists("shapviz_Q.rds")) sv <- readRDS("shapviz_Q.rds")

# Translate Landcover category names from Dutch to English
sv[["X"]][["Landcover"]] <- recode(sv[["X"]][["Landcover"]],
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
                           "oever, slootkant" = "embankment, ditch side",
                           "bouwland" = "arable land",
                           "waterloop" = "water course")


# plot dependence for Landcover, with x labels rotated to keep them readable and x-axis font size increased
sv_dependence(sv, v = "Soilcode") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

sv_dependence(sv, v = "Distance_to_path") +
  theme(axis.text.x = element_text(hjust = 1, size = 12))


# plot importance bar with larger and black font on y-axis
sv_importance(sv, kind = "bar", show_numbers= TRUE) +
  theme(axis.text.y = element_text(size = 12, color = "black"))

rotate_x_axis <- function(p, angle = 45, hjust = 1) {
  p + theme(
    axis.text.x = element_text(angle = angle,
                               hjust   = hjust,
                               vjust   = ifelse(angle < 90, 1, 0.5))
  )
}

library(ggplot2)      # sv_dependence returns a ggplot object
library(patchwork)    # or gridExtra / cowplot – any layout works

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
if (file.exists("BRT_Q.rds")) gbm_mod <- readRDS("BRT_Q.rds")

#remove Latitude_GIS and Longitude_GIS from test set for prediction
noLoc_test_set <- test_set %>% dplyr::select(-Latitude_GIS, -Longitude_GIS)

pred_test <- predict(gbm_mod, newdata = noLoc_test_set)

residuals <- test_set$alpha_diversity - pred_test
library(spdep)
# Create spatial weights based on the locations of the test set

# Assuming test_set has columns 'longitude' and 'latitude' for spatial coordinates
coordinates <- test_set %>% select(Latitude_GIS, Longitude_GIS)
# perform Moran's I for spatial autocorrelation in the residuals for dataset with identical points

#create .csv with residuals and latitude and longitude for each point from test set
residuals_df <- data.frame(residuals = residuals,
                           Latitude_GIS = test_set$Latitude_GIS,
                           Longitude_GIS = test_set$Longitude_GIS)
write.csv(residuals_df, "Q_residuals_with_coordinates.csv", row.names = FALSE)


knn <- knearneigh(coordinates, k = 4)
weights <- knn2nb(knn)
# Perform Moran's I test for spatial autocorrelation in the residuals
moran_test <- moran.test(residuals, nb2listw(weights))
print(moran_test)

