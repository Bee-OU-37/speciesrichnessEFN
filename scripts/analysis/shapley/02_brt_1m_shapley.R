# SHAPley analysis split from legacy combined model script.
source(file.path("..", "model_training", "02_brt_1m_train.R"))
# -------------------------------------------------
#  SHAP values
# -------------------------------------------------

#gbm_mod <- alpha_div.tc2.lr01
# load RDS file containing the model
if (file.exists("BRT_1m.rds")) gbm_mod <- readRDS("BRT_1m.rds")

shap_data <- test_set %>%
  dplyr::select(-alpha_diversity, -location)

shap_values <- kernelshap(gbm_mod, shap_data, nsim = 100)

sv <- shapviz(shap_values)

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

#save shapviz to file
#saveRDS(sv, file = "shapviz_1m.rds")

sv_importance(sv, kind = "bee")
sv_importance(sv, kind = "bar")
sv_importance(sv, kind = "no")
sv_importance(sv, kind = "both")
sv_importance(sv, kind = "bar", show_numbers= TRUE)

# plot importance bar with larger and black font on y-axis
sv_importance(sv, kind = "bar") +
  theme(axis.text.y = element_text(size = 12, color = "black"))

# plot dependence plots of all columns with all x-axises labels rotated to keep them readable
sv_dependence(sv, v = colnames(shap_data), color_var = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

#plot depence plots of all columns
sv_dependence(sv, v = colnames(shap_data), color_var = NULL)
#save to file
ggsave("shap_dependence_all_vars_1m.png", width = 12, height = 8)




# plot dependence for Landcover, with x labels rotated to keep them readable
sv_dependence(sv, v = "Soilcode") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot dependence for pH, without interactions colouring
sv_dependence(sv, v = "Distance_to_path", color_var = NULL)
sv_dependence(sv, v = "NDVI")

# -------------------------------------------------
# Percentage contribution of each variable to variance explained
# -------------------------------------------------
test_df <- test_set %>%
  dplyr::select(-location)

library(tidyverse)


# Observed variance
var_y <- var(test_df$alpha_diversity)

# contrib_vec <- sapply(select(shap_wide, -row),
#                       function(col) var(col, na.rm = TRUE) / var_y)
# 
# contrib_tbl <- tibble(variable = names(contrib_vec),
#                       perc_explained = 100 * contrib_vec) %>%
#   arrange(desc(perc_explained))
# 
# print(contrib_tbl)

shap_mat <- sv$S

shap_var <- apply(shap_mat, 2, var, na.rm = TRUE)
frac_explained <- shap_var / var_y

var_expl_tbl <- tibble(
  variable        = colnames(shap_mat),
  var_shap        = shap_var,
  frac_explained  = frac_explained,
  perc_explained  = 100 * frac_explained
) %>% arrange(desc(frac_explained))

print(var_expl_tbl)


#total variance explained
total_perc_expl <- sum(var_expl_tbl$perc_explained)
cat("\nTotal percentage of variance explained by all variables:", round(total_perc_expl, 2), "%\n")
#bar plot of percentage contribution

library(ggplot2)
ggplot(var_expl_tbl, aes(x = reorder(variable, -perc_explained), y = perc_explained)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Percentage Contribution of Each Variable to Variance Explained",
       x = "Variable",
       y = "Percentage of Variance Explained") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --------------------------------------------------------------
# Bootstrap CIs ---------------------------------
# --------------------------------------------------------------

set.seed(321)
B <- 2000
n_test <- nrow(test_df)
boot_frac <- matrix(NA, nrow = B, ncol = ncol(shap_mat))
colnames(boot_frac) <- colnames(shap_mat)

for (b in seq_len(B)) {
  idx   <- sample.int(n_test, n_test, replace = TRUE)
  y_b   <- test_df$alpha_diversity[idx]
  shap_b <- shap_mat[idx, , drop = FALSE]
  
  var_y_b    <- var(y_b, na.rm = TRUE)
  shap_var_b <- apply(shap_b, 2, var, na.rm = TRUE)
  boot_frac[b, ] <- shap_var_b / var_y_b
}

ci_low  <- apply(boot_frac, 2, quantile, probs = 0.025)
ci_high <- apply(boot_frac, 2, quantile, probs = 0.975)

var_expl_tbl <- var_expl_tbl %>%
  mutate(ci_low  = 100 * ci_low,
         ci_high = 100 * ci_high)

print(var_expl_tbl)

# -----------------------------------------------------------------
# prepare data for testing spatial autocorrelation in the residuals of the BRT model
# -----------------------------------------------------------------

#load model from file
if (file.exists("BRT_1m.rds")) gbm_mod <- readRDS("BRT_1m.rds")

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
write.csv(residuals_df, "1m_residuals_with_coordinates.csv", row.names = FALSE)
