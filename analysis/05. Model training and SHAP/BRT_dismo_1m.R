library(dismo)   # provides gbm.step
library(gbm)     # underlying gbm implementation
library(dplyr)
library(kernelshap)
library(shapviz)
library(ggplot2)
library(tidyr)

# -------------------------------------------------
#  Load ecological data
# -------------------------------------------------
#df <- read.csv("env_data_total_1m.csv")
df <- read.csv("env_data_total_1m_inclLocations.csv")

head(df)

# remove variables Temp_min, Temp_mean, Precipitation, and DTM because of VIF analysis
df_raw <- df %>%
  dplyr::select(-Temp_min, -Temp_mean, -Precipitation, -DTM)

cat("selected data set size     :", nrow(df_raw), "\n")

# Convert the location column to a factor
df_raw$location <- factor(df_raw$location)

# -------------------------------------------------
#  Create percentage‑based balanced sampling
# -------------------------------------------------
# reproducibility
set.seed(2026)   

pct_per_loc <- 0.80   # <-- 80% of each site's records go to the training set

train_balanced <- df_raw %>%
  group_by(location) %>%                     # work within each location
  slice_sample(prop = pct_per_loc) %>%       # sample the given proportion
  ungroup()

# Remaining rows become the external test set
test_set <- anti_join(df_raw, train_balanced, by = colnames(df_raw))

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
saveRDS(levels(my_factor_lc), "BRT_dismo_1m_levels_lc.rds")
saveRDS(levels(my_factor_soil), "BRT_dismo_1m_levels_soil.rds")

# -------------------------------------------------
# Run gbm.step
# -------------------------------------------------
set.seed(2025)

end_column <- ncol(train_balanced)-1  # predictor columns, do not include location column
print(end_column)

# #checks
# class(train_balanced$alpha_diversity)
# sum(is.na(train_balanced$alpha_diversity))
# is.vector(train_balanced$alpha_diversity)
# which(names(train_balanced) == "alpha_diversity")
# sapply(train_balanced[2:15], class)
# colSums(is.na(train_balanced[2:15]))
# str(train_balanced$alpha_diversity)
#train_balanced$alpha_diversity <- as.numeric(train_balanced$alpha_diversity)

gbm_x <- 2:end_column   
print(gbm_x)
print(names(train_balanced)[gbm_x])
colSums(is.na(train_balanced[gbm_x]))


#change train_balanced from tibble to data.frame to prevent errors
train_balanced <- as.data.frame(train_balanced)

any(train_balanced$alpha_diversity < 0)

# alpha_div.tc5.lr005 <- gbm.step(data=train_balanced, gbm.x = 2:15, gbm.y = 1,
#                             family = "poisson", tree.complexity = 5,
#                             learning.rate = 0.001, bag.fraction = 0.75, 
#                             silent = F)

#summary(alpha_div.tc5.lr005)

alpha_div.tc2.lr01 <- gbm.step(data=train_balanced, gbm.x = 2:end_column, gbm.y = 1,
                                family = "gaussian", tree.complexity = 2,
                                learning.rate = 0.01, bag.fraction = 0.75,
                                silent = F)

# alpha_div.tc5.lr05 <- gbm.step(data=train_balanced, gbm.x = 2:end_column, gbm.y = 1,
#                                family = "gaussian", tree.complexity = 5,
#                                learning.rate = 0.05, bag.fraction = 0.5, 
#                                n.folds = 5, silent = F)

# alpha_div.fixed <- gbm(data=train_balanced, gbm.x = 2:15, gbm.y = 1,
#                          learning.rate=0.005, tree.complexity=2, n.trees=3400)
# 
#summary(alpha_div.tc5.lr05)
# 
# names(alpha_div.tc2.lr01)

#create linear regression model for comparison
#alpha_div.lm <- lm(alpha_diversity ~ ., data = train_balanced %>% dplyr::select(-location))

pred_test <- predict(alpha_div.tc2.lr01, newdata = test_set)
#pred_test <- predict(alpha_div.lm, newdata = test_set)

test_r2   <- cor(test_set$alpha_diversity, pred_test)^2
test_RMSE <- sqrt(mean((test_set$alpha_diversity - pred_test)^2))

cat("\nExternal test performance:\n")
cat("  R²   =", round(test_r2, 3), "\n")
cat("  RMSE =", round(test_RMSE, 3), "\n")


# Compare the CV‑(R^{2}) to the naïve baseline (predict the mean of the training set)
train_mean <- mean(train_balanced$alpha_diversity)
print(train_mean)
#print CV R2


# Plot observed vs predicted
plot(test_set$alpha_diversity, pred_test,
     xlab = "Observed species richness",
     ylab = "Predicted species richness",
     main = "Observed vs Predicted Species Richness on Test Set")
abline(0, 1, col = "red")  # 1:1 line

alpha_div.simp <- gbm.simplify(alpha_div.tc2.lr01)

alpha_div.tc2.lr01.simp <- gbm.step(train_balanced,
                                  gbm.x=alpha_div.simp$pred.list[[2]], gbm.y=1,
                                  family = "gaussian", tree.complexity=5, 
                                  learning.rate=0.005)

summary(alpha_div.tc2.lr01.simp)

pred_test <- predict(alpha_div.tc2.lr01.simp, newdata = test_set)

test_rmse <- sqrt(mean((test_set$alpha_diversity - pred_test)^2))
test_r2   <- cor(test_set$alpha_diversity, pred_test)^2

cat("\nSimplified BRT - External test performance:\n")
cat("  RMSE =", round(test_rmse, 3), "\n")
cat("  R²   =", round(test_r2, 3), "\n")

plot(test_set$alpha_diversity, pred_test,
     xlab = "Observed alpha diversity",
     ylab = "Predicted alpha diversity",
     main = "Observed vs Predicted Alpha Diversity on Test Set (Simplified BRT)")
abline(0, 1, col = "red")  # 1:1 line

#save model to file
#saveRDS(alpha_div.tc2.lr01, file = "BRT_1m_model.rds")

# CV statistics:
alpha_div.tc2.lr01[34]

cat("\nCross-validated performance (on training set):\n")
cat("  CV R²   =", round(alpha_div.tc2.lr01$cv.statistics$correlation.mean^2, 3), "\n")
cat("  CV RMSE =", round(alpha_div.tc2.lr01$cv.statistics$deviance.mean^0.5, 3), "\n")
cat("  CV Deviance =", round(alpha_div.tc2.lr01$cv.statistics$deviance.mean, 3), "\n")
cat("  CV Correlation =", round(alpha_div.tc2.lr01$cv.statistics$correlation.mean, 3), "\n")

# -------------------------------------------------
#  SHAP values
# -------------------------------------------------

#gbm_mod <- alpha_div.tc2.lr01
# load RDS file containing the model
gbm_mod <- readRDS("BRT_1m.rds")

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
total_perc_expl <- sum(contrib_tbl$perc_explained)
cat("\nTotal percentage of variance explained by all variables:", round(total_perc_expl, 2), "%\n")
#bar plot of percentage contribution

library(ggplot2)
ggplot(contrib_tbl, aes(x = reorder(variable, -perc_explained), y = perc_explained)) +
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
  y_b   <- y_test[idx]
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
gbm_mod <- readRDS("BRT_1m.rds")

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

