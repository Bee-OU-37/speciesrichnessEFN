# SHAPley analysis split from legacy combined model script.
source(file.path("..", "model_training", "03_brt_100m_train.R"))
# -------------------------------------------------
#  SHAP values
# -------------------------------------------------
lm_mod <- alpha_div.lm
gbm_mod <- alpha_div.tc2.lr01
#gbm_mod <- alpha_div.tc5.lr05.simp

shap_data <- df_selection %>%
  dplyr::select(-all_of(response_col), -location)

shap_values <- kernelshap(gbm_mod, shap_data, nsim = 100)

sv <- shapviz(shap_values)

sv_importance(sv, kind = "bee")

# plot dependence for Landcover, with x labels rotated to keep them readable
sv_dependence(sv, v = "Soilcode") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot dependence for pH
sv_dependence(sv, v = "Groundwater")

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
# Pairwise interactions
# -------------------------------------------------
find.int <- gbm.interactions(gbm_mod)
find.int$interactions

find.int$rank.list
gbm.perspec(gbm_mod, 10, 5)
gbm.perspec(gbm_mod, 6, 1)
