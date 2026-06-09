# Run species-richness model training and SHAP interpretation scripts.
# Existing model algorithms, settings, and libraries are preserved.

helpers_path <- if (file.exists("R/helpers.R")) "R/helpers.R" else "../R/helpers.R"
source(helpers_path)

project_root <- find_project_root()

run_script_sequence(
  c(
    "analysis/05. Model training and SHAP/BRT_dismo_Q.R",
    "analysis/05. Model training and SHAP/BRT_dismo_1m.R",
    "analysis/05. Model training and SHAP/BRT_dismo_100m.R",
    "analysis/05. Model training and SHAP/BRT_dismo_100mQ.R"
  ),
  project_root = project_root
)
