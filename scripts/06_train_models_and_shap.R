# Run species-richness model training and SHAP interpretation scripts.
# Existing model algorithms, settings, and libraries are preserved.

init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

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
