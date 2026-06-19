# Execute the full reproducible species richness pipeline end-to-end.
# This script runs package setup through final report rendering.

init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()
config <- load_project_config(project_root)

source("scripts/00_install_packages.R")
source("scripts/01_ensure_data_dirs.R")
source("scripts/02_calculate_species_richness.R")
source("scripts/03_extract_environmental_variables.R")
source("scripts/04_investigate_data.R")
source("scripts/05_variable_preselection_vif.R")
source("scripts/06_train_models_and_shap.R")
source("scripts/07_prediction_map_analysis.R")
source("scripts/08_render_report.R")

# rmarkdown::render(
#   "analysis/delta_micro_vs_macro.Rmd",
#   output_dir = "output"
# )

log_step("Full pipeline finished.")
