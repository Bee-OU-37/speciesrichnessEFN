# Execute the full reproducible species richness pipeline end-to-end.

source("scripts/_init.R")

project_root <- find_project_root()

pipeline_scripts <- c(
  "scripts/00_install_packages.R",
  "scripts/01_prepare_EFN_data.R",
  "scripts/02_calculate_species_richness.R",
  "scripts/03_extract_environmental_variables.R",
  "scripts/04_investigate_data.R",
  "scripts/05_variable_preselection_vif.R",
  "scripts/06_train_models.R",
  "scripts/07_shapley_analysis.R",
  "scripts/08_prediction_map_analysis.R",
  "scripts/09_render_report.R"
)

run_script_sequence(pipeline_scripts, project_root = project_root)

log_step("Full pipeline finished.")
