# Prepare canonical project directories and validate expected raw-data layout.
# This script does not alter scientific content; it standardizes folder structure.

helpers_path <- if (file.exists("R/helpers.R")) "R/helpers.R" else "../R/helpers.R"
source(helpers_path)

project_root <- find_project_root()
config <- load_project_config(project_root)

# Ensure required directories exist for reproducible inputs/outputs.
ensure_dir(config$paths$raw_data_root)
ensure_dir(config$paths$efn_vegetation)
ensure_dir(config$paths$predictor_maps)
ensure_dir(config$paths$prediction_maps)
ensure_dir(config$paths$processed_data)
ensure_dir(config$paths$analysis_output)
ensure_dir(config$paths$reports)

# Inform users about expected raw inputs if folders are empty.
if (length(list.files(config$paths$efn_vegetation, all.files = FALSE)) == 0) {
  log_step("No EFN vegetation survey files found in data/raw/EFN vegetation survey data.")
}

if (length(list.files(config$paths$predictor_maps, all.files = FALSE)) == 0) {
  log_step("No predictor map files found in data/raw/Predictor maps.")
}

log_step("Project data directories are prepared.")
