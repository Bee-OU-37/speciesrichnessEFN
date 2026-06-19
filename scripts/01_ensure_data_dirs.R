# Prepare canonical project directories and validate expected raw-data layout.

init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()
config <- load_project_config(project_root)

# Ensure required directories exist for reproducible inputs/outputs.
ensure_dir(config$paths$raw_data_root)
ensure_dir(config$paths$efn_vegetation)
ensure_dir(config$paths$predictor_maps)
ensure_dir(config$paths$prediction_maps)
ensure_dir(config$paths$processed_data)
ensure_dir(config$paths$output)
ensure_dir(config$paths$models_dir)
ensure_dir(config$paths$output_maps)
ensure_dir(config$paths$reports)

# Inform users about expected raw inputs if folders are empty.
if (length(list.files(config$paths$efn_vegetation, all.files = FALSE)) == 0) {
  log_step("No EFN vegetation survey files found in data/raw/EFN vegetation survey data.")
}

if (length(list.files(config$paths$predictor_maps, all.files = FALSE)) == 0) {
  log_step("No predictor map files found in data/raw/Predictor maps.")
}

log_step("Project data directories are prepared.")
