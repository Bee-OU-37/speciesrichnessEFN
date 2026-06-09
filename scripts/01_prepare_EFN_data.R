# Prepare canonical project directories and validate expected raw-data layout.

init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()
config <- load_project_config(project_root)

required_dirs <- c(
  config$paths$raw_data_root,
  config$paths$efn_vegetation,
  config$paths$predictor_maps,
  config$paths$prediction_maps,
  config$paths$processed_data,
  config$paths$model_input_data,
  config$paths$analysis_output,
  config$paths$reports,
  unlist(config$paths$output_subfolders, use.names = FALSE)
)

for (relative_dir in required_dirs) {
  ensure_dir(file.path(project_root, relative_dir))
}

if (length(list.files(file.path(project_root, config$paths$efn_vegetation), all.files = FALSE)) == 0) {
  log_step("No EFN vegetation survey files found in data/raw/EFN vegetation survey data.")
}

if (length(list.files(file.path(project_root, config$paths$predictor_maps), all.files = FALSE)) == 0) {
  log_step("No predictor map files found in data/raw/Predictor maps.")
}

log_step("Project data directories are prepared.")
