# Execute the full reproducible species richness pipeline end-to-end.
# This script runs package setup through final report rendering.

helpers_path <- if (file.exists("R/helpers.R")) "R/helpers.R" else "../R/helpers.R"
source(helpers_path)

project_root <- find_project_root()
config <- load_project_config(project_root)

run_script_sequence(config$pipeline_steps, project_root = project_root)

log_step("Full pipeline finished.")
