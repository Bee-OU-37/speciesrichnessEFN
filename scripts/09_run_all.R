# Execute the full reproducible species richness pipeline end-to-end.
# This script runs package setup through final report rendering.

init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()
config <- load_project_config(project_root)

run_script_sequence(config$pipeline_steps, project_root = project_root)

log_step("Full pipeline finished.")
