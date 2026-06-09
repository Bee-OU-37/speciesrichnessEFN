# Run post-processing analysis of species-richness prediction rasters.
# This keeps current scale-comparison calculations and diagnostics intact.

init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()

run_script_sequence(
  c("analysis/06. Analyze prediction maps/Analyse Prediction Patterns.R"),
  project_root = project_root
)
