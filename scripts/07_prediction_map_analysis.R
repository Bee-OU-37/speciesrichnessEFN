# Run post-processing analysis of species-richness prediction rasters.
# This keeps current scale-comparison calculations and diagnostics intact.

helpers_path <- if (file.exists("R/helpers.R")) "R/helpers.R" else "../R/helpers.R"
source(helpers_path)

project_root <- find_project_root()

run_script_sequence(
  c("analysis/06. Analyze prediction maps/Analyse Prediction Patterns.R"),
  project_root = project_root
)
