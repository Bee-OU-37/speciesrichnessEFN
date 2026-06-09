# Run VIF/GVIF predictor pre-selection scripts.
# This step keeps variable filtering decisions and diagnostics unchanged.

helpers_path <- if (file.exists("R/helpers.R")) "R/helpers.R" else "../R/helpers.R"
source(helpers_path)

project_root <- find_project_root()

run_script_sequence(
  c(
    "analysis/04. Variable pre-selection/VIF_total_1mQ.R",
    "analysis/04. Variable pre-selection/VIF_total_100m.R",
    "analysis/04. Variable pre-selection/GVIF_total_1m.R",
    "analysis/04. Variable pre-selection/GVIF_total_100m.R"
  ),
  project_root = project_root
)
