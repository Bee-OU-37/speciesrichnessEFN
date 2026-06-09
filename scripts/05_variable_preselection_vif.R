# Run VIF/GVIF predictor pre-selection scripts.
# This step keeps variable filtering decisions and diagnostics unchanged.

init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

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
