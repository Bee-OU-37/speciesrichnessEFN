init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()

run_script_sequence(
  c("scripts/analysis/prediction/01_analyse_prediction_patterns.R"),
  project_root = project_root
)
