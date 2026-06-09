init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()

run_script_sequence(
  c(
    "scripts/analysis/preselection/01_vif_total_1mQ.R",
    "scripts/analysis/preselection/02_vif_total_100m.R",
    "scripts/analysis/preselection/03_gvif_total_1m.R",
    "scripts/analysis/preselection/04_gvif_total_100m.R"
  ),
  project_root = project_root
)
