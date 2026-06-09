init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()

run_script_sequence(
  c(
    "scripts/analysis/shapley/01_brt_q_shapley.R",
    "scripts/analysis/shapley/02_brt_1m_shapley.R",
    "scripts/analysis/shapley/03_brt_100m_shapley.R",
    "scripts/analysis/shapley/04_brt_100mQ_shapley.R"
  ),
  project_root = project_root
)
