init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()

run_script_sequence(
  c(
    "scripts/analysis/model_training/01_brt_q_train.R",
    "scripts/analysis/model_training/02_brt_1m_train.R",
    "scripts/analysis/model_training/03_brt_100m_train.R",
    "scripts/analysis/model_training/04_brt_100mQ_train.R"
  ),
  project_root = project_root
)
