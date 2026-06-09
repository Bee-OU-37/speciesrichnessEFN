init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()

run_script_sequence(
  c(
    "scripts/analysis/environment/01_extract_env_vars.R",
    "scripts/analysis/environment/02_extract_env_vars_100m_buffer.R",
    "scripts/analysis/environment/03_join_q_and_1m_lat_long.R"
  ),
  project_root = project_root
)
