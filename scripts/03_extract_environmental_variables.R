# Run environmental predictor extraction and harmonization scripts.
# Scripts are kept as-is and only orchestrated for reproducibility.

init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()

run_script_sequence(
  c(
    "analysis/03. Extract environmental variables/Extract_env_vars.R",
    "analysis/03. Extract environmental variables/Extract_env_vars_100m - Buffer.R",
    "analysis/03. Extract environmental variables/joinQand1mForLatLong.R"
  ),
  project_root = project_root
)
