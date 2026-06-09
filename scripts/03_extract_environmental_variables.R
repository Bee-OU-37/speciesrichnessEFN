# Run environmental predictor extraction and harmonization scripts.
# Scripts are kept as-is and only orchestrated for reproducibility.

helpers_path <- if (file.exists("R/helpers.R")) "R/helpers.R" else "../R/helpers.R"
source(helpers_path)

project_root <- find_project_root()

run_script_sequence(
  c(
    "analysis/03. Extract environmental variables/Extract_env_vars.R",
    "analysis/03. Extract environmental variables/Extract_env_vars_100m - Buffer.R",
    "analysis/03. Extract environmental variables/joinQand1mForLatLong.R"
  ),
  project_root = project_root
)
