# Run exploratory statistical checks on prepared model-input datasets.
# This preserves current investigation scripts and plotting behaviour.

helpers_path <- if (file.exists("R/helpers.R")) "R/helpers.R" else "../R/helpers.R"
source(helpers_path)

project_root <- find_project_root()

run_script_sequence(
  c(
    "analysis/02. Statistical species richness data analysis/Investigate data.R",
    "analysis/02. Statistical species richness data analysis/Investigate data_100m.R"
  ),
  project_root = project_root
)
