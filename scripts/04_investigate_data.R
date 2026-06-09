# Run exploratory statistical checks on prepared model-input datasets.
# This preserves current investigation scripts and plotting behaviour.

init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()

run_script_sequence(
  c(
    "analysis/02. Statistical species richness data analysis/Investigate data.R",
    "analysis/02. Statistical species richness data analysis/Investigate data_100m.R"
  ),
  project_root = project_root
)
