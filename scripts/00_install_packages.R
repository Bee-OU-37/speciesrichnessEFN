# Install and load all packages required by this repository workflow.
# Run this script first to set up a reproducible local R environment.

required_packages <- c(
  "car",
  "corrplot",
  "dismo",
  "dplyr",
  "gbm",
  "ggbeeswarm",
  "ggplot2",
  "ggspatial",
  "Hmisc",
  "kernelshap",
  "moments",
  "patchwork",
  "readr",
  "readxl",
  "reshape2",
  "rmarkdown",
  "sf",
  "shapviz",
  "spdep",
  "terra",
  "tibble",
  "tidyr",
  "writexl"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) > 0) {
  install.packages(missing)
}

message("Package check complete.")
