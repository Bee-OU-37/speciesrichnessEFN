required_packages <- c(
  "dplyr", "ggspatial"
)

missing <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing) > 0) {
  install.packages(missing)
}

message("Package check complete.")
