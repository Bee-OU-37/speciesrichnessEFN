# Run species richness derivation scripts.
# Legacy scripts are executed in-place to keep existing algorithms unchanged.

helpers_path <- if (file.exists("R/helpers.R")) "R/helpers.R" else "../R/helpers.R"
source(helpers_path)

project_root <- find_project_root()

run_script_sequence(
  c(
    "analysis/01. Calculate species richness/species_richness_austerlitz.r",
    "analysis/01. Calculate species richness/species_richness_fochteloerveen.r",
    "analysis/01. Calculate species richness/species_richness_rusthoeve.r",
    "analysis/01. Calculate species richness/species_richness_sluiskil.r",
    "analysis/01. Calculate species richness/species_richness_USP.r",
    "analysis/01. Calculate species richness/AverageAndTotal100mAlphaDiv.R"
  ),
  project_root = project_root
)
