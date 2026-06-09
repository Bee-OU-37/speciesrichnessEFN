init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()

run_script_sequence(
  c(
    "scripts/analysis/species_richness/01_species_richness_austerlitz.R",
    "scripts/analysis/species_richness/02_species_richness_fochteloerveen.R",
    "scripts/analysis/species_richness/03_species_richness_rusthoeve.R",
    "scripts/analysis/species_richness/04_species_richness_sluiskil.R",
    "scripts/analysis/species_richness/05_species_richness_USP.R",
    "scripts/analysis/species_richness/06_average_and_total_100m_alpha_div.R"
  ),
  project_root = project_root
)
