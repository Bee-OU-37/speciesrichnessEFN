# speciesrichnessEFN

Reproducible species-richness workflow for EFN vegetation data.

## Architecture

```text
speciesrichnessEFN/
├── config/
│   └── config.yml
├── scripts/
│   ├── helpers.R
│   ├── _init.R
│   ├── 00_install_packages.R
│   ├── 01_prepare_EFN_data.R
│   ├── 02_calculate_species_richness.R
│   ├── 03_extract_environmental_variables.R
│   ├── 04_investigate_data.R
│   ├── 05_variable_preselection_vif.R
│   ├── 06_train_models.R
│   ├── 07_shapley_analysis.R
│   ├── 08_prediction_map_analysis.R
│   ├── 09_render_report.R
│   └── analysis/  # analysis code now consolidated under scripts/
├── run_all.R
├── data/
│   ├── raw/
│   ├── processed-data/
│   └── prediction-maps/
├── analysis-output/
│   ├── ml-models/
│   ├── shapviz/
│   ├── prediction-maps/
│   ├── residuals/
│   └── figures/
└── reports/
```

## How it works

1. `run_all.R` executes each pipeline script in order.
2. `scripts/_init.R` loads shared helpers from `scripts/helpers.R`.
3. `config/config.yml` defines paths, regions, outputs, and reporting settings.
4. Stage scripts in `scripts/` orchestrate the concrete analysis scripts in `scripts/analysis/`.
5. Machine-learning (`scripts/06_train_models.R`) and Shapley (`scripts/07_shapley_analysis.R`) run as separate stages.
6. Outputs are organized under dedicated folders in `analysis-output/`.

## Run

```r
source("run_all.R")
```
