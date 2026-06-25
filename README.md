# speciesrichnessEFN

Reproducible species richness machine learning workflow for EFN vegetation data.

## Repository architecture

```text
speciesrichnessEFN/
├── config/
│   └── config.R
├── scripts/
│   ├── 00_install_packages.R         # Install required R packages (run once at setup)
│   ├── 01_ensure_data_dirs.R         # Check existence of required data directories and raw data
│   ├── 02_calculate_species_richness.R
│   ├── 03_extract_environmental_variables.R
│   ├── 04_investigate_data.R
│   ├── 05_variable_preselection_vif.R
│   ├── 06_train_models_and_shap.R
│   ├── 07_predict_species_richness_with_models.R
│   ├── 08_prediction_map_analysis.R
│   └── 09_run_all.R
│   └── helpers.R                    # Helper functions used across multiple scripts
├── analysis/                        # Original analysis scripts (kept)
├── data/
│   ├── raw/                         # Raw EFN survey + predictor maps (gitignored)
│   ├── processed-data/              # intermediate processed data (gitignored)
│   └── prediction-maps/             # input maps for model-based predictions (gitignored)
├── reports/
│   └── speciesrichness_prediction_report.Rmd
└── output/
│   ├── models/                      # ML models and SHAP results (gitignored)
│   └── maps/                        # maps containing predicted species richness (ML model outputs)

```

## Visual workflow overview

```text
00_install_packages
        ↓
01_prepare_EFN_data
        ↓
02_calculate_species_richness
        ↓
03_extract_environmental_variables
        ↓
04_investigate_data
        ↓
05_variable_preselection_vif
        ↓
06_train_models_and_shap
        ↓
07_prediction_map_analysis
        ↓
08_render_report
```

## How to run

### Full end-to-end run

```r
source("scripts/09_run_all.R")
```

### Stepwise run

Run scripts in numeric order from `scripts/00_install_packages.R` to `scripts/08_render_report.R`.

## Data conventions

- Put EFN vegetation data source files in `data/raw-data/EFN-survey-data/`.
- Make sure the EFN vegetation data source files are .xlsx files and define the expected sheet and column names in the config file
- Put predictor raster/vector map inputs in `data/raw-data/predictor-maps/`.
- Put maps used as input for species richness prediction based on the ML models in `data/prediction-maps/`.
- Processed intermediate files stay under `data/processed-data/`.

## Notes

