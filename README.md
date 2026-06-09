# speciesrichnessEFN

Reproducible species richness machine learning workflow for EFN vegetation data using the existing project algorithms, libraries, and analysis content.

## What this refactor changes

- Keeps current modelling content (VIF/GVIF selection, BRT model training, SHAP analysis, prediction-map analysis).
- Introduces a reproducible script pipeline from `scripts/00_install_packages.R` through report rendering.
- Adds centralized configuration (`config/project_config.R`) and shared helpers (`R/helpers.R`).
- Adds a report file in R Markdown (`reports/speciesrichness_workflow_report.Rmd`).
- Replaces `README.txt` with this `README.md`.
- Standardizes raw-input directory names:
  - `data/raw/EFN vegetation survey data`
  - `data/raw/Predictor maps`
- Adds dedicated prediction-map input directory:
  - `data/prediction-maps`

## Repository architecture

```text
speciesrichnessEFN/
├── config/
│   └── project_config.R
├── R/
│   └── helpers.R
├── scripts/
│   ├── 00_install_packages.R
│   ├── 01_prepare_EFN_data.R
│   ├── 02_calculate_species_richness.R
│   ├── 03_extract_environmental_variables.R
│   ├── 04_investigate_data.R
│   ├── 05_variable_preselection_vif.R
│   ├── 06_train_models_and_shap.R
│   ├── 07_prediction_map_analysis.R
│   ├── 08_render_report.R
│   └── 09_run_all.R
├── analysis/                        # Original analysis scripts (kept)
├── data/
│   ├── raw/                         # Raw EFN survey + predictor maps (gitignored)
│   ├── processed-data/
│   └── prediction-maps/             # Raster maps for model-based predictions
├── reports/
│   └── speciesrichness_workflow_report.Rmd
└── analysis-output/
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

- Put EFN vegetation source files in `data/raw/EFN vegetation survey data/`.
- Put predictor raster/vector map inputs in `data/raw/Predictor maps/`.
- Put maps used for prediction execution in `data/prediction-maps/`.
- Processed intermediate files stay under `data/processed-data/`.

## Notes

- Legacy analysis files are intentionally retained and orchestrated through wrapper scripts to preserve current scientific behaviour.
- Some legacy scripts use relative paths and/or manual settings; wrappers run scripts in their original directories to keep compatibility.
