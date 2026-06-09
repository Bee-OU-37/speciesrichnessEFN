# Project configuration for reproducible species richness modelling workflow.
# This file centralizes directory names and ordered pipeline steps.

get_project_config <- function(project_root) {
  list(
    project_root = project_root,
    paths = list(
      raw_data_root = file.path(project_root, "data", "raw"),
      efn_vegetation = file.path(project_root, "data", "raw", "EFN vegetation survey data"),
      predictor_maps = file.path(project_root, "data", "raw", "Predictor maps"),
      prediction_maps = file.path(project_root, "data", "prediction-maps"),
      processed_data = file.path(project_root, "data", "processed-data"),
      analysis_output = file.path(project_root, "analysis-output"),
      reports = file.path(project_root, "reports")
    ),
    reporting = list(
      report_template = file.path(project_root, "reports", "speciesrichness_workflow_report.Rmd"),
      report_output_format = "html_document",
      prediction_map_filenames = c(
        Q = "predictions_Q.tif",
        `1m` = "predictions_1m.tif",
        `100m_mean` = "predictions_100m_mean.tif",
        `100m_total` = "predictions_100m_total.tif"
      ),
      target_crs = "EPSG:28992"
    ),
    pipeline_steps = c(
      "scripts/00_install_packages.R",
      "scripts/01_prepare_EFN_data.R",
      "scripts/02_calculate_species_richness.R",
      "scripts/03_extract_environmental_variables.R",
      "scripts/04_investigate_data.R",
      "scripts/05_variable_preselection_vif.R",
      "scripts/06_train_models_and_shap.R",
      "scripts/07_prediction_map_analysis.R",
      "scripts/08_render_report.R"
    )
  )
}
