# Project configuration for reproducible species richness modelling workflow.
# This file centralizes directory names and ordered pipeline steps.

get_project_config <- function(project_root) {
  list(
    project_root = project_root,
    paths = list(
      raw_data_root = file.path(project_root, "data", "raw-data"),
      efn_vegetation = file.path(project_root, "data", "raw-data", "EFN-survey-data"),
      predictor_maps = file.path(project_root, "data", "raw-data", "predictor-maps"),
      prediction_maps = file.path(project_root, "data", "prediction-maps"),
      processed_data = file.path(project_root, "data", "processed-data"),
      output = file.path(project_root, "output"),
      models_dir = file.path(project_root, "output", "models"),
      output_maps = file.path(project_root, "output", "maps"),
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
    )
  )
}
    
# Base paths
base_input_dir <- "data/raw-data/EFN-survey-data"
base_output_dir <- "data/processed-data"



# --- Which EcoFracNet locations to process? ---
# Change this vector to run specific subsets or all
active_locations <- c(
  "Austerlitz", 
  "Fochteloerveen", 
  "Rusthoeve", 
  "Sluiskil", 
  "de_Driehoek", 
  "USP_BLUE", 
  "USP_GREEN", 
  "USP_YELLOW", 
  "USP_DARK_RED", 
  "USP_PURPLE"
)

# --- 2. Configuration Details per Location ---
# A named list where each name matches the location ID
location_config <- list(
  
  # Case for Austerlitz
  Austerlitz = list(
    input_file_name = "Austerlitz data.xlsx",
    output_file_name = "Austerlitz species richness.xlsx",
    sheets = list(
      metadata = "Metadata survey 2",
      species = "Species data May"
    )
  ),
  
  # Case for Fochteloerveen
  Fochteloerveen = list(
    input_file_name = "Fochteloerveen data.xlsx",
    output_file_name = "Fochteloerveen species richness.xlsx",
    sheets = list(
      metadata = "Metadata survey 2",
      species = "Plant_Species_Data_June"
    )
  ),
  
  # Case for Rusthoeve
  Rusthoeve = list(
    input_file_name = "Rusthoeve data.xlsx",
    output_file_name = "Rusthoeve species richness.xlsx",
    sheets = list(
      metadata = "Metadata survey 1",
      species = "Plant_Species_Data"
    )
  ),
  
  # Case for Sluiskil
  Sluiskil = list(
    input_file_name = "Sluiskil data.xlsx",
    output_file_name = "Sluiskil species richness.xlsx",
    sheets = list(
      metadata = "Metadata survey 1",
      species = "Plant_Species_Data"
    )
  ),
  
  # Case for de Driehoek
  de_Driehoek = list(
    input_file_name = "de Driehoek data.xlsx",
    output_file_name = "de Driehoek species richness.xlsx",
    sheets = list(
      metadata = "Metadata",
      species = "Species data March and May"
    )
  ),
  
  # Case for USP BLUE
  USP_BLUE = list(
    input_file_name = "USP BLUE data.xlsx",
    output_file_name = "USP BLUE species richness.xlsx",
    sheets = list(
      metadata = "Metadata",
      species = "Species data"
    )
  ),
  
  # Case for USP DARK RED
  USP_DARK_RED = list(
    input_file_name = "USP DARK RED data.xlsx",
    output_file_name = "USP DARK RED species richness.xlsx",
    sheets = list(
      metadata = "Metadata (2)",
      species = "Species data"
    )
  ),
  
  # Case for USP GREEN
  USP_GREEN = list(
    input_file_name = "USP GREEN copy data.xlsx",
    output_file_name = "USP GREEN species richness.xlsx",
    sheets = list(
      metadata = "Metadata",
      species = "Species data"
    )
  ),
  
  # Case for USP PURPLE
  USP_PURPLE = list(
    input_file_name = "USP PURPLE data.xlsx",
    output_file_name = "USP PURPLE species richness.xlsx",
    sheets = list(
      metadata = "Metadata",
      species = "Species data"
    )
  ),
  
  # Case for USP YELLOW
  USP_YELLOW = list(
    input_file_name = "USP YELLOW data.xlsx",
    output_file_name = "USP YELLOW species richness.xlsx",
    sheets = list(
      metadata = "Metadata",
      species = "Species data"
    )
  )
)


  
