# Run environmental predictor extraction and harmonization scripts.
# Scripts are kept as-is and only orchestrated for reproducibility.

init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()

# config contains the names of the required EFN locations and the details of the input files for each location
source("config/config.R")

# Set directories and location list to process
input_maps_dir <- config$paths$predictor_maps          # predictor maps directory from the config
processed_data_dir <- config$paths$processed_data      # processed data directory from the config
output_dir_name <- "environmental data"                # specific output directory name for the extracted environmental data
location_list <- active_locations                      # active locations from the config

# Function to pre-process the USP EcoFracNet species richness files: combine them into one USP species richness file
combine_species_richness_files <- function(input_dir, combi_key, loc_list) {

  # List all species richness files in the input directory that contain the combi_key in their names
  species_richness_files <- list.files(input_dir, pattern = paste0(combi_key, ".*\\.xlsx$"), full.names = TRUE)
  
  # if combi_key is "USP", then also add de_Driehoek to the file list
  if (combi_key == "USP") {
    species_richness_files <- c(species_richness_files, list.files(input_dir, pattern = "de Driehoek", full.names = TRUE))
    
    # adjust the location list to include "USP", and exclude "de Driehoek" and separate files with "USP" in their names. Keep the other names
    loc_list <- c("USP", setdiff(loc_list, c("de_Driehoek", grep("USP", loc_list, value = TRUE))))
  }
  
  # 2. Create a master workbook as a NAMED LIST
  master_df <- list(
    Species_count_Plotcode = {
      dplyr::bind_rows(lapply(species_richness_files, function(f) {
        df <- read_xlsx(f, sheet = "Species_count_Plotcode")
        df$Location <- tools::file_path_sans_ext(basename(f))  # Add a column for the location based on the file name
        return(df)
      }))
    },
    
    Species_count_Quadrat = {
      dplyr::bind_rows(lapply(species_richness_files, function(f) {
        df <- read_xlsx(f, sheet = "Species_count_Quadrat")
        df$Location <- tools::file_path_sans_ext(basename(f))  # Add a column for the location based on the file name
        return(df)
      }))
    },  
    
    Species = {
      dplyr::bind_rows(lapply(species_richness_files, function(f) {
        df <- read_xlsx(f, sheet = "Species")
        df$Location <- tools::file_path_sans_ext(basename(f))  # Add a column for the location based on the file name
        return(df)
      }))
    }
  )
  
  # Write to ONE Excel file with multiple tabs
  cat("Writing master workbook...\n")
  output_file <- file.path(input_dir, paste0(combi_key, " species richness.xlsx"))
  write_xlsx(master_df, path = output_file)
  
  # Confirm results
  cat("✅ Saved:", output_file, "\n\n")
  cat("Sheets created:\n")
  for (sheet_name in names(master_df)) {
    cat(sprintf("  - %s (%d rows)\n", 
                sheet_name, 
                nrow(master_df[[sheet_name]])))
  }
  
  return(loc_list)
}

###############################################################
# Pre-process the USP: combine the separate EFN files into one USP location
###############################################################
location_list <- combine_species_richness_files(processed_data_dir, "USP", location_list)

# Initialize a list to store results for all locations
results_all <- list()

# Loop through each active location and process the data
for (loc in location_list) {
 
  tryCatch({
    # Call the function in calculate_species_richness.R to process the data of the EFN location and 
    # calculate species richness, passing the config details dynamically
    results <- extract_env_vars(loc, input_maps_dir, processed_data_dir, output_dir_name)
    
    # Store the results in a list for further use
    results_all[[loc]] <- results
    cat("  -> Environmental variable extraction SUCCESS\n\n")
    
  }, error = function(e) {
    warning("Failed environmental variable extraction processing ", loc, ": ", e)
  })
}


# run_script_sequence(
#   c(
#     "analysis/03. Extract environmental variables/Extract_env_vars.R",
#     "analysis/03. Extract environmental variables/Extract_env_vars_100m - Buffer.R",
#     "analysis/03. Extract environmental variables/joinQand1mForLatLong.R"
#   ),
#   project_root = project_root
# )
