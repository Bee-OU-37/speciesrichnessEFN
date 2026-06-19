# Calculate species richness for all active EcoFracNet locations

init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()

# config contains the names of the required EFN locations and the details of the input files for each location
source("config/config.R")

# Source the function file
source("analysis/calculate_species_richness.R")

# Initialize a list to store results for all locations
results_all <- list()

# Loop through each active location and process the data
for (loc in active_locations) {
  tryCatch({
    # Call the function in calculate_species_richness.R to process the data of the EFN location and 
    # calculate species richness, passing the config details dynamically
    results <- species_richness_Q_1m(
      location_id = loc,
      config_list = location_config,
      base_input_dir = base_input_dir,
      base_output_dir = base_output_dir
    )
    
    # Store the results in a list for potential further use
    results_all[[loc]] <- results
    cat("  -> SUCCESS\n\n")
    
  }, error = function(e) {
    warning("Failed processing ", loc, ": ", e$message)
  })
}

# Calculate the 100m scale species richness values
tryCatch({
  # Call the function to calculate species richness at 100m scale
  species_richness_100m(input_dir = base_output_dir, base_output_dir)
  
}, error = function(e) {
  warning("Failed calculating 100m scale species richness: ", e$message)
})

cat("\n=== All species richness calculations complete ===\n")
  
