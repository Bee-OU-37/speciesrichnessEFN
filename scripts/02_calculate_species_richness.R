# Calculate species richness for all active EcoFracNet locations

# Load helpers and configuration
init_path <- if (file.exists("scripts/_init.R")) "scripts/_init.R" else "_init.R"
source(init_path)

project_root <- find_project_root()

# config contains the names of the required EFN locations and the details of the input files for each location
source("config/config.R")

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
    
    # Store the results in a list for further use
    results_all[[loc]] <- results
    cat("  -> SUCCESS\n\n")
    
  }, error = function(e) {
    warning("Failed processing ", loc, ": ", e)
  })
}

## Write the results to Excel files for all locations combined
# 1. Define output directories and filenames
output_dir <- file.path(base_output_dir, "combined_results")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

file_Q <- file.path(output_dir, "All_locations_Q_specrich.xlsx")
file_1m <- file.path(output_dir, "All_locations_1m_specrich.xlsx")

# 2. Combine ALL Quadrat Data into ONE DataFrame
# This automatically adds 'location' based on the list names
combined_quadrat <- bind_rows(
  imap(results_all, ~{
    if(!is.null(.x$quadrat_counts) && nrow(.x$quadrat_counts) > 0) {
      mutate(.x$quadrat_counts, Location = .y)
    } else {
      NULL
    }
  })
)

# 3. Combine ALL 1m Plot Data into ONE DataFrame
combined_plot <- bind_rows(
  imap(results_all, ~{
    if(!is.null(.x$plot_counts) && nrow(.x$plot_counts) > 0) {
      mutate(.x$plot_counts, Location = .y)
    } else {
      NULL
    }
  })
)

# 4. Write to Excel
tryCatch({
  write_xlsx(combined_quadrat, path = file_Q)
  cat("✅ Saved:", file_1m, "\n")
  write_xlsx(combined_plot, path = file_1m)
  cat("✅ Saved:", file_1m, "\n")
}, error = function(e) {
  cat("❌ Error writing files:", e$message, "\n")
})


# Calculate the 100m scale species richness values
tryCatch({
  # Call the function to calculate species richness at 100m scale
  results <- species_richness_100m(input_dir = base_output_dir, base_output_dir)
  }, error = function(e) {
    warning("Failed calculating 100m scale species richness: ", e)
  })

cat("\n=== All species richness calculations complete ===\n")
