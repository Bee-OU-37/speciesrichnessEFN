# This file contains the functions to calculate species richness from the raw EFN vegetation survey datasets of location_name. 
# It reads the raw data from the specified input Excel file, processes it to calculate species richness 
# per quadrant and per plotcode, and then writes the results to a new Excel file with two sheets, given as output_file. 
# The function also returns the processed dataframes for further use in the session.

library(readxl)
library(dplyr)
library(writexl)
library(tidyr)

# function that calculates the species richness per quadrat and per 1m2 plot for a given location, 
# based on the configuration details provided in the config_list.
species_richness_Q_1m <- function(location_id, config_list, base_input_dir, base_output_dir) {
  
  # 1. Validate Location exists in config
  if (!location_id %in% names(config_list)) {
    stop("Location '", location_id, "' not found in configuration.")
  }
  
  # 2. Extract details for THIS specific location
  loc_details <- config_list[[location_id]]
  sheets_to_use <- loc_details$sheets
  
  # 3. Construct full file paths
  input_path <- file.path(base_input_dir, loc_details$input_file_name)
  output_path <- file.path(base_output_dir, loc_details$output_file_name)
  
  # Check file existence
  if (!file.exists(input_path)) {
    stop("Input file not found: ", input_path)
  }
  
  # Ensure output directory exists
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  cat("Processing:", location_id, "\n")
  
  cat("  - Input Sheet Metadata:", sheets_to_use$metadata, "\n")
  cat("  - Input Sheet Species:", sheets_to_use$species, "\n")
  
  # 4. Read Data using the DYNAMIC sheet names
  df1_full <- read_excel(normalizePath(input_path), sheet = sheets_to_use$metadata)
  df2_full <- read_excel(normalizePath(input_path), sheet = sheets_to_use$species)

  # Select only the necessary columns from each sheet
  # Renaming columns dynamically if case sensitivity varies, though assuming exact match here
  df1_selected <- df1_full %>% 
    select(Plotcode, `Quadrant code`, Latitude_GIS, Longitude_GIS)
  
  df2_selected <- df2_full %>% 
    select(Plotcode, `Quadrant code`, Species)
  
  # --- FIRST SHEET: For a count of species per quadrant ---
  # Group by Plotcode and Quadrant code, count unique values in Species
  df2_summary <- df2_selected %>%
    group_by(Plotcode, `Quadrant code`) %>%
    summarise(alpha_diversity = n_distinct(Species), .groups = "drop")
  
  # Perform the inner join on columns "Plotcode" and "Quadrant code"
  merged_df1 <- inner_join(df1_selected, df2_summary, by = c("Plotcode", "Quadrant code"))
  
  # --- SECOND SHEET: Unique Species per Plotcode ---
  unique_Species_per_Plotcode <- df2_selected %>%
    group_by(Plotcode) %>%
    summarise(unique_Species_count = n_distinct(Species), .groups = "drop")
  
  # Perform inner join on columns "plotcode"
  # Re-selecting specific columns from df1 to avoid duplication before joining
  df1_coords <- df1_selected %>% select(Plotcode, Latitude_GIS, Longitude_GIS)
  merged_df2 <- inner_join(df1_coords, unique_Species_per_Plotcode, by = "Plotcode")
  
  # Remove duplicates if any rows become identical after join
  unique_df2 <- distinct(merged_df2)
  
  # Write both sheets to the same Excel file, together with a sheet listing the species
  write_xlsx(
    x = list(
      Species_count_Quadrat = merged_df1,
      Species_count_Plotcode = unique_df2,
      Species = df2_selected
    ),
    path = output_path
  )
  
  return(list(quadrat_counts = merged_df1, plot_counts = unique_df2))
}

# Function that calculates the species richness at 100m scales for all files in the input dir
species_richness_100m <- function(input_dir, base_output_dir) {
  # This function processes multiple species .xlsx files containing species richness counts per plotcode and species occurrences. 
  # It calculates mean species richness and true species richness per triangle defined in a combinations file.
  # * Plotcodes missing from the Species sheet are included and contribute zero species to species richness.
  # * Plotcodes not in triangles are excluded entirely.
  # * If a plot or triangle has no species, its species richness is set to 0
  # * Output is one row per triangle per file, with mean plot species richness, total species richness, number of plots, GPS coordinates.
  
  comb_file_dir <- "." #current directory
  comb_file <- file.path(comb_file_dir, "small scale plot combinations.xlsx")
  # Read plot combinations file
  plot_combinations <- read_excel(comb_file)
  
  # List all .xlsx files in the data directory
  xlsx_files <- list.files(input_dir, pattern = "\\.xlsx$", full.names = TRUE)
  
  results <- list()
  
  for (f in xlsx_files) {
    sheet_names <- excel_sheets(f)
    if (!("Species_count_Plotcode" %in% sheet_names && "Species" %in% sheet_names)) next
    
    # Remove " species.xlsx" from filename
    location_name <- sub(" species\\.xlsx$", "", basename(f))
    
    # Read plotcode-level species count and coordinates
    plotcode_df <- read_excel(f, sheet = "Species_count_Plotcode")
    plotcode_df$unique_Species_count <- as.numeric(plotcode_df$unique_Species_count)
    plotcode_df$Plotcode <- as.numeric(plotcode_df$Plotcode)
    plotcode_df$Longitude_GIS <- as.numeric(plotcode_df$Longitude_GIS)
    plotcode_df$Latitude_GIS <- as.numeric(plotcode_df$Latitude_GIS)
    plotcode_df$Location <- location_name
    
    # Assign triangles to plotcodes. Plotcodes that are not part of triangles, can be ignored
    plotcode_tri <- inner_join(plotcode_df, plot_combinations, by = "Plotcode")
    
    # Read species occurrences
    species_df <- read_excel(f, sheet = "Species")
    
    # Only keep plotcodes that are part of triangles
    species_tri <- left_join(species_df, plot_combinations, by = "Plotcode") %>%
      filter(!is.na(Triangle))
    
    # All plotcodes in triangles for this file
    triangle_plotcodes <- plotcode_tri %>%
      select(Triangle, Plotcode) %>%
      distinct()
    
    # Compute mean unique_Species_count and centroids per triangle
    triangle_stats <- plotcode_tri %>%
      group_by(Triangle) %>%
      summarize(
        Location = first(Location),
        mean_unique_Species_count = mean(unique_Species_count, na.rm = TRUE),
        n_plots = n_distinct(Plotcode),
        triangle_longitude = mean(Longitude_GIS, na.rm = TRUE),
        triangle_latitude = mean(Latitude_GIS, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Compute true species richness (unique species across all plotcodes in triangle)
    species_richness <- triangle_plotcodes %>%
      group_by(Triangle) %>%
      summarize(plotcodes = list(Plotcode), .groups = "drop") %>%
      mutate(total_species_richness = sapply(
        plotcodes,
        function(pc) {
          length(unique(species_tri$Species[species_tri$Plotcode %in% pc & !is.na(species_tri$Species)]))
        }
      )) %>%
      select(Triangle, total_species_richness)
    
    
    # Join stats and richness (all triangles in plotcode_tri will be included), group by triangle and by quadrat
    triangle_stats <- left_join(triangle_stats, species_richness, by = "Triangle")
    # If a triangle has no species at all, total_species_richness will be NA, set to 0
    #  triangle_stats$total_species_richness[is.na(triangle_stats$total_species_richness)] <- 0
    
    results[[f]] <- triangle_stats
    
  }
  
  final_summary <- bind_rows(results)
  write_xlsx(final_summary, "mean_and_total_alpha_diversity_100m.xlsx")
  
}