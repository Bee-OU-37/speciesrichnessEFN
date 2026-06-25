# Helper functions for the reproducible species richness workflow.
# These functions standardize path handling, logging, and script execution.

################################################################
# General Functions
################################################################

# Locate the project root by searching for the .Rproj file.
find_project_root <- function(start_dir = getwd()) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)

  repeat {
    rproj_files <- list.files(current, pattern = "\\.Rproj$", full.names = TRUE)
    if (length(rproj_files) > 0) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find project root containing an .Rproj file.")
    }
    current <- parent
  }
}

# Print a timestamped workflow message.
log_step <- function(message_text) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s\n", timestamp, message_text))
}

# Create a directory tree when it does not exist.
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

# Load and return centralized project configuration.
load_project_config <- function(project_root = find_project_root()) {
  source(file.path(project_root, "config", "config.R"), local = TRUE)
  get_project_config(project_root)
}

# Source a script from its own working directory for compatibility with legacy paths.
source_with_working_dir <- function(script_path, project_root = find_project_root()) {
  absolute_script <- file.path(project_root, script_path)

  if (!file.exists(absolute_script)) {
    stop(sprintf("Script not found: %s", absolute_script))
  }

  log_step(sprintf("Running %s", script_path))
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  setwd(dirname(absolute_script))
  source(absolute_script, local = new.env(parent = baseenv()))
  invisible(TRUE)
}

# Execute a vector of scripts in sequence.
run_script_sequence <- function(script_paths, project_root = find_project_root()) {
  for (script_path in script_paths) {
    source_with_working_dir(script_path, project_root = project_root)
  }
  invisible(TRUE)
}

################################################################
# Calculate Species Richness Functions
################################################################

# Function to clean trailing spaces and other clutter from Excel values on e.g. Lat long columns, and convert to numeric
clean_excel_numeric <- function(x) {
  x %>%
    # Remove everything EXCEPT digits, dots, and minus signs
    str_replace_all("[^0-9.-]", "") |> 
    as.numeric()
}

# Function Rename old plotcodes
rename_old_plotcodes <- function(df, plotcode_col = "Plotcode", mapping_list = old_to_new_plotcodes) {
  
  # 1. Check if column exists
  if (!plotcode_col %in% names(df)) {
    stop("Column '", plotcode_col, "' not found.")
  }
  
  # Extract mapping keys and values
  # Ensure keys are characters to match against our temporarily-converted column
  old_keys <- as.character(names(mapping_list)) 
  new_vals <- unlist(mapping_list) 
  
  # 2. Convert target column to character for matching
  current_vals <- as.character(df[[plotcode_col]])
  
  # 3. Find matches
  match_pos <- match(current_vals, old_keys)
  
  # 4. Replace matched values; keep original if no match
  # Note: new_vals[match_pos] returns the numeric values from your list
  df[[plotcode_col]] <- ifelse(
    is.na(match_pos), 
    df[[plotcode_col]],              # Keep original (numeric)
    as.numeric(new_vals[match_pos])  # New value (explicitly numeric)
  )
  
  return(df)
}

remove_plotcodes <- function(df, plotcode_col = "Plotcode", plotcodes_to_remove) {
  # This function removes rows from the dataframe where the plot code matches any in the provided list.
  # It takes a dataframe, the name of the column containing plot codes, and a vector of plot codes to remove.
  # It returns the filtered dataframe.
  
  df_filtered <- df[!df[[plotcode_col]] %in% plotcodes_to_remove, ]
  return(df_filtered)
}

# function that calculates the species richness per quadrat and per 1m2 plot for a given location,
# based on the configuration details provided in the config_list.
species_richness_Q_1m <- function(location_id,
                                  config_list,
                                  base_input_dir,
                                  base_output_dir) {
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
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)
  
  cat("Processing:", location_id, "\n")
  
  cat("  - Input Sheet Metadata:", sheets_to_use$metadata, "\n")
  cat("  - Input Sheet Species:", sheets_to_use$species, "\n")
  
  # 4. Read Data using the DYNAMIC sheet names
  df1_full <- read_excel(normalizePath(input_path), sheet = sheets_to_use$metadata)
  df2_full <- read_excel(normalizePath(input_path), sheet = sheets_to_use$species)
  
  # Select only the necessary columns from each sheet
  df1_selected <- df1_full %>%
     dplyr::select(Plotcode, `Quadrant code`, Latitude_GIS, Longitude_GIS)
  
  # clean up trailing spaces from Latitude_GIS, also non-breaking space (Unicode U+00A0) or another invisible Unicode character at the end, which standard trimws() does not remove by default.
  df1_selected$Latitude_GIS <- gsub("[^0-9.-]", "", df1_selected$Latitude_GIS)
  df1_selected$Longitude_GIS <- gsub("[^0-9.-]", "", df1_selected$Longitude_GIS)
  df1_selected$Latitude_GIS <- as.numeric(df1_selected$Latitude_GIS)
  df1_selected$Longitude_GIS <- as.numeric(df1_selected$Longitude_GIS)
  
  # Select only the necessary columns from each sheet
  df2_selected <- df2_full %>%
     dplyr::select(Plotcode, `Quadrant code`, Species)
  
  # fix the old plotcodes in Sluiskil location
  if (location_id == "Sluiskil") {
    cat("Fixing old plotcodes in Sluiskil location...\n")
    
    # use the old_to_new_plotcodes mapping to recode the Plotcode column in both dataframes. Change the Plotcodes where necessary, keep the rest as is.
    df1_selected <- rename_old_plotcodes(df1_selected, "Plotcode", old_to_new_plotcodes)
    df2_selected <- rename_old_plotcodes(df2_selected, "Plotcode", old_to_new_plotcodes)
  }
  
  # remove plotcodes 40-45 from Rusthoeve location
  if (location_id == "Rusthoeve") {
    cat("Removing plotcodes 40-45 from Rusthoeve location...\n")
    plotcode_list <- as.character(40:45)
    
    df1_selected <- remove_plotcodes(df1_selected, "Plotcode", plotcode_list)
    df2_selected <- remove_plotcodes(df2_selected, "Plotcode", plotcode_list)
  }
  
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
    summarise(unique_Species_count = n_distinct(Species),
              .groups = "drop")
  
  # Perform inner join on columns "plotcode"
  # Re-selecting specific columns from df1 to avoid duplication before joining
  df1_coords <- df1_selected %>%  dplyr::select(Plotcode, Latitude_GIS, Longitude_GIS)
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
species_richness_100m <- function(input_dir, output_dir) {
  # This function processes multiple species .xlsx files containing species richness counts per plotcode and species occurrences.
  # It calculates mean species richness and true species richness per triangle defined in a combinations file.
  # * Plotcodes missing from the Species sheet are included and contribute zero species to species richness.
  # * Plotcodes not in triangles are excluded entirely.
  # * If a plot or triangle has no species, its species richness is set to 0
  # * Output is one row per triangle per file, with mean plot species richness, total species richness, number of plots, GPS coordinates.
  
  # locate the plot combination file in the config directory
  comb_file_dir <- file.path(find_project_root(), "config")
  comb_file <- file.path(comb_file_dir, "small scale plot combinations.csv")
  # Read plot combinations file
  plot_combinations <- read.csv(comb_file)
  
  # List all .xlsx files in the data directory
  xlsx_files <- list.files(input_dir, pattern = "\\.xlsx$", full.names = TRUE)
  
  print(paste("Found", length(xlsx_files), "xlsx files in", input_dir))
  
  results <- list()
  
  for (f in xlsx_files) {
    print(f)
    sheet_names <- excel_sheets(f)
    if (!("Species_count_Plotcode" %in% sheet_names &&
          "Species" %in% sheet_names))
      next
    
    # Remove " species richness.xlsx" from filename
    location_name <- sub(" species richness\\.xlsx$", "", basename(f))
    
    # Read plotcode-level species count and coordinates
    plotcode_df <- read_excel(f, sheet = "Species_count_Plotcode")
    plotcode_df$unique_Species_count <- as.numeric(plotcode_df$unique_Species_count)
    plotcode_df$Plotcode <- as.numeric(plotcode_df$Plotcode)
    plotcode_df$Longitude_GIS <- as.numeric(plotcode_df$Longitude_GIS)
    plotcode_df$Latitude_GIS <- as.numeric(plotcode_df$Latitude_GIS)
    plotcode_df$Location <- location_name
    
    # Assign triangles to plotcodes. Plotcodes that are not part of triangles, can be ignored
    plotcode_tri <- dplyr::inner_join(plotcode_df, plot_combinations, by = "Plotcode")
    
    # Read species occurrences
    species_df <- read_excel(f, sheet = "Species")
    
    # Only keep plotcodes that are part of triangles
    species_tri <- dplyr::left_join(species_df, plot_combinations, by = "Plotcode") %>%
      filter(!is.na(Triangle))
    
    # All plotcodes in triangles for this file
    triangle_plotcodes <- plotcode_tri %>%
       dplyr::select(Triangle, Plotcode) %>%
      distinct()
    
    # Compute mean unique_Species_count and centroids per triangle
    triangle_stats <- plotcode_tri %>%
      dplyr::group_by(Triangle) %>%
      dplyr::summarize(
        Location = first(Location),
        mean_unique_Species_count = mean(unique_Species_count, na.rm = TRUE),
        n_plots = n_distinct(Plotcode),
        triangle_longitude = mean(Longitude_GIS, na.rm = TRUE),
        triangle_latitude = mean(Latitude_GIS, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Compute true species richness (unique species across all plotcodes in triangle)
    species_richness <- triangle_plotcodes %>%
      dplyr::group_by(Triangle) %>%
      dplyr::summarise(plotcodes = list(Plotcode), .groups = "drop") %>%
      mutate(total_species_richness = sapply(plotcodes, function(pc) {
        length(unique(species_tri$Species[species_tri$Plotcode %in% pc &
                                            !is.na(species_tri$Species)]))
      })) %>%
       dplyr::select(Triangle, total_species_richness)
    
    
    # Join stats and richness (all triangles in plotcode_tri will be included), group by triangle and by quadrat
    triangle_stats <- left_join(triangle_stats, species_richness, by = "Triangle")
    # If a triangle has no species at all, total_species_richness will be NA, set to 0
    #  triangle_stats$total_species_richness[is.na(triangle_stats$total_species_richness)] <- 0
    
    results[[f]] <- triangle_stats
  }
  
  final_summary <- bind_rows(results)
  
  # if not already there, create directory "100m scale species richness" in the output dir
  new_output_dir <- file.path(output_dir, "100m_scale")
  if (!dir.exists(new_output_dir)) {
    dir.create(new_output_dir)
  }
  
  full_path <- file.path(new_output_dir, "mean_and_total_alpha_diversity_100m.xlsx")
  write_xlsx(final_summary, path = full_path)
  
  return(final_summary)
}

