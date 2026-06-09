library(readxl)
library(writexl)
library(dplyr)
library(tidyr)

# This script processes multiple species .xlsx files containing species richness counts per plotcode and species occurrences. 
# It calculates mean species richness and true species richness per triangle defined in a combinations file.
# * Plotcodes missing from the Species sheet are included and contribute zero species to species richness.
# * Plotcodes not in triangles are excluded entirely.
# * If a plot or triangle has no species, its species richness is set to 0
# * Output is one row per triangle per file, with mean plot species richness, total species richness, number of plots, GPS coordinates.


# Set the directories containing the .xlsx files
data_dir <- "data/species count data"   #directory with species count data
comb_file_dir <- "." #current directory

comb_file <- file.path(comb_file_dir, "small scale plot combinations.xlsx")

# Read plot combinations file
plot_combinations <- read_excel(comb_file)

# List all .xlsx files in the data directory
xlsx_files <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE)

head(xlsx_files)

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

