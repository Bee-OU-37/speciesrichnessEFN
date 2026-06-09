# ----------------------------------------------------------------------------
# Description: This script reads data from an Excel file containing metadata and species data for Austerlitz.
# It calculates species richness per quadrant and per plotcode, then writes the results to a new Excel file with two sheets.
# ----------------------------------------------------------------------------

# Load necessary libraries
library(readxl)
library(dplyr)
library(writexl)

# Set the path to your Excel files
input_file <- "data\raw data\Austerlitz data.xlsx"
output_file <- "data\species count data\Austerlitz species richness.xlsx"

# Load the Excel file (reads the first sheet by default)
df <- read_excel(input_file)

# View the first few rows
head(df)

# Read data from the two sheets
df1 <- read_excel(input_file, sheet = "Metadata survey 2")
df2 <- read_excel(input_file, sheet = "Species data May")

# Select only the necessary columns from each sheet
df1_selected <- df1[, c("Plotcode", "Quadrant code", "Latitude_GIS", "Longitude_GIS")]  
df2_selected <- df2[, c("Plotcode", "Quadrant code", "Species")]

# --- FIRST SHEET: For a count of species per quadrant: group by Plotcode and Quadrant code, count unique values in Species
df2_summary <- df2_selected %>%
  group_by(Plotcode, `Quadrant code`) %>%
  summarise(alpha_diversity = n_distinct(Species), .groups = "drop")

#Perform the inner join on columns "Plotcode" and "Quadrant code"
merged_df1 <- inner_join(df1_selected, df2_summary, by = c("Plotcode", "Quadrant code"))

# --- SECOND SHEET: Unique Species per Plotcode (from Species data May) ---
unique_Species_per_Plotcode <- df2_selected %>%
  group_by(Plotcode) %>%
  summarise(unique_Species_count = n_distinct(Species), .groups = "drop")

#Perform inner join on columns "plotcode"
merged_df2 <- inner_join(df1_selected[, c("Plotcode", "Latitude_GIS", "Longitude_GIS")], unique_Species_per_Plotcode, by = "Plotcode")
unique_df2 <- distinct(merged_df2)

# Write both sheets to the same Excel file
write_xlsx(
  list(
    Species_count_Quadrat = merged_df1,
    Species_count_Plotcode = unique_df2
  ),
  output_file
)