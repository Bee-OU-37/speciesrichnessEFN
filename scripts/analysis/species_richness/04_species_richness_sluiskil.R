library(readxl)
library(dplyr)
library(writexl)
library(stringr)

# Set the path to your Excel files
input_file <- "Data Sluiskil.xlsx"
output_file <- "Sluiskil species richness.xlsx"

# Load the Excel file (reads the first sheet by default)
df <- read_excel(input_file)

# View the first few rows
head(df)

# Read data from the two sheets
df1 <- read_excel(input_file, sheet = "Metadata survey 1")
df2 <- read_excel(input_file, sheet = "Plant_Species_Data")

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

#remove any whitespace from df
merged_df_nospaces <-as.data.frame(apply(merged_df1,2, str_remove_all, " "))
unique_df_nospaces <-as.data.frame(apply(unique_df2,2, str_remove_all, " "))

# Write both sheets to the same Excel file
write_xlsx(
  list(
    Species_count_Quadrat = merged_df_nospaces,
    Species_count_Plotcode = unique_df_nospaces
  ),
  output_file
)
