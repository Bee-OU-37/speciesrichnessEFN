# load and merge two csv files
# one with alpha diversity and one with lat and long for each location, then save the merged data frame to a new csv file


library(readr)
library(dplyr)

# Load the first CSV file
alpha_div <- read.csv("env_data_total_Q_inclLocations.csv")
# Load the second CSV file
locs <- read.csv("env_data_total_1m_inclLocations.csv")

head(alpha_div)
head(locs)

#change location into Location in locs
colnames(locs)[colnames(locs) == "location"] <- "Location"

# keep only Location, filename, Plotcode, Latitude_GIS, Longitude_GIS from locs
locs <- locs %>%
  select(Location, filename, Plotcode, Latitude_GIS, Longitude_GIS)

# Merge the two data frames based on the Location, Plotcode, and filename columns, keeping only Latitude and Longitude from the second data frame
merged_data <- alpha_div %>%
  left_join(locs %>% select(Location, Plotcode, filename, Latitude_GIS, Longitude_GIS), by = c("Location", "Plotcode", "filename"))

# Check the merged data frame
head(merged_data)

# save the merged data frame to a new CSV file
write.csv(merged_data, "env_data_total_Q_inclLocations_lat_long.csv", row.names = FALSE)