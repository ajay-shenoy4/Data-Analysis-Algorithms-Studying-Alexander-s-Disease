library(dplyr)
library(tidyr)
library(stringr)

# Load the CSV files
#tda_data <- read.csv("/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/TDA_Results/tda_data_old.csv")
tda_data <- read.csv("/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/TDA_Results/tda_data_new.csv")
head(tda_data)
dbscan_data <- read.csv("/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Radii_Python_newData/newData_binned_radii_results.csv")
head(dbscan_data)

# ----- Process old tda_data -----
#tda_data <- tda_data %>%
# separate(Subject, into = c("Image_Type", "Sex", "Genotype", "Brain", "Laterality", "Region", "Extra_genotype", "Group"), sep = "_", remove = FALSE) %>%
#mutate(
# Standard_Subject = paste(Image_Type, Sex, Genotype, Brain, Laterality, sep = "_")
#)
#head(tda_data)

# ----- Process NEW tda_data -----
tda_data <- tda_data %>%
  separate(Subject, into = c("Sex", "Genotype", "Image_Type", "Brain", "Region", "Laterality"), sep = "_", remove = FALSE) %>%
  mutate(
    Standard_Subject = paste(Image_Type, Sex, Genotype, Brain, Laterality, sep = "_")
  )
head(tda_data)

# ----- Process dbscan_data -----
dbscan_data <- dbscan_data %>%
  separate(Subject, into = c("Sex", "Genotype", "Image_Type", "Brain", "Laterality"), sep = "_", remove = FALSE) %>%
  mutate(Standard_Subject = paste(Image_Type, Sex, Genotype, Brain, Laterality, sep = "_"))
head(dbscan_data)

# ----- OPTIONAL: Drop old Subject column and rename standardized column -----
tda_data <- tda_data %>%
  select(-Subject) %>%
  rename(Subject = Standard_Subject)

dbscan_data <- dbscan_data %>%
  select(-Subject) %>%
  rename(Subject = Standard_Subject)

# ----- Preview -----
head(tda_data)
head(dbscan_data)

# Extract unique subjects
subjects <- unique(c(tda_data$Subject, dbscan_data$Subject))
subjects

#minPts <- # or whatever values are relevant for your analysis
minPts <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

# Generate the complete bin range from 0 to 0.2 in steps of 0.001
bins <- sprintf("[%.3f, %.3f)", seq(0, 0.199, by = 0.001), seq(0.001, 0.2, by = 0.001))

# Create a complete dataset of all subject-bin-minPts combinations
all_combinations <- expand.grid(Bin = bins, Subject = subjects, minPts = minPts, stringsAsFactors = FALSE)

# Merge the complete grid with the existing DBSCAN data
dbscan_complete <- full_join(all_combinations, dbscan_data, by = c("Bin", "Subject", "minPts"))
head(dbscan_complete)

# Replace NA values with 0 for all columns except Bin and Subject
dbscan_complete[is.na(dbscan_complete)] <- 0
head(dbscan_complete)

# Save the updated DBSCAN dataset
write.csv(dbscan_complete, "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Radii_Python_newData/newData_filled_radii_binned_results.csv", row.names = FALSE)

# Sort each dataset by Bin and Subject and minPts
tda_data <- tda_data %>% arrange(Bin, Subject)
head(tda_data)
dbscan_complete <- dbscan_complete %>% arrange(Bin, Subject, minPts)
head(dbscan_complete)

# Find the highest and lowest Bin values
highest_bin_tda <- max(tda_data$Bin, na.rm = TRUE)
lowest_bin_tda <- min(tda_data$Bin, na.rm = TRUE)
highest_bin_dbscan <- max(dbscan_complete$Bin, na.rm = TRUE)
lowest_bin_dbscan <- min(dbscan_complete$Bin, na.rm = TRUE)

# Print the results
cat("TDA Data - Lowest Bin:", lowest_bin_tda, "\n")
cat("TDA Data - Highest Bin:", highest_bin_tda, "\n")
cat("DBSCAN Data - Lowest Bin:", lowest_bin_dbscan, "\n")
cat("DBSCAN Data - Highest Bin:", highest_bin_dbscan, "\n")

# Create a unique identifier column by combining Bin and Subject
tda_data <- tda_data %>% mutate(Identifier = paste(Bin, Subject, sep = "_"))
print(head(tda_data))
dbscan_complete <- dbscan_complete %>% mutate(Identifier = paste(Bin, Subject, sep = "_"))
print(head(dbscan_complete))

# Find common Identifiers in both datasets
common_identifiers <- intersect(tda_data$Identifier, dbscan_complete$Identifier)
#print(common_identifiers)

# Filter both datasets to keep only rows with matching Identifiers
tda_filtered <- tda_data %>% filter(Identifier %in% common_identifiers)
print(head(tda_filtered))
dbscan_filtered <- dbscan_complete %>% filter(Identifier %in% common_identifiers)
print(head(dbscan_filtered))

# Merge the filtered datasets on Bin and Subject
merged_data <- inner_join(tda_filtered, dbscan_filtered, by = c("Bin", "Subject"))
print(head(merged_data))

# Drop the Identifier columns (optional)
merged_data <- merged_data %>% select(-Identifier.x)
merged_data <- merged_data %>% select(-Identifier.y)
head(merged_data)

# Identify all duplicate column pairs (those ending in .x and .y)
x_cols <- names(merged_data)[grepl("\\.x$", names(merged_data))]
y_cols <- names(merged_data)[grepl("\\.y$", names(merged_data))]

# Extract base column names (e.g., "Sex" from "Sex.x")
base_cols <- gsub("\\.x$", "", x_cols)

# Coalesce each .x/.y pair into a single new column
for (col in base_cols) {
  merged_data[[col]] <- coalesce(merged_data[[paste0(col, ".x")]], merged_data[[paste0(col, ".y")]])
}

# Drop the old .x and .y columns
merged_data <- merged_data %>% select(-all_of(c(x_cols, y_cols)))

# Resulting cleaned dataframe
head(merged_data)

# Save the merged data to a new CSV file
write.csv(merged_data, "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Correlations/NEW_tda_radii_merged_data.csv", row.names = FALSE)


