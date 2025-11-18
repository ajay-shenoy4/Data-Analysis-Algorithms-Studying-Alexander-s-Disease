# Load necessary libraries
library(dplyr)

# Path to the AxD folder
#folder_path <- "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Input_Data/AxD_TDA_Old"
folder_path <- "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Input_Data/AxD_Sigma_TDA_New"

# List all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
csv_files

# Function to find the maximum value in a single CSV file
find_max_in_file <- function(file_path) {
  df <- read.csv(file_path, header = FALSE, col.names = c("Birth", "Death"))
  return(max(c(df$Birth, df$Death), na.rm = TRUE))
}

# Create bins that go from 0 to 0.2, increasing by 0.001
bin_edges <- seq(0.000, 0.2, by = 0.001)

# Function to count how many events fall into each bin
count_events_in_bins <- function(time_column, bin_edges) {
  bin_labels <- cut(time_column, breaks = bin_edges, include.lowest = TRUE, right = FALSE)
  event_counts <- table(bin_labels)
  bin_count_df <- data.frame(Bin = as.character(names(event_counts)), Count = as.numeric(event_counts))
  return(bin_count_df)
}

# Function to process each CSV file and extract birth and death counts by bin
process_csv_file <- function(file_path, bin_edges) {
  df <- read.csv(file_path, header = FALSE, col.names = c("Birth", "Death"))
  birth_counts <- count_events_in_bins(df$Birth, bin_edges)
  death_counts <- count_events_in_bins(df$Death, bin_edges)
  merged_df <- merge(birth_counts, death_counts, by = "Bin", all = TRUE)
  merged_df[is.na(merged_df)] <- 0
  file_name <- basename(file_path)
  subject_name <- sub("\\.csv$", "", file_name)
  base_subject <- sub("H[0-2]$", "", subject_name)
  suffix <- substring(subject_name, nchar(subject_name) - 1, nchar(subject_name))
  colnames(merged_df)[2:3] <- paste(suffix, c("Birth", "Death"), sep = "_")
  merged_df$Subject <- base_subject
  return(merged_df)
}

# Process all CSV files
processed_files <- lapply(csv_files, process_csv_file, bin_edges = bin_edges)

# Combine data by base subject, grouping by the first 8 parts of the Subject
combined_results <- processed_files %>%
  bind_rows() %>%
  # Extract the first 8 parts of the Subject
  mutate(Grouped_Subject = sapply(strsplit(Subject, "_"), function(parts) paste(parts[1:min(8, length(parts))], collapse = "_"))) %>%
  group_by(Bin, Grouped_Subject) %>%
  summarize(
    H0_Birth = sum(H0_Birth, na.rm = TRUE),
    H0_Death = sum(H0_Death, na.rm = TRUE),
    H1_Birth = sum(H1_Birth, na.rm = TRUE),
    H1_Death = sum(H1_Death, na.rm = TRUE),
    H2_Birth = sum(H2_Birth, na.rm = TRUE),
    H2_Death = sum(H2_Death, na.rm = TRUE),
    .groups = "drop"
  )

# Rename Grouped_Subject back to Subject for clarity
colnames(combined_results)[2] <- "Subject"

# Modify the 'Bin' column to change [0, 0.001) to [0.000, 0.001)
# and ensure there is a space after every comma
combined_results <- combined_results %>%
  mutate(Bin = gsub("^\\[0,", "[0.000,", Bin)) %>%
  mutate(Bin = gsub(",", ", ", Bin))  # Ensures there is a space after every comma

# Save the final table to a CSV file
#output_path <- "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/TDA_Results/tda_data_old.csv"
output_path <- "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/TDA_Results/tda_data_new.csv"
write.csv(combined_results, output_path, row.names = FALSE)

# Print the final table
head(combined_results)

cat("Combined binned data saved to:", output_path, "\n")