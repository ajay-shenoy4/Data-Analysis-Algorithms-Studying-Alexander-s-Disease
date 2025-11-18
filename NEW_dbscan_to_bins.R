# Required libraries
library(dplyr)
library(stringr)

combined_csv_path <-  "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Radii_Python_newData/newData_combined_coreRadii_all_minPts.csv"

# Load the combined data
combined_data <- read.csv(combined_csv_path)
head(combined_data)

#Use this to remove HPF and replace with laterality for subject
combined_data <- combined_data %>%
  mutate(
    Subject = str_replace(Subject, "HPF$", Laterality)
  )
# Load the combined data
head(combined_data)

# Specify the bin size
bin_size <- 0.001

# Function to create bins and summarize data
summarized_table <- combined_data %>%
  # Create bins for recorded_radius
  mutate(Bin = cut(recorded_radius, 
                   breaks = seq(0, max(recorded_radius, na.rm = TRUE) + bin_size, by = bin_size),
                   include.lowest = TRUE, 
                   right = FALSE,
                   labels = paste0("[", 
                                   seq(0, max(recorded_radius, na.rm = TRUE), by = bin_size),
                                   ", ", 
                                   seq(bin_size, max(recorded_radius, na.rm = TRUE) + bin_size, by = bin_size),
                                   ")"))) %>%
  group_by(Bin, Subject, minPts, subregion) %>% # Group by bin and subject
  summarise(
    cluster_count = n(),      # Count the points in each bin
    avg_i = mean(i, na.rm = TRUE), # Calculate the average of column `i`
    avg_x = mean(x, na.rm = TRUE), # Calculate the average of column `x`
    avg_y = mean(y, na.rm = TRUE), # Calculate the average of column `y`
    avg_z = mean(z, na.rm = TRUE)  # Calculate the average of column `z`
  ) %>%
  arrange(Bin, Subject, minPts, subregion)       # Sort by bin and subject

output_summary_path <- "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Radii_Python_newData/newData_binned_radii_results.csv"
write.csv(summarized_table, file = output_summary_path, row.names = FALSE)

# Print a preview of the summarized table
head(summarized_table)

