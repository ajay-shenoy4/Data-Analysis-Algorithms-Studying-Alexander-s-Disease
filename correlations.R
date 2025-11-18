library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Read the sorted CSV file
#sorted_data <- read.csv("/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Correlations/tda_dbscan_merged_data.csv")
#sorted_data <- read.csv("/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Correlations/tda_radii_merged_data.csv")
sorted_data <- read.csv("/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Correlations/NEW_tda_radii_merged_data.csv")
head(sorted_data)

# Get unique minPts and Subregion values
minPts_values <- unique(sorted_data$minPts)

# Initialize final results list
all_results <- list()

# Loop through each minPts and Subregion
for (mp in minPts_values) {
  
  cat("Processing minPts =", mp, "\n")
  
  # Filter all data for this minPts (ignores Subregion)
  data_sub <- sorted_data %>%
    filter(minPts == mp)
  
  # Get subjects
  subjects <- unique(data_sub$Subject)
  
  # Initialize results per (minPts, Subregion)
  results_list <- list()
  
  for (subject in subjects) {
    subject_data <- data_sub %>% filter(Subject == subject)
    
    # Only calculate if there's enough data
    if (nrow(subject_data) > 1) {
      cor_vals <- function(x, y) cor(x, y, use = "complete.obs")
      
      # Calculate correlations
      row <- data.frame(
        minPts = mp,
        Subject = subject,
        
        Correlation_Cluster_H0_Birth = cor_vals(subject_data$cluster_count, subject_data$H0_Birth), 
        Correlation_Cluster_H0_Death = cor_vals(subject_data$cluster_count, subject_data$H0_Death),
        Correlation_Cluster_H1_Birth = cor_vals(subject_data$cluster_count, subject_data$H1_Birth), 
        Correlation_Cluster_H1_Death = cor_vals(subject_data$cluster_count, subject_data$H1_Death),
        Correlation_Cluster_H2_Birth = cor_vals(subject_data$cluster_count, subject_data$H2_Birth), 
        Correlation_Cluster_H2_Death = cor_vals(subject_data$cluster_count, subject_data$H2_Death),
        
        Correlation_Avg_X_H0_Birth = cor_vals(subject_data$avg_x, subject_data$H0_Birth), 
        Correlation_Avg_X_H0_Death = cor_vals(subject_data$avg_x, subject_data$H0_Death),
        Correlation_Avg_X_H1_Birth = cor_vals(subject_data$avg_x, subject_data$H1_Birth), 
        Correlation_Avg_X_H1_Death = cor_vals(subject_data$avg_x, subject_data$H1_Death),
        Correlation_Avg_X_H2_Birth = cor_vals(subject_data$avg_x, subject_data$H2_Birth), 
        Correlation_Avg_X_H2_Death = cor_vals(subject_data$avg_x, subject_data$H2_Death),
        
        Correlation_Avg_Y_H0_Birth = cor_vals(subject_data$avg_y, subject_data$H0_Birth), 
        Correlation_Avg_Y_H0_Death = cor_vals(subject_data$avg_y, subject_data$H0_Death),
        Correlation_Avg_Y_H1_Birth = cor_vals(subject_data$avg_y, subject_data$H1_Birth), 
        Correlation_Avg_Y_H1_Death = cor_vals(subject_data$avg_y, subject_data$H1_Death),
        Correlation_Avg_Y_H2_Birth = cor_vals(subject_data$avg_y, subject_data$H2_Birth), 
        Correlation_Avg_Y_H2_Death = cor_vals(subject_data$avg_y, subject_data$H2_Death),
        
        Correlation_Avg_Z_H0_Birth = cor_vals(subject_data$avg_z, subject_data$H0_Birth), 
        Correlation_Avg_Z_H0_Death = cor_vals(subject_data$avg_z, subject_data$H0_Death),
        Correlation_Avg_Z_H1_Birth = cor_vals(subject_data$avg_z, subject_data$H1_Birth), 
        Correlation_Avg_Z_H1_Death = cor_vals(subject_data$avg_z, subject_data$H1_Death),
        Correlation_Avg_Z_H2_Birth = cor_vals(subject_data$avg_z, subject_data$H2_Birth), 
        Correlation_Avg_Z_H2_Death = cor_vals(subject_data$avg_z, subject_data$H2_Death),
        
        Correlation_Avg_I_H0_Birth = cor_vals(subject_data$avg_i, subject_data$H0_Birth), 
        Correlation_Avg_I_H0_Death = cor_vals(subject_data$avg_i, subject_data$H0_Death),
        Correlation_Avg_I_H1_Birth = cor_vals(subject_data$avg_i, subject_data$H1_Birth), 
        Correlation_Avg_I_H1_Death = cor_vals(subject_data$avg_i, subject_data$H1_Death),
        Correlation_Avg_I_H2_Birth = cor_vals(subject_data$avg_i, subject_data$H2_Birth), 
        Correlation_Avg_I_H2_Death = cor_vals(subject_data$avg_i, subject_data$H2_Death)
      )
    } else {
      row <- data.frame(
        minPts = mp,
        Subject = subject,
        Correlation_Cluster_H0_Birth = NA, Correlation_Cluster_H0_Death = NA,
        Correlation_Cluster_H1_Birth = NA, Correlation_Cluster_H1_Death = NA,
        Correlation_Cluster_H2_Birth = NA, Correlation_Cluster_H2_Death = NA,
        Correlation_Avg_X_H0_Birth = NA, Correlation_Avg_X_H0_Death = NA,
        Correlation_Avg_X_H1_Birth = NA, Correlation_Avg_X_H1_Death = NA,
        Correlation_Avg_X_H2_Birth = NA, Correlation_Avg_X_H2_Death = NA,
        Correlation_Avg_Y_H0_Birth = NA, Correlation_Avg_Y_H0_Death = NA,
        Correlation_Avg_Y_H1_Birth = NA, Correlation_Avg_Y_H1_Death = NA,
        Correlation_Avg_Y_H2_Birth = NA, Correlation_Avg_Y_H2_Death = NA,
        Correlation_Avg_Z_H0_Birth = NA, Correlation_Avg_Z_H0_Death = NA,
        Correlation_Avg_Z_H1_Birth = NA, Correlation_Avg_Z_H1_Death = NA,
        Correlation_Avg_Z_H2_Birth = NA, Correlation_Avg_Z_H2_Death = NA,
        Correlation_Avg_I_H0_Birth = NA, Correlation_Avg_I_H0_Death = NA,
        Correlation_Avg_I_H1_Birth = NA, Correlation_Avg_I_H1_Death = NA,
        Correlation_Avg_I_H2_Birth = NA, Correlation_Avg_I_H2_Death = NA
      )
    }
    
    results_list[[length(results_list) + 1]] <- row
  }
  
  all_results[[length(all_results) + 1]] <- bind_rows(results_list)
}

# Combine all results
final_results <- bind_rows(all_results)

# Save to CSV
#write.csv(final_results, "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Correlations/correlation_dbscan_results.csv", row.names = FALSE)
#write.csv(final_results, "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Correlations/correlation_radii_results.csv", row.names = FALSE)
write.csv(final_results, "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Correlations/NEW_correlation_radii_results.csv", row.names = FALSE)
head(final_results)

# Function to reshape + clean + plot only cluster correlations
make_cluster_plot <- function(df, title_prefix = "") {
  long_df <- df %>%
    pivot_longer(
      cols = starts_with("Correlation_"),
      names_to = "variable",
      values_to = "correlation",
      names_prefix = "Correlation_"
    ) %>%
    select(Subject, minPts, variable, correlation) %>%
    mutate(
      # Keep only first three underscore-separated parts of Subject
      Subject = str_c(word(Subject, 1, 3, sep = "_"), sep = "_")
    )
  
  # Filter for cluster_count variables only
  cluster_data <- long_df %>% filter(grepl("^Cluster_", variable))
  
  ggplot(cluster_data, aes(x = as.factor(minPts), y = correlation)) +
    geom_boxplot() +
    facet_wrap(~ variable, scales = "free_y") +
    theme_minimal() +
    labs(
      title = paste(title_prefix, "Correlation vs minPts for Cluster Variables"),
      x = "minPts",
      y = "Correlation"
    )
}

# Run the function on the results df
cluster_plot <- make_cluster_plot(final_results, "DBSCAN Results by minPts")

# Display the plot
cluster_plot


# Save the plot as PNG
#ggsave(filename = "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Correlations/plotted_correlation_dbscan_results.png", plot = cluster_plot, width = 10, height = 6, dpi = 300, bg = "white")
#ggsave(filename = "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Correlations/plotted_correlation_radii_results.png", plot = cluster_plot, width = 10, height = 6, dpi = 300, bg = "white")
ggsave(filename = "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Correlations/NEW_plotted_correlation_radii_results.png", plot = cluster_plot, width = 10, height = 6, dpi = 300, bg = "white")
