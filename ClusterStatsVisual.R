############################################################ 
# Cluster Analysis - Spatial Heatmaps by x_bin / y_bin
############################################################ 
library(dplyr) 
library(ggplot2) 
library(viridis) 
library(tidyr) 
library(stringr)
library(ggh4x)
library(patchwork)

############################################################ 
# Define Data + Output Directory 
############################################################ 
output_dir <- "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Radii_Python_newData/filtered_images/cluster_analysis_results"

heatmap_dir <- file.path(output_dir, "spatial_heatmaps")
if (!dir.exists(heatmap_dir)) dir.create(heatmap_dir, recursive = TRUE)

############################################################ 
# Load + Combine CSV Files 
############################################################ 
csv_files <- list.files(path = output_dir, pattern = "\\.csv$", full.names = TRUE)
data_list <- lapply(csv_files, read.csv)
names(data_list) <- tools::file_path_sans_ext(basename(csv_files))
combined_df <- bind_rows(data_list, .id = "batch")

# Turn NA to 0 for numeric columns
combined_df <- combined_df %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)))

############################################################ 
# Clean / Format Variables 
############################################################ 
combined_df <- combined_df %>% 
  mutate( 
    subject  = as.character(subject), 
    batch    = as.factor(batch), 
    x_bin    = as.numeric(x_bin), 
    y_bin    = as.numeric(y_bin) 
  )

############################################################ 
# Parse Subject Metadata from Subject String
# New format: Female_AxD_NDI_brain1_HPF
############################################################ 
combined_df <- combined_df %>%
  mutate(
    sex = case_when(
      str_detect(subject, regex("female", ignore_case = TRUE)) ~ "Female",
      str_detect(subject, regex("male",   ignore_case = TRUE)) ~ "Male",
      TRUE ~ "Unknown"
    ),
    genotype = case_when(
      str_detect(subject, "AxD") ~ "AxD",
      str_detect(subject, "WT")  ~ "WT",
      TRUE ~ "Unknown"
    ),
    metric = case_when(
      str_detect(subject, "NDI") ~ "NDI",
      str_detect(subject, "ODI") ~ "ODI",
      TRUE ~ "Unknown"
    ),
    group = paste(metric, genotype),
    sex      = factor(sex,      levels = c("Female", "Male")),
    genotype = factor(genotype, levels = c("AxD", "WT")),
    metric   = factor(metric,   levels = c("NDI", "ODI")),
    group    = factor(group,    levels = c("NDI AxD", "NDI WT", "ODI AxD", "ODI WT"))
  )

############################################################ 
# Save Combined Dataset 
############################################################ 
write.csv(combined_df, file.path(output_dir, "combined_cluster_results.csv"), row.names = FALSE)
cat("Combined dataset saved.\n")

############################################################ 
# Pivot to Long Format
# Set axis_version to "x" or "y" to switch which clustering to visualize
############################################################ 
axis_version <- "x"

pct_prefix <- paste0("pct_", axis_version, "_")

long_df <- combined_df %>%
  select(subject, sex, genotype, metric, group,
         minPts_x, minPts_y, x_bin, y_bin,
         starts_with(pct_prefix)) %>%
  pivot_longer(
    cols      = starts_with(pct_prefix),
    names_to  = "subregion",
    values_to = "pct"
  ) %>%
  mutate(
    subregion = gsub(pct_prefix, "", subregion),
    subregion = factor(subregion, levels = c("CA1", "CA2", "CA3", "DG"))
  )

############################################################ 
# Get all unique minPts combinations
############################################################ 
minpts_combos <- combined_df %>%
  select(minPts_x, minPts_y) %>%
  distinct() %>%
  arrange(minPts_x, minPts_y)

cat(sprintf("\nFound %d unique minPts_x / minPts_y combinations.\n", nrow(minpts_combos)))

############################################################ 
# Helper: build and save one heatmap
# Each subregion gets its own panel + independent colorbar via patchwork
############################################################ 
save_heatmap <- function(plot_df, fill_var, fill_label, mx, my, metric_name, out_dir) {
  
  sr_levels <- levels(plot_df$subregion)
  
  subregion_plots <- lapply(sr_levels, function(sr) {
    
    df_sr <- plot_df %>% filter(subregion == sr)
    is_top <- sr == sr_levels[1]  # only show column strips on the top subregion panel
    
    ggplot(df_sr, aes(x = factor(x_bin), y = factor(y_bin), fill = .data[[fill_var]])) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_viridis_c(
        option   = "plasma",
        name     = sprintf("%s\n(%s)", fill_label, sr),  # legend labelled with subregion
        na.value = "grey80"
      ) +
      facet_nested(
        . ~ sex + group,
        nest_line = element_line(color = "grey40", linewidth = 0.5)
      ) +
      theme_minimal(base_size = 11) +
      theme(
        # Only show column strip labels on the top panel
        strip.text       = if (is_top) element_text(face = "bold", size = 10) else element_blank(),
        strip.background = if (is_top) element_rect(fill = "grey92", color = NA) else element_blank(),
        panel.grid       = element_blank(),
        # Only show x-axis text on the bottom panel
        axis.text.x      = if (sr == sr_levels[length(sr_levels)]) element_text(angle = 45, hjust = 1, size = 7) else element_blank(),
        axis.title.x     = if (sr == sr_levels[length(sr_levels)]) element_text() else element_blank(),
        axis.text.y      = element_text(size = 7),
        legend.position  = "right",
        panel.spacing    = unit(0.4, "lines"),
        plot.title       = element_text(face = "bold", size = 11, hjust = 0)
      ) +
      labs(
        title = sr,   # subregion name as panel title
        x     = if (sr == sr_levels[length(sr_levels)]) "x_bin" else "",
        y     = "y_bin"
      )
  })
  
  # Stack panels vertically with patchwork
  combined <- wrap_plots(subregion_plots, ncol = 1) +
    plot_annotation(
      title    = sprintf("%s — %s  |  minPts_x = %d, minPts_y = %d",
                         metric_name, fill_label, mx, my),
      subtitle = "Rows: Subregion  |  Columns: Sex > Group  |  Axes: x_bin × y_bin",
      theme    = theme(
        plot.title    = element_text(face = "bold", hjust = 0.5, size = 13),
        plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 10)
      )
    )
  
  fname <- sprintf("heatmap_%s_%s_minPtsX%02d_minPtsY%02d.png",
                   metric_name, fill_var, mx, my)
  ggsave(file.path(out_dir, fname), plot = combined,
         width = 16, height = 16, dpi = 300)
  cat("  Saved:", fname, "\n")
}

############################################################ 
# Main Loop
############################################################ 
metrics <- c("NDI", "ODI")

for (i in seq_len(nrow(minpts_combos))) {
  
  mx <- minpts_combos$minPts_x[i]
  my <- minpts_combos$minPts_y[i]
  
  cat(sprintf("\nProcessing minPts_x=%d, minPts_y=%d ...\n", mx, my))
  
  combo_df <- long_df %>%
    filter(minPts_x == mx, minPts_y == my)
  
  if (nrow(combo_df) == 0) {
    cat("  No data for this combination, skipping.\n")
    next
  }
  
  for (mt in metrics) {
    
    cat(sprintf("  Metric: %s\n", mt))
    
    sr_df <- combo_df %>%
      filter(metric == mt) %>%
      group_by(sex, group, subregion, x_bin, y_bin) %>%
      summarise(
        mean_pct = mean(pct, na.rm = TRUE),
        var_pct  = var(pct,  na.rm = TRUE),
        .groups  = "drop"
      )
    
    if (nrow(sr_df) == 0) {
      cat("    No data for this combination, skipping.\n")
      next
    }
    
    save_heatmap(sr_df, "mean_pct", "Mean %",   mx, my, mt, heatmap_dir)
    save_heatmap(sr_df, "var_pct",  "Variance", mx, my, mt, heatmap_dir)
  }
}

############################################################ 
# Completion Message 
############################################################ 
cat("\n============================================================\n")
cat("Script Complete!\n")
cat("============================================================\n")
cat("Spatial heatmaps saved to:\n", heatmap_dir, "\n")
cat("============================================================\n")