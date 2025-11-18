############################################################
# ANOVA Analysis by Brain and minPts
# Tests: Sex, Genotype, Image, subregion, and their interactions
############################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(pheatmap)  
library(effectsize)
library(emmeans)

############################################################
# 1. Load and preprocess data
############################################################
data_path <- "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Radii_Python_newData/newData_combined_coreRadii_all_minPts.csv"
combined_dbscan_data <- read.csv(data_path)

combined_dbscan_data <- combined_dbscan_data %>%
  mutate(
    recorded_radius = as.numeric(recorded_radius),
    minPts = as.numeric(minPts),
    Sex = factor(Sex),
    Genotype = factor(Genotype),
    subregion = factor(subregion),
    Image = factor(Image),
    brain = factor(Brain)
  )

# Get unique values
brains <- unique(combined_dbscan_data$Brain)
minPts_values <- unique(combined_dbscan_data$minPts)

############################################################
# 2. Run ANOVA for each minPts combination
############################################################
output_dir <- "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Plots/"

anova_results_list <- list()

for (m in minPts_values) {
  cat("\n========================================\n")
  cat("minPts:", m, "\n")
  cat("========================================\n")
  
  # Filter data for this minPts
  data_sub <- combined_dbscan_data %>%
    filter(minPts == m)
  
  # Check if we have enough data
  if (nrow(data_sub) < 10) {
    cat("Skipping: insufficient data (n =", nrow(data_sub), ")\n")
    next
  }
  
  # Check if we have variation in all factors
  n_sex <- length(unique(data_sub$Sex))
  n_genotype <- length(unique(data_sub$Genotype))
  n_image <- length(unique(data_sub$Image))
  n_subregion <- length(unique(data_sub$subregion))
  
  if (n_sex < 2 || n_genotype < 2 || n_image < 2 || n_subregion < 2) {
    cat("Skipping: insufficient factor levels\n")
    cat("  Sex levels:", n_sex, "| Genotype levels:", n_genotype, 
        "| Image levels:", n_image, "| Subregion levels:", n_subregion, "\n")
    next
  }
  
  # Run ANOVA with all requested terms
  tryCatch({
    model <- aov(recorded_radius ~ Sex + Genotype + Image + subregion + 
                   Sex:Genotype + 
                   Genotype:subregion + 
                   Sex:subregion + 
                   Sex:Genotype:subregion + 
                   Sex:Genotype:subregion:Image, 
                 data = data_sub)
    
    # Get ANOVA summary
    anova_summary <- summary(model)
    print(anova_summary)
    
    # Extract p-values
    anova_table <- as.data.frame(anova_summary[[1]])
    
    # Clean up rownames by removing trailing spaces
    rownames(anova_table) <- trimws(rownames(anova_table))
    
    # Print rownames to debug
    cat("Available terms in ANOVA table:\n")
    print(rownames(anova_table))
    
    # Create results dataframe
    results_df <- data.frame(
      minPts = m,
      n_observations = nrow(data_sub),
      
      # Main effects
      Sex_pval = ifelse("Sex" %in% rownames(anova_table), 
                        anova_table["Sex", "Pr(>F)"], NA),
      Genotype_pval = ifelse("Genotype" %in% rownames(anova_table), 
                             anova_table["Genotype", "Pr(>F)"], NA),
      Image_pval = ifelse("Image" %in% rownames(anova_table), 
                          anova_table["Image", "Pr(>F)"], NA),
      subregion_pval = ifelse("subregion" %in% rownames(anova_table), 
                              anova_table["subregion", "Pr(>F)"], NA),
      
      # Two-way interactions
      Sex_Genotype_pval = ifelse("Sex:Genotype" %in% rownames(anova_table), 
                                 anova_table["Sex:Genotype", "Pr(>F)"], NA),
      Genotype_subregion_pval = ifelse("Genotype:subregion" %in% rownames(anova_table), 
                                       anova_table["Genotype:subregion", "Pr(>F)"], NA),
      Sex_subregion_pval = ifelse("Sex:subregion" %in% rownames(anova_table), 
                                  anova_table["Sex:subregion", "Pr(>F)"], NA),
      
      # Three-way interaction
      Sex_Genotype_subregion_pval = ifelse("Sex:Genotype:subregion" %in% rownames(anova_table), 
                                           anova_table["Sex:Genotype:subregion", "Pr(>F)"], NA),
      
      # Four-way interaction - try both possible orderings
      Sex_Genotype_subregion_Image_pval = ifelse("Sex:Genotype:Image:subregion" %in% rownames(anova_table), 
                                                 anova_table["Sex:Genotype:Image:subregion", "Pr(>F)"], 
                                                 ifelse("Sex:Genotype:subregion:Image" %in% rownames(anova_table),
                                                        anova_table["Sex:Genotype:subregion:Image", "Pr(>F)"], NA))
    )
    
    # Store results
    anova_results_list[[paste(m, sep = "_")]] <- results_df
    
  }, error = function(e) {
    cat("Error fitting model:", e$message, "\n")
  })
}


############################################################
# 3. Combine and save results
############################################################
anova_results_combined <- bind_rows(anova_results_list)

# Add significance indicators
anova_results_combined <- anova_results_combined %>%
  mutate(
    Sex_sig = case_when(
      Sex_pval < 0.001 ~ "***",
      Sex_pval < 0.01  ~ "**",
      Sex_pval < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Genotype_sig = case_when(
      Genotype_pval < 0.001 ~ "***",
      Genotype_pval < 0.01  ~ "**",
      Genotype_pval < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Image_sig = case_when(
      Image_pval < 0.001 ~ "***",
      Image_pval < 0.01  ~ "**",
      Image_pval < 0.05  ~ "*",
      TRUE ~ ""
    ),
    subregion_sig = case_when(
      subregion_pval < 0.001 ~ "***",
      subregion_pval < 0.01  ~ "**",
      subregion_pval < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Sex_Genotype_sig = case_when(
      Sex_Genotype_pval < 0.001 ~ "***",
      Sex_Genotype_pval < 0.01  ~ "**",
      Sex_Genotype_pval < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Genotype_subregion_sig = case_when(
      Genotype_subregion_pval < 0.001 ~ "***",
      Genotype_subregion_pval < 0.01  ~ "**",
      Genotype_subregion_pval < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Sex_subregion_sig = case_when(
      Sex_subregion_pval < 0.001 ~ "***",
      Sex_subregion_pval < 0.01  ~ "**",
      Sex_subregion_pval < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Sex_Genotype_subregion_sig = case_when(
      Sex_Genotype_subregion_pval < 0.001 ~ "***",
      Sex_Genotype_subregion_pval < 0.01  ~ "**",
      Sex_Genotype_subregion_pval < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Sex_Genotype_subregion_Image_sig = case_when(
      Sex_Genotype_subregion_Image_pval < 0.001 ~ "***",
      Sex_Genotype_subregion_Image_pval < 0.01  ~ "**",
      Sex_Genotype_subregion_Image_pval < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# Display results
print(anova_results_combined)

# Save to CSV
write.csv(anova_results_combined, 
          paste0(output_dir, "ANOVA_results_by_minPts.csv"), 
          row.names = FALSE)

cat("\n========================================\n")
cat("Analysis complete! Results saved to:\n")
cat(paste0(output_dir, "ANOVA_results_by_minPts.csv\n"))
cat("========================================\n")

############################################################
# 4. Summary of significant effects
############################################################
cat("\n\nSUMMARY OF SIGNIFICANT EFFECTS (p < 0.05):\n")
cat("==========================================\n\n")

summary_sig <- anova_results_combined %>%
  summarise(
    n_total = n(),
    Sex_sig_count = sum(Sex_pval < 0.05, na.rm = TRUE),
    Genotype_sig_count = sum(Genotype_pval < 0.05, na.rm = TRUE),
    Image_sig_count = sum(Image_pval < 0.05, na.rm = TRUE),
    subregion_sig_count = sum(subregion_pval < 0.05, na.rm = TRUE),
    Sex_Genotype_sig_count = sum(Sex_Genotype_pval < 0.05, na.rm = TRUE),
    Genotype_subregion_sig_count = sum(Genotype_subregion_pval < 0.05, na.rm = TRUE),
    Sex_subregion_sig_count = sum(Sex_subregion_pval < 0.05, na.rm = TRUE),
    Sex_Genotype_subregion_sig_count = sum(Sex_Genotype_subregion_pval < 0.05, na.rm = TRUE),
    Sex_Genotype_subregion_Image_sig_count = sum(Sex_Genotype_subregion_Image_pval < 0.05, na.rm = TRUE)
  )

print(summary_sig)
summary_sig %>% t()
eta_squared(model)

############################################################
# Option 1: Heatmap of -log10(p-values) for all effects
############################################################

# Prepare data: select p-value columns and transform
heatmap_data <- anova_results_combined %>%
  select(minPts, ends_with("_pval")) %>%
  pivot_longer(cols = ends_with("_pval"), 
               names_to = "Effect", 
               values_to = "pval") %>%
  mutate(
    Effect = gsub("_pval", "", Effect),
    Effect = gsub("_", " × ", Effect),  # Make interaction terms clearer
    neg_log10_pval = -log10(pval),
    neg_log10_pval = ifelse(is.infinite(neg_log10_pval), 300, neg_log10_pval)  # Cap infinite values
  )

# Create heatmap with ggplot2
p1 <- ggplot(heatmap_data, aes(x = Effect, y = as.factor(minPts), fill = neg_log10_pval)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis(option = "plasma", name = "-log10(p-value)") +
  geom_text(aes(label = ifelse(pval < 0.001, "***", 
                               ifelse(pval < 0.01, "**",
                                      ifelse(pval < 0.05, "*", "")))),
            color = "white", size = 3) +
  labs(title = "ANOVA Results Heatmap: Significance Across minPts",
       x = "Effect", y = "minPts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 9),
        strip.text = element_text(face = "bold", size = 11))

print(p1)
ggsave(paste0(output_dir, "ANOVA_heatmap_all_effects.png"), 
       p1, width = 14, height = 8, dpi = 300)

############################################################
# Option 2: Heatmap of raw p-values for all effects
############################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Prepare data: select p-value columns and transform
heatmap_data <- anova_results_combined %>%
  select(minPts, ends_with("_pval")) %>%
  pivot_longer(
    cols = ends_with("_pval"), 
    names_to = "Effect", 
    values_to = "pval"
  ) %>%
  mutate(
    Effect = gsub("_pval", "", Effect),
    Effect = gsub("_", " × ", Effect),  # Make interaction terms clearer
    pval = ifelse(is.infinite(pval), NA, pval)  # Handle any infinite values just in case
  )

# Create heatmap with ggplot2
p1 <- ggplot(heatmap_data, aes(x = Effect, y = as.factor(minPts), fill = pval)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis(
    option = "plasma",
    name = "p-value",
    direction = -1,
    limits = c(0, 0.05),     # Optional: cap color scale at 0.05 for better contrast
    oob = scales::squish
  ) +
  geom_text(aes(
    label = ifelse(pval < 0.001, "***",
                   ifelse(pval < 0.01, "**",
                          ifelse(pval < 0.05, "*", "")))
  ),
  color = "white", size = 3) +
  labs(
    title = "ANOVA Results Heatmap: Raw p-values Across minPts",
    x = "Effect", y = "minPts"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold", size = 11)
  )

print(p1)

# Save figure
ggsave(
  paste0(output_dir, "ANOVA_heatmap_raw_pvalues.png"), 
  p1, width = 14, height = 8, dpi = 300
)

subregions <- unique(combined_dbscan_data$subregion)
unique_images <- unique(combined_dbscan_data$Image)

############################################################
# 4. Two-way ANOVA and EMMeans and Tukey test
############################################################
emmeans_list <- list()
contrast_list <- list()
tukey_list <- list()

for (img_type in unique_images) {
  for (s in subregions) {
    for (m in unique(combined_dbscan_data$minPts)) {
      cat("\nImage:", img_type, "| Subregion:", s, "| minPts:", m, "\n")
      
      # Filter data
      data_sub <- combined_dbscan_data %>%
        filter(Image == img_type, subregion == s, minPts == m)
      print(head(data_sub))
      
      # Skip if not enough variation
      if (nrow(data_sub) < 5 ||
          length(unique(data_sub$Sex)) < 2 ||
          length(unique(data_sub$Genotype)) < 2) {
        cat("Skipping due to insufficient data.\n")
        next
      }
      
      # Run ANOVA
      model <- aov(recorded_radius ~ Sex * Genotype, data = data_sub)
      print(summary(model))
      
      # EMMeans
      em_df <- as.data.frame(emmeans(model, ~ Sex * Genotype)) %>%
        mutate(Image = img_type, subregion = s, minPts = m)
      emmeans_list[[paste(img_type, s, m, sep = "_")]] <- em_df
      
      # Contrasts (Genotype differences within each Sex)
      cont_df <- tryCatch({
        as.data.frame(emmeans(model, pairwise ~ Genotype | Sex)$contrasts) %>%
          mutate(Image = img_type, subregion = s, minPts = m)
      }, error = function(e) NULL)
      
      contrast_list[[paste(img_type, s, m, sep = "_")]] <- cont_df
      
      # Tukey HSD
      tukey_df <- tryCatch({
        tukey_result <- TukeyHSD(model, which = "Sex:Genotype")
        as.data.frame(tukey_result$`Sex:Genotype`) %>%
          tibble::rownames_to_column("comparison") %>%
          mutate(Image = img_type, subregion = s, minPts = m)
      }, error = function(e) NULL)
      tukey_list[[paste(img_type, s, m, sep = "_")]] <- tukey_df
    }
  }
}

# Combine and save
emmeans_results <- bind_rows(emmeans_list)
contrast_results <- bind_rows(contrast_list)
tukey_results <- bind_rows(tukey_list)

# View results
print(head(emmeans_results))
print(head(contrast_results))
print(head(tukey_results))

# Optional: Save results to CSV
write.csv(emmeans_results, 
          paste0(output_dir, "emmeans_results_by_Image_Subregion_minPts.csv"), 
          row.names = FALSE)
write.csv(contrast_results, 
          paste0(output_dir, "contrast_results_by_Image_Subregion_minPts.csv"), 
          row.names = FALSE)
write.csv(tukey_results, 
          paste0(output_dir, "tukey_results_by_Image_Subregion_minPts.csv"), 
          row.names = FALSE)

############################################################
# 5. Add significance labels and merge p-values
############################################################

# Add significance symbols based on p-values
contrast_results <- contrast_results %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  )

# View the structure to confirm columns
head(contrast_results)

# Merge EMMeans results with p-values and significance by matching relevant grouping variables
emmeans_plot <- emmeans_results %>%
  left_join(
    contrast_results %>%
      select(subregion, Sex, minPts, Image, p.value, sig),
    by = c("subregion", "Sex", "minPts", "Image")
  ) %>%
  mutate(
    Sex_Genotype = paste(Sex, Genotype, sep = "_")
  )

# View the first few rows
head(emmeans_plot)

############################################################
# Nested Bubble Plot: minPts x Subregion x Sex x Genotype x Image
# Circle fill = p-value, Size = emmean
############################################################

heatmap_data <- emmeans_plot %>%
  mutate(
    # Labeling and ordering
    y_label = paste0(subregion, " | ", Sex, " | ", Genotype),
    subregion_order = as.numeric(factor(subregion)),
    sex_order = ifelse(Sex == "Female", 0, 0.5),
    genotype_order = ifelse(Genotype == "AxD", 0, 0.25),
    y_order = subregion_order + sex_order + genotype_order
  ) %>%
  arrange(subregion, Sex, Genotype, minPts, Image)

# Add visualization helpers
heatmap_data_sep <- heatmap_data %>%
  group_by(subregion) %>%
  mutate(subregion_num = cur_group_id()) %>%
  ungroup() %>%
  mutate(
    y_label_clean = paste0(Sex, "-", Genotype),
    facet_label = paste0(subregion, " (", Image, ")")
  )

############################################################
# Create Filled Bubble Plot
############################################################

p2 <- ggplot(
  heatmap_data_sep,
  aes(
    x = as.factor(minPts),
    y = y_label_clean,
    size = emmean,        # point size = emmean
    fill = p.value        # point fill = p-value
  )
) +
  geom_point(shape = 21, color = "black", stroke = 0.6, alpha = 0.9) +  # filled circles with border
  facet_grid(subregion ~ Image, scales = "free_y", space = "free_y") +
  
  # Fill color scale for p-values
  scale_fill_viridis(
    option = "plasma",
    name = "p-value",
    direction = -1,
    trans = "log10",
    labels = scales::scientific
  ) +
  
  # Size scale for emmeans
  scale_size_continuous(
    name = "Estimated marginal mean",
    range = c(3, 12)
  ) +
  
  # Overlay significance stars
  geom_text(
    aes(label = sig),
    color = "white",
    size = 4,
    fontface = "bold",
    vjust = 0.5
  ) +
  
  labs(
    title = "Emmeans Bubble Plot: Genotype Differences by Experimental Conditions",
    subtitle = "Circle size = EMMean; Fill = p-value (AxD vs WT comparison within each Sex)",
    x = "minPts",
    y = "Sex–Genotype"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 11),
    strip.text.y = element_text(angle = 0),
    legend.position = "right",
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5)
  )

print(p2)

############################################################
# Save figure
############################################################
ggsave(
  paste0(output_dir, "ANOVA_nested_bubbleplot_filled_faceted.png"),
  p2, width = 14, height = 12, dpi = 300
)

cat("\nFilled bubble plot saved successfully!\n")
cat("- ANOVA_nested_bubbleplot_filled_faceted.png\n")


############################################################
# Tukey Post-hoc Circle Heatmap
# Fill color = adjusted p-value, size = diff
############################################################
# Prepare Tukey data
tukey_plot <- tukey_results %>%
  filter(!comparison %in% c("Female:WT-Male:AxD", "Male:WT-Female:AxD")) %>%
  mutate(
    comparison = factor(comparison, levels = unique(comparison)),  # preserve original order
    sig_label = case_when(
      `p adj` < 0.001 ~ "***",
      `p adj` < 0.01  ~ "**",
      `p adj` < 0.05  ~ "*",
      TRUE ~ ""
    ),
    diff_abs = abs(diff)  # absolute difference for size scaling
  )

############################################################
# Create Circle Heatmap
############################################################

p_tukey <- ggplot(tukey_plot, aes(
  x = as.factor(minPts),
  y = comparison
)) +
  geom_point(
    aes(size = diff_abs, fill = `p adj`),
    shape = 21, color = "black", alpha = 0.9
  ) +
  geom_text(
    aes(label = sig_label),
    color = "white", size = 3.5, fontface = "bold"
  ) +
  facet_grid(subregion ~ Image, scales = "free_y", space = "free_y") +
  
  scale_fill_viridis(
    option = "plasma",
    name = "Adjusted p-value",
    direction = -1,
    trans = "log10",
    labels = scales::scientific
  ) +
  scale_size_continuous(
    name = "|Difference|",
    range = c(3, 12)
  ) +
  
  labs(
    title = "Tukey Post-hoc Results: minPts × Comparison",
    subtitle = "Circle size = absolute mean difference | fill color = adjusted p-value",
    x = "minPts",
    y = "Comparison"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold", size = 11),
    strip.text.y = element_text(angle = 0),
    legend.position = "right",
    panel.spacing = unit(0.6, "lines"),
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5)
  )

print(p_tukey)

############################################################
# Save Plot
############################################################
ggsave(
  paste0(output_dir, "Tukey_circle_heatmap_by_subregion_Image.png"),
  p_tukey, width = 14, height = 10, dpi = 300
)

cat("\nTukey circle heatmap saved successfully!\n")
cat("- Tukey_circle_heatmap_by_subregion_Image.png\n")

