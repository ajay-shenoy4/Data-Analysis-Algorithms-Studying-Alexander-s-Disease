############################################################
# ANOVA Analysis with Bonferroni Correction
############################################################

library(dplyr)
library(tidyr)
library(ggplot2)
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

brains <- unique(combined_dbscan_data$Brain)
minPts_values <- unique(combined_dbscan_data$minPts)
output_dir <- "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Plots/"

############################################################
# 2. Run ANOVA for each minPts combination
############################################################

anova_results_list <- list()

for (m in minPts_values) {
  cat("\n========================================\n")
  cat("minPts:", m, "\n")
  cat("========================================\n")
  
  data_sub <- combined_dbscan_data %>% filter(minPts == m)
  
  if (nrow(data_sub) < 10) {
    cat("Skipping: insufficient data (n =", nrow(data_sub), ")\n")
    next
  }
  
  n_sex <- length(unique(data_sub$Sex))
  n_genotype <- length(unique(data_sub$Genotype))
  n_image <- length(unique(data_sub$Image))
  n_subregion <- length(unique(data_sub$subregion))
  
  if (n_sex < 2 || n_genotype < 2 || n_image < 2 || n_subregion < 2) {
    cat("Skipping: insufficient factor levels\n")
    next
  }
  
  tryCatch({
    model <- aov(recorded_radius ~ Sex + Genotype + Image + subregion + 
                   Sex:Genotype + 
                   Genotype:subregion + 
                   Sex:subregion + 
                   Sex:Genotype:subregion + 
                   Sex:Genotype:subregion:Image, 
                 data = data_sub)
    
    anova_summary <- summary(model)
    print(anova_summary)
    
    anova_table <- as.data.frame(anova_summary[[1]])
    rownames(anova_table) <- trimws(rownames(anova_table))
    
    results_df <- data.frame(
      minPts = m,
      n_observations = nrow(data_sub),
      Sex_pval = ifelse("Sex" %in% rownames(anova_table), 
                        anova_table["Sex", "Pr(>F)"], NA),
      Genotype_pval = ifelse("Genotype" %in% rownames(anova_table), 
                             anova_table["Genotype", "Pr(>F)"], NA),
      Image_pval = ifelse("Image" %in% rownames(anova_table), 
                          anova_table["Image", "Pr(>F)"], NA),
      subregion_pval = ifelse("subregion" %in% rownames(anova_table), 
                              anova_table["subregion", "Pr(>F)"], NA),
      Sex_Genotype_pval = ifelse("Sex:Genotype" %in% rownames(anova_table), 
                                 anova_table["Sex:Genotype", "Pr(>F)"], NA),
      Genotype_subregion_pval = ifelse("Genotype:subregion" %in% rownames(anova_table), 
                                       anova_table["Genotype:subregion", "Pr(>F)"], NA),
      Sex_subregion_pval = ifelse("Sex:subregion" %in% rownames(anova_table), 
                                  anova_table["Sex:subregion", "Pr(>F)"], NA),
      Sex_Genotype_subregion_pval = ifelse("Sex:Genotype:subregion" %in% rownames(anova_table), 
                                           anova_table["Sex:Genotype:subregion", "Pr(>F)"], NA),
      Sex_Genotype_subregion_Image_pval = ifelse("Sex:Genotype:Image:subregion" %in% rownames(anova_table), 
                                                 anova_table["Sex:Genotype:Image:subregion", "Pr(>F)"], 
                                                 ifelse("Sex:Genotype:subregion:Image" %in% rownames(anova_table),
                                                        anova_table["Sex:Genotype:subregion:Image", "Pr(>F)"], NA))
    )
    
    anova_results_list[[paste(m, sep = "_")]] <- results_df
    
  }, error = function(e) {
    cat("Error fitting model:", e$message, "\n")
  })
}

############################################################
# 3. Apply Bonferroni Correction
############################################################

anova_results_combined <- bind_rows(anova_results_list)

# Calculate total number of tests across ALL p-value columns
n_tests <- sum(!is.na(anova_results_combined[, grep("_pval$", names(anova_results_combined))]))
n_tests

cat("\n========================================\n")
cat("BONFERRONI CORRECTION INFO\n")
cat("========================================\n")
cat("Total number of tests:", n_tests, "\n")
cat("Bonferroni-adjusted alpha:", 0.05 / n_tests, "\n")
cat("========================================\n\n")

# Apply Bonferroni correction
anova_results_combined <- anova_results_combined %>%
  mutate(
    Sex_pval_bonf = pmin(Sex_pval * n_tests, 1),
    Genotype_pval_bonf = pmin(Genotype_pval * n_tests, 1),
    Image_pval_bonf = pmin(Image_pval * n_tests, 1),
    subregion_pval_bonf = pmin(subregion_pval * n_tests, 1),
    Sex_Genotype_pval_bonf = pmin(Sex_Genotype_pval * n_tests, 1),
    Genotype_subregion_pval_bonf = pmin(Genotype_subregion_pval * n_tests, 1),
    Sex_subregion_pval_bonf = pmin(Sex_subregion_pval * n_tests, 1),
    Sex_Genotype_subregion_pval_bonf = pmin(Sex_Genotype_subregion_pval * n_tests, 1),
    Sex_Genotype_subregion_Image_pval_bonf = pmin(Sex_Genotype_subregion_Image_pval * n_tests, 1)
  )

# Add significance indicators BASED ON BONFERRONI-CORRECTED P-VALUES
anova_results_combined <- anova_results_combined %>%
  mutate(
    Sex_sig = case_when(
      Sex_pval_bonf < 0.001 ~ "***",
      Sex_pval_bonf < 0.01  ~ "**",
      Sex_pval_bonf < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Genotype_sig = case_when(
      Genotype_pval_bonf < 0.001 ~ "***",
      Genotype_pval_bonf < 0.01  ~ "**",
      Genotype_pval_bonf < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Image_sig = case_when(
      Image_pval_bonf < 0.001 ~ "***",
      Image_pval_bonf < 0.01  ~ "**",
      Image_pval_bonf < 0.05  ~ "*",
      TRUE ~ ""
    ),
    subregion_sig = case_when(
      subregion_pval_bonf < 0.001 ~ "***",
      subregion_pval_bonf < 0.01  ~ "**",
      subregion_pval_bonf < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Sex_Genotype_sig = case_when(
      Sex_Genotype_pval_bonf < 0.001 ~ "***",
      Sex_Genotype_pval_bonf < 0.01  ~ "**",
      Sex_Genotype_pval_bonf < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Genotype_subregion_sig = case_when(
      Genotype_subregion_pval_bonf < 0.001 ~ "***",
      Genotype_subregion_pval_bonf < 0.01  ~ "**",
      Genotype_subregion_pval_bonf < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Sex_subregion_sig = case_when(
      Sex_subregion_pval_bonf < 0.001 ~ "***",
      Sex_subregion_pval_bonf < 0.01  ~ "**",
      Sex_subregion_pval_bonf < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Sex_Genotype_subregion_sig = case_when(
      Sex_Genotype_subregion_pval_bonf < 0.001 ~ "***",
      Sex_Genotype_subregion_pval_bonf < 0.01  ~ "**",
      Sex_Genotype_subregion_pval_bonf < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Sex_Genotype_subregion_Image_sig = case_when(
      Sex_Genotype_subregion_Image_pval_bonf < 0.001 ~ "***",
      Sex_Genotype_subregion_Image_pval_bonf < 0.01  ~ "**",
      Sex_Genotype_subregion_Image_pval_bonf < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

print(anova_results_combined)
write.csv(anova_results_combined, 
          paste0(output_dir, "ANOVA_results_by_minPts_bonferroni.csv"), 
          row.names = FALSE)

############################################################
# 4. Summary with Bonferroni correction
############################################################

cat("\nSUMMARY OF SIGNIFICANT EFFECTS (Bonferroni-corrected p < 0.05):\n")
cat("==========================================\n\n")

summary_sig <- anova_results_combined %>%
  summarise(
    n_total = n(),
    Sex_sig_count = sum(Sex_pval_bonf < 0.05, na.rm = TRUE),
    Genotype_sig_count = sum(Genotype_pval_bonf < 0.05, na.rm = TRUE),
    Image_sig_count = sum(Image_pval_bonf < 0.05, na.rm = TRUE),
    subregion_sig_count = sum(subregion_pval_bonf < 0.05, na.rm = TRUE),
    Sex_Genotype_sig_count = sum(Sex_Genotype_pval_bonf < 0.05, na.rm = TRUE),
    Genotype_subregion_sig_count = sum(Genotype_subregion_pval_bonf < 0.05, na.rm = TRUE),
    Sex_subregion_sig_count = sum(Sex_subregion_pval_bonf < 0.05, na.rm = TRUE),
    Sex_Genotype_subregion_sig_count = sum(Sex_Genotype_subregion_pval_bonf < 0.05, na.rm = TRUE),
    Sex_Genotype_subregion_Image_sig_count = sum(Sex_Genotype_subregion_Image_pval_bonf < 0.05, na.rm = TRUE)
  )

print(summary_sig)
print(t(summary_sig))

############################################################
# 5. Heatmap with Bonferroni-corrected p-values (raw scale)
############################################################

heatmap_data <- anova_results_combined %>%
  select(minPts, ends_with("_pval_bonf")) %>%
  pivot_longer(
    cols = ends_with("_pval_bonf"), 
    names_to = "Effect", 
    values_to = "pval_bonf"
  ) %>%
  mutate(
    Effect = gsub("_pval_bonf", "", Effect),
    Effect = gsub("_", " × ", Effect),   # make interactions readable
    sig_bonf = case_when(
      pval_bonf < 0.001 ~ "***",
      pval_bonf < 0.01  ~ "**",
      pval_bonf < 0.05  ~ "*",
      TRUE               ~ ""
    )
  )

p1 <- ggplot(heatmap_data, aes(x = Effect, y = as.factor(minPts), fill = pval_bonf)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sig_bonf), color = "white", size = 3) +
  scale_fill_viridis(
    option = "plasma",
    name = "Bonferroni p-value",
    direction = -1,
    limits = c(0, 0.05),        # optional: cap for better color contrast
    oob = scales::squish
  ) +
  labs(
    title = "ANOVA Results: Bonferroni-Corrected Significance",
    subtitle = paste0("Corrected for ", n_tests, " multiple comparisons"),
    x = "Effect",
    y = "minPts"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold", size = 11)
  )

print(p1)
ggsave(
  paste0(output_dir, "ANOVA_heatmap_bonferroni.png"), 
  p1, width = 14, height = 8, dpi = 300, bg = "white"
)

############################################################
# 6. Two-way ANOVA with Bonferroni correction
############################################################

subregions <- unique(combined_dbscan_data$subregion)
unique_images <- unique(combined_dbscan_data$Image)

emmeans_list <- list()
contrast_list <- list()
tukey_list <- list()

for (img_type in unique_images) {
  for (s in subregions) {
    for (m in unique(combined_dbscan_data$minPts)) {
      cat("\nImage:", img_type, "| Subregion:", s, "| minPts:", m, "\n")
      
      data_sub <- combined_dbscan_data %>%
        filter(Image == img_type, subregion == s, minPts == m)
      
      if (nrow(data_sub) < 5 ||
          length(unique(data_sub$Sex)) < 2 ||
          length(unique(data_sub$Genotype)) < 2) {
        cat("Skipping due to insufficient data.\n")
        next
      }
      
      model <- aov(recorded_radius ~ Sex * Genotype, data = data_sub)
      print(summary(model))
      
      em_df <- as.data.frame(emmeans(model, ~ Sex * Genotype)) %>%
        mutate(Image = img_type, subregion = s, minPts = m)
      emmeans_list[[paste(img_type, s, m, sep = "_")]] <- em_df
      
      cont_df <- tryCatch({
        as.data.frame(emmeans(model, pairwise ~ Genotype | Sex)$contrasts) %>%
          mutate(Image = img_type, subregion = s, minPts = m)
      }, error = function(e) NULL)
      
      contrast_list[[paste(img_type, s, m, sep = "_")]] <- cont_df
      
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

emmeans_results <- bind_rows(emmeans_list)
contrast_results <- bind_rows(contrast_list)
tukey_results <- bind_rows(tukey_list)

############################################################
# 7. Apply Bonferroni to contrast and Tukey results
############################################################

# For contrasts
n_contrasts <- sum(!is.na(contrast_results$p.value))
n_contrasts

contrast_results <- contrast_results %>%
  mutate(
    p.value_bonf = pmin(p.value * n_contrasts, 1),
    sig_bonf = case_when(
      p.value_bonf < 0.001 ~ "***",
      p.value_bonf < 0.01  ~ "**",
      p.value_bonf < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# For Tukey
n_tukey <- sum(!is.na(tukey_results$`p adj`))
n_tukey

tukey_results <- tukey_results %>%
  mutate(
    p.adj_bonf = pmin(`p adj` * n_tukey, 1),
    sig_bonf = case_when(
      p.adj_bonf < 0.001 ~ "***",
      p.adj_bonf < 0.01  ~ "**",
      p.adj_bonf < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

cat("\n========================================\n")
cat("POST-HOC BONFERRONI CORRECTION INFO\n")
cat("========================================\n")
cat("Contrasts - Total tests:", n_contrasts, "\n")
cat("Tukey - Total tests:", n_tukey, "\n")
cat("========================================\n\n")

# Save results
write.csv(emmeans_results, 
          paste0(output_dir, "emmeans_results_bonferroni.csv"), 
          row.names = FALSE)
write.csv(contrast_results, 
          paste0(output_dir, "contrast_results_bonferroni.csv"), 
          row.names = FALSE)
write.csv(tukey_results, 
          paste0(output_dir, "tukey_results_bonferroni.csv"), 
          row.names = FALSE)

############################################################
# 8. Updated EMMeans Plot with Bonferroni correction
# Fill = emmean, Circle size = significance level (stars)
############################################################

# Merge with Bonferroni-corrected p-values
emmeans_plot <- emmeans_results %>%
  left_join(
    contrast_results %>%
      select(subregion, Sex, minPts, Image, p.value_bonf, sig_bonf),
    by = c("subregion", "Sex", "minPts", "Image")
  ) %>%
  mutate(
    Sex_Genotype = paste(Sex, Genotype, sep = "_"),
    sig_category = sig_bonf,
    size_value = case_when(
      sig_bonf == "***" ~ 12,
      sig_bonf == "**"  ~ 9,
      sig_bonf == "*"   ~ 6,
      TRUE ~ 3
    )
  )

# Bubble plot with Bonferroni correction
heatmap_data_sep <- emmeans_plot %>%
  mutate(
    y_label_clean = paste0(Sex, "-", Genotype),
    facet_label = paste0(subregion, " (", Image, ")")
  )

p2 <- ggplot(
  heatmap_data_sep,
  aes(
    x = as.factor(minPts),
    y = y_label_clean,
    fill = emmean,         # color by mean estimate
    size = size_value      # size by significance
  )
) +
  geom_point(shape = 21, color = "black", stroke = 0.6, alpha = 0.9) +
  geom_text(aes(label = sig_category), color = "white", size = 4, fontface = "bold", vjust = 0.5) +
  facet_grid(subregion ~ Image, scales = "free_y", space = "free_y") +
  scale_fill_viridis(option = "plasma", name = "Estimated marginal mean") +
  scale_size_identity(
    guide = "legend",
    breaks = c(3, 6, 9, 12),
    labels = c("ns", "*", "**", "***"),
    name = "Significance"
  ) +
  labs(
    title = "EMMeans: Bonferroni-Corrected Significance",
    subtitle = paste0("Circle size reflects Bonferroni-corrected significance level"),
    x = "minPts", y = "Sex–Genotype"
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
ggsave(paste0(output_dir, "Emmeans_bubbleplot_bonferroni_starsize.png"),
       p2, width = 14, height = 12, dpi = 300, bg = "white")

cat("\n========================================\n")
cat("ANALYSIS COMPLETE WITH BONFERRONI CORRECTION (Stars -> Size)\n")
cat("Color = emmean, Dot size = significance level (*, **, ***)\n")
cat("========================================\n")

############################################################
# Tukey plot with Bonferroni correction
# Color = signed difference
# Size = significance threshold (***, **, *)
############################################################

tukey_plot <- tukey_results %>%
  filter(!comparison %in% c("Female:WT-Male:AxD", "Male:WT-Female:AxD")) %>%
  mutate(
    comparison = factor(comparison, levels = unique(comparison)),
    # use signed difference, not absolute
    sig_category = case_when(
      p.adj_bonf < 0.001 ~ "***",
      p.adj_bonf < 0.01  ~ "**",
      p.adj_bonf < 0.05  ~ "*",
      TRUE ~ ""
    ),
    size_value = case_when(          # numeric mapping for point size
      p.adj_bonf < 0.001 ~ 12,
      p.adj_bonf < 0.01  ~ 9,
      p.adj_bonf < 0.05  ~ 6,
      TRUE ~ 3
    )
  )

p_tukey <- ggplot(
  tukey_plot,
  aes(
    x = as.factor(minPts),
    y = comparison,
    fill = diff,          # color by signed difference
    size = size_value     # use pre-mapped numeric size
  )
) +
  geom_point(shape = 21, color = "black", alpha = 0.9) +
  geom_text(
    aes(label = sig_category),
    color = "white", size = 3.5, fontface = "bold"
  ) +
  facet_grid(subregion ~ Image, scales = "free_y", space = "free_y") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    name = "Difference"
  ) +
  scale_size_identity(guide = "legend", 
                      breaks = c(3, 6, 9, 12),
                      labels = c("ns", "*", "**", "***"),
                      name = "Significance") +
  labs(
    title = "Tukey Post-hoc: Bonferroni-Corrected Significance",
    subtitle = paste0("Circle size reflects significance level"),
    x = "minPts", y = "Comparison"
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
ggsave(paste0(output_dir, "Tukey_heatmap_bonferroni_signed_diff_sigsize.png"),
       p_tukey, width = 14, height = 10, dpi = 300, bg = "white")
