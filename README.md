# Data Analysis Algorithms: Studying Alexander's Disease

[![Language: R](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)
[![Language: Python](https://img.shields.io/badge/Language-Python-green.svg)](https://www.python.org/)

## Research Aim & Abstract
Biological datasets, particularly those of neural tissue architecture, are high-dimensional, noisy, and complex. This research aims to bridge the gap between **Global Trends** and **Local Details** by combining two distinct computational methods:

1.  **Topological Data Analysis (TDA):** Captures global features (loops, holes, voids) through persistent homology, resistant to noise but lacking individual point data.
2.  **Density-Based Spatial Clustering of Applications with Noise (DBSCAN):** Captures localized individual information by forming clusters according to density thresholds.


**Application:** We apply this dual-approach to **Alexander’s Disease (AxD)**, a fatal neurodegenerative disease involving GFAP mutations and Rosenthal fiber accumulation. Using a neonatal-onset rat model, we analyze NODDI imaging (NDI and ODI voxels). By merging TDA and DBSCAN, we map precise local clustering to global structural features, identifying potential new biomarkers and therapeutic insights for AxD.

---

## Project Pipeline & Scripts

The analysis follows a structured multi-step workflow from raw point clouds to statistical validation.

### Step 1: Feature Extraction
* **`TDA.R`**: Processes folders of TDA persistence CSV files. It bins birth/death events into specific time intervals and aggregates them by subject and homology level ($H_0$, $H_1$, $H_2$) to output a combined topological table. Receives input files from AxD_Sigma_TDA_New
* **`dbscan_radii_newData_minPts.py`**: A Python script that computes the "core radius" for every point in 3D point-cloud CSVs. It iterates through `minPts` values (3 to 15), using `scikit-learn`'s `NearestNeighbors` to find the exact radius where a point becomes a core point. Receives input files from Pointmaps_AxD_New. 

### Step 2: Data Transformation
* **`NEW_dbscan_to_bins.R`**: Organizes the combined core-radii data into bins (size 0.001). It groups data by Subject, Bin, and subregion to calculate point density and average coordinates $(x, y, z)$ and intensity $(i)$.
* **`New_merge_tda_dbscan.R`**: Standardizes and merges the TDA and DBSCAN datasets. It creates a complete grid of all variables, fills missing entries with zeros, and resolves duplicated columns to create a "master" dataset.

### Step 3: Correlation & Statistical Analysis
* **`correlations.R`**: Calculates correlations between DBSCAN cluster metrics and TDA homology measures. It produces a CSV of results and a faceted boxplot showing how these correlations fluctuate across different `minPts` values.
* ** `BonfPlots.R`**: Performs systematic ANOVA analysis across experimental conditions (minPts, subregion, Image). It handles post-hoc testing with Bonferroni corrections to generate publication-ready significance plots.

### Step 4: Visualization
* **`New_Atlas_Graphing.py`**: Generates interactive 3D scatter plots of core points. Points are binned by their recorded radius and colored by subregion, saved as HTML files for dynamic spatial exploration.
* **`PointmapSelector.ipynb`**: A batch processing script that validates the biological "purity" of clusters. It maps 2D persistence bins back to 3D coordinates to determine if mathematical clusters correspond to specific anatomical subregions. It uses scikit-learn for neighbor lookups and processes minPts pairs in batches to optimize memory usage.
* **`ClusterStatsVisual.R`**: A R script that visualizes clustering data into heatmaps. Maps the persistence bins of minPts combinations and visualizes how often clusters fall into each subregion. 

---

## Getting Started

### Prerequisites
* **Python 3.x**: `pandas`, `numpy`, `scikit-learn`, `plotly`
* **R**: `tidyverse`, `ggplot2`, `broom`

### Workflow Order
1.  Run `TDA.R` and `dbscan_radii_newData_minPts.py` on your raw data.
2.  Run `NEW_dbscan_to_bins.R` to process the radii.
3.  Run `New_merge_tda_dbscan.R` to create the final analysis table.
4.  Execute `correlations.R` and `NewTests.r` for results.
5.  Use `New_Atlas_Graphing.py` for 3D visualization.
6.  Use `PointmapSelector.ipynb` for 2D and 3D visualization.
7.  Use `ClusterStatsVisual.R` for 2D visualization. 

## Abstract

Biological datasets, mainly those of neural tissue architecture or protein structures, are high-dimensional, noisy, and complex. Most computational methods and algorithms capture global trends or local details of the data, but not both. One such algorithm that only captures global trends is Topological Data Analysis (TDA). TDA extracts global features from point cloud data to reveal connected components such as loops, holes, lines, and voids through persistent homology. Additionally, TDA measures how these topological features appear and disappear as the radius increases, while being resistant to noise in our dataset. Despite this, TDA does not provide information about individual points or clusters in our dataset. In contrast, Density-Based Spatial Clustering of Applications with Noise (DBSCAN) provides more localized and individual information. DBSCAN incrementally expands a radius around each voxel (3-D point) in the dataset to form clusters according to set density thresholds. In doing so, DBSCAN records the distance at which each point forms a cluster, but does not offer insights about the overall dataset like TDA does.

An example of where these data modeling strategies could be applied is to Alexander’s Disease (AxD). AxD is a rare, progressive, and fatal neurodegenerative disease caused by mutations in the Glial Fibrillary Acidic Protein (GFAP) gene. These mutations cause Rosenthal fibers to accumulate in astrocytes, impairing the cell’s ability to regulate white matter, leading to demyelination, astrogliosis, and lesion formation. Using a neonatal-onset rat model of AxD, we can model changes in the brain’s white matter through NODDI imaging, measuring neurite density index (NDI) and orientation dispersion index (ODI) of individual voxels. By applying both TDA and DBSCAN on the NODDI imaging dataset, we can map features such as NDI, ODI, and clustering distance to topological features in the point cloud. By doing so, we are able to bridge the deficiencies of both DBSCAN and TDA, and combine global, structural features with precise, local information. With this approach, we gain new insights into biological systems such as protein folding and neural tissue architecture, and can identify new biomarkers and less invasive therapeutic strategies for diseases like AxD. 

---
