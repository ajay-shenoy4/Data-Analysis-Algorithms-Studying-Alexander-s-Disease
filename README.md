Steps:
1) Run TDA on our data, TDA.R
2) Run DBSCAN, dbscan_radii_newData_minPts.py on our data.
3) Turn our dbscan data into bins, NEW_dbscan_to_bins.R
4) Merge our TDA and DBSCAN data together, New_merge_tda_dbscan.R.
5) Find the correlations between both datasets, correlations.R
6) Graph our dbscan results, New_Atlas_Graphing.py
7) Visualize the relations between our TDA and DBSCAN data through tests, NewTests.r.







What is the aim of the research?
Abstract

Biological datasets, mainly those of neural tissue architecture or protein structures, are high-dimensional, noisy, and complex. Most computational methods and algorithms capture global trends or local details of the data, but not both. One such algorithm that only captures global trends is Topological Data Analysis (TDA). TDA extracts global features from point cloud data to reveal connected components such as loops, holes, lines, and voids through persistent homology. Additionally, TDA measures how these topological features appear and disappear as the radius increases, while being resistant to noise in our dataset. Despite this, TDA does not provide information about individual points or clusters in our dataset. In contrast, Density-Based Spatial Clustering of Applications with Noise (DBSCAN) provides more localized and individual information. DBSCAN incrementally expands a radius around each voxel (3-D point) in the dataset to form clusters according to set density thresholds. In doing so, DBSCAN records the distance at which each point forms a cluster, but does not offer insights about the overall dataset like TDA does.
An example of where these data modeling strategies could be applied is to Alexander’s Disease (AxD). AxD is a rare, progressive, and fatal neurodegenerative disease caused by mutations in the Glial Fibrillary Acidic Protein (GFAP) gene. These mutations cause Rosenthal fibers to accumulate in astrocytes, impairing the cell’s ability to regulate white matter, leading to demyelination, astrogliosis, and lesion formation. Using a neonatal-onset rat model of AxD, we can model changes in the brain’s white matter through NODDI imaging, measuring neurite density index (NDI) and orientation dispersion index (ODI) of individual voxels. By applying both TDA and DBSCAN on the NODDI imaging dataset, we can map features such as NDI, ODI, and clustering distance to topological features in the point cloud. By doing so, we are able to bridge the deficiencies of both DBSCAN and TDA, and combine global, structural features with precise, local information. With this approach, we gain new insights into biological systems such as protein folding and neural tissue architecture, and can identify new biomarkers and less invasive therapeutic strategies for diseases like AxD. 
