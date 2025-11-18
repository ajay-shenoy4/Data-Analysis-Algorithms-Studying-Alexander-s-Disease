import os
import glob
import pandas as pd
import numpy as np
from sklearn.neighbors import NearestNeighbors
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker

# Load all CSVs
input_folder = "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Input_Data/Pointmaps_AxD_New/"
csv_files = glob.glob(os.path.join(input_folder, "*.csv"))

# Function to compute radius when each point becomes core
def record_core_radii(file_path, min_pts_value, max_radius=1.0, increment=0.001):
    df = pd.read_csv(file_path)
    print(f"Processing: {os.path.basename(file_path)} with minPts = {min_pts_value}")

    # Parse metadata from filename
    fname_parts = os.path.basename(file_path).replace(".csv", "").split("_")
    df["Image"] = fname_parts[2]
    df["Sex"] = fname_parts[0]
    df["Genotype"] = fname_parts[1]
    df["Brain"] = fname_parts[3]
    df["Laterality"] = fname_parts[5]
    df["Region"] = fname_parts[4]
    df["Subject"] = "_".join(fname_parts[:4] + [df["Laterality"].iloc[0]])
    df["minPts"] = min_pts_value

    # Initialize
    df["recorded_radius"] = np.nan
    coords = df[["x", "y", "z", "i"]].values

    # Loop to find radius at which each point becomes a core point
    for radius in np.arange(0.001, max_radius + increment, increment):
        nbrs = NearestNeighbors(radius=radius).fit(coords)
        _, indices = nbrs.radius_neighbors(coords)
        core_flags = np.array([len(neigh) >= min_pts_value for neigh in indices])
        newly_core = df["recorded_radius"].isna() & core_flags
        df.loc[newly_core, "recorded_radius"] = radius
        if df["recorded_radius"].notna().all():
            break

    total_points = len(df)
    core_points = df["recorded_radius"].notna().sum()
    print(f"Finished: {df['Subject'].iloc[0]} | Core points: {core_points}/{total_points}")

    # Save individual file
    output_name = f"{os.path.splitext(os.path.basename(file_path))[0]}_minPts{min_pts_value}_coreRadii.csv"
    output_dir = "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Radii_Python_newData"
    os.makedirs(output_dir, exist_ok=True)
    output_path = os.path.join(output_dir, output_name)
    df.to_csv(output_path, index=False)
    return df

# Run across all files and minPts values
min_pts_values = [3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] 
all_results = {}

# Loop over files and minPts values
for file_path in csv_files:
    for min_pts_value in min_pts_values:
        result = record_core_radii(file_path, min_pts_value)  # You can modify this to return df if needed
        result_id = f"{os.path.basename(file_path)}_minPts{min_pts_value}"
        all_results[result_id] = result  # You can store summary stats or DataFrames if you modify the function
        
# Combine all results
final_df = pd.concat(all_results.values(), ignore_index=True)
final_df["minPts"] = final_df["minPts"].astype("category")

# Save combined DataFrame
combined_output = "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Radii_Python_newData/newData_combined_coreRadii_all_minPts.csv"

final_df.to_csv(combined_output, index=False)
print(f"✅ Combined CSV saved to: {combined_output}")