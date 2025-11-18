import os
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import numpy as np

# Set file paths
folder_path = "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Radii_Python_newData"
plot_path = "/Users/ajayshenoy/Downloads/AShenoy_data_directory/Research/Radii_Python_newData/images"

# Ensure output directory exists
if not os.path.exists(plot_path):
    os.makedirs(plot_path)

# Get list of CSV files
csv_files = [f for f in os.listdir(folder_path) if f.endswith("coreRadii.csv")]

# Define a function to generate 3D scatter plots with traces for each subregion
def plot_3D_data(file_path):
    df = pd.read_csv(file_path)

    if 'subregion' not in df.columns or 'recorded_radius' not in df.columns:
        print(f"Skipping file (missing required columns): {file_path}")
        return

    # Define discrete color palette (Paul Tol’s)
    discrete_rainbow = [
        "#882E72", "#B178A6", "#D6C1DE",  # purples
        "#1965B0", "#437DBF", "#7092BE",  # blues
        "#92C5DE", "#CAE0E5",             # light blue
        "#F7F056", "#F7CB45",             # yellows
        "#F4A736", "#E8601C", "#DC050C",  # oranges/reds
        "#A5170E"                         # deep red
    ]
    n_bins = len(discrete_rainbow)

    # Bin recorded_radius into discrete bins
    df['radius_bin'] = pd.cut(
        df['recorded_radius'],
        bins=n_bins,
        labels=False,
        include_lowest=True
    )
    
    # Print bin edges
    bin_edges = pd.cut(df['recorded_radius'], bins=n_bins, include_lowest=True).unique().categories
    #print(f"  Bin edges: {bin_edges}")

    # Print range of binned values
    #print(f"  Binned radius_bin range: min = {df['radius_bin'].min()}, max = {df['radius_bin'].max()}")
    
    tick_vals = list(range(n_bins))
    tick_text = [f"{interval.left:.2f}–{interval.right:.2f}" for interval in bin_edges]

    fig = make_subplots()

    for i, subregion in enumerate(df['subregion'].unique()):
        subregion_data = df[df['subregion'] == subregion]

        fig.add_trace(
            go.Scatter3d(
                x=subregion_data['x'],
                y=subregion_data['y'],
                z=subregion_data['z'],
                mode='markers',
                name=subregion,
                marker=dict(
                    size=3,
                    color=subregion_data['radius_bin'],
                    colorscale=[[j / (n_bins - 1), color] for j, color in enumerate(discrete_rainbow)],
                    cmin=0,
                    cmax=n_bins - 1,
                    showscale = True,  # Only show scale on the first trace
                    colorbar=dict(
                        title="Recorded Radius",
                        tickmode="array",
                        tickvals=tick_vals,
                        ticktext=tick_text
                    )
                )
            )
        )

    fig.update_layout(
        title=f"3D Plot for {os.path.basename(file_path)}",
        scene=dict(
            xaxis_title="X",
            yaxis_title="Y",
            zaxis_title="Z"
        ),
        legend=dict(
            title="Subregion",
            x=-0.3,
            y=0.5,
            xanchor="left",
            yanchor="middle"
        )
    )

    output_file = os.path.join(plot_path, f"{os.path.basename(file_path)}.html")
    fig.write_html(output_file)

# Generate plots
for csv_file in csv_files:
    plot_3D_data(os.path.join(folder_path, csv_file))

print("3D plots have been generated and saved.")
