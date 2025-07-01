import pandas as pd
import xgboost as xgb
import shap
import os
import joblib
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
import numpy as np
import re

# Create a directory to save the SHAP plots
if not os.path.exists('shap_plots'):
    os.makedirs('shap_plots')

for i in range(1, 5):
    print(f"--- Processing and Generating SHAP Plots for Class {i} ---")
    model_path = f'trained_models/xgb_model_class_{i}.joblib'
    data_path = f'data/class_{i}_modeling_data.csv'
    try:
        model = joblib.load(model_path)
        df = pd.read_csv(data_path)
    except FileNotFoundError as e:
        print(f"Error: Could not find a necessary file. {e}")
        continue

    X = df.drop('udpyilal', axis=1)
    y = df['udpyilal']
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42, stratify=y
    )

    explainer = shap.TreeExplainer(model)
    print("Calculating SHAP values for the test set...")
    shap_values = explainer.shap_values(X_test)

    # --- Bar Plot ---
    print("Generating SHAP bar plot...")
    plt.figure(figsize=(12, 8))
    shap.summary_plot(shap_values, X_test, plot_type="bar", show=False, plot_size=(12, 8))
    plt.title(f'Feature Importance - Class {i}', fontsize=20)
    plt.xlabel('mean(|SHAP value|)', fontsize=16)
    plt.ylabel('Features', fontsize=16)
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.tight_layout()
    bar_plot_path = f'shap_plots/class_{i}_bar_plot.png'
    plt.savefig(bar_plot_path, bbox_inches='tight', dpi=400)
    plt.close()
    print(f"Saved bar plot to: {bar_plot_path}")

    # --- Summary (Beeswarm) Plot ---
    print("Generating SHAP summary (beeswarm) plot...")
    plt.figure(figsize=(12, 8))
    shap.summary_plot(shap_values, X_test, show=False, plot_size=(12, 8))
    plt.title(f'SHAP Summary Plot - Class {i}', fontsize=20)
    plt.xlabel('SHAP value', fontsize=16)
    plt.ylabel('Features', fontsize=16)
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.tight_layout()
    summary_plot_path = f'shap_plots/class_{i}_summary_plot.png'
    plt.savefig(summary_plot_path, bbox_inches='tight', dpi=400)
    plt.close()
    print(f"Saved summary plot to: {summary_plot_path}\n")

    # --- Aggregated Bar Plot ---
    print("Aggregating SHAP values for original categorical features...")
    # For multiclass, use shap_values[1] (positive class) or sum/mean across classes
    shap_array = shap_values[1] if isinstance(shap_values, list) else shap_values
    shap_df = pd.DataFrame(shap_array, columns=X_test.columns)
    base_features = [re.split(r'\\.', f)[0] for f in X_test.columns]
    abs_shap_df = shap_df.abs()
    aggregated_shap_values = abs_shap_df.groupby(base_features, axis=1).sum()
    mean_aggregated_shap = aggregated_shap_values.mean(axis=0)
    if hasattr(mean_aggregated_shap, 'sort_values'):
        mean_aggregated_shap = mean_aggregated_shap.sort_values(ascending=False)
    print("Generating new aggregated SHAP bar plot...")
    plt.figure(figsize=(12, 8))
    mean_aggregated_shap.plot(kind='barh', color='dodgerblue')
    plt.gca().invert_yaxis()
    plt.title(f'Aggregated Feature Importance - Class {i}', fontsize=20)
    plt.xlabel('mean(|SHAP value|) (average impact on model output magnitude)', fontsize=16)
    plt.ylabel('Features', fontsize=16)
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.grid(axis='x', linestyle='--', alpha=0.6)
    plt.tight_layout()
    agg_plot_path = f'shap_plots/class_{i}_bar_plot_aggregated.png'
    plt.savefig(agg_plot_path, bbox_inches='tight', dpi=400)
    plt.close()
    print(f"Saved aggregated bar plot to: {agg_plot_path}\n")

print("--- Step 3 Complete. All SHAP plots have been generated and saved. ---") 