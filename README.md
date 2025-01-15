# Latent Class Analysis of Substance Use Disorder Risks

## Project Overview
This project uses Latent Class Analysis (LCA) to identify distinct subgroups of individuals based on their risk factors for Substance Use Disorder (SUD). The analysis is based on the National Survey on Drug Use and Health (NSDUH) 2015-2019 dataset. Following the LCA, predictive models (XGBoost, Random Forest) are built for each class to identify the most significant predictors of SUD within each subgroup.

## Project Structure
- `data/`: Contains the raw and processed data.
- `scripts/`: Contains R and Python scripts for data preparation, analysis, and modeling.
- `notebooks/`: Contains Jupyter notebooks for modeling and analysis.
- `plots/`: Contains generated plots and visualizations.
- `shap_plots/`: Contains SHAP plots for model interpretation.
- `trained_models/`: Contains saved trained models.
- `results/`: Contains result files like tables.
- `docs/`: Contains project documentation.
