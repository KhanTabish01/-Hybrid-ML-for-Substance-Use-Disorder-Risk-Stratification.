# Latent Class Analysis of Substance Use Disorder Risks

## Project Overview
This project presents a hybrid machine learning approach to identify and stratify unique risk factors for Substance Use Disorder (SUD). The study leverages Latent Class Analysis (LCA) on the National Survey on Drug Use and Health (NSDUH) 2015-2019 dataset to uncover distinct subgroups (latent classes) of individuals based on shared characteristics and risk factors.

Following the identification of these latent classes, a series of predictive models, including XGBoost and Random Forest, are trained for each class. The goal of these models is to determine the most significant predictors of SUD within each specific subgroup. By doing so, this research moves beyond a one-size-fits-all approach to risk assessment and provides a more nuanced understanding of the heterogeneous nature of SUD.

The project is structured as follows:
1.  **Data Preparation:** Cleaning and preprocessing of the NSDUH dataset.
2.  **Latent Class Analysis:** Identification of latent classes using `poLCA` in R.
3.  **Predictive Modeling:** Training XGBoost and Random Forest models for each class.
4.  **Model Interpretation:** Using SHAP (SHapley Additive exPlanations) to interpret the machine learning models and identify key risk factors for each class.

The findings from this research can help inform targeted prevention and intervention strategies tailored to the specific needs of different population subgroups.

## Project Structure
- `data/`: Contains the raw and processed data.
- `scripts/`: Contains R and Python scripts for data preparation, analysis, and modeling.
- `notebooks/`: Contains Jupyter notebooks for modeling and analysis.
- `plots/`: Contains generated plots and visualizations.
- `shap_plots/`: Contains SHAP plots for model interpretation.
- `trained_models/`: Contains saved trained models.
- `results/`: Contains result files like tables.
- `docs/`: Contains project documentation.


