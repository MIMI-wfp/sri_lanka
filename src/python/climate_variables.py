import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
import os

# Load data
climate_adm2 = pd.read_csv("data/raw/features/climate_features_lka_19.csv")
ml_targets = pd.read_csv("data/processed/sl_ml_targets_2025-10-06.csv")
hh_info = pd.read_csv("data/processed/hh_info.csv")
adm2_average = pd.read_csv("data/processed/adm2_average.csv")

# Merge and summarize climate data
climate_adm2 = (
    climate_adm2
    .merge(ml_targets[['hhid', 'overall_mar']], left_on='household_id', right_on='hhid', how='left')
    .merge(hh_info[['hhid', 'adm2']].assign(hhid=lambda df: pd.to_numeric(df['hhid'], errors='coerce')),
           left_on='household_id', right_on='hhid', how='left')
    .groupby('adm2', as_index=False)
    .agg({
        'r3q': 'mean',
        'rfh_avg': 'mean',
        'vim_avg': 'mean',
        'overall_mar': 'mean'
    })
    .rename(columns={'overall_mar': 'mar'})
)

# Merge with nutrition indicators
adm2_inad = (
    adm2_average[['adm2', 'energy_kcal_q50'] + [col for col in adm2_average.columns if col.endswith('_inad')]]
    .merge(climate_adm2, on='adm2', how='left')
)

# Add province names
province_map = {
    1: "Western", 2: "Central", 3: "Southern", 4: "Northern", 5: "Eastern",
    6: "North Western", 7: "North Central", 8: "Uva", 9: "Sabaragamuwa"
}
adm2_inad['province'] = (adm2_inad['adm2'] // 10).round().map(province_map)

# Define variable groups
mn_col_names = list(adm2_inad.columns[2:8]) + [adm2_inad.columns[11]]
clim_col_names = list(adm2_inad.columns[8:11])

# Create output directory
os.makedirs("outputs/plots/climate", exist_ok=True)

# Generate plots
for i in mn_col_names:
    for j in clim_col_names:
        print(f"Plotting {i} vs {j}")
        plt.figure(figsize=(8, 6))
        sns.scatterplot(data=adm2_inad, x=j, y=i, hue='province', s=50)
        sns.regplot(data=adm2_inad, x=j, y=i, scatter=False, color='black')

        
        X = sm.add_constant(adm2_inad[j])
        y = adm2_inad[i]

        # Combine X and y into a single DataFrame for cleaning
        data = pd.concat([X, y], axis=1)

        # Remove rows with NaN or inf values
        data_clean = data.replace([np.inf, -np.inf], np.nan).dropna()

        # Separate cleaned X and y
        X_clean = data_clean.iloc[:, :-1]
        y_clean = data_clean.iloc[:, -1]

        # Fit the model
        model = sm.OLS(y_clean, X_clean).fit()

        r_squared = model.rsquared
        intercept, slope = model.params

        pearson_r = model.params[1] / abs(model.params[1]) * r_squared**0.5

        # Annotate with R² and equation
        eq_label = f"$y = {intercept:.2f} + {slope:.2f}x$\n$R^2 = {r_squared:.2f}$\n$Pearson = {pearson_r:.2f}$"
        plt.text(0.05, 0.95, eq_label, transform=plt.gca().transAxes,
                 verticalalignment='top', horizontalalignment='left', fontsize=10)

        plt.title(f"Scatterplot of {i} vs {j}")
        plt.tight_layout()
        plt.savefig(f"outputs/plots/climate/{i}_{j}.png")
        plt.close()
