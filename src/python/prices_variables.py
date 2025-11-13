import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
import os

# Load data
prices_data = pd.read_csv('data/raw/features/prices_2019_15102025.csv')
ml_targets = pd.read_csv("data/processed/sl_ml_targets_2025-10-06.csv")
hh_info = pd.read_csv("data/processed/hh_info.csv")
adm2_average = pd.read_csv("data/processed/adm2_average.csv")

# filter out the forecasted data
prices_data = prices_data[prices_data['Data Type'] != "Forecast"]
prices_data

# get all commodities
prices_data = prices_data.groupby(['Admin 2', 'Commodity']).agg({'Price': "median"}).reset_index()
prices_data = prices_data.pivot(index="Admin 2", columns="Commodity", values="Price")

# Plot heatmap
plt.figure(figsize=(10, 6))
sns.heatmap(prices_data, annot=True,fmt=".2f", cmap="Blues")
plt.title("Prices of Commodities per Admin 2")
plt.xlabel("Commodity")
plt.ylabel("Admin 2")
plt.tight_layout()
plt.show()

#
prices_data.columns = [str(col) for col in prices_data.columns]
prices_data = prices_data.reset_index()


# Define the district-to-HIES code mapping
districts = [
    {"adm2": 81, "Admin 2": "Badulla"},
    {"adm2": 11, "Admin 2": "Colombo"},
    {"adm2": 12, "Admin 2": "Gampaha"},
    {"adm2": 41, "Admin 2": "Jaffna"},
    {"adm2": 13, "Admin 2": "Kalutara"},
    {"adm2": 21, "Admin 2": "Kandy"},
    {"adm2": 92, "Admin 2": "Kegalle"},
    {"adm2": 61, "Admin 2": "Kurunegala"},
    {"adm2": 43, "Admin 2": "Mannar"},
    {"adm2": 22, "Admin 2": "Matale"},
    {"adm2": 82, "Admin 2": "Moneragala"},
    {"adm2": 23, "Admin 2": "Nuwara Eliya"},
    {"adm2": 72, "Admin 2": "Polonnaruwa"},
    {"adm2": 91, "Admin 2": "Ratnapura"},
    {"adm2": 53, "Admin 2": "Trincomalee"},
    {"adm2": 44, "Admin 2": "Vavuniya"}
]

# Convert to DataFrame
df_districts = pd.DataFrame(districts)
prices_data=prices_data.merge(df_districts, on='Admin 2')

adm2_inad = (
    adm2_average[['adm2', 'energy_kcal_q50'] + [col for col in adm2_average.columns if col.endswith('_inad')]]
    .merge(prices_data, on='adm2', how='left')
)


# Add province names
province_map = {
    1: "Western", 2: "Central", 3: "Southern", 4: "Northern", 5: "Eastern",
    6: "North Western", 7: "North Central", 8: "Uva", 9: "Sabaragamuwa"
}
adm2_inad['province'] = (adm2_inad['adm2'] // 10).round().map(province_map)

# Define variable groups
mn_col_names = list(adm2_inad.columns[2:8])
price_col_names = list(adm2_inad.columns[11:15])

# Create output directory
os.makedirs("outputs/plots/prices", exist_ok=True)



# Generate plots
for i in mn_col_names:
    for j in price_col_names:
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
        plt.savefig(f"outputs/plots/prices/{i}_{j}.png")
        plt.close()




