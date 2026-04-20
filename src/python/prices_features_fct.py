import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
from datetime import datetime
import os


prices_data = pd.read_csv('../../data/raw/features/prices_2019_15102025.csv')


def preprocess_prices(df):
    """Filter and pivot price data for analysis."""
    # filtered = df[df['Data Type'] != "Forecast"]
    filtered = df
    # combine all rice types
    filtered['Commodity'] = filtered['Commodity'].apply(
        lambda x: 'Rice' if 'rice' in x.lower() else x
    )

    grouped = (
        filtered.groupby(['Admin 2', 'Commodity'])
        .agg({'Price': 'median'})
        .reset_index()
    )

    
    pivoted = grouped.pivot(index='Admin 2', columns='Commodity', values='Price')
    return pivoted

if __name__ == "__main__":
    print(preprocess_prices(price_data))