import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.api as sm
from datetime import datetime
import os
import geopandas as gpd
from shapely.geometry import Point
from pathlib import Path







class FeaturePlotter:
    """
    class for reading in data from the Sri Lanka data
    """

    def __init__(self, survey_id: str, path_to_datasets: str) -> None:
        self.survey_id = survey_id
        self.path_to_datasets = path_to_datasets
        self.hhid = 'hhid'

    
    @staticmethod
    def read_datasets(path_to_datasets: str, dataset_name: str, fsq24: bool = True) -> pd.DataFrame:
        if fsq24:
            path_to_fsq = "food_security_survey_2024/data-fs-sp_final-v2.xlsx"
            csv_path = Path(path_to_datasets+path_to_fsq)
            df = pd.read_excel(csv_path)
        else:
            path_to_hies =  "HIES_2019/HIES_2019/"
            csv_path = Path(path_to_datasets+path_to_hies) / f"{dataset_name}.csv"
            df = pd.read_csv(csv_path)
        return df


####
    
path_to_datasets = 'C:/Users/gabriel.battcock/OneDrive - World Food Programme/General - MIMI Project/Countries/Sri Lanka/data/'


hies_19 = FeaturePlotter('hies19', path_to_datasets)

print(food_security_24.survey_id)