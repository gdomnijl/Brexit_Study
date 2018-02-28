import pandas as pd
import numpy as np
import plotly.plotly as py
import plotly.offline as py_offline
import plotly.graph_objs as go

def filter_for_columns(input_file="all_waves.csv", begin_wave=1, end_wave=13, wanted_columns=[], output_file="result.csv"):

    df = pd.read_csv(input_file, index_col="id")

    immigration_cols = [col for col in df.columns if 'immigration' in col.lower() or 'migration' in col.lower() or 'migra' in col.lower() or 'immi' in col.lower()]

    needed_columns_names = ["wt_new_W1_W13"] + wanted_columns + immigration_cols

    variable = "euRefVote"
    for i in range(begin_wave, end_wave + 1):
        col_name = "euRefVoteW" + str(i)
        needed_columns_names.append(col_name)

    needed_columns = df.loc[:, needed_columns_names]
    #needed_columns = needed_columns.dropna(axis=1, how='all').dropna(axis=0, how='any')
    needed_columns.to_csv(output_file)
    return output_file

filter_for_columns(input_file="all_waves.csv", begin_wave=1, end_wave=13, wanted_columns=[], output_file="immigration.csv")