import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from functools import reduce

data = pd.read_csv("../wt_age_origin.csv")

wave_labels = []

for i in range(1,14):
	wave_labels.append("wave"+str(i))


print(wave_labels)

#df = df[df[['col_1','col_2']].apply(lambda x: f(*x), axis=1)]
#Source: https://stackoverflow.com/questions/22086116/how-do-you-filter-pandas-dataframes-by-multiple-columns

#all_waves = data[wave_labels].apply(lambda x: x == 1, axis=1)

#all_waves = data[wave_labels] == 1


#Real, more helpful source: https://stackoverflow.com/questions/42711186/python-pandas-how-to-filter-multiple-columns-by-one-value

new_data = data[(data.loc[:,"wave1":"wave13"] == 1).all(axis=1)]
new_data.to_csv('all_waves.csv')
