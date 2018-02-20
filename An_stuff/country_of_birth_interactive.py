import pandas as pd
import numpy as np
import plotly.plotly as py
from plotly.graph_objs import *

df = pd.read_csv("../An_dataset.csv")

countryOfBirth = df["countryOfBirth"]

country_list = countryOfBirth.unique()

# england = df[countryOfBirth == "England"]
# x = england[["wave"]].values.flatten()
# y = england[["stay_leave_ratio"]].values.flatten()

# print(x)
# print(y)

trace_list = []

for country in country_list:
	origin_list = df[countryOfBirth == country]
	x = origin_list[["wave"]].values.flatten()
	y = origin_list[["stay_leave_ratio"]].values.flatten()
	trace = Scatter(
    x = x,
    y = y,
    mode = country,
    name = country)

	trace_list.append(trace)

data = Data(trace_list)

py.iplot(data, filename = 'country_of_birth')