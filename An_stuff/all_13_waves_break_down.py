import pandas as pd
import numpy as np
import plotly.plotly as py
import plotly.offline as py_offline
import plotly.graph_objs as go
import itertools

# Step 1: get the necessary columns


# Filter for the columns we need
# def filter_for_columns(begin_wave=1, end_wave=13, wanted_columns=[], name_of_file="result.csv" ):

#     df = pd.read_csv("all_waves.csv",index_col = "id")
#     needed_columns_names = ["wt_new_W1_W13"] + wanted_columns

#     variable = "euRefVote"
#     for i in range(begin_wave, end_wave +1):
#       col_name = "euRefVoteW" + str(i)
#       needed_columns_names.append(col_name)

#     needed_columns = df.loc[:,needed_columns_names]
#     needed_columns = needed_columns.dropna(axis=1,how='all').dropna(axis=0,how='any')
#     needed_columns.to_csv(name_of_file)


# Step 2: Filter for no N/A rows


# Step 3: caculating data for the graph

# NOTE 1: FOR ASSIGN DIFFERENT COLORS TO DIFFERENT SERIES, SEE THIS:
# https://stackoverflow.com/questions/40673490/how-to-get-plotly-js-default-colors-list

df = pd.read_csv("all_13_waves_age.csv")




all_series = {}

total_weight = df["wt_new_W1_W13"].sum()

sum_dict = {}

category = df.groupby("ageGroup")

for group in category.groups.keys():
    sum_dict[group] = category.get_group(group)["wt_new_W1_W13"].sum()


# print(list(itertools.product(unique_values,df["euRefVoteW1"].unique())))

i = 0
for col in df.loc[:, "euRefVoteW1":"euRefVoteW13"]:
    each_wave = df.groupby(["ageGroup", col])
    i += 1

    for group in each_wave.groups.keys():
        category, opinion = group
        sum_weight = each_wave.get_group(group).loc[:, "wt_new_W1_W13"].sum()
        ratio = sum_weight / sum_dict[category]
        if group in all_series:
            all_series[group].append(ratio)
        else:
            all_series[group] = [ratio]

    for keys, values in all_series.items():
        if len(values) < i:
            values.append(0)

all_series["Waves"] = [1,2,3,4,6,7,8,9,10,11,12,13]

for key, value in all_series.items():
    print(key, " : ",len(value))

# ## Print out this csv
all_13_waves_general_graph = pd.DataFrame.from_dict(all_series)
all_13_waves_general_graph.to_csv("all_13_waves_age_graph.csv")




colors = ["#1f77b4", "#aec7e8", "#ff7f0e",
"#ffbb78",
"#2ca02c",
"#98df8a",
"#d62728",
"#ff9896",
"#9467bd",
"#c5b0d5",
"#8c564b",
"#c49c94",
"#e377c2",
"#f7b6d2",
"#7f7f7f",
"#c7c7c7",
"#bcbd22",
"#dbdb8d",
"#17becf",
"#9edae5"]

color_map = {}
unique_values = df["ageGroup"].unique()

print(unique_values)

for index, unique_value in enumerate(unique_values):
    color_map[unique_value] = colors[index]

marker_style_map = {"Leave the EU": "circle", 'Stay/remain in the EU': "square", 'I would/will not vote':"diamond", "Don't know" : "triangle-down"}

waves = all_series.pop('Waves',None)

trace_list = []

for (group,opinion),values in all_series.items():
    trace = go.Scatter(
      x = waves,
      y = values,
      legendgroup = group,
      name = "Age: " + group + ", Opinion: " + opinion,
      line = dict(color= color_map[group]),
      marker = dict(symbol= marker_style_map[opinion], size=10)
        )

    trace_list.append(trace)


layout = go.Layout(
    title='Panel Data 13 Waves Break Down By Age, n= 5299 ',
    xaxis=dict(
        title='Waves',
        titlefont=dict(
            family='Courier New, monospace',
            size=18,
            color='#7f7f7f'
        )
    ),
    yaxis=dict(
        title='Ratio (num_votes/total)',
        titlefont=dict(
            family='Courier New, monospace',
            size=18,
            color='#7f7f7f'
        )
    )
)

fig = go.Figure(data=trace_list, layout=layout)

##html export
py_offline.plot(fig, filename= 'all_13_waves_age_graph')

# # online export
plot_url = py.plot(fig, filename='all_13_waves_age_graph')




# Step 4: Making the graph with plot.ly
#

# df = pd.read_csv("all_13_waves_general_graph.csv")
# dictionary = df.to_dict('list')

# waves = dictionary["Waves"]

# dictionary.pop('Waves',None)

# trace_list = []

# for key, values in dictionary.items():
#   trace = go.Scatter(
#       x = waves,
#       y = values,
#       mode = key,
#       name = key)

#   trace_list.append(trace)



# layout = go.Layout(
#     title='Panel Data 13 Waves general, n= 5299 ',
#     xaxis=dict(
#         title='Waves',
#         titlefont=dict(
#             family='Courier New, monospace',
#             size=18,
#             color='#7f7f7f'
#         )
#     ),
#     yaxis=dict(
#         title='Ratio (num_votes/total)',
#         titlefont=dict(
#             family='Courier New, monospace',
#             size=18,
#             color='#7f7f7f'
#         )
#     )
# )

# fig = go.Figure(data=trace_list, layout=layout)

# # online export
# # plot_url = py.plot(fig, filename='all_13_waves_general_graph')


# #html export
# # py_offline.plot(fig, filename= 'all_13_waves_general_graph')
# #

# #image export
# py_offline.plot(fig, filename= 'all_13_waves_general_graph', image = 'png')

