import pandas as pd
import numpy as np
import plotly.plotly as py
import plotly.offline as py_offline
import plotly.graph_objs as go


##Step 1: get the necessary columns
# df = pd.read_csv("all_waves.csv",index_col = "id")

## Filter for the columns we need

# begin_wave = 1
# end_wave = 13
# needed_columns_names = ["wt_new_W1_W13"]


# variable = "euRefVote"
# for i in range(begin_wave, end_wave +1):
# 	col_name = "euRefVoteW" + str(i)
# 	needed_columns_names.append(col_name)


# needed_columns = df.loc[:,needed_columns_names]

# needed_columns.to_csv("all_waves_general.csv")



## Step 2: Filter for no N/A rows
#
# df = pd.read_csv("all_waves_general.csv")


# no_null_data = df.dropna(axis=1,how='all').dropna(axis=0,how='any')

# print(no_null_data.head())
# no_null_data.to_csv("all_waves_general_no_null.csv")

## Step 3: caculating data for the graph
#
# df = pd.read_csv("all_waves_general_no_null.csv")

# all_series = {}

# total_weight = df["wt_new_W1_W13"].sum()

# print(total_weight)

# for col in df.loc[:,"euRefVoteW1":"euRefVoteW13"]:
# 	each_wave = df.groupby(col)
# 	## Found out that wave 9 has no one saying "I would not vote"
# 	if len(each_wave.groups.keys()) == 3:
# 		print(col)

# 	#quick fix
# 	if col == "euRefVoteW9":
# 			print("Found!")
# 			all_series["I would/will not vote"].append(0)

# 	for group in each_wave.groups.keys():
# 		sum_weight = each_wave.get_group(group).loc[:,"wt_new_W1_W13"].sum()
# 		ratio = sum_weight/total_weight
# 		if group in all_series:
# 			all_series[group].append(ratio)
# 		else:
# 			all_series[group] = [ratio]

# all_series["Waves"] = [1,2,3,4,6,7,8,9,10,11,12,13]
# for key, value in all_series.items():
# 	print(key, " : ",len(value))

### Print out this csv
# all_13_waves_general_graph = pd.DataFrame.from_dict(all_series)
# all_13_waves_general_graph.to_csv("all_13_waves_general_graph.csv")






##Step 4: Making the graph with plot.ly
#

df = pd.read_csv("all_13_waves_general_graph.csv")
dictionary = df.to_dict('list')

waves = dictionary["Waves"]

dictionary.pop('Waves',None)

trace_list = []

for key, values in dictionary.items():
	trace = go.Scatter(
		x = waves,
		y = values,
		mode = key,
		name = key)

	trace_list.append(trace)



layout = go.Layout(
    title='Panel Data 13 Waves general, n= 5299 ',
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

# online export
# plot_url = py.plot(fig, filename='all_13_waves_general_graph')


#html export
# py_offline.plot(fig, filename= 'all_13_waves_general_graph')
#

#image export
py_offline.plot(fig, filename= 'all_13_waves_general_graph', image = 'png')

