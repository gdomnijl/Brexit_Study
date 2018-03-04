import pandas as pd
import numpy as np
import plotly.plotly as py
import plotly.offline as py_offline
import plotly.graph_objs as go

# Step 1: get the necessary columns


# Filter for the columns we need
def filter_for_columns(input_file ="all_waves.csv", begin_wave=1, end_wave=13, wanted_columns=[], output_file="result.csv" ):

    df = pd.read_csv(input_file,index_col = "id")
    needed_columns_names = ["wt_new_W1_W13"] + wanted_columns

    variable = "euRefVote"
    for i in range(begin_wave, end_wave +1):
      col_name = "euRefVoteW" + str(i)
      needed_columns_names.append(col_name)

    needed_columns = df.loc[:,needed_columns_names]
    needed_columns = needed_columns.dropna(axis=1,how='all').dropna(axis=0,how='any')
    needed_columns.to_csv(output_file)
    return output_file


# Step 2: Filter for no N/A rows


# Step 3: caculating data for the graph

# NOTE 1: FOR ASSIGN DIFFERENT COLORS TO DIFFERENT SERIES, SEE THIS:
# https://stackoverflow.com/questions/40673490/how-to-get-plotly-js-default-colors-list

def transform_raw_data(input_file = "all_13_waves_age.csv", break_down_var= "ageGroup", use_within_group_ratio= False, output_file="all_13_waves_age_graph.csv"):

    df = pd.read_csv(input_file, index_col = "id")


    all_series = {}
    all_series["Waves"] = [1,2,3,4,6,7,8,9,10,11,12,13]

    sum_dict = {}

    total_weight = df["wt_new_W1_W13"].sum()



    category = df.groupby(break_down_var)

    for group in category.groups.keys():
        sum_dict[group] = category.get_group(group)["wt_new_W1_W13"].sum()


    i = 0
    for col in df.loc[:, "euRefVoteW1":"euRefVoteW13"]:
        each_wave = df.groupby([break_down_var, col])
        i += 1

        for group in each_wave.groups.keys():
            category, opinion = group
            sum_weight = each_wave.get_group(group).loc[:, "wt_new_W1_W13"].sum()
            ratio = sum_weight / sum_dict[category] if use_within_group_ratio else sum_weight/total_weight
            if group in all_series:
                all_series[group].append(ratio)
            else:
                all_series[group] = [ratio]

        for keys, values in all_series.items():
            while len(values) < i:
                values.append(0)


    unique_values = df[break_down_var].unique()

    for key,value in all_series.items():
        print("Key: ",key," Value length: ", len(value))

    # ## Print out this csv
    all_13_waves_general_graph = pd.DataFrame.from_dict(all_series)
    all_13_waves_general_graph.to_csv(output_file)
    return output_file, unique_values, all_series

# Step 4: Making the graph with plot.ly

def make_graph(input_file = "all_13_waves_age_graph.csv", sub_groups_arr = ['66+','56-65','26-35','36-45','46-55','18-25'], dictionary=None, break_down_var_legend_name = "Age", graph_title='Panel Data 13 Waves Break Down By Age, n= 5299', x_axis_title="Waves", y_axis_title= 'Ratio (num_votes/total)', offline_output_file="all_13_waves_age_graph"):

    df = pd.read_csv(input_file, index_col=False)

    ###Preset work
    # mapping colors to groups
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


    for index, unique_value in enumerate(sub_groups_arr):
        color_map[unique_value] = colors[index]

    #mapping opinion to the series' marker

    marker_style_map = {"Leave the EU": "circle", 'Stay/remain in the EU': "square", 'I would/will not vote':"diamond", "Don't know" : "triangle-down"}

    ### Working with data
    all_series = dictionary
    waves = all_series.pop('Waves',None)
    all_series.pop('Unnamed: 0', None)
    trace_list = []

    #making the series

    for key,values in all_series.items():
        #print("Value: ",values)
        group,opinion = key
        trace = go.Scatter(
          x = waves,
          y = values,
          legendgroup = str(group),
          name = str(break_down_var_legend_name + ": " + group + ", Opinion: " + opinion),
          line = dict(color= color_map[group]),
          marker = dict(symbol= marker_style_map[opinion], size=10)
          )

        trace_list.append(trace)


    layout = go.Layout(
        title= str(graph_title),
        xaxis=dict(
            title= str(x_axis_title),
            titlefont=dict(
                family='Courier New, monospace',
                size=18,
                color='#7f7f7f'
            )
        ),
        yaxis=dict(
            title= str(y_axis_title),
            titlefont=dict(
                family='Courier New, monospace',
                size=18,
                color='#7f7f7f'
            )
        )
    )


    fig = go.Figure(data=trace_list, layout=layout)

    ##html export
    py_offline.plot(fig, filename= str(offline_output_file))

    # # online export
    # plot_url = py.plot(fig, filename='all_13_waves_age_graph')

#filter_for_columns(input_file=None, break_down_var=None)

break_down_var = "lr_scale"

filter_for_columns(input_file ="all_waves.csv", begin_wave=1, end_wave=13, wanted_columns=[break_down_var], output_file=break_down_var + ".csv" )

input_file, unique_values, all_series = transform_raw_data(input_file = break_down_var + ".csv", break_down_var= break_down_var, use_within_group_ratio= True, output_file="all_13_waves_" + break_down_var +"_graph.csv")

make_graph(input_file = input_file, sub_groups_arr = unique_values, dictionary = all_series,break_down_var_legend_name = break_down_var, graph_title='Panel Data 13 Waves Break Down By ' + break_down_var + ' n= 5299', x_axis_title="Waves", y_axis_title= 'Ratio (num_votes/total)', offline_output_file="all_13_waves_"+break_down_var+"_graph.html")





# # online export
# # plot_url = py.plot(fig, filename='all_13_waves_general_graph')


# #html export
# # py_offline.plot(fig, filename= 'all_13_waves_general_graph')
# #

# #image export
# py_offline.plot(fig, filename= 'all_13_waves_general_graph', image = 'png')