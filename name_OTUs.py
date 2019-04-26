import numpy as np
import matplotlib
import pandas as pd  # Use this for working with dataframes
import networkx

import pylab

#read in the naming file
namefile_path = 'bac_tax.csv'
name_file = pd.read_csv(namefile_path, index_col='OTU')
#print('name file', name_file)
# clean data file
del name_file['Size']
name_file.replace(to_replace='[a-z]__',value='',inplace=True,regex=True)
name_file.replace(to_replace='\([0-9]*\)',value='',inplace=True,regex=True)


graph = networkx.read_graphml('graph.graphml')
g = networkx.draw(graph, with_labels=True)
graph_df = networkx.to_pandas_adjacency(graph)
print('df', graph_df)
#pylab.show(g)

#make the dictionary
dicty = name_file.T.to_dict('list')
name_file['name'] = [[i for i in dicty[k] if i][-1] for k in dicty]
name_df = pd.DataFrame(index=name_file.index)
name_df['name'] = name_file['name']

merged = pd.concat([graph_df, name_df['name']], axis=1, join_axes=[graph_df.index])
print('name cols', merged['name'])
merged['name'].to_csv('node_labels.csv')
name_dict = merged['name'].to_dict()
print('name_dict', name_dict)
print('column vals', merged.index.values)

networkx.write_graphml(graph, 'unnamed_graph.graphml')
#networkx.relabel_nodes(graph,name_dict,copy=False)
#h = networkx.draw(graph, with_labels=True)
renamed_df = networkx.to_pandas_adjacency(graph)
print('renamed 1', renamed_df)
renamed_df = renamed_df.rename(index=name_dict)
print('renamed 2', renamed_df)
#g2 = networkx.draw(graph, with_labels=True)
#graph2_df = networkx.to_pandas_dataframe(graph)
#print('graph 2 df', graph2_df)
#graph2_df.to_csv('graphtest.csv')
#pylab.show(g2)
#pylab.show(H)


networkx.write_graphml(graph, 'named_graph.graphml')


