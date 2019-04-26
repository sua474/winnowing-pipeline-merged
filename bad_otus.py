import pandas
import numpy as np
import matplotlib.pyplot as plt
import networkx


dir_file='no_bad_arc_Brome_bacfunarc_otu_table44_results/adj_matrix-None.csv'

#corr_matrix=pandas.read_csv('no_bad_arc_Brome_bacfunarc_otu_table10_results/adj_matrix-None.csv', index_col=0)
corr_matrix=pandas.read_csv(dir_file, index_col=0)

#corr_matrix.drop(corr_matrix.columns[0],axis=1)
#print(corr_matrix)
corr_matrix_n=networkx.from_pandas_adjacency(corr_matrix)

graphs=list(networkx.connected_component_subgraphs(corr_matrix_n))
print(len(graphs))

for graph in graphs:
    print(networkx.info(graph))
    print(networkx.nodes(graph))

#plt.matshow(corr_matrix.corr())
#plt.show()

#print(corr_matrix)

#count=0
#for column in corr_matrix:
#    if max(corr_matrix[column])<0.2:
#        count+=1
#        print(column)

#print(count)

#x = corr_matrix.as_matrix()
#y=pandas.DataFrame(x[:,np.amax(x,0)<=0.2])
#print(y)


