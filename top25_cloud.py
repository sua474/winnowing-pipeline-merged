import pandas as pd
import pylab
import networkx
import matplotlib.pyplot as plt
import numpy as np
import minepy


data = pd.read_csv('/home/michelle/PycharmProjects/pipeline/weighted/bromeA_all-graph_centrality-eigenvector-select25by25-abundances.csv', header=0, index_col=0)
#create a pandas dataframe from the csv file with the otus, samples, and corresponding abundances

corr_type = 'MIC'


if corr_type == 'MIC':
    # the pstats output for the mic and tic are condensed 1D arrays
    # we need to turn the output into a 2D upper triangular matrix and then mirror it to get the
    # full correlation matrix
    micp, ticp = minepy.pstats(data.T, alpha=0.6, c=15, est="mic_approx")
    num_features = data.shape[1]
    tri = np.zeros((num_features, num_features))
    tri[np.triu_indices(num_features, 1)] = micp
    full_corr = tri + tri.T

    corr_matrix = pd.DataFrame(full_corr)
    corr_matrix.columns = data.columns.values
    corr_matrix.index = data.columns.values
else:
    corr_matrix=data.corr(method='spearman')


#create an nxn spearman correlation matrix

def inverse_correlation(matrix):
    one=matrix.abs()
    two=one.copy() -1
    inv_corr= two.abs()
    return inv_corr
#inverse the correlation matrix so that 0 means fully correlated and 1 means not correlated

inv_correlation=inverse_correlation(corr_matrix)

inv_correlation[(inv_correlation.abs() > 0.5)] = 1
#make any number above 0.5 equal to 1



labels=list(inv_correlation.index)
inv_correlation.insert(0,'var1',labels)
#insert a labels column called 'var1' of the OTUs to be used to melt the matrix into 3 columns

butter=pd.melt(inv_correlation,'var1',var_name='var2',value_name='edge')
#melt the matrix into three columns
print (butter)

butter=butter.loc[(butter['edge'] != 1)]
#only keep the edge values that are not 1


#butter.to_csv('/home/michelle/PycharmProjects/pipeline/bromeA_all-graph_centrality-degree-select25by25-dataframe.csv')
graph=networkx.from_pandas_dataframe(butter,'var1','var2','edge')
networkx.write_graphml(graph,'eigenvector_weighted_25by25_spearman.graphml')
#networkx.draw_networkx(graph,node_color='dodgerblue',edge_color='dimgrey')

#networkx.draw(graph,with_labels=True)
#pylab.show()
#make the networkx graph and show the graph
