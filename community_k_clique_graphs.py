import networkx
from networkx.algorithms.community import k_clique_communities
import pandas
import pprint
import numpy
import minepy


def inverse_correlation(matrix):
    one = matrix.abs()
    two = one.copy() - 1
    inv_corr = two.abs()
    return inv_corr
# inverse the correlation matrix so that 0 means fully correlated and 1 means not correlated

#use this to generate graphml files for the different communities so that you can visualize said communities in cytoscape

centrality='degree'
k=3
corr_type='MIC'
thresh=0.2
file='bacfunarc_otu_table'

abundances=pandas.read_csv(file+'_results_'+centrality+'/'+file+'-abundances.csv')

#read graphml into a networkx graph
graphml=networkx.read_graphml(centrality+'.graphml')

#generate the list of k communities for the respective graph and k number
k_comm=list(k_clique_communities(graphml,k))
#pprint.pprint(k_comm)


#This will generate the graphml files for each community. Any community with less than 3 otus will NOT be written to a graphml file, however it will
#be printed out
#print(abundances)
print(len(k_comm))
less_3=0
count=0
for community in k_comm:
#    comm_len=(len(community))
    comm_df=pandas.DataFrame()
    for otu in community:
        comm_df=comm_df.append(abundances[otu])
    comm_df=comm_df.transpose()
    pprint.pprint(comm_df)
    comm_df_col_len=(len(comm_df.columns))
    if comm_df_col_len <= 3:
        less_3+=1
        continue
    count+=1

#print(count)
    data=comm_df
    if corr_type == 'MIC':
        # the pstats output for the mic and tic are condensed 1D arrays
        # we need to turn the output into a 2D upper triangular matrix and then mirror it to get the
        # full correlation matrix
        micp, ticp = minepy.pstats(data.T, alpha=0.6, c=15, est="mic_approx")
        num_features = data.shape[1]
        tri = numpy.zeros((num_features, num_features))
        tri[numpy.triu_indices(num_features, 1)] = micp
        full_corr = tri + tri.T

        corr_matrix = pandas.DataFrame(full_corr)
        corr_matrix.columns = data.columns.values
        corr_matrix.index = data.columns.values
    else:
        corr_matrix=data.corr(method='spearman')

    inv_correlation = inverse_correlation(corr_matrix)
    labels=list(inv_correlation.index)

    inv_correlation.insert(0,'var1',labels)
    #insert a labels column called 'var1' of the OTUs to be used to melt the matrix into 3 columns


    butter=pandas.melt(inv_correlation,'var1',var_name='var2',value_name='weight',)
    #melt the matrix into three columns
    #print (butter)

    #butter=butter.loc[((butter['weight'] <= 1 - thresh)& (butter['weight'] >=0.0)), :]
    #only keep the edge values that are less than 1-threshold and greater than or equal to 0
    #print(butter)


    butter.to_csv(file+'-'+centrality+'_'+corr_type+'-dataframe.csv')
    graph=networkx.from_pandas_edgelist(butter,'var1','var2','weight')
    networkx.write_graphml(graph,centrality+'_'+corr_type+'_'+'comm#_'+str(count)+'.graphml')

    del comm_df
    del data

print(less_3)
print(count)