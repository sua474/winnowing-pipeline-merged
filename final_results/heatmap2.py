import os
import re
from glob import glob
import pandas as pd
import csv
import numpy as np
import sys
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns; sns.set(color_codes=True)
import matplotlib.patches as mpatches
from matplotlib.legend import Legend
#import rbo


#
# @author: Ritesh Agrawal
# @Date: 13 Feb 2013
# @Description: This is an implementation of rank biased overlap score
# (Refererence: http://www.umiacs.umd.edu/~wew/papers/wmz10_tois.pdf).
# This is a modified implementation of  https://github.com/maslinych/linis-scripts/blob/master/rbo_calc.py
# It is a linear implementation of the RBO and assumes there are no
# duplicates and doesn't handle for ties.
#

def score(l1, l2, p=0.98):
    """
        Calculates Ranked Biased Overlap (RBO) score.
        l1 -- Ranked List 1
        l2 -- Ranked List 2
    """
    if l1 == None: l1 = []
    if l2 == None: l2 = []

    sl, ll = sorted([(len(l1), l1), (len(l2), l2)])
    s, S = sl
    l, L = ll
    if s == 0: return 0

    # Calculate the overlaps at ranks 1 through l
    # (the longer of the two lists)
    ss = set([])  # contains elements from the smaller list till depth i
    ls = set([])  # contains elements from the longer list till depth i
    x_d = {0: 0}
    sum1 = 0.0
    for i in range(l):
        x = L[i]
        y = S[i] if i < s else None
        d = i + 1

        # if two elements are same then
        # we don't need to add to either of the set
        if x == y:
            x_d[d] = x_d[d - 1] + 1.0
        # else add items to respective list
        # and calculate overlap
        else:
            ls.add(x)
            if y != None: ss.add(y)
            x_d[d] = x_d[d - 1] + (1.0 if x in ss else 0.0) + (1.0 if y in ls else 0.0)
            # calculate average overlap
        sum1 += x_d[d] / d * pow(p, d)

    sum2 = 0.0
    for i in range(l - s):
        d = s + i + 1
        sum2 += x_d[d] * (d - s) / (d * s) * pow(p, d)

    sum3 = ((x_d[l] - x_d[s]) / l + x_d[s] / s) * pow(p, l)

    # Equation 32
    rbo_ext = (1 - p) / p * (sum1 + sum2) + sum3
    return rbo_ext


results_file = 'seriously_final/results-brome_total_bad_removed.csv' #sys.argv[1]
# run results again with runtime and then put parameter run number in these results

# create a dataframe to create the results csv file
#columns = ['file_number','filename','metric','centrality_type','select_total','select_iter','min_count','conditioning','threshold','corr_type','weighted','corr_prop']
res_df = pd.read_csv(results_file, dtype={"file_number": int})
res_df.sort_values(['centrality_type','corr_type','conditioning'],inplace=True)

print('res_df',res_df)
res_df.insert(1, 'code', res_df['centrality_type'].astype(str).str[0] \
                 + res_df['conditioning'].astype(str).str[0] \
                + res_df['corr_type'].astype(str).str[0] \
                + res_df['threshold'].astype(str) + '-' \
                + res_df['select_iter'].astype(str) + '-' \
                + res_df['select_total'].astype(str) )
print('res_df',res_df)

#* Take all the otus returned from the pipeline results and rank them based
#  on how many runs they appeared in. DONE

# Do a spearman correlation using the summed values
# Use that correlation matrix to display a heat map
    # higher correlations (darker colours) might mean that the runs
    # have similar results despite the changing parameters.
    #


#Each OTU returned in any sensitivity analysis run is given a rank based on how many runs it appears in.
#This is turned into the ranked dictionary.
#print('test',res_df.loc[1, '1':])
'''row1 = res_df.loc[1, '1':].dropna()
results = pd.DataFrame({'rank-row1':row1.index,'otu':row1.values})
print('row1')
print(results)


row2 = res_df.loc[2, '1':].dropna()
results2 = pd.DataFrame({'rank-row2':row2.index,'otu':row2.values})
print('row2')
print(results2)'''

#combined = pd.merge(results, results2, how='left', on='otu')
#print('combined')
#print(combined)
print('testing::::::', 'baM-2-2'[2])


length =  len(res_df.index)
rank_arr = np.empty((length, length))
arr_codes = np.empty(length, dtype=object)
print('arrcodes', arr_codes)
for r1 in range(len(res_df.index)):
    row1 = pd.Series(res_df.iloc[r1]).loc['1':]
    row1.dropna(inplace=True)
    arr_codes[r1] = res_df.iloc[r1]['code']
    for r2 in range(len(res_df.index)):
        row2 = pd.Series(res_df.iloc[r2]).loc['1':]
        row2.dropna(inplace=True)
        sc = score(row1.tolist(), row2.tolist(), p=0.99)
        if sc > 1.0:
            print('score', sc)
        rank_arr[r1][r2] = sc
print('arr codes', arr_codes)
print('rank array is:::::', rank_arr)
sim_df = pd.DataFrame(data=rank_arr, index=arr_codes, columns=arr_codes)
print('sim df', sim_df)

#https://ragrawal.wordpress.com/2013/01/18/comparing-ranked-list/
#https://github.com/dlukes/rbo
#http://codalism.com/research/papers/wmz10_tois.pdf

#sorted_selected = selected_otu_counts.sort_values(ascending=False)
#ranked_otus = selected_otu_counts.rank(ascending=0).sort_values(ascending=True)
#ranked_dict = ranked_otus.to_dict()
#print('ranked_dict', ranked_dict)



#color code the runs based on the centrality, conditioning, and correlation
color_mapping = pd.DataFrame(columns={'names','Centrality','Conditioning','Correlation'})
color_mapping['names'] = sim_df.index.copy()

print('length MIC', len(color_mapping.names.astype(str).str[2] == 'M'))
print('length spearman', len(color_mapping.names.astype(str).str[2] == 's'))


color_mapping['Centrality'][color_mapping.names.astype(str).str.startswith('b')] = "#93aa8c"
color_mapping['Centrality'][color_mapping.names.astype(str).str.startswith('c')] = "#b3c9cc"
color_mapping['Centrality'][color_mapping.names.astype(str).str.startswith('d')] = "#ab98d3"
color_mapping['Centrality'][color_mapping.names.astype(str).str.startswith('e')] = "#e2acac"

color_mapping['Conditioning'][color_mapping.names.astype(str).str[1] == 'h'] = "#364968"
color_mapping['Conditioning'][color_mapping.names.astype(str).str[1] == 'a'] = "#356865"

color_mapping['Correlation'][color_mapping.names.astype(str).str[2] == 'M'] = "#a54d94"
color_mapping['Correlation'][color_mapping.names.astype(str).str[2] == 's'] = "#75aa68"

color_mapping.set_index('names', inplace=True)


print('color mapping df',color_mapping['Centrality'])

cent_leg = {'Betweenness':'#93aa8c','Closeness':'#b3c9cc','Degree':'#ab98d3','Eigenvector':'#e2acac'}
cond_leg = {'Hellinger':'#364968','Add One':'#356865'}
corr_leg = {'MIC':'#a54d94','Spearman':'#75aa68'}

legend_cent = []
for key in cent_leg:
    legend_cent.append(mpatches.Patch(color=cent_leg[key], label=key) )

legend_cond = []
for key in cond_leg:
    legend_cond.append(mpatches.Patch(color=cond_leg[key], label=key) )

legend_corr = []
for key in corr_leg:
    legend_corr.append(mpatches.Patch(color=corr_leg[key], label=key) )

sns.set(font_scale=2)
g = sns.clustermap(sim_df, cmap="Blues", figsize=(25, 15),
                   method='average', robust=True,
                   row_cluster=False, col_cluster=False,
                   cbar_kws=dict(ticks=[0, 0.2, 0.4, 0.6, 0.8, 1.0],label='Correlation'),
                   vmin=0, vmax=1.0,
                   xticklabels=False, yticklabels=False,
                   row_colors=color_mapping, col_colors=color_mapping)

plt.setp(g.ax_heatmap.get_yticklabels(), rotation=0)
plt.setp(g.ax_heatmap.get_xticklabels(), rotation=90)
g.ax_heatmap.set_xlabel("")
g.ax_heatmap.set_ylabel("")

l2=g.ax_heatmap.legend(loc='upper left',bbox_to_anchor=(-.35,1.05),handles=legend_cond,frameon=True,prop={'size':20})
l2.set_title(title='Conditioning Type',prop={'size':24})
g.ax_heatmap.add_artist(l2)

l3 = g.ax_heatmap.legend(loc='upper left',bbox_to_anchor=(-.35,0.85),handles=legend_cent,frameon=True,prop={'size':20})
l3.set_title(title='Centrality Type',prop={'size':24})
g.ax_heatmap.add_artist(l3)

l4 = g.ax_heatmap.legend(loc='upper left',bbox_to_anchor=(-.35,0.60),handles=legend_corr,frameon=True,prop={'size':20})
l4.set_title(title='Correlation Type',prop={'size':24})
g.ax_heatmap.add_artist(l4)

plt.savefig('test-heatmap.jpg')
plt.show()
