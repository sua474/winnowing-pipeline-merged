import matplotlib.pyplot as plt
import matplotlib.cm as colormaps
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib


#read in the naming file
namefile_path = 'bac_tax.csv'
name_file = pd.read_csv(namefile_path, index_col='OTU')

# clean data file
del name_file['Size']
name_file.replace(to_replace='[a-z]__',value='',inplace=True,regex=True)
name_file.replace(to_replace='\([0-9]*\)',value='',inplace=True,regex=True)

dicty = name_file.T.to_dict('list')
name_file['name'] = [[i for i in dicty[k] if i][-1] for k in dicty]
name_df = pd.DataFrame(index=name_file.index)
name_df['name'] = name_file['name']

# Read the data into a pandas DataFrame.
metric_results = pd.read_csv("metric_results-poster.csv", header=None)
default_headers = ['ab_comp','infile1_path','metric_type','centrality_type','total_select','iter_select',
                   'min_count','smoothing','conditioning','threshold','correlation','weighted','corr_prop',
                   'runtime']
default_header_cols = len(default_headers)
OTUs_selected = metric_results.shape[1] - default_header_cols
otu_header_cols = list(range(1,OTUs_selected+1))
header_column_names = default_headers + otu_header_cols
metric_results.columns = header_column_names

print('metric results', metric_results)

corr_types = metric_results['correlation'].unique()
print('corr_Types', corr_types)


params = {'legend.fontsize': 'medium',
          'figure.figsize': (12, 8),
         'axes.labelsize': 'large',
         'axes.titlesize':'large',
         'xtick.labelsize':'medium',
         'ytick.labelsize':'large'}
plt.rcParams.update(params)

for corr_type in corr_types:
    c_data = metric_results[metric_results['correlation'] == corr_type]
    c_data.sort_values(by=['centrality_type','iter_select'], inplace=True)
    print('cdata', c_data)



    #histogram of the otus
    OTU_counts = c_data.ix[:,default_header_cols:]
    melted = pd.melt(OTU_counts)
    counts = melted.groupby('value').size()
    counts.sort_values(ascending=False, inplace=True)
    counts = pd.concat([counts, name_file['name']], axis=1, join_axes=[counts.index])
    top25 = counts.ix[:25]
    print(counts)
    print('counts', counts)
    #print('otu counts', OTU_counts)
    #print('top 25', top25)
    #print('total number of otus selected:', counts.shape[0])

    #fig = plt.figure(figsize=(12,6))
    #ax = fig.add_subplot(1,1,1)
    counts.set_index('name').plot(kind='bar', legend=None)
    plt.title('Frequency of Important OTUs Selected Using Correlation Type - {:s}'.format(corr_type))
    plt.xlabel('Selected OTUs')
    plt.ylabel('Frequency')
    #plt.xticks(rotation=70, fontsize=10)
    plt.tight_layout()
    plt.savefig('otu_hist-{:s}.svg'.format(corr_type))
    plt.show()

    top25.set_index('name').plot(kind='bar', legend=None)
    plt.title('Frequency Of Top 25 Important OTUs Selected Using Correlation Type - {:s}'.format(corr_type))
    plt.xlabel('Selected OTUs')
    plt.ylabel('Frequency')
    #plt.xticks(fontsize=14)
    plt.tight_layout()
    plt.savefig('top25_otu_hist-{:s}.svg'.format(corr_type))
    plt.show()

    color_list = [(188, 189, 34), (219, 219, 141),
                 (44, 160, 44), (152, 223, 138), (214, 39, 40),
                 (89,85,22), (102,255,0), (191,255,234),
                 (242,182,238), (255,64,140), (204,102,116), (242,190,182), (242,162,0), (242,255,64),
                 (76,22,89), (166,0,111), (102,77,83), (229,0,0),
                 (130,230,115), (64,255,217), (38,47,51), (32,57,128), (191,64,255), (242,0,194), (204,0,54),
                 (148, 103, 189), (197, 176, 213), (140, 86, 75), (196, 156, 148),
                 (227, 119, 194), (247, 182, 210), (127, 127, 127), (199, 199, 199),
                 (255, 127, 14), (255, 187, 120),(31, 119, 180), (174, 199, 232),
                  (23, 190, 207), (158, 218, 229),(0,60,89), (115,130,153), (113,96,128),
                 (255,0,0), (140,56,0), (191,175,143), (61,115,0), (16,64,35), (0,226,242),
                 (64,166,255), (0,0,77),
                 (51,28,13), (166,155,0), (169,191,143), (38,153,115), (77,148,153), (191,225,255),
                 (57,57,230), (172,96,191), (64,32,53), (51,0,7), (115,29,29), (242,170,121)]

    # Scale the RGB values to the [0, 1] range, which is the format matplotlib accepts.
    for i in range(len(color_list)):
        r, g, b = color_list[i]
        color_list[i] = (r / 255., g / 255., b / 255.)


    count = 0
    fig = plt.figure(figsize=(14,6))
    #ax1 = fig.add_subplot(1,1,1)

    # for each of the OTUs selected....
    for i, val in counts.iterrows():
        print('i, val', i, val)
        # look at their rank at each of the conditions (i.e for every row)
        rank_list= []
        condition_list = []
        condition_val = 0
        condition_names = []
        for index, data in c_data.iterrows():
            print('index, data', index, data)
            total = str(data['total_select'])
            iter = str(data['iter_select'])
            if data['weighted']:
                weighted = 'weighted'
            else:
                weighted = 'unweighted'
            condition = data['centrality_type'] + '-' + total + 'by' + iter + '-' + weighted
            condition_val +=1
            condition_list.append(condition_val)
            condition_names.append(condition)
            if data[data == i].empty:
                rank_list.append(28)
            else:
                rank = data[data == i].index[0]
                rank_list.append(rank)
        print('condition list', condition_list)
        print('rank list', rank_list)

        plt.plot(condition_list, rank_list, marker='o', color=color_list[count%len(color_list)], label=name_df.loc[i][0])
        plt.xlabel('Condition')
        plt.ylim(25,-1)
        plt.ylabel('Rank')
        print('condition names', condition_names)
        plt.xticks(np.arange(0,len(condition_names)), condition_names, rotation=45, ha='center')
        plt.title('Important OTUs Ranked Across Conditions - Using {:s}'.format(corr_type))
        count += 1
    plt.tight_layout()
    art =[]
    lgd = plt.legend(bbox_to_anchor=(1.01, 1.01), ncol=3)
    art.append(lgd)
    plt.savefig(
        "rankedByCond-{:s}.svg".format(corr_type), additional_artists=art,
        bbox_inches="tight")
    plt.show()
