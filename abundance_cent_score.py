import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os

infile_folder = 'abundance_results/'

for filename in os.listdir(infile_folder):

    path = str(infile_folder + '/' + filename)
    filename_str = os.path.splitext(filename)[0]
    print('filename', filename_str)

    infile = pd.read_csv(path, index_col=0)
    fig = plt.figure(figsize=(14,6))
    ax = fig.add_subplot(1,1,1)
    plt.scatter(infile.loc[:,'metric_result'], infile.loc[:,'abundances'],  label=infile.index.values)
    plt.xlabel('Centrality Score')
    plt.ylabel('Abundance')
    for k, v in infile.iterrows():
        ax.annotate(k, v)
    plt.title('Abundance vs Centrality for - {:s}'.format(filename_str))
    plt.tight_layout()
    plt.savefig("scatter-{:s}.pdf".format(filename_str),
        bbox_inches="tight")
    plt.show()
