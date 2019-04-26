#!/bin/sh

python pipeline.py -f1 bromeA_all.csv -m graph_centrality -e kl_divergence -min 3 -c add_one -st 25 -si 25 -cent betweenness -th .5 -cor MIC -cp both
python pipeline.py -f1 bromeA_all.csv -m graph_centrality -e kl_divergence -min 3 -c add_one -st 25 -si 1 -cent degree -th .5 -cor MIC -cp both
python pipeline.py -f1 bromeA_all.csv -m graph_centrality -e kl_divergence -min 3 -c add_one -st 25 -si 1 -cent closeness -wt -th .5 -cor MIC -cp both
python pipeline.py -f1 bromeA_all.csv -m graph_centrality -e kl_divergence -min 3 -c add_one -st 25 -si 25 -cent eigenvector -wt -th .5 -cor MIC -cp both
