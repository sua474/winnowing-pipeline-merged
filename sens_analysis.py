import pipeline
import time

input_file = 'bromeA_all_Oct2017.csv'
centrality = ['betweenness']
corr_type = ['spearman']
smoothing_type = ['hellinger']
total = '100'
increment=['10','20','all']
weighted = [True]
corr_prop = ['both']
threshold = ['0.3','0.5']

selected = increment
for weight in weighted:
    for corr in corr_type:
        for c_prop in corr_prop:
            for cent in centrality:
                for incr in increment:
                    try:
                        t_start = time.perf_counter()
                        pipeline.main(False, input_file, '', 'graph_centrality', 'add_one', 3, total, incr, None, None,
                                      None, cent, threshold, corr, weight, c_prop, 'kl_divergence')
                        t_end = time.perf_counter()
                        print('runtime = ', t_end - t_start)
                    except:
                        pass

