import multiprocessing
import pipeline
import time
import os


def func_wrapper(args):
    return call_pipeline(*args)

def call_pipeline(met,input_file,cent,corr,conditioning,total,incr,weight,c_prop,
                  thresh,min,plot_metric,create_graph,plot_pca,name_file,run_number):
    """
def main(ab_comp, infile1, infile2, metric, c_type, min_count,
         total_select, iteration_select, pca_components, smooth_type,
         window_size, centrality_type, keep_threshold, correlation,
         weighted, corr_prop, evaluation_type, plot_metric,
         create_graph, plot_pca, naming_file):"""
    #try:
    proc_id = os.getpid()
    t_start = time.perf_counter()
    pipeline.main(False, input_file, '', met, conditioning, min, total, incr, None,
                  None, None, cent, thresh, corr, weight, c_prop, 'kl_divergence',plot_metric,
                  create_graph, plot_pca,name_file, run_number)
    t_end = time.perf_counter()
    print('runtime = ', t_end - t_start)
    #except:
    #    pass
    return

if __name__ == '__main__':
    jobs = []
    #for full Canola Wheat dataset, still need MIC, add_one
    input_file = 'CanolaWheat_bac.csv' #'CanolaWheat.csv'  #'bromeA_all_Oct2017.csv' #'Brome_allOtus.csv'
    centrality = ['betweenness','closeness','degree','eigenvector']
    corr_type = ['spearman']
    conditioning_type = ['hellinger','add_one']
    total = '50'
    increment = ['1','5','10','25','50']
    weighted = [True]
    corr_prop = ['both']
    threshold = [0.3,0.5,0.7]
    min_count = [3]
    metric = ['graph_centrality']
    plot_metric = False
    create_graph = False
    plot_pca = False
    name_file = None#'bac_tax.csv'

    #print("There are %d CPUs on this machine" % multiprocessing.cpu_count())

    fl = []
    count = 1
    number_processes = 30 #multiprocessing.cpu_count() -1
    for min in min_count:
        for thresh in threshold:
            for met in metric:
                for weight in weighted:
                    for conditioning in conditioning_type:
                        for corr in corr_type:
                            for c_prop in corr_prop:
                                for cent in centrality:
                                    for incr in increment:
                                        print('count', count)
                                        fl.append((met,input_file,cent,corr,conditioning,total,incr,weight,c_prop,
                                                thresh,min,plot_metric,create_graph,plot_pca,name_file,count))
                                        count = count + 1;

    pool = multiprocessing.Pool(number_processes)
    results = pool.map_async(func_wrapper, fl)
    pool.close()
    pool.join()
