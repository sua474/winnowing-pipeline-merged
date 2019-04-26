import pandas
import pipeline
import time
import pprint


#function for actually running the pipeline. Parameters are min count 3, select all by all, centrality degree, 0.2 threshold, correlation MIC
def run_pipeline(filename):
    """filename: file path for the otu table you want to run the pipeline on
    run the pipeline using the parameters from the pipeline.py file. See documentation on pipeline.py if confused for parameters"""
    t_start = time.perf_counter()

    pipeline.main(False,filename,'','graph_centrality', 'add_one',3,'all','all',
                  None, None, None, 'degree', 0.2, 'MIC', True,'both', 'kl_divergence',False,False,False,'',None)
    t_end = time.perf_counter()
    print('runtime = ', t_end - t_start)
    return

#uncomment this run_pipeline call for the baseline pipeline results with all samples if running for the first time. It will run the pipeline on the original otu table without removing any samples

#run_pipeline('no_bad_arc_Brome_bacfunarc_otu_table.csv')
baseline_pipeline_results=pandas.read_csv('no_bad_arc_Brome_bacfunarc_otu_table_results/no_bad_arc_Brome_bacfunarc_otu_table-None.csv',index_col=0)




#these are the otus you want to extract from all the leave one out runs
og_otus=['bac.Otu3097',
'bac.Otu3121',
'bac.Otu3874',
'bac.Otu3880',
'bac.Otu4164',
'bac.Otu4238',
'bac.Otu4350',
'bac.Otu4383',
'bac.Otu4431',
'bac.Otu4451',
'bac.Otu4477',
'bac.Otu4727',
'bac.Otu4807',
'bac.Otu4927',
'bac.Otu4960',
'bac.Otu5059',
'bac.Otu5087',
'bac.Otu5178',
'bac.Otu5183',
'bac.Otu5222',
'bac.Otu5270',
'bac.Otu5330',
'bac.Otu5349',
'bac.Otu5442',
'bac.Otu5573',
'bac.Otu5588',
'bac.Otu5711',
'bac.Otu5737',
'bac.Otu5798',
'bac.Otu5851',
'bac.Otu5886',
'bac.Otu5899',
'bac.Otu5916',
'bac.Otu5981',
'bac.Otu6047',
'bac.Otu6078',
'bac.Otu6125',
'bac.Otu6188',
'bac.Otu6245',
'bac.Otu6251',
'bac.Otu6264',
'bac.Otu6267',
'bac.Otu6283',
'bac.Otu6311',
'bac.Otu6338',
'bac.Otu6342',
'bac.Otu6346',
'bac.Otu6377',
'bac.Otu6390',
'bac.Otu6413',
'bac.Otu6430',
'bac.Otu6448',
'bac.Otu6472',
'bac.Otu6512',
'bac.Otu6517',
'bac.Otu6520',
'bac.Otu6532',
'bac.Otu6544',
'bac.Otu6568',
'bac.Otu6569',
'bac.Otu6575',
'bac.Otu6579',
'bac.Otu6583',
'bac.Otu6586',
'bac.Otu6593',
'bac.Otu6596',
'bac.Otu6599',
'bac.Otu6602',
'bac.Otu6620',
'bac.Otu6624',
'bac.Otu6625',
'bac.Otu6626',
'bac.Otu6627',
'bac.Otu6641',
'bac.Otu6643',
'bac.Otu6646',
'bac.Otu6663',
'bac.Otu6664',
'bac.Otu6671',
'bac.Otu6677',
'bac.Otu6684',
'bac.Otu6697',
'bac.Otu6701',
'bac.Otu6704',
'bac.Otu6712',
'bac.Otu6719',
'bac.Otu6725',
'bac.Otu6727',
'bac.Otu6743',
'bac.Otu6746',
'bac.Otu6747',
'fun.s__Devriesia',
'fun.s__Exophiala_sp_KL_2011f',
'fun.s__Gibberella_sp_SB5_2',
'fun.s__Helotiales_sp_CWG_F5_E16',
'fun.s__Helotiales_sp_r345',
'fun.s__Myrmecridium_schulzeri',
'fun.s__oat_root_associated_euascomycete_00015',
'fun.s__Pleosporales_sp_REF125',
'fun.s__soil_fungal_sp_SA9_6',
'fun.s__Thelebolus_globosus',
'fun.s__Trichopezizella_otanii',
'fun.s__uncultured_Phialophora',
'fun.s_g__Davidiella_unc',
'fun.s_g__Phialophora_unc',
'fun.s_g__Tetracladium_unc',
'fun.unknown_c__Leotiomycetes_OTU_6',
'fun.unknown_fungi_OTU_112',
'fun.unknown_fungi_OTU_354',
'fun.unknown_fungi_OTU_38',
'fun.unknown_o__Helotiales_OTU_9',
'fun.unknown_p__Ascomycota_OTU_134',
'fun.unknown_p__Ascomycota_OTU_157',
'fun.unknown_p__Ascomycota_OTU_191',
'fun.unknown_p__Ascomycota_OTU_223',
'fun.unknown_p__Ascomycota_OTU_331',
'fun.unknown_p__Ascomycota_OTU_5',
'fun.unknown_p__Ascomycota_OTU_778']

og_21_df=pandas.DataFrame()

#extract the otus from the baseline pipeline results to start the dataframe. this will be the original abundance and metric column in the final result. These are the results
#of these specific otus without extracting any samples
for otu in og_otus:
    #print(baseline_pipeline_results.loc[otu])
    og_21_df=og_21_df.append(baseline_pipeline_results.loc[otu])

#print(og_21_df)

#read in original otu table
otu_df=pandas.read_csv('no_bad_arc_Brome_bacfunarc_otu_table.csv')
#print(otu_df)






#range is the samples you wish to remove from the original otu dataframe. To remove all samples one at a time, range(0,109) (inclusive, exclusive).
#make a copy of the original otu dataframe read in above.
#drop the row of interest or of that specific iteration
#write the new otu table without said row to a new csv
#run pipeline on new csv
#read in the pipeline results to a new dataframe
#create empty dataframe
#for otu in all otus of interest add the results to the empty dataframe created above
#rename those columns to abundance_sample#extracted and metric_samplenumberextracted
#merge the original otu dataframe with the baseline abundances and metric to the new dataframe containing the abundances and metrics with the sample extracted
#empty the dataframe with the abundances and metrics from the pipeline run with the extracted sample
#some runs will come up with disjoint graphs, if this is the case then the columns for the sample which was removed will just be passed. Watch for removing sample 10 and 44

for row in range(10,11):
    otu_df_cp=otu_df.copy()
#    print(otu_df_cp.shape)
    otu_df_cp = otu_df_cp.drop([row])
#    print(otu_df_cp.shape)
    file='no_bad_arc_Brome_bacfunarc_otu_table'+str(row)
    otu_df_cp.to_csv(file+".csv",index=False)
    run_pipeline(file+".csv")
    pipeline_results=pandas.read_csv(file+"_results/"+file+'-None.csv',index_col=0)
#    print(pipeline_results)
    otu_21_df = pandas.DataFrame()
    try:
        for otu in og_otus:
            #print(pipeline_results.loc[otu])
            otu_21_df=otu_21_df.append(pipeline_results.loc[otu])
            #print(otu_21_df.columns)
        otu_21_df=otu_21_df.rename(columns={'abundances':'abundances_'+str(row),'metric':'metric_'+str(row)})
        #print(otu_21_df)
        og_21_df=og_21_df.merge(otu_21_df, left_index=True, right_index=True)
        #print(og_21_df)
        otu_21_df=otu_21_df.drop(otu_21_df.index,inplace=True)
    except:
        pass


print(og_21_df)

#write the merged file to a new csv
og_21_df.to_csv('results_degree_10.csv')




