import networkx
from networkx.algorithms.community import k_clique_communities
import pandas
import pprint


#otus of interest, this script will tell you what otu is in which community

otus=['bac.Otu3097',
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

print(len(otus))


#can do all centralities that you have the graphml file for, in the format centrality.graphml
centrality=['closeness']

#we did 3, 5, and 7
k_cliques=[3]


master_df=pandas.DataFrame(index=otus)
#print(master_df)
for cent in centrality:
    for k in k_cliques:
        master_otu_comm={}
        graph=networkx.read_graphml(cent+'.graphml')
        comm_k_clique=list(k_clique_communities(graph,k))
#        pprint.pprint(comm_k_clique)
#        print(len(comm_k_clique))

        for otu in otus:
            for i,comm in enumerate(comm_k_clique):
                if otu in comm:
                    master_otu_comm[otu]=i


    #pprint.pprint(master_otu_comm)
        del comm_k_clique
        cent_k_df=pandas.DataFrame.from_dict(master_otu_comm,orient='index',columns=[str(cent)+'_'+str(k)])
        #print(cent_k_df)
        del master_otu_comm
#        print(cent_k_df)
        master_df=pandas.concat([master_df,cent_k_df],axis=1,join_axes=[master_df.index])
        cent_k_df=cent_k_df.drop(cent_k_df.index,inplace=True)

print(master_df)


#prints the information to a csv
master_df.to_csv('community_master.csv')

