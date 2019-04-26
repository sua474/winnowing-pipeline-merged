import pandas

#otu_table=pandas.read_csv('Brome_bacfunarc_otu_table.csv')
#otu_table=pandas.read_csv('no_bad_arc_Brome_bacfunarc_otu_table10.csv')
otu_table=pandas.read_csv('no_bad_arc_Brome_bacfunarc_otu_table44.csv',index_col=0)
#print(otu_table)
#cp_otu_table=otu_table.copy()

#bad_otus=['arc.Otu0350', 'arc.Otu0119','arc.Otu0108', 'arc.Otu0299']
#bad_otus=['arc.Otu0266', 'arc.Otu0307'] degree 10
#bad_otus=['arc.Otu0178', 'arc.Otu0010'] degree 44
#bad_otus=['arc.Otu0266', 'arc.Otu0307']
bad_otus=['arc.Otu0178', 'arc.Otu0010']
print(otu_table.shape)

for baddies in bad_otus:
    print((otu_table[baddies]))
    del otu_table[baddies]

#print(otu_table)

otu_table.to_csv('no_bad_arc_Brome_bacfunarc_otu_table44.csv',index=False)