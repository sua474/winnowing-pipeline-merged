library(vegan)
library(scales) # For adjusting transparency
library(data.table)
library(zoo) # Interpolating data

rm(list = ls())

## BROME
## AB horizon, BFA
brome.sample <- read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/Brome_BFA_AB_sample_info.csv", header = T)

# AUC curves for subsetting
brome.dg.auc <- read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/brome.dg.auc.csv", header = T)

# Hellinger
bro.dg.auc100 <- 
  read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/ADD1_AUC100_MIC0.2_Brome_bacfunarc_dw_otu_table-graph_centrality-degree-selectallbyall-abundances.csv", header = T)
bro.dg.auc99 <- bro.dg.auc100[,c(1:brome.dg.auc[99,3])]; bro.dg.auc98 <- bro.dg.auc100[,c(1:brome.dg.auc[98,3])]; bro.dg.auc97 <- bro.dg.auc100[,c(1:brome.dg.auc[97,3])]; 
bro.dg.auc96 <- bro.dg.auc100[,c(1:brome.dg.auc[96,3])]; bro.dg.auc95 <- bro.dg.auc100[,c(1:brome.dg.auc[95,3])]; bro.dg.auc94 <- bro.dg.auc100[,c(1:brome.dg.auc[94,3])]; 
bro.dg.auc93 <- bro.dg.auc100[,c(1:brome.dg.auc[93,3])]; bro.dg.auc92 <- bro.dg.auc100[,c(1:brome.dg.auc[92,3])]; bro.dg.auc91 <- bro.dg.auc100[,c(1:brome.dg.auc[91,3])]; 
bro.dg.auc90 <- bro.dg.auc100[,c(1:brome.dg.auc[90,3])]; bro.dg.auc89 <- bro.dg.auc100[,c(1:brome.dg.auc[89,3])]; bro.dg.auc88 <- bro.dg.auc100[,c(1:brome.dg.auc[88,3])]; 
bro.dg.auc87 <- bro.dg.auc100[,c(1:brome.dg.auc[87,3])]; bro.dg.auc86 <- bro.dg.auc100[,c(1:brome.dg.auc[86,3])]; bro.dg.auc85 <- bro.dg.auc100[,c(1:brome.dg.auc[85,3])]; 
bro.dg.auc84 <- bro.dg.auc100[,c(1:brome.dg.auc[84,3])]; bro.dg.auc83 <- bro.dg.auc100[,c(1:brome.dg.auc[83,3])]; bro.dg.auc82 <- bro.dg.auc100[,c(1:brome.dg.auc[82,3])]; 
bro.dg.auc81 <- bro.dg.auc100[,c(1:brome.dg.auc[81,3])]; bro.dg.auc80 <- bro.dg.auc100[,c(1:brome.dg.auc[80,3])]; bro.dg.auc79 <- bro.dg.auc100[,c(1:brome.dg.auc[79,3])]; 
bro.dg.auc78 <- bro.dg.auc100[,c(1:brome.dg.auc[78,3])]; bro.dg.auc77 <- bro.dg.auc100[,c(1:brome.dg.auc[77,3])]; bro.dg.auc76 <- bro.dg.auc100[,c(1:brome.dg.auc[76,3])]; 
bro.dg.auc75 <- bro.dg.auc100[,c(1:brome.dg.auc[75,3])]; bro.dg.auc74 <- bro.dg.auc100[,c(1:brome.dg.auc[74,3])]; bro.dg.auc73 <- bro.dg.auc100[,c(1:brome.dg.auc[73,3])]; 
bro.dg.auc72 <- bro.dg.auc100[,c(1:brome.dg.auc[72,3])]; bro.dg.auc71 <- bro.dg.auc100[,c(1:brome.dg.auc[71,3])]; bro.dg.auc70 <- bro.dg.auc100[,c(1:brome.dg.auc[70,3])]; 
bro.dg.auc69 <- bro.dg.auc100[,c(1:brome.dg.auc[69,3])]; bro.dg.auc68 <- bro.dg.auc100[,c(1:brome.dg.auc[68,3])]; bro.dg.auc67 <- bro.dg.auc100[,c(1:brome.dg.auc[67,3])]; 
bro.dg.auc66 <- bro.dg.auc100[,c(1:brome.dg.auc[66,3])]; bro.dg.auc65 <- bro.dg.auc100[,c(1:brome.dg.auc[65,3])]; bro.dg.auc64 <- bro.dg.auc100[,c(1:brome.dg.auc[64,3])]; 
bro.dg.auc63 <- bro.dg.auc100[,c(1:brome.dg.auc[63,3])]; bro.dg.auc62 <- bro.dg.auc100[,c(1:brome.dg.auc[62,3])]; bro.dg.auc61 <- bro.dg.auc100[,c(1:brome.dg.auc[61,3])]; 
bro.dg.auc60 <- bro.dg.auc100[,c(1:brome.dg.auc[60,3])]; bro.dg.auc59 <- bro.dg.auc100[,c(1:brome.dg.auc[59,3])]; bro.dg.auc58 <- bro.dg.auc100[,c(1:brome.dg.auc[58,3])]; 
bro.dg.auc57 <- bro.dg.auc100[,c(1:brome.dg.auc[57,3])]; bro.dg.auc56 <- bro.dg.auc100[,c(1:brome.dg.auc[56,3])]; bro.dg.auc55 <- bro.dg.auc100[,c(1:brome.dg.auc[55,3])]; 
bro.dg.auc54 <- bro.dg.auc100[,c(1:brome.dg.auc[54,3])]; bro.dg.auc53 <- bro.dg.auc100[,c(1:brome.dg.auc[53,3])]; bro.dg.auc52 <- bro.dg.auc100[,c(1:brome.dg.auc[52,3])]; 
bro.dg.auc51 <- bro.dg.auc100[,c(1:brome.dg.auc[51,3])]; bro.dg.auc50 <- bro.dg.auc100[,c(1:brome.dg.auc[50,3])]; bro.dg.auc49 <- bro.dg.auc100[,c(1:brome.dg.auc[49,3])]; 
bro.dg.auc48 <- bro.dg.auc100[,c(1:brome.dg.auc[48,3])]; bro.dg.auc47 <- bro.dg.auc100[,c(1:brome.dg.auc[47,3])]; bro.dg.auc46 <- bro.dg.auc100[,c(1:brome.dg.auc[46,3])]; 
bro.dg.auc45 <- bro.dg.auc100[,c(1:brome.dg.auc[45,3])]; bro.dg.auc44 <- bro.dg.auc100[,c(1:brome.dg.auc[44,3])]; bro.dg.auc43 <- bro.dg.auc100[,c(1:brome.dg.auc[43,3])]; 
bro.dg.auc42 <- bro.dg.auc100[,c(1:brome.dg.auc[42,3])]; bro.dg.auc41 <- bro.dg.auc100[,c(1:brome.dg.auc[41,3])]; bro.dg.auc40 <- bro.dg.auc100[,c(1:brome.dg.auc[40,3])]; 
bro.dg.auc39 <- bro.dg.auc100[,c(1:brome.dg.auc[39,3])]; bro.dg.auc38 <- bro.dg.auc100[,c(1:brome.dg.auc[38,3])]; bro.dg.auc37 <- bro.dg.auc100[,c(1:brome.dg.auc[37,3])]; 
bro.dg.auc36 <- bro.dg.auc100[,c(1:brome.dg.auc[36,3])]; bro.dg.auc35 <- bro.dg.auc100[,c(1:brome.dg.auc[35,3])]; bro.dg.auc34 <- bro.dg.auc100[,c(1:brome.dg.auc[34,3])]; 
bro.dg.auc33 <- bro.dg.auc100[,c(1:brome.dg.auc[33,3])]; bro.dg.auc32 <- bro.dg.auc100[,c(1:brome.dg.auc[32,3])]; bro.dg.auc31 <- bro.dg.auc100[,c(1:brome.dg.auc[31,3])]; 
bro.dg.auc30 <- bro.dg.auc100[,c(1:brome.dg.auc[30,3])]; bro.dg.auc29 <- bro.dg.auc100[,c(1:brome.dg.auc[29,3])]; bro.dg.auc28 <- bro.dg.auc100[,c(1:brome.dg.auc[28,3])]; 
bro.dg.auc27 <- bro.dg.auc100[,c(1:brome.dg.auc[27,3])]; bro.dg.auc26 <- bro.dg.auc100[,c(1:brome.dg.auc[26,3])]; bro.dg.auc25 <- bro.dg.auc100[,c(1:brome.dg.auc[25,3])]; 
bro.dg.auc24 <- bro.dg.auc100[,c(1:brome.dg.auc[24,3])]; bro.dg.auc23 <- bro.dg.auc100[,c(1:brome.dg.auc[23,3])]; bro.dg.auc22 <- bro.dg.auc100[,c(1:brome.dg.auc[22,3])]; 
bro.dg.auc21 <- bro.dg.auc100[,c(1:brome.dg.auc[21,3])]; bro.dg.auc20 <- bro.dg.auc100[,c(1:brome.dg.auc[20,3])]; bro.dg.auc19 <- bro.dg.auc100[,c(1:brome.dg.auc[19,3])]; 
bro.dg.auc18 <- bro.dg.auc100[,c(1:brome.dg.auc[18,3])]; bro.dg.auc17 <- bro.dg.auc100[,c(1:brome.dg.auc[17,3])]; bro.dg.auc16 <- bro.dg.auc100[,c(1:brome.dg.auc[16,3])]; 
bro.dg.auc15 <- bro.dg.auc100[,c(1:brome.dg.auc[15,3])]; bro.dg.auc14 <- bro.dg.auc100[,c(1:brome.dg.auc[14,3])]; bro.dg.auc13 <- bro.dg.auc100[,c(1:brome.dg.auc[13,3])]; 
bro.dg.auc12 <- bro.dg.auc100[,c(1:brome.dg.auc[12,3])]; bro.dg.auc11 <- bro.dg.auc100[,c(1:brome.dg.auc[11,3])]; bro.dg.auc10 <- bro.dg.auc100[,c(1:brome.dg.auc[10,3])]; 
bro.dg.auc09 <- bro.dg.auc100[,c(1:brome.dg.auc[9,3])]; bro.dg.auc08 <- bro.dg.auc100[,c(1:brome.dg.auc[8,3])]; bro.dg.auc07 <- bro.dg.auc100[,c(1:brome.dg.auc[7,3])]; 
bro.dg.auc06 <- bro.dg.auc100[,c(1:brome.dg.auc[6,3])]; bro.dg.auc05 <- bro.dg.auc100[,c(1:brome.dg.auc[5,3])]; bro.dg.auc04 <- bro.dg.auc100[,c(1:brome.dg.auc[4,3])]; 
bro.dg.auc03 <- bro.dg.auc100[,c(1:brome.dg.auc[3,3])]; bro.dg.auc02 <- bro.dg.auc100[,c(1:brome.dg.auc[2,3])]; bro.dg.auc01 <- bro.dg.auc100[,c(1:brome.dg.auc[1,3])]

# Convert to Hellinger distance matrix
bro.dg.auc01.hel <- vegdist(decostand(bro.dg.auc01, "hellinger"), "euclidean"); bro.dg.auc02.hel <- vegdist(decostand(bro.dg.auc02, "hellinger"), "euclidean"); 
bro.dg.auc03.hel <- vegdist(decostand(bro.dg.auc03, "hellinger"), "euclidean"); bro.dg.auc04.hel <- vegdist(decostand(bro.dg.auc04, "hellinger"), "euclidean"); 
bro.dg.auc05.hel <- vegdist(decostand(bro.dg.auc05, "hellinger"), "euclidean"); bro.dg.auc06.hel <- vegdist(decostand(bro.dg.auc06, "hellinger"), "euclidean");
bro.dg.auc07.hel <- vegdist(decostand(bro.dg.auc07, "hellinger"), "euclidean"); bro.dg.auc08.hel <- vegdist(decostand(bro.dg.auc08, "hellinger"), "euclidean"); 
bro.dg.auc09.hel <- vegdist(decostand(bro.dg.auc09, "hellinger"), "euclidean"); bro.dg.auc10.hel <- vegdist(decostand(bro.dg.auc10, "hellinger"), "euclidean"); 
bro.dg.auc11.hel <- vegdist(decostand(bro.dg.auc11, "hellinger"), "euclidean"); bro.dg.auc12.hel <- vegdist(decostand(bro.dg.auc12, "hellinger"), "euclidean");
bro.dg.auc13.hel <- vegdist(decostand(bro.dg.auc13, "hellinger"), "euclidean"); bro.dg.auc14.hel <- vegdist(decostand(bro.dg.auc14, "hellinger"), "euclidean"); 
bro.dg.auc15.hel <- vegdist(decostand(bro.dg.auc15, "hellinger"), "euclidean"); bro.dg.auc16.hel <- vegdist(decostand(bro.dg.auc16, "hellinger"), "euclidean"); 
bro.dg.auc17.hel <- vegdist(decostand(bro.dg.auc17, "hellinger"), "euclidean"); bro.dg.auc18.hel <- vegdist(decostand(bro.dg.auc18, "hellinger"), "euclidean");
bro.dg.auc19.hel <- vegdist(decostand(bro.dg.auc19, "hellinger"), "euclidean"); bro.dg.auc20.hel <- vegdist(decostand(bro.dg.auc20, "hellinger"), "euclidean"); 
bro.dg.auc21.hel <- vegdist(decostand(bro.dg.auc21, "hellinger"), "euclidean"); bro.dg.auc22.hel <- vegdist(decostand(bro.dg.auc22, "hellinger"), "euclidean"); 
bro.dg.auc23.hel <- vegdist(decostand(bro.dg.auc23, "hellinger"), "euclidean"); bro.dg.auc24.hel <- vegdist(decostand(bro.dg.auc24, "hellinger"), "euclidean");
bro.dg.auc25.hel <- vegdist(decostand(bro.dg.auc25, "hellinger"), "euclidean"); bro.dg.auc26.hel <- vegdist(decostand(bro.dg.auc26, "hellinger"), "euclidean"); 
bro.dg.auc27.hel <- vegdist(decostand(bro.dg.auc27, "hellinger"), "euclidean"); bro.dg.auc28.hel <- vegdist(decostand(bro.dg.auc28, "hellinger"), "euclidean"); 
bro.dg.auc29.hel <- vegdist(decostand(bro.dg.auc29, "hellinger"), "euclidean"); bro.dg.auc30.hel <- vegdist(decostand(bro.dg.auc30, "hellinger"), "euclidean");
bro.dg.auc31.hel <- vegdist(decostand(bro.dg.auc31, "hellinger"), "euclidean"); bro.dg.auc32.hel <- vegdist(decostand(bro.dg.auc32, "hellinger"), "euclidean"); 
bro.dg.auc33.hel <- vegdist(decostand(bro.dg.auc33, "hellinger"), "euclidean"); bro.dg.auc34.hel <- vegdist(decostand(bro.dg.auc34, "hellinger"), "euclidean"); 
bro.dg.auc35.hel <- vegdist(decostand(bro.dg.auc35, "hellinger"), "euclidean"); bro.dg.auc36.hel <- vegdist(decostand(bro.dg.auc36, "hellinger"), "euclidean");
bro.dg.auc37.hel <- vegdist(decostand(bro.dg.auc37, "hellinger"), "euclidean"); bro.dg.auc38.hel <- vegdist(decostand(bro.dg.auc38, "hellinger"), "euclidean"); 
bro.dg.auc39.hel <- vegdist(decostand(bro.dg.auc39, "hellinger"), "euclidean"); bro.dg.auc40.hel <- vegdist(decostand(bro.dg.auc40, "hellinger"), "euclidean"); 
bro.dg.auc41.hel <- vegdist(decostand(bro.dg.auc41, "hellinger"), "euclidean"); bro.dg.auc42.hel <- vegdist(decostand(bro.dg.auc42, "hellinger"), "euclidean");
bro.dg.auc43.hel <- vegdist(decostand(bro.dg.auc43, "hellinger"), "euclidean"); bro.dg.auc44.hel <- vegdist(decostand(bro.dg.auc44, "hellinger"), "euclidean"); 
bro.dg.auc45.hel <- vegdist(decostand(bro.dg.auc45, "hellinger"), "euclidean"); bro.dg.auc46.hel <- vegdist(decostand(bro.dg.auc46, "hellinger"), "euclidean"); 
bro.dg.auc47.hel <- vegdist(decostand(bro.dg.auc47, "hellinger"), "euclidean"); bro.dg.auc48.hel <- vegdist(decostand(bro.dg.auc48, "hellinger"), "euclidean");
bro.dg.auc49.hel <- vegdist(decostand(bro.dg.auc49, "hellinger"), "euclidean"); bro.dg.auc50.hel <- vegdist(decostand(bro.dg.auc50, "hellinger"), "euclidean"); 
bro.dg.auc51.hel <- vegdist(decostand(bro.dg.auc51, "hellinger"), "euclidean"); bro.dg.auc52.hel <- vegdist(decostand(bro.dg.auc52, "hellinger"), "euclidean"); 
bro.dg.auc53.hel <- vegdist(decostand(bro.dg.auc53, "hellinger"), "euclidean"); bro.dg.auc54.hel <- vegdist(decostand(bro.dg.auc54, "hellinger"), "euclidean");
bro.dg.auc55.hel <- vegdist(decostand(bro.dg.auc55, "hellinger"), "euclidean"); bro.dg.auc56.hel <- vegdist(decostand(bro.dg.auc56, "hellinger"), "euclidean"); 
bro.dg.auc57.hel <- vegdist(decostand(bro.dg.auc57, "hellinger"), "euclidean"); bro.dg.auc58.hel <- vegdist(decostand(bro.dg.auc58, "hellinger"), "euclidean"); 
bro.dg.auc59.hel <- vegdist(decostand(bro.dg.auc59, "hellinger"), "euclidean"); bro.dg.auc60.hel <- vegdist(decostand(bro.dg.auc60, "hellinger"), "euclidean");
bro.dg.auc61.hel <- vegdist(decostand(bro.dg.auc61, "hellinger"), "euclidean"); bro.dg.auc62.hel <- vegdist(decostand(bro.dg.auc62, "hellinger"), "euclidean"); 
bro.dg.auc63.hel <- vegdist(decostand(bro.dg.auc63, "hellinger"), "euclidean"); bro.dg.auc64.hel <- vegdist(decostand(bro.dg.auc64, "hellinger"), "euclidean"); 
bro.dg.auc65.hel <- vegdist(decostand(bro.dg.auc65, "hellinger"), "euclidean"); bro.dg.auc66.hel <- vegdist(decostand(bro.dg.auc66, "hellinger"), "euclidean");
bro.dg.auc67.hel <- vegdist(decostand(bro.dg.auc67, "hellinger"), "euclidean"); bro.dg.auc68.hel <- vegdist(decostand(bro.dg.auc68, "hellinger"), "euclidean"); 
bro.dg.auc69.hel <- vegdist(decostand(bro.dg.auc69, "hellinger"), "euclidean"); bro.dg.auc70.hel <- vegdist(decostand(bro.dg.auc70, "hellinger"), "euclidean"); 
bro.dg.auc71.hel <- vegdist(decostand(bro.dg.auc71, "hellinger"), "euclidean"); bro.dg.auc72.hel <- vegdist(decostand(bro.dg.auc72, "hellinger"), "euclidean");
bro.dg.auc73.hel <- vegdist(decostand(bro.dg.auc73, "hellinger"), "euclidean"); bro.dg.auc74.hel <- vegdist(decostand(bro.dg.auc74, "hellinger"), "euclidean"); 
bro.dg.auc75.hel <- vegdist(decostand(bro.dg.auc75, "hellinger"), "euclidean"); bro.dg.auc76.hel <- vegdist(decostand(bro.dg.auc76, "hellinger"), "euclidean"); 
bro.dg.auc77.hel <- vegdist(decostand(bro.dg.auc77, "hellinger"), "euclidean"); bro.dg.auc78.hel <- vegdist(decostand(bro.dg.auc78, "hellinger"), "euclidean");
bro.dg.auc79.hel <- vegdist(decostand(bro.dg.auc79, "hellinger"), "euclidean"); bro.dg.auc80.hel <- vegdist(decostand(bro.dg.auc80, "hellinger"), "euclidean"); 
bro.dg.auc81.hel <- vegdist(decostand(bro.dg.auc81, "hellinger"), "euclidean"); bro.dg.auc82.hel <- vegdist(decostand(bro.dg.auc82, "hellinger"), "euclidean"); 
bro.dg.auc83.hel <- vegdist(decostand(bro.dg.auc83, "hellinger"), "euclidean"); bro.dg.auc84.hel <- vegdist(decostand(bro.dg.auc84, "hellinger"), "euclidean");
bro.dg.auc85.hel <- vegdist(decostand(bro.dg.auc85, "hellinger"), "euclidean"); bro.dg.auc86.hel <- vegdist(decostand(bro.dg.auc86, "hellinger"), "euclidean"); 
bro.dg.auc87.hel <- vegdist(decostand(bro.dg.auc87, "hellinger"), "euclidean"); bro.dg.auc88.hel <- vegdist(decostand(bro.dg.auc88, "hellinger"), "euclidean"); 
bro.dg.auc89.hel <- vegdist(decostand(bro.dg.auc89, "hellinger"), "euclidean"); bro.dg.auc90.hel <- vegdist(decostand(bro.dg.auc90, "hellinger"), "euclidean");
bro.dg.auc91.hel <- vegdist(decostand(bro.dg.auc91, "hellinger"), "euclidean"); bro.dg.auc92.hel <- vegdist(decostand(bro.dg.auc92, "hellinger"), "euclidean"); 
bro.dg.auc93.hel <- vegdist(decostand(bro.dg.auc93, "hellinger"), "euclidean"); bro.dg.auc94.hel <- vegdist(decostand(bro.dg.auc94, "hellinger"), "euclidean"); 
bro.dg.auc95.hel <- vegdist(decostand(bro.dg.auc95, "hellinger"), "euclidean"); bro.dg.auc96.hel <- vegdist(decostand(bro.dg.auc96, "hellinger"), "euclidean");
bro.dg.auc97.hel <- vegdist(decostand(bro.dg.auc97, "hellinger"), "euclidean"); bro.dg.auc98.hel <- vegdist(decostand(bro.dg.auc98, "hellinger"), "euclidean"); 
bro.dg.auc99.hel <- vegdist(decostand(bro.dg.auc99, "hellinger"), "euclidean"); bro.dg.auc100.hel <- vegdist(decostand(bro.dg.auc100, "hellinger"), "euclidean")

#############
# PERMANOVA
set.seed(1); bro.dg.auc01.pman <- adonis(bro.dg.auc01.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc02.pman <- adonis(bro.dg.auc02.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc03.pman <- adonis(bro.dg.auc03.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc04.pman <- adonis(bro.dg.auc04.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc05.pman <- adonis(bro.dg.auc05.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc06.pman <- adonis(bro.dg.auc06.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc07.pman <- adonis(bro.dg.auc07.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc08.pman <- adonis(bro.dg.auc08.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc09.pman <- adonis(bro.dg.auc09.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc10.pman <- adonis(bro.dg.auc10.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc11.pman <- adonis(bro.dg.auc11.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc12.pman <- adonis(bro.dg.auc12.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc13.pman <- adonis(bro.dg.auc13.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc14.pman <- adonis(bro.dg.auc14.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc15.pman <- adonis(bro.dg.auc15.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc16.pman <- adonis(bro.dg.auc16.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc17.pman <- adonis(bro.dg.auc17.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc18.pman <- adonis(bro.dg.auc18.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc19.pman <- adonis(bro.dg.auc19.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc20.pman <- adonis(bro.dg.auc20.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc21.pman <- adonis(bro.dg.auc21.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc22.pman <- adonis(bro.dg.auc22.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc23.pman <- adonis(bro.dg.auc23.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc24.pman <- adonis(bro.dg.auc24.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc25.pman <- adonis(bro.dg.auc25.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc26.pman <- adonis(bro.dg.auc26.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc27.pman <- adonis(bro.dg.auc27.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc28.pman <- adonis(bro.dg.auc28.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc29.pman <- adonis(bro.dg.auc29.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc30.pman <- adonis(bro.dg.auc30.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc31.pman <- adonis(bro.dg.auc31.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc32.pman <- adonis(bro.dg.auc32.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc33.pman <- adonis(bro.dg.auc33.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc34.pman <- adonis(bro.dg.auc34.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc35.pman <- adonis(bro.dg.auc35.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc36.pman <- adonis(bro.dg.auc36.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc37.pman <- adonis(bro.dg.auc37.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc38.pman <- adonis(bro.dg.auc38.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc39.pman <- adonis(bro.dg.auc39.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc40.pman <- adonis(bro.dg.auc40.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc41.pman <- adonis(bro.dg.auc41.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc42.pman <- adonis(bro.dg.auc42.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc43.pman <- adonis(bro.dg.auc43.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc44.pman <- adonis(bro.dg.auc44.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc45.pman <- adonis(bro.dg.auc45.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc46.pman <- adonis(bro.dg.auc46.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc47.pman <- adonis(bro.dg.auc47.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc48.pman <- adonis(bro.dg.auc48.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc49.pman <- adonis(bro.dg.auc49.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc50.pman <- adonis(bro.dg.auc50.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc51.pman <- adonis(bro.dg.auc51.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc52.pman <- adonis(bro.dg.auc52.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc53.pman <- adonis(bro.dg.auc53.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc54.pman <- adonis(bro.dg.auc54.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc55.pman <- adonis(bro.dg.auc55.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc56.pman <- adonis(bro.dg.auc56.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc57.pman <- adonis(bro.dg.auc57.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc58.pman <- adonis(bro.dg.auc58.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc59.pman <- adonis(bro.dg.auc59.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc60.pman <- adonis(bro.dg.auc60.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc61.pman <- adonis(bro.dg.auc61.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc62.pman <- adonis(bro.dg.auc62.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc63.pman <- adonis(bro.dg.auc63.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc64.pman <- adonis(bro.dg.auc64.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc65.pman <- adonis(bro.dg.auc65.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc66.pman <- adonis(bro.dg.auc66.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc67.pman <- adonis(bro.dg.auc67.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc68.pman <- adonis(bro.dg.auc68.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc69.pman <- adonis(bro.dg.auc69.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc70.pman <- adonis(bro.dg.auc70.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc71.pman <- adonis(bro.dg.auc71.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc72.pman <- adonis(bro.dg.auc72.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc73.pman <- adonis(bro.dg.auc73.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc74.pman <- adonis(bro.dg.auc74.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc75.pman <- adonis(bro.dg.auc75.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc76.pman <- adonis(bro.dg.auc76.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc77.pman <- adonis(bro.dg.auc77.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc78.pman <- adonis(bro.dg.auc78.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc79.pman <- adonis(bro.dg.auc79.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc80.pman <- adonis(bro.dg.auc80.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc81.pman <- adonis(bro.dg.auc81.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc82.pman <- adonis(bro.dg.auc82.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc83.pman <- adonis(bro.dg.auc83.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc84.pman <- adonis(bro.dg.auc84.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc85.pman <- adonis(bro.dg.auc85.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc86.pman <- adonis(bro.dg.auc86.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc87.pman <- adonis(bro.dg.auc87.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc88.pman <- adonis(bro.dg.auc88.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc89.pman <- adonis(bro.dg.auc89.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc90.pman <- adonis(bro.dg.auc90.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc91.pman <- adonis(bro.dg.auc91.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc92.pman <- adonis(bro.dg.auc92.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc93.pman <- adonis(bro.dg.auc93.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc94.pman <- adonis(bro.dg.auc94.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc95.pman <- adonis(bro.dg.auc95.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc96.pman <- adonis(bro.dg.auc96.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.dg.auc97.pman <- adonis(bro.dg.auc97.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc98.pman <- adonis(bro.dg.auc98.hel ~ brome.sample$type, permutations=999);
set.seed(1); bro.dg.auc99.pman <- adonis(bro.dg.auc99.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.dg.auc100.pman <- adonis(bro.dg.auc100.hel ~ brome.sample$type, permutations=999) 

# Create a table of the outputs
brome.dg.permanova <- data.frame(test = c(paste("auc0", seq(1,9,1), sep = ""),
                                          paste("auc", seq(10,99,1), sep = ""),
                                          "auc100"),
                                 order = rep(c(1:100), 1),
                                 auc = c(seq(1,100,1)),
                                 SumsOfSqs = c(bro.dg.auc01.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc02.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc03.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc04.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc05.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc06.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc07.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc08.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc09.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc10.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc11.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc12.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc13.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc14.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc15.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc16.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc17.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc18.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc19.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc20.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc21.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc22.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc23.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc24.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc25.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc26.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc27.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc28.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc29.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc30.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc31.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc32.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc33.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc34.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc35.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc36.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc37.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc38.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc39.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc40.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc41.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc42.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc43.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc44.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc45.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc46.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc47.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc48.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc49.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc50.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc51.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc52.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc53.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc54.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc55.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc56.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc57.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc58.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc59.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc60.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc61.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc62.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc63.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc64.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc65.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc66.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc67.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc68.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc69.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc70.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc71.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc72.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc73.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc74.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc75.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc76.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc77.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc78.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc79.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc80.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc81.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc82.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc83.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc84.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc85.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc86.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc87.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc88.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc89.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc90.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc91.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc92.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc93.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc94.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc95.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc96.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.dg.auc97.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc98.pman$aov.tab$SumsOfSqs[[1]], bro.dg.auc99.pman$aov.tab$SumsOfSqs[[1]],
                                               bro.dg.auc100.pman$aov.tab$SumsOfSqs[[1]]),
                                 
                                 MeanSqs = c(bro.dg.auc01.pman$aov.tab$MeanSqs[[1]], bro.dg.auc02.pman$aov.tab$MeanSqs[[1]], bro.dg.auc03.pman$aov.tab$MeanSqs[[1]], bro.dg.auc04.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc05.pman$aov.tab$MeanSqs[[1]], bro.dg.auc06.pman$aov.tab$MeanSqs[[1]], bro.dg.auc07.pman$aov.tab$MeanSqs[[1]], bro.dg.auc08.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc09.pman$aov.tab$MeanSqs[[1]], bro.dg.auc10.pman$aov.tab$MeanSqs[[1]], bro.dg.auc11.pman$aov.tab$MeanSqs[[1]], bro.dg.auc12.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc13.pman$aov.tab$MeanSqs[[1]], bro.dg.auc14.pman$aov.tab$MeanSqs[[1]], bro.dg.auc15.pman$aov.tab$MeanSqs[[1]], bro.dg.auc16.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc17.pman$aov.tab$MeanSqs[[1]], bro.dg.auc18.pman$aov.tab$MeanSqs[[1]], bro.dg.auc19.pman$aov.tab$MeanSqs[[1]], bro.dg.auc20.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc21.pman$aov.tab$MeanSqs[[1]], bro.dg.auc22.pman$aov.tab$MeanSqs[[1]], bro.dg.auc23.pman$aov.tab$MeanSqs[[1]], bro.dg.auc24.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc25.pman$aov.tab$MeanSqs[[1]], bro.dg.auc26.pman$aov.tab$MeanSqs[[1]], bro.dg.auc27.pman$aov.tab$MeanSqs[[1]], bro.dg.auc28.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc29.pman$aov.tab$MeanSqs[[1]], bro.dg.auc30.pman$aov.tab$MeanSqs[[1]], bro.dg.auc31.pman$aov.tab$MeanSqs[[1]], bro.dg.auc32.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc33.pman$aov.tab$MeanSqs[[1]], bro.dg.auc34.pman$aov.tab$MeanSqs[[1]], bro.dg.auc35.pman$aov.tab$MeanSqs[[1]], bro.dg.auc36.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc37.pman$aov.tab$MeanSqs[[1]], bro.dg.auc38.pman$aov.tab$MeanSqs[[1]], bro.dg.auc39.pman$aov.tab$MeanSqs[[1]], bro.dg.auc40.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc41.pman$aov.tab$MeanSqs[[1]], bro.dg.auc42.pman$aov.tab$MeanSqs[[1]], bro.dg.auc43.pman$aov.tab$MeanSqs[[1]], bro.dg.auc44.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc45.pman$aov.tab$MeanSqs[[1]], bro.dg.auc46.pman$aov.tab$MeanSqs[[1]], bro.dg.auc47.pman$aov.tab$MeanSqs[[1]], bro.dg.auc48.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc49.pman$aov.tab$MeanSqs[[1]], bro.dg.auc50.pman$aov.tab$MeanSqs[[1]], bro.dg.auc51.pman$aov.tab$MeanSqs[[1]], bro.dg.auc52.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc53.pman$aov.tab$MeanSqs[[1]], bro.dg.auc54.pman$aov.tab$MeanSqs[[1]], bro.dg.auc55.pman$aov.tab$MeanSqs[[1]], bro.dg.auc56.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc57.pman$aov.tab$MeanSqs[[1]], bro.dg.auc58.pman$aov.tab$MeanSqs[[1]], bro.dg.auc59.pman$aov.tab$MeanSqs[[1]], bro.dg.auc60.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc61.pman$aov.tab$MeanSqs[[1]], bro.dg.auc62.pman$aov.tab$MeanSqs[[1]], bro.dg.auc63.pman$aov.tab$MeanSqs[[1]], bro.dg.auc64.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc65.pman$aov.tab$MeanSqs[[1]], bro.dg.auc66.pman$aov.tab$MeanSqs[[1]], bro.dg.auc67.pman$aov.tab$MeanSqs[[1]], bro.dg.auc68.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc69.pman$aov.tab$MeanSqs[[1]], bro.dg.auc70.pman$aov.tab$MeanSqs[[1]], bro.dg.auc71.pman$aov.tab$MeanSqs[[1]], bro.dg.auc72.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc73.pman$aov.tab$MeanSqs[[1]], bro.dg.auc74.pman$aov.tab$MeanSqs[[1]], bro.dg.auc75.pman$aov.tab$MeanSqs[[1]], bro.dg.auc76.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc77.pman$aov.tab$MeanSqs[[1]], bro.dg.auc78.pman$aov.tab$MeanSqs[[1]], bro.dg.auc79.pman$aov.tab$MeanSqs[[1]], bro.dg.auc80.pman$aov.tab$MeanSqs[[1]],
                                             bro.dg.auc81.pman$aov.tab$MeanSqs[[1]], bro.dg.auc82.pman$aov.tab$MeanSqs[[1]], bro.dg.auc83.pman$aov.tab$MeanSqs[[1]], bro.dg.auc84.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc85.pman$aov.tab$MeanSqs[[1]], bro.dg.auc86.pman$aov.tab$MeanSqs[[1]], bro.dg.auc87.pman$aov.tab$MeanSqs[[1]], bro.dg.auc88.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc89.pman$aov.tab$MeanSqs[[1]], bro.dg.auc90.pman$aov.tab$MeanSqs[[1]], bro.dg.auc91.pman$aov.tab$MeanSqs[[1]], bro.dg.auc92.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc93.pman$aov.tab$MeanSqs[[1]], bro.dg.auc94.pman$aov.tab$MeanSqs[[1]], bro.dg.auc95.pman$aov.tab$MeanSqs[[1]], bro.dg.auc96.pman$aov.tab$MeanSqs[[1]], 
                                             bro.dg.auc97.pman$aov.tab$MeanSqs[[1]], bro.dg.auc98.pman$aov.tab$MeanSqs[[1]], bro.dg.auc99.pman$aov.tab$MeanSqs[[1]], bro.dg.auc100.pman$aov.tab$MeanSqs[[1]]),
                                 
                                 F.model = c(bro.dg.auc01.pman$aov.tab$F.Model[1], bro.dg.auc02.pman$aov.tab$F.Model[1], bro.dg.auc03.pman$aov.tab$F.Model[1], bro.dg.auc04.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc05.pman$aov.tab$F.Model[1], bro.dg.auc06.pman$aov.tab$F.Model[1], bro.dg.auc07.pman$aov.tab$F.Model[1], bro.dg.auc08.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc09.pman$aov.tab$F.Model[1], bro.dg.auc10.pman$aov.tab$F.Model[1], bro.dg.auc11.pman$aov.tab$F.Model[1], bro.dg.auc12.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc13.pman$aov.tab$F.Model[1], bro.dg.auc14.pman$aov.tab$F.Model[1], bro.dg.auc15.pman$aov.tab$F.Model[1], bro.dg.auc16.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc17.pman$aov.tab$F.Model[1], bro.dg.auc18.pman$aov.tab$F.Model[1], bro.dg.auc19.pman$aov.tab$F.Model[1], bro.dg.auc20.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc21.pman$aov.tab$F.Model[1], bro.dg.auc22.pman$aov.tab$F.Model[1], bro.dg.auc23.pman$aov.tab$F.Model[1], bro.dg.auc24.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc25.pman$aov.tab$F.Model[1], bro.dg.auc26.pman$aov.tab$F.Model[1], bro.dg.auc27.pman$aov.tab$F.Model[1], bro.dg.auc28.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc29.pman$aov.tab$F.Model[1], bro.dg.auc30.pman$aov.tab$F.Model[1], bro.dg.auc31.pman$aov.tab$F.Model[1], bro.dg.auc32.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc33.pman$aov.tab$F.Model[1], bro.dg.auc34.pman$aov.tab$F.Model[1], bro.dg.auc35.pman$aov.tab$F.Model[1], bro.dg.auc36.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc37.pman$aov.tab$F.Model[1], bro.dg.auc38.pman$aov.tab$F.Model[1], bro.dg.auc39.pman$aov.tab$F.Model[1], bro.dg.auc40.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc41.pman$aov.tab$F.Model[1], bro.dg.auc42.pman$aov.tab$F.Model[1], bro.dg.auc43.pman$aov.tab$F.Model[1], bro.dg.auc44.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc45.pman$aov.tab$F.Model[1], bro.dg.auc46.pman$aov.tab$F.Model[1], bro.dg.auc47.pman$aov.tab$F.Model[1], bro.dg.auc48.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc49.pman$aov.tab$F.Model[1], bro.dg.auc50.pman$aov.tab$F.Model[1], bro.dg.auc51.pman$aov.tab$F.Model[1], bro.dg.auc52.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc53.pman$aov.tab$F.Model[1], bro.dg.auc54.pman$aov.tab$F.Model[1], bro.dg.auc55.pman$aov.tab$F.Model[1], bro.dg.auc56.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc57.pman$aov.tab$F.Model[1], bro.dg.auc58.pman$aov.tab$F.Model[1], bro.dg.auc59.pman$aov.tab$F.Model[1], bro.dg.auc60.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc61.pman$aov.tab$F.Model[1], bro.dg.auc62.pman$aov.tab$F.Model[1], bro.dg.auc63.pman$aov.tab$F.Model[1], bro.dg.auc64.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc65.pman$aov.tab$F.Model[1], bro.dg.auc66.pman$aov.tab$F.Model[1], bro.dg.auc67.pman$aov.tab$F.Model[1], bro.dg.auc68.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc69.pman$aov.tab$F.Model[1], bro.dg.auc70.pman$aov.tab$F.Model[1], bro.dg.auc71.pman$aov.tab$F.Model[1], bro.dg.auc72.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc73.pman$aov.tab$F.Model[1], bro.dg.auc74.pman$aov.tab$F.Model[1], bro.dg.auc75.pman$aov.tab$F.Model[1], bro.dg.auc76.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc77.pman$aov.tab$F.Model[1], bro.dg.auc78.pman$aov.tab$F.Model[1], bro.dg.auc79.pman$aov.tab$F.Model[1], bro.dg.auc80.pman$aov.tab$F.Model[1],
                                             bro.dg.auc81.pman$aov.tab$F.Model[1], bro.dg.auc82.pman$aov.tab$F.Model[1], bro.dg.auc83.pman$aov.tab$F.Model[1], bro.dg.auc84.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc85.pman$aov.tab$F.Model[1], bro.dg.auc86.pman$aov.tab$F.Model[1], bro.dg.auc87.pman$aov.tab$F.Model[1], bro.dg.auc88.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc89.pman$aov.tab$F.Model[1], bro.dg.auc90.pman$aov.tab$F.Model[1], bro.dg.auc91.pman$aov.tab$F.Model[1], bro.dg.auc92.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc93.pman$aov.tab$F.Model[1], bro.dg.auc94.pman$aov.tab$F.Model[1], bro.dg.auc95.pman$aov.tab$F.Model[1], bro.dg.auc96.pman$aov.tab$F.Model[1], 
                                             bro.dg.auc97.pman$aov.tab$F.Model[1], bro.dg.auc98.pman$aov.tab$F.Model[1], bro.dg.auc99.pman$aov.tab$F.Model[1], bro.dg.auc100.pman$aov.tab$F.Model[1]),
                                 
                                 R2 = c(bro.dg.auc01.pman$aov.tab$R2[1], bro.dg.auc02.pman$aov.tab$R2[1], bro.dg.auc03.pman$aov.tab$R2[1], bro.dg.auc04.pman$aov.tab$R2[1], 
                                        bro.dg.auc05.pman$aov.tab$R2[1], bro.dg.auc06.pman$aov.tab$R2[1], bro.dg.auc07.pman$aov.tab$R2[1], bro.dg.auc08.pman$aov.tab$R2[1], 
                                        bro.dg.auc09.pman$aov.tab$R2[1], bro.dg.auc10.pman$aov.tab$R2[1], bro.dg.auc11.pman$aov.tab$R2[1], bro.dg.auc12.pman$aov.tab$R2[1], 
                                        bro.dg.auc13.pman$aov.tab$R2[1], bro.dg.auc14.pman$aov.tab$R2[1], bro.dg.auc15.pman$aov.tab$R2[1], bro.dg.auc16.pman$aov.tab$R2[1], 
                                        bro.dg.auc17.pman$aov.tab$R2[1], bro.dg.auc18.pman$aov.tab$R2[1], bro.dg.auc19.pman$aov.tab$R2[1], bro.dg.auc20.pman$aov.tab$R2[1], 
                                        bro.dg.auc21.pman$aov.tab$R2[1], bro.dg.auc22.pman$aov.tab$R2[1], bro.dg.auc23.pman$aov.tab$R2[1], bro.dg.auc24.pman$aov.tab$R2[1], 
                                        bro.dg.auc25.pman$aov.tab$R2[1], bro.dg.auc26.pman$aov.tab$R2[1], bro.dg.auc27.pman$aov.tab$R2[1], bro.dg.auc28.pman$aov.tab$R2[1], 
                                        bro.dg.auc29.pman$aov.tab$R2[1], bro.dg.auc30.pman$aov.tab$R2[1], bro.dg.auc31.pman$aov.tab$R2[1], bro.dg.auc32.pman$aov.tab$R2[1], 
                                        bro.dg.auc33.pman$aov.tab$R2[1], bro.dg.auc34.pman$aov.tab$R2[1], bro.dg.auc35.pman$aov.tab$R2[1], bro.dg.auc36.pman$aov.tab$R2[1], 
                                        bro.dg.auc37.pman$aov.tab$R2[1], bro.dg.auc38.pman$aov.tab$R2[1], bro.dg.auc39.pman$aov.tab$R2[1], bro.dg.auc40.pman$aov.tab$R2[1], 
                                        bro.dg.auc41.pman$aov.tab$R2[1], bro.dg.auc42.pman$aov.tab$R2[1], bro.dg.auc43.pman$aov.tab$R2[1], bro.dg.auc44.pman$aov.tab$R2[1], 
                                        bro.dg.auc45.pman$aov.tab$R2[1], bro.dg.auc46.pman$aov.tab$R2[1], bro.dg.auc47.pman$aov.tab$R2[1], bro.dg.auc48.pman$aov.tab$R2[1], 
                                        bro.dg.auc49.pman$aov.tab$R2[1], bro.dg.auc50.pman$aov.tab$R2[1], bro.dg.auc51.pman$aov.tab$R2[1], bro.dg.auc52.pman$aov.tab$R2[1], 
                                        bro.dg.auc53.pman$aov.tab$R2[1], bro.dg.auc54.pman$aov.tab$R2[1], bro.dg.auc55.pman$aov.tab$R2[1], bro.dg.auc56.pman$aov.tab$R2[1], 
                                        bro.dg.auc57.pman$aov.tab$R2[1], bro.dg.auc58.pman$aov.tab$R2[1], bro.dg.auc59.pman$aov.tab$R2[1], bro.dg.auc60.pman$aov.tab$R2[1], 
                                        bro.dg.auc61.pman$aov.tab$R2[1], bro.dg.auc62.pman$aov.tab$R2[1], bro.dg.auc63.pman$aov.tab$R2[1], bro.dg.auc64.pman$aov.tab$R2[1], 
                                        bro.dg.auc65.pman$aov.tab$R2[1], bro.dg.auc66.pman$aov.tab$R2[1], bro.dg.auc67.pman$aov.tab$R2[1], bro.dg.auc68.pman$aov.tab$R2[1], 
                                        bro.dg.auc69.pman$aov.tab$R2[1], bro.dg.auc70.pman$aov.tab$R2[1], bro.dg.auc71.pman$aov.tab$R2[1], bro.dg.auc72.pman$aov.tab$R2[1], 
                                        bro.dg.auc73.pman$aov.tab$R2[1], bro.dg.auc74.pman$aov.tab$R2[1], bro.dg.auc75.pman$aov.tab$R2[1], bro.dg.auc76.pman$aov.tab$R2[1], 
                                        bro.dg.auc77.pman$aov.tab$R2[1], bro.dg.auc78.pman$aov.tab$R2[1], bro.dg.auc79.pman$aov.tab$R2[1], bro.dg.auc80.pman$aov.tab$R2[1],
                                        bro.dg.auc81.pman$aov.tab$R2[1], bro.dg.auc82.pman$aov.tab$R2[1], bro.dg.auc83.pman$aov.tab$R2[1], bro.dg.auc84.pman$aov.tab$R2[1], 
                                        bro.dg.auc85.pman$aov.tab$R2[1], bro.dg.auc86.pman$aov.tab$R2[1], bro.dg.auc87.pman$aov.tab$R2[1], bro.dg.auc88.pman$aov.tab$R2[1], 
                                        bro.dg.auc89.pman$aov.tab$R2[1], bro.dg.auc90.pman$aov.tab$R2[1], bro.dg.auc91.pman$aov.tab$R2[1], bro.dg.auc92.pman$aov.tab$R2[1], 
                                        bro.dg.auc93.pman$aov.tab$R2[1], bro.dg.auc94.pman$aov.tab$R2[1], bro.dg.auc95.pman$aov.tab$R2[1], bro.dg.auc96.pman$aov.tab$R2[1], 
                                        bro.dg.auc97.pman$aov.tab$R2[1], bro.dg.auc98.pman$aov.tab$R2[1], bro.dg.auc99.pman$aov.tab$R2[1], bro.dg.auc100.pman$aov.tab$R2[1]),
                                 
                                 Pval = c(bro.dg.auc01.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc02.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc03.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc04.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc05.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc06.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc07.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc08.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc09.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc10.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc11.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc12.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc13.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc14.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc15.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc16.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc17.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc18.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc19.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc20.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc21.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc22.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc23.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc24.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc25.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc26.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc27.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc28.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc29.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc30.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc31.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc32.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc33.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc34.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc35.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc36.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc37.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc38.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc39.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc40.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc41.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc42.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc43.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc44.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc45.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc46.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc47.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc48.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc49.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc50.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc51.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc52.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc53.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc54.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc55.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc56.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc57.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc58.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc59.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc60.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc61.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc62.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc63.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc64.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc65.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc66.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc67.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc68.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc69.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc70.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc71.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc72.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc73.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc74.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc75.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc76.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc77.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc78.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc79.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc80.pman$aov.tab$`Pr(>F)`[1],
                                          bro.dg.auc81.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc82.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc83.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc84.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc85.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc86.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc87.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc88.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc89.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc90.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc91.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc92.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc93.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc94.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc95.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc96.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.dg.auc97.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc98.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc99.pman$aov.tab$`Pr(>F)`[1], bro.dg.auc100.pman$aov.tab$`Pr(>F)`[1]),
                                 
                                 N.taxa = c(ncol(bro.dg.auc01), ncol(bro.dg.auc02), ncol(bro.dg.auc03), ncol(bro.dg.auc04), ncol(bro.dg.auc05), ncol(bro.dg.auc06), 
                                            ncol(bro.dg.auc07), ncol(bro.dg.auc08), ncol(bro.dg.auc09), ncol(bro.dg.auc10), ncol(bro.dg.auc11), ncol(bro.dg.auc12), 
                                            ncol(bro.dg.auc13), ncol(bro.dg.auc14), ncol(bro.dg.auc15), ncol(bro.dg.auc16), ncol(bro.dg.auc17), ncol(bro.dg.auc18), 
                                            ncol(bro.dg.auc19), ncol(bro.dg.auc20), ncol(bro.dg.auc21), ncol(bro.dg.auc22), ncol(bro.dg.auc23), ncol(bro.dg.auc24), 
                                            ncol(bro.dg.auc25), ncol(bro.dg.auc26), ncol(bro.dg.auc27), ncol(bro.dg.auc28), ncol(bro.dg.auc29), ncol(bro.dg.auc30), 
                                            ncol(bro.dg.auc31), ncol(bro.dg.auc32), ncol(bro.dg.auc33), ncol(bro.dg.auc34), ncol(bro.dg.auc35), ncol(bro.dg.auc36), 
                                            ncol(bro.dg.auc37), ncol(bro.dg.auc38), ncol(bro.dg.auc39), ncol(bro.dg.auc40), ncol(bro.dg.auc41), ncol(bro.dg.auc42), 
                                            ncol(bro.dg.auc43), ncol(bro.dg.auc44), ncol(bro.dg.auc45), ncol(bro.dg.auc46), ncol(bro.dg.auc47), ncol(bro.dg.auc48), 
                                            ncol(bro.dg.auc49), ncol(bro.dg.auc50), ncol(bro.dg.auc51), ncol(bro.dg.auc52), ncol(bro.dg.auc53), ncol(bro.dg.auc54), 
                                            ncol(bro.dg.auc55), ncol(bro.dg.auc56), ncol(bro.dg.auc57), ncol(bro.dg.auc58), ncol(bro.dg.auc59), ncol(bro.dg.auc60), 
                                            ncol(bro.dg.auc61), ncol(bro.dg.auc62), ncol(bro.dg.auc63), ncol(bro.dg.auc64), ncol(bro.dg.auc65), ncol(bro.dg.auc66), 
                                            ncol(bro.dg.auc67), ncol(bro.dg.auc68), ncol(bro.dg.auc69), ncol(bro.dg.auc70), ncol(bro.dg.auc71), ncol(bro.dg.auc72), 
                                            ncol(bro.dg.auc73), ncol(bro.dg.auc74), ncol(bro.dg.auc75), ncol(bro.dg.auc76), ncol(bro.dg.auc77), ncol(bro.dg.auc78), 
                                            ncol(bro.dg.auc79), ncol(bro.dg.auc80), ncol(bro.dg.auc81), ncol(bro.dg.auc82), ncol(bro.dg.auc83), ncol(bro.dg.auc84), 
                                            ncol(bro.dg.auc85), ncol(bro.dg.auc86), ncol(bro.dg.auc87), ncol(bro.dg.auc88), ncol(bro.dg.auc89), ncol(bro.dg.auc90), 
                                            ncol(bro.dg.auc91), ncol(bro.dg.auc92), ncol(bro.dg.auc93), ncol(bro.dg.auc94), ncol(bro.dg.auc95), ncol(bro.dg.auc96), 
                                            ncol(bro.dg.auc97), ncol(bro.dg.auc98), ncol(bro.dg.auc99), ncol(bro.dg.auc100)))

# Scale the F-values to 0-1
brome.dg.Fscaled <- (brome.dg.permanova$F.model - min(brome.dg.permanova$F.model)) / (max(brome.dg.permanova$F.model) - min(brome.dg.permanova$F.model))
brome.dg.permanova$F.model.scale <- NA
brome.dg.permanova$F.model.scale <- brome.dg.Fscaled

write.csv(brome.dg.permanova, "~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/Brome.dg.permanova.csv")

###############################################################
###############################################################
###############################################################

brome.cl.auc <- read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/brome.cl.auc.csv", header = T)

# Hellinger
bro.cl.auc100 <- 
  read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/ADD1_AUC100_MIN3_Brome_bacfunarc_dw_otu_table-graph_centrality-closeness-selectallbyall-abundances.csv", header = T)
bro.cl.auc99 <- bro.cl.auc100[,c(1:brome.cl.auc[99,3])]; bro.cl.auc98 <- bro.cl.auc100[,c(1:brome.cl.auc[98,3])]; bro.cl.auc97 <- bro.cl.auc100[,c(1:brome.cl.auc[97,3])]; 
bro.cl.auc96 <- bro.cl.auc100[,c(1:brome.cl.auc[96,3])]; bro.cl.auc95 <- bro.cl.auc100[,c(1:brome.cl.auc[95,3])]; bro.cl.auc94 <- bro.cl.auc100[,c(1:brome.cl.auc[94,3])]; 
bro.cl.auc93 <- bro.cl.auc100[,c(1:brome.cl.auc[93,3])]; bro.cl.auc92 <- bro.cl.auc100[,c(1:brome.cl.auc[92,3])]; bro.cl.auc91 <- bro.cl.auc100[,c(1:brome.cl.auc[91,3])]; 
bro.cl.auc90 <- bro.cl.auc100[,c(1:brome.cl.auc[90,3])]; bro.cl.auc89 <- bro.cl.auc100[,c(1:brome.cl.auc[89,3])]; bro.cl.auc88 <- bro.cl.auc100[,c(1:brome.cl.auc[88,3])]; 
bro.cl.auc87 <- bro.cl.auc100[,c(1:brome.cl.auc[87,3])]; bro.cl.auc86 <- bro.cl.auc100[,c(1:brome.cl.auc[86,3])]; bro.cl.auc85 <- bro.cl.auc100[,c(1:brome.cl.auc[85,3])]; 
bro.cl.auc84 <- bro.cl.auc100[,c(1:brome.cl.auc[84,3])]; bro.cl.auc83 <- bro.cl.auc100[,c(1:brome.cl.auc[83,3])]; bro.cl.auc82 <- bro.cl.auc100[,c(1:brome.cl.auc[82,3])]; 
bro.cl.auc81 <- bro.cl.auc100[,c(1:brome.cl.auc[81,3])]; bro.cl.auc80 <- bro.cl.auc100[,c(1:brome.cl.auc[80,3])]; bro.cl.auc79 <- bro.cl.auc100[,c(1:brome.cl.auc[79,3])]; 
bro.cl.auc78 <- bro.cl.auc100[,c(1:brome.cl.auc[78,3])]; bro.cl.auc77 <- bro.cl.auc100[,c(1:brome.cl.auc[77,3])]; bro.cl.auc76 <- bro.cl.auc100[,c(1:brome.cl.auc[76,3])]; 
bro.cl.auc75 <- bro.cl.auc100[,c(1:brome.cl.auc[75,3])]; bro.cl.auc74 <- bro.cl.auc100[,c(1:brome.cl.auc[74,3])]; bro.cl.auc73 <- bro.cl.auc100[,c(1:brome.cl.auc[73,3])]; 
bro.cl.auc72 <- bro.cl.auc100[,c(1:brome.cl.auc[72,3])]; bro.cl.auc71 <- bro.cl.auc100[,c(1:brome.cl.auc[71,3])]; bro.cl.auc70 <- bro.cl.auc100[,c(1:brome.cl.auc[70,3])]; 
bro.cl.auc69 <- bro.cl.auc100[,c(1:brome.cl.auc[69,3])]; bro.cl.auc68 <- bro.cl.auc100[,c(1:brome.cl.auc[68,3])]; bro.cl.auc67 <- bro.cl.auc100[,c(1:brome.cl.auc[67,3])]; 
bro.cl.auc66 <- bro.cl.auc100[,c(1:brome.cl.auc[66,3])]; bro.cl.auc65 <- bro.cl.auc100[,c(1:brome.cl.auc[65,3])]; bro.cl.auc64 <- bro.cl.auc100[,c(1:brome.cl.auc[64,3])]; 
bro.cl.auc63 <- bro.cl.auc100[,c(1:brome.cl.auc[63,3])]; bro.cl.auc62 <- bro.cl.auc100[,c(1:brome.cl.auc[62,3])]; bro.cl.auc61 <- bro.cl.auc100[,c(1:brome.cl.auc[61,3])]; 
bro.cl.auc60 <- bro.cl.auc100[,c(1:brome.cl.auc[60,3])]; bro.cl.auc59 <- bro.cl.auc100[,c(1:brome.cl.auc[59,3])]; bro.cl.auc58 <- bro.cl.auc100[,c(1:brome.cl.auc[58,3])]; 
bro.cl.auc57 <- bro.cl.auc100[,c(1:brome.cl.auc[57,3])]; bro.cl.auc56 <- bro.cl.auc100[,c(1:brome.cl.auc[56,3])]; bro.cl.auc55 <- bro.cl.auc100[,c(1:brome.cl.auc[55,3])]; 
bro.cl.auc54 <- bro.cl.auc100[,c(1:brome.cl.auc[54,3])]; bro.cl.auc53 <- bro.cl.auc100[,c(1:brome.cl.auc[53,3])]; bro.cl.auc52 <- bro.cl.auc100[,c(1:brome.cl.auc[52,3])]; 
bro.cl.auc51 <- bro.cl.auc100[,c(1:brome.cl.auc[51,3])]; bro.cl.auc50 <- bro.cl.auc100[,c(1:brome.cl.auc[50,3])]; bro.cl.auc49 <- bro.cl.auc100[,c(1:brome.cl.auc[49,3])]; 
bro.cl.auc48 <- bro.cl.auc100[,c(1:brome.cl.auc[48,3])]; bro.cl.auc47 <- bro.cl.auc100[,c(1:brome.cl.auc[47,3])]; bro.cl.auc46 <- bro.cl.auc100[,c(1:brome.cl.auc[46,3])]; 
bro.cl.auc45 <- bro.cl.auc100[,c(1:brome.cl.auc[45,3])]; bro.cl.auc44 <- bro.cl.auc100[,c(1:brome.cl.auc[44,3])]; bro.cl.auc43 <- bro.cl.auc100[,c(1:brome.cl.auc[43,3])]; 
bro.cl.auc42 <- bro.cl.auc100[,c(1:brome.cl.auc[42,3])]; bro.cl.auc41 <- bro.cl.auc100[,c(1:brome.cl.auc[41,3])]; bro.cl.auc40 <- bro.cl.auc100[,c(1:brome.cl.auc[40,3])]; 
bro.cl.auc39 <- bro.cl.auc100[,c(1:brome.cl.auc[39,3])]; bro.cl.auc38 <- bro.cl.auc100[,c(1:brome.cl.auc[38,3])]; bro.cl.auc37 <- bro.cl.auc100[,c(1:brome.cl.auc[37,3])]; 
bro.cl.auc36 <- bro.cl.auc100[,c(1:brome.cl.auc[36,3])]; bro.cl.auc35 <- bro.cl.auc100[,c(1:brome.cl.auc[35,3])]; bro.cl.auc34 <- bro.cl.auc100[,c(1:brome.cl.auc[34,3])]; 
bro.cl.auc33 <- bro.cl.auc100[,c(1:brome.cl.auc[33,3])]; bro.cl.auc32 <- bro.cl.auc100[,c(1:brome.cl.auc[32,3])]; bro.cl.auc31 <- bro.cl.auc100[,c(1:brome.cl.auc[31,3])]; 
bro.cl.auc30 <- bro.cl.auc100[,c(1:brome.cl.auc[30,3])]; bro.cl.auc29 <- bro.cl.auc100[,c(1:brome.cl.auc[29,3])]; bro.cl.auc28 <- bro.cl.auc100[,c(1:brome.cl.auc[28,3])]; 
bro.cl.auc27 <- bro.cl.auc100[,c(1:brome.cl.auc[27,3])]; bro.cl.auc26 <- bro.cl.auc100[,c(1:brome.cl.auc[26,3])]; bro.cl.auc25 <- bro.cl.auc100[,c(1:brome.cl.auc[25,3])]; 
bro.cl.auc24 <- bro.cl.auc100[,c(1:brome.cl.auc[24,3])]; bro.cl.auc23 <- bro.cl.auc100[,c(1:brome.cl.auc[23,3])]; bro.cl.auc22 <- bro.cl.auc100[,c(1:brome.cl.auc[22,3])]; 
bro.cl.auc21 <- bro.cl.auc100[,c(1:brome.cl.auc[21,3])]; bro.cl.auc20 <- bro.cl.auc100[,c(1:brome.cl.auc[20,3])]; bro.cl.auc19 <- bro.cl.auc100[,c(1:brome.cl.auc[19,3])]; 
bro.cl.auc18 <- bro.cl.auc100[,c(1:brome.cl.auc[18,3])]; bro.cl.auc17 <- bro.cl.auc100[,c(1:brome.cl.auc[17,3])]; bro.cl.auc16 <- bro.cl.auc100[,c(1:brome.cl.auc[16,3])]; 
bro.cl.auc15 <- bro.cl.auc100[,c(1:brome.cl.auc[15,3])]; bro.cl.auc14 <- bro.cl.auc100[,c(1:brome.cl.auc[14,3])]; bro.cl.auc13 <- bro.cl.auc100[,c(1:brome.cl.auc[13,3])]; 
bro.cl.auc12 <- bro.cl.auc100[,c(1:brome.cl.auc[12,3])]; bro.cl.auc11 <- bro.cl.auc100[,c(1:brome.cl.auc[11,3])]; bro.cl.auc10 <- bro.cl.auc100[,c(1:brome.cl.auc[10,3])]; 
bro.cl.auc09 <- bro.cl.auc100[,c(1:brome.cl.auc[9,3])]; bro.cl.auc08 <- bro.cl.auc100[,c(1:brome.cl.auc[8,3])]; bro.cl.auc07 <- bro.cl.auc100[,c(1:brome.cl.auc[7,3])]; 
bro.cl.auc06 <- bro.cl.auc100[,c(1:brome.cl.auc[6,3])]; bro.cl.auc05 <- bro.cl.auc100[,c(1:brome.cl.auc[5,3])]; bro.cl.auc04 <- bro.cl.auc100[,c(1:brome.cl.auc[4,3])]; 
bro.cl.auc03 <- bro.cl.auc100[,c(1:brome.cl.auc[3,3])]; bro.cl.auc02 <- bro.cl.auc100[,c(1:brome.cl.auc[2,3])]; bro.cl.auc01 <- bro.cl.auc100[,c(1:brome.cl.auc[1,3])];

# Convert to Hellinger distance matrix
bro.cl.auc01.hel <- vegdist(decostand(bro.cl.auc01, "hellinger"), "euclidean"); bro.cl.auc02.hel <- vegdist(decostand(bro.cl.auc02, "hellinger"), "euclidean"); 
bro.cl.auc03.hel <- vegdist(decostand(bro.cl.auc03, "hellinger"), "euclidean"); bro.cl.auc04.hel <- vegdist(decostand(bro.cl.auc04, "hellinger"), "euclidean"); 
bro.cl.auc05.hel <- vegdist(decostand(bro.cl.auc05, "hellinger"), "euclidean"); bro.cl.auc06.hel <- vegdist(decostand(bro.cl.auc06, "hellinger"), "euclidean");
bro.cl.auc07.hel <- vegdist(decostand(bro.cl.auc07, "hellinger"), "euclidean"); bro.cl.auc08.hel <- vegdist(decostand(bro.cl.auc08, "hellinger"), "euclidean"); 
bro.cl.auc09.hel <- vegdist(decostand(bro.cl.auc09, "hellinger"), "euclidean"); bro.cl.auc10.hel <- vegdist(decostand(bro.cl.auc10, "hellinger"), "euclidean"); 
bro.cl.auc11.hel <- vegdist(decostand(bro.cl.auc11, "hellinger"), "euclidean"); bro.cl.auc12.hel <- vegdist(decostand(bro.cl.auc12, "hellinger"), "euclidean");
bro.cl.auc13.hel <- vegdist(decostand(bro.cl.auc13, "hellinger"), "euclidean"); bro.cl.auc14.hel <- vegdist(decostand(bro.cl.auc14, "hellinger"), "euclidean"); 
bro.cl.auc15.hel <- vegdist(decostand(bro.cl.auc15, "hellinger"), "euclidean"); bro.cl.auc16.hel <- vegdist(decostand(bro.cl.auc16, "hellinger"), "euclidean"); 
bro.cl.auc17.hel <- vegdist(decostand(bro.cl.auc17, "hellinger"), "euclidean"); bro.cl.auc18.hel <- vegdist(decostand(bro.cl.auc18, "hellinger"), "euclidean");
bro.cl.auc19.hel <- vegdist(decostand(bro.cl.auc19, "hellinger"), "euclidean"); bro.cl.auc20.hel <- vegdist(decostand(bro.cl.auc20, "hellinger"), "euclidean"); 
bro.cl.auc21.hel <- vegdist(decostand(bro.cl.auc21, "hellinger"), "euclidean"); bro.cl.auc22.hel <- vegdist(decostand(bro.cl.auc22, "hellinger"), "euclidean"); 
bro.cl.auc23.hel <- vegdist(decostand(bro.cl.auc23, "hellinger"), "euclidean"); bro.cl.auc24.hel <- vegdist(decostand(bro.cl.auc24, "hellinger"), "euclidean");
bro.cl.auc25.hel <- vegdist(decostand(bro.cl.auc25, "hellinger"), "euclidean"); bro.cl.auc26.hel <- vegdist(decostand(bro.cl.auc26, "hellinger"), "euclidean"); 
bro.cl.auc27.hel <- vegdist(decostand(bro.cl.auc27, "hellinger"), "euclidean"); bro.cl.auc28.hel <- vegdist(decostand(bro.cl.auc28, "hellinger"), "euclidean"); 
bro.cl.auc29.hel <- vegdist(decostand(bro.cl.auc29, "hellinger"), "euclidean"); bro.cl.auc30.hel <- vegdist(decostand(bro.cl.auc30, "hellinger"), "euclidean");
bro.cl.auc31.hel <- vegdist(decostand(bro.cl.auc31, "hellinger"), "euclidean"); bro.cl.auc32.hel <- vegdist(decostand(bro.cl.auc32, "hellinger"), "euclidean"); 
bro.cl.auc33.hel <- vegdist(decostand(bro.cl.auc33, "hellinger"), "euclidean"); bro.cl.auc34.hel <- vegdist(decostand(bro.cl.auc34, "hellinger"), "euclidean"); 
bro.cl.auc35.hel <- vegdist(decostand(bro.cl.auc35, "hellinger"), "euclidean"); bro.cl.auc36.hel <- vegdist(decostand(bro.cl.auc36, "hellinger"), "euclidean");
bro.cl.auc37.hel <- vegdist(decostand(bro.cl.auc37, "hellinger"), "euclidean"); bro.cl.auc38.hel <- vegdist(decostand(bro.cl.auc38, "hellinger"), "euclidean"); 
bro.cl.auc39.hel <- vegdist(decostand(bro.cl.auc39, "hellinger"), "euclidean"); bro.cl.auc40.hel <- vegdist(decostand(bro.cl.auc40, "hellinger"), "euclidean"); 
bro.cl.auc41.hel <- vegdist(decostand(bro.cl.auc41, "hellinger"), "euclidean"); bro.cl.auc42.hel <- vegdist(decostand(bro.cl.auc42, "hellinger"), "euclidean");
bro.cl.auc43.hel <- vegdist(decostand(bro.cl.auc43, "hellinger"), "euclidean"); bro.cl.auc44.hel <- vegdist(decostand(bro.cl.auc44, "hellinger"), "euclidean"); 
bro.cl.auc45.hel <- vegdist(decostand(bro.cl.auc45, "hellinger"), "euclidean"); bro.cl.auc46.hel <- vegdist(decostand(bro.cl.auc46, "hellinger"), "euclidean"); 
bro.cl.auc47.hel <- vegdist(decostand(bro.cl.auc47, "hellinger"), "euclidean"); bro.cl.auc48.hel <- vegdist(decostand(bro.cl.auc48, "hellinger"), "euclidean");
bro.cl.auc49.hel <- vegdist(decostand(bro.cl.auc49, "hellinger"), "euclidean"); bro.cl.auc50.hel <- vegdist(decostand(bro.cl.auc50, "hellinger"), "euclidean"); 
bro.cl.auc51.hel <- vegdist(decostand(bro.cl.auc51, "hellinger"), "euclidean"); bro.cl.auc52.hel <- vegdist(decostand(bro.cl.auc52, "hellinger"), "euclidean"); 
bro.cl.auc53.hel <- vegdist(decostand(bro.cl.auc53, "hellinger"), "euclidean"); bro.cl.auc54.hel <- vegdist(decostand(bro.cl.auc54, "hellinger"), "euclidean");
bro.cl.auc55.hel <- vegdist(decostand(bro.cl.auc55, "hellinger"), "euclidean"); bro.cl.auc56.hel <- vegdist(decostand(bro.cl.auc56, "hellinger"), "euclidean"); 
bro.cl.auc57.hel <- vegdist(decostand(bro.cl.auc57, "hellinger"), "euclidean"); bro.cl.auc58.hel <- vegdist(decostand(bro.cl.auc58, "hellinger"), "euclidean"); 
bro.cl.auc59.hel <- vegdist(decostand(bro.cl.auc59, "hellinger"), "euclidean"); bro.cl.auc60.hel <- vegdist(decostand(bro.cl.auc60, "hellinger"), "euclidean");
bro.cl.auc61.hel <- vegdist(decostand(bro.cl.auc61, "hellinger"), "euclidean"); bro.cl.auc62.hel <- vegdist(decostand(bro.cl.auc62, "hellinger"), "euclidean"); 
bro.cl.auc63.hel <- vegdist(decostand(bro.cl.auc63, "hellinger"), "euclidean"); bro.cl.auc64.hel <- vegdist(decostand(bro.cl.auc64, "hellinger"), "euclidean"); 
bro.cl.auc65.hel <- vegdist(decostand(bro.cl.auc65, "hellinger"), "euclidean"); bro.cl.auc66.hel <- vegdist(decostand(bro.cl.auc66, "hellinger"), "euclidean");
bro.cl.auc67.hel <- vegdist(decostand(bro.cl.auc67, "hellinger"), "euclidean"); bro.cl.auc68.hel <- vegdist(decostand(bro.cl.auc68, "hellinger"), "euclidean"); 
bro.cl.auc69.hel <- vegdist(decostand(bro.cl.auc69, "hellinger"), "euclidean"); bro.cl.auc70.hel <- vegdist(decostand(bro.cl.auc70, "hellinger"), "euclidean"); 
bro.cl.auc71.hel <- vegdist(decostand(bro.cl.auc71, "hellinger"), "euclidean"); bro.cl.auc72.hel <- vegdist(decostand(bro.cl.auc72, "hellinger"), "euclidean");
bro.cl.auc73.hel <- vegdist(decostand(bro.cl.auc73, "hellinger"), "euclidean"); bro.cl.auc74.hel <- vegdist(decostand(bro.cl.auc74, "hellinger"), "euclidean"); 
bro.cl.auc75.hel <- vegdist(decostand(bro.cl.auc75, "hellinger"), "euclidean"); bro.cl.auc76.hel <- vegdist(decostand(bro.cl.auc76, "hellinger"), "euclidean"); 
bro.cl.auc77.hel <- vegdist(decostand(bro.cl.auc77, "hellinger"), "euclidean"); bro.cl.auc78.hel <- vegdist(decostand(bro.cl.auc78, "hellinger"), "euclidean");
bro.cl.auc79.hel <- vegdist(decostand(bro.cl.auc79, "hellinger"), "euclidean"); bro.cl.auc80.hel <- vegdist(decostand(bro.cl.auc80, "hellinger"), "euclidean"); 
bro.cl.auc81.hel <- vegdist(decostand(bro.cl.auc81, "hellinger"), "euclidean"); bro.cl.auc82.hel <- vegdist(decostand(bro.cl.auc82, "hellinger"), "euclidean"); 
bro.cl.auc83.hel <- vegdist(decostand(bro.cl.auc83, "hellinger"), "euclidean"); bro.cl.auc84.hel <- vegdist(decostand(bro.cl.auc84, "hellinger"), "euclidean");
bro.cl.auc85.hel <- vegdist(decostand(bro.cl.auc85, "hellinger"), "euclidean"); bro.cl.auc86.hel <- vegdist(decostand(bro.cl.auc86, "hellinger"), "euclidean"); 
bro.cl.auc87.hel <- vegdist(decostand(bro.cl.auc87, "hellinger"), "euclidean"); bro.cl.auc88.hel <- vegdist(decostand(bro.cl.auc88, "hellinger"), "euclidean"); 
bro.cl.auc89.hel <- vegdist(decostand(bro.cl.auc89, "hellinger"), "euclidean"); bro.cl.auc90.hel <- vegdist(decostand(bro.cl.auc90, "hellinger"), "euclidean");
bro.cl.auc91.hel <- vegdist(decostand(bro.cl.auc91, "hellinger"), "euclidean"); bro.cl.auc92.hel <- vegdist(decostand(bro.cl.auc92, "hellinger"), "euclidean"); 
bro.cl.auc93.hel <- vegdist(decostand(bro.cl.auc93, "hellinger"), "euclidean"); bro.cl.auc94.hel <- vegdist(decostand(bro.cl.auc94, "hellinger"), "euclidean"); 
bro.cl.auc95.hel <- vegdist(decostand(bro.cl.auc95, "hellinger"), "euclidean"); bro.cl.auc96.hel <- vegdist(decostand(bro.cl.auc96, "hellinger"), "euclidean");
bro.cl.auc97.hel <- vegdist(decostand(bro.cl.auc97, "hellinger"), "euclidean"); bro.cl.auc98.hel <- vegdist(decostand(bro.cl.auc98, "hellinger"), "euclidean"); 
bro.cl.auc99.hel <- vegdist(decostand(bro.cl.auc99, "hellinger"), "euclidean"); bro.cl.auc100.hel <- vegdist(decostand(bro.cl.auc100, "hellinger"), "euclidean");

#############
# PERMANOVA
set.seed(1); bro.cl.auc01.pman <- adonis(bro.cl.auc01.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc02.pman <- adonis(bro.cl.auc02.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc03.pman <- adonis(bro.cl.auc03.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc04.pman <- adonis(bro.cl.auc04.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc05.pman <- adonis(bro.cl.auc05.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc06.pman <- adonis(bro.cl.auc06.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc07.pman <- adonis(bro.cl.auc07.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc08.pman <- adonis(bro.cl.auc08.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc09.pman <- adonis(bro.cl.auc09.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc10.pman <- adonis(bro.cl.auc10.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc11.pman <- adonis(bro.cl.auc11.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc12.pman <- adonis(bro.cl.auc12.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc13.pman <- adonis(bro.cl.auc13.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc14.pman <- adonis(bro.cl.auc14.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc15.pman <- adonis(bro.cl.auc15.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc16.pman <- adonis(bro.cl.auc16.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc17.pman <- adonis(bro.cl.auc17.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc18.pman <- adonis(bro.cl.auc18.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc19.pman <- adonis(bro.cl.auc19.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc20.pman <- adonis(bro.cl.auc20.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc21.pman <- adonis(bro.cl.auc21.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc22.pman <- adonis(bro.cl.auc22.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc23.pman <- adonis(bro.cl.auc23.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc24.pman <- adonis(bro.cl.auc24.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc25.pman <- adonis(bro.cl.auc25.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc26.pman <- adonis(bro.cl.auc26.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc27.pman <- adonis(bro.cl.auc27.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc28.pman <- adonis(bro.cl.auc28.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc29.pman <- adonis(bro.cl.auc29.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc30.pman <- adonis(bro.cl.auc30.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc31.pman <- adonis(bro.cl.auc31.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc32.pman <- adonis(bro.cl.auc32.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc33.pman <- adonis(bro.cl.auc33.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc34.pman <- adonis(bro.cl.auc34.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc35.pman <- adonis(bro.cl.auc35.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc36.pman <- adonis(bro.cl.auc36.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc37.pman <- adonis(bro.cl.auc37.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc38.pman <- adonis(bro.cl.auc38.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc39.pman <- adonis(bro.cl.auc39.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc40.pman <- adonis(bro.cl.auc40.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc41.pman <- adonis(bro.cl.auc41.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc42.pman <- adonis(bro.cl.auc42.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc43.pman <- adonis(bro.cl.auc43.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc44.pman <- adonis(bro.cl.auc44.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc45.pman <- adonis(bro.cl.auc45.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc46.pman <- adonis(bro.cl.auc46.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc47.pman <- adonis(bro.cl.auc47.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc48.pman <- adonis(bro.cl.auc48.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc49.pman <- adonis(bro.cl.auc49.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc50.pman <- adonis(bro.cl.auc50.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc51.pman <- adonis(bro.cl.auc51.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc52.pman <- adonis(bro.cl.auc52.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc53.pman <- adonis(bro.cl.auc53.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc54.pman <- adonis(bro.cl.auc54.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc55.pman <- adonis(bro.cl.auc55.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc56.pman <- adonis(bro.cl.auc56.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc57.pman <- adonis(bro.cl.auc57.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc58.pman <- adonis(bro.cl.auc58.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc59.pman <- adonis(bro.cl.auc59.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc60.pman <- adonis(bro.cl.auc60.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc61.pman <- adonis(bro.cl.auc61.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc62.pman <- adonis(bro.cl.auc62.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc63.pman <- adonis(bro.cl.auc63.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc64.pman <- adonis(bro.cl.auc64.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc65.pman <- adonis(bro.cl.auc65.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc66.pman <- adonis(bro.cl.auc66.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc67.pman <- adonis(bro.cl.auc67.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc68.pman <- adonis(bro.cl.auc68.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc69.pman <- adonis(bro.cl.auc69.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc70.pman <- adonis(bro.cl.auc70.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc71.pman <- adonis(bro.cl.auc71.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc72.pman <- adonis(bro.cl.auc72.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc73.pman <- adonis(bro.cl.auc73.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc74.pman <- adonis(bro.cl.auc74.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc75.pman <- adonis(bro.cl.auc75.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc76.pman <- adonis(bro.cl.auc76.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc77.pman <- adonis(bro.cl.auc77.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc78.pman <- adonis(bro.cl.auc78.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc79.pman <- adonis(bro.cl.auc79.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc80.pman <- adonis(bro.cl.auc80.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc81.pman <- adonis(bro.cl.auc81.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc82.pman <- adonis(bro.cl.auc82.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc83.pman <- adonis(bro.cl.auc83.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc84.pman <- adonis(bro.cl.auc84.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc85.pman <- adonis(bro.cl.auc85.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc86.pman <- adonis(bro.cl.auc86.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc87.pman <- adonis(bro.cl.auc87.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc88.pman <- adonis(bro.cl.auc88.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc89.pman <- adonis(bro.cl.auc89.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc90.pman <- adonis(bro.cl.auc90.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc91.pman <- adonis(bro.cl.auc91.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc92.pman <- adonis(bro.cl.auc92.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc93.pman <- adonis(bro.cl.auc93.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc94.pman <- adonis(bro.cl.auc94.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc95.pman <- adonis(bro.cl.auc95.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc96.pman <- adonis(bro.cl.auc96.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.cl.auc97.pman <- adonis(bro.cl.auc97.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc98.pman <- adonis(bro.cl.auc98.hel ~ brome.sample$type, permutations=999);
set.seed(1); bro.cl.auc99.pman <- adonis(bro.cl.auc99.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.cl.auc100.pman <- adonis(bro.cl.auc100.hel ~ brome.sample$type, permutations=999) 

# Create a table of the outputs
brome.cl.permanova <- data.frame(test = c(paste("auc0", seq(1,9,1), sep = ""),
                                          paste("auc", seq(10,99,1), sep = ""),
                                          "auc100"),
                                 order = rep(c(1:100), 1),
                                 auc = c(seq(1,100,1)),
                                 SumsOfSqs = c(bro.cl.auc01.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc02.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc03.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc04.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc05.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc06.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc07.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc08.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc09.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc10.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc11.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc12.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc13.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc14.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc15.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc16.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc17.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc18.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc19.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc20.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc21.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc22.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc23.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc24.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc25.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc26.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc27.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc28.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc29.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc30.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc31.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc32.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc33.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc34.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc35.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc36.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc37.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc38.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc39.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc40.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc41.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc42.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc43.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc44.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc45.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc46.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc47.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc48.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc49.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc50.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc51.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc52.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc53.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc54.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc55.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc56.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc57.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc58.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc59.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc60.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc61.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc62.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc63.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc64.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc65.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc66.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc67.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc68.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc69.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc70.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc71.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc72.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc73.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc74.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc75.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc76.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc77.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc78.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc79.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc80.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc81.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc82.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc83.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc84.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc85.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc86.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc87.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc88.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc89.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc90.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc91.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc92.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc93.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc94.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc95.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc96.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.cl.auc97.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc98.pman$aov.tab$SumsOfSqs[[1]], bro.cl.auc99.pman$aov.tab$SumsOfSqs[[1]],
                                               bro.cl.auc100.pman$aov.tab$SumsOfSqs[[1]]),
                                 
                                 MeanSqs = c(bro.cl.auc01.pman$aov.tab$MeanSqs[[1]], bro.cl.auc02.pman$aov.tab$MeanSqs[[1]], bro.cl.auc03.pman$aov.tab$MeanSqs[[1]], bro.cl.auc04.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc05.pman$aov.tab$MeanSqs[[1]], bro.cl.auc06.pman$aov.tab$MeanSqs[[1]], bro.cl.auc07.pman$aov.tab$MeanSqs[[1]], bro.cl.auc08.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc09.pman$aov.tab$MeanSqs[[1]], bro.cl.auc10.pman$aov.tab$MeanSqs[[1]], bro.cl.auc11.pman$aov.tab$MeanSqs[[1]], bro.cl.auc12.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc13.pman$aov.tab$MeanSqs[[1]], bro.cl.auc14.pman$aov.tab$MeanSqs[[1]], bro.cl.auc15.pman$aov.tab$MeanSqs[[1]], bro.cl.auc16.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc17.pman$aov.tab$MeanSqs[[1]], bro.cl.auc18.pman$aov.tab$MeanSqs[[1]], bro.cl.auc19.pman$aov.tab$MeanSqs[[1]], bro.cl.auc20.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc21.pman$aov.tab$MeanSqs[[1]], bro.cl.auc22.pman$aov.tab$MeanSqs[[1]], bro.cl.auc23.pman$aov.tab$MeanSqs[[1]], bro.cl.auc24.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc25.pman$aov.tab$MeanSqs[[1]], bro.cl.auc26.pman$aov.tab$MeanSqs[[1]], bro.cl.auc27.pman$aov.tab$MeanSqs[[1]], bro.cl.auc28.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc29.pman$aov.tab$MeanSqs[[1]], bro.cl.auc30.pman$aov.tab$MeanSqs[[1]], bro.cl.auc31.pman$aov.tab$MeanSqs[[1]], bro.cl.auc32.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc33.pman$aov.tab$MeanSqs[[1]], bro.cl.auc34.pman$aov.tab$MeanSqs[[1]], bro.cl.auc35.pman$aov.tab$MeanSqs[[1]], bro.cl.auc36.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc37.pman$aov.tab$MeanSqs[[1]], bro.cl.auc38.pman$aov.tab$MeanSqs[[1]], bro.cl.auc39.pman$aov.tab$MeanSqs[[1]], bro.cl.auc40.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc41.pman$aov.tab$MeanSqs[[1]], bro.cl.auc42.pman$aov.tab$MeanSqs[[1]], bro.cl.auc43.pman$aov.tab$MeanSqs[[1]], bro.cl.auc44.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc45.pman$aov.tab$MeanSqs[[1]], bro.cl.auc46.pman$aov.tab$MeanSqs[[1]], bro.cl.auc47.pman$aov.tab$MeanSqs[[1]], bro.cl.auc48.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc49.pman$aov.tab$MeanSqs[[1]], bro.cl.auc50.pman$aov.tab$MeanSqs[[1]], bro.cl.auc51.pman$aov.tab$MeanSqs[[1]], bro.cl.auc52.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc53.pman$aov.tab$MeanSqs[[1]], bro.cl.auc54.pman$aov.tab$MeanSqs[[1]], bro.cl.auc55.pman$aov.tab$MeanSqs[[1]], bro.cl.auc56.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc57.pman$aov.tab$MeanSqs[[1]], bro.cl.auc58.pman$aov.tab$MeanSqs[[1]], bro.cl.auc59.pman$aov.tab$MeanSqs[[1]], bro.cl.auc60.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc61.pman$aov.tab$MeanSqs[[1]], bro.cl.auc62.pman$aov.tab$MeanSqs[[1]], bro.cl.auc63.pman$aov.tab$MeanSqs[[1]], bro.cl.auc64.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc65.pman$aov.tab$MeanSqs[[1]], bro.cl.auc66.pman$aov.tab$MeanSqs[[1]], bro.cl.auc67.pman$aov.tab$MeanSqs[[1]], bro.cl.auc68.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc69.pman$aov.tab$MeanSqs[[1]], bro.cl.auc70.pman$aov.tab$MeanSqs[[1]], bro.cl.auc71.pman$aov.tab$MeanSqs[[1]], bro.cl.auc72.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc73.pman$aov.tab$MeanSqs[[1]], bro.cl.auc74.pman$aov.tab$MeanSqs[[1]], bro.cl.auc75.pman$aov.tab$MeanSqs[[1]], bro.cl.auc76.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc77.pman$aov.tab$MeanSqs[[1]], bro.cl.auc78.pman$aov.tab$MeanSqs[[1]], bro.cl.auc79.pman$aov.tab$MeanSqs[[1]], bro.cl.auc80.pman$aov.tab$MeanSqs[[1]],
                                             bro.cl.auc81.pman$aov.tab$MeanSqs[[1]], bro.cl.auc82.pman$aov.tab$MeanSqs[[1]], bro.cl.auc83.pman$aov.tab$MeanSqs[[1]], bro.cl.auc84.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc85.pman$aov.tab$MeanSqs[[1]], bro.cl.auc86.pman$aov.tab$MeanSqs[[1]], bro.cl.auc87.pman$aov.tab$MeanSqs[[1]], bro.cl.auc88.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc89.pman$aov.tab$MeanSqs[[1]], bro.cl.auc90.pman$aov.tab$MeanSqs[[1]], bro.cl.auc91.pman$aov.tab$MeanSqs[[1]], bro.cl.auc92.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc93.pman$aov.tab$MeanSqs[[1]], bro.cl.auc94.pman$aov.tab$MeanSqs[[1]], bro.cl.auc95.pman$aov.tab$MeanSqs[[1]], bro.cl.auc96.pman$aov.tab$MeanSqs[[1]], 
                                             bro.cl.auc97.pman$aov.tab$MeanSqs[[1]], bro.cl.auc98.pman$aov.tab$MeanSqs[[1]], bro.cl.auc99.pman$aov.tab$MeanSqs[[1]], bro.cl.auc100.pman$aov.tab$MeanSqs[[1]]),
                                 
                                 F.model = c(bro.cl.auc01.pman$aov.tab$F.Model[1], bro.cl.auc02.pman$aov.tab$F.Model[1], bro.cl.auc03.pman$aov.tab$F.Model[1], bro.cl.auc04.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc05.pman$aov.tab$F.Model[1], bro.cl.auc06.pman$aov.tab$F.Model[1], bro.cl.auc07.pman$aov.tab$F.Model[1], bro.cl.auc08.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc09.pman$aov.tab$F.Model[1], bro.cl.auc10.pman$aov.tab$F.Model[1], bro.cl.auc11.pman$aov.tab$F.Model[1], bro.cl.auc12.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc13.pman$aov.tab$F.Model[1], bro.cl.auc14.pman$aov.tab$F.Model[1], bro.cl.auc15.pman$aov.tab$F.Model[1], bro.cl.auc16.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc17.pman$aov.tab$F.Model[1], bro.cl.auc18.pman$aov.tab$F.Model[1], bro.cl.auc19.pman$aov.tab$F.Model[1], bro.cl.auc20.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc21.pman$aov.tab$F.Model[1], bro.cl.auc22.pman$aov.tab$F.Model[1], bro.cl.auc23.pman$aov.tab$F.Model[1], bro.cl.auc24.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc25.pman$aov.tab$F.Model[1], bro.cl.auc26.pman$aov.tab$F.Model[1], bro.cl.auc27.pman$aov.tab$F.Model[1], bro.cl.auc28.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc29.pman$aov.tab$F.Model[1], bro.cl.auc30.pman$aov.tab$F.Model[1], bro.cl.auc31.pman$aov.tab$F.Model[1], bro.cl.auc32.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc33.pman$aov.tab$F.Model[1], bro.cl.auc34.pman$aov.tab$F.Model[1], bro.cl.auc35.pman$aov.tab$F.Model[1], bro.cl.auc36.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc37.pman$aov.tab$F.Model[1], bro.cl.auc38.pman$aov.tab$F.Model[1], bro.cl.auc39.pman$aov.tab$F.Model[1], bro.cl.auc40.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc41.pman$aov.tab$F.Model[1], bro.cl.auc42.pman$aov.tab$F.Model[1], bro.cl.auc43.pman$aov.tab$F.Model[1], bro.cl.auc44.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc45.pman$aov.tab$F.Model[1], bro.cl.auc46.pman$aov.tab$F.Model[1], bro.cl.auc47.pman$aov.tab$F.Model[1], bro.cl.auc48.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc49.pman$aov.tab$F.Model[1], bro.cl.auc50.pman$aov.tab$F.Model[1], bro.cl.auc51.pman$aov.tab$F.Model[1], bro.cl.auc52.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc53.pman$aov.tab$F.Model[1], bro.cl.auc54.pman$aov.tab$F.Model[1], bro.cl.auc55.pman$aov.tab$F.Model[1], bro.cl.auc56.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc57.pman$aov.tab$F.Model[1], bro.cl.auc58.pman$aov.tab$F.Model[1], bro.cl.auc59.pman$aov.tab$F.Model[1], bro.cl.auc60.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc61.pman$aov.tab$F.Model[1], bro.cl.auc62.pman$aov.tab$F.Model[1], bro.cl.auc63.pman$aov.tab$F.Model[1], bro.cl.auc64.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc65.pman$aov.tab$F.Model[1], bro.cl.auc66.pman$aov.tab$F.Model[1], bro.cl.auc67.pman$aov.tab$F.Model[1], bro.cl.auc68.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc69.pman$aov.tab$F.Model[1], bro.cl.auc70.pman$aov.tab$F.Model[1], bro.cl.auc71.pman$aov.tab$F.Model[1], bro.cl.auc72.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc73.pman$aov.tab$F.Model[1], bro.cl.auc74.pman$aov.tab$F.Model[1], bro.cl.auc75.pman$aov.tab$F.Model[1], bro.cl.auc76.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc77.pman$aov.tab$F.Model[1], bro.cl.auc78.pman$aov.tab$F.Model[1], bro.cl.auc79.pman$aov.tab$F.Model[1], bro.cl.auc80.pman$aov.tab$F.Model[1],
                                             bro.cl.auc81.pman$aov.tab$F.Model[1], bro.cl.auc82.pman$aov.tab$F.Model[1], bro.cl.auc83.pman$aov.tab$F.Model[1], bro.cl.auc84.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc85.pman$aov.tab$F.Model[1], bro.cl.auc86.pman$aov.tab$F.Model[1], bro.cl.auc87.pman$aov.tab$F.Model[1], bro.cl.auc88.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc89.pman$aov.tab$F.Model[1], bro.cl.auc90.pman$aov.tab$F.Model[1], bro.cl.auc91.pman$aov.tab$F.Model[1], bro.cl.auc92.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc93.pman$aov.tab$F.Model[1], bro.cl.auc94.pman$aov.tab$F.Model[1], bro.cl.auc95.pman$aov.tab$F.Model[1], bro.cl.auc96.pman$aov.tab$F.Model[1], 
                                             bro.cl.auc97.pman$aov.tab$F.Model[1], bro.cl.auc98.pman$aov.tab$F.Model[1], bro.cl.auc99.pman$aov.tab$F.Model[1], bro.cl.auc100.pman$aov.tab$F.Model[1]),
                                 
                                 R2 = c(bro.cl.auc01.pman$aov.tab$R2[1], bro.cl.auc02.pman$aov.tab$R2[1], bro.cl.auc03.pman$aov.tab$R2[1], bro.cl.auc04.pman$aov.tab$R2[1], 
                                        bro.cl.auc05.pman$aov.tab$R2[1], bro.cl.auc06.pman$aov.tab$R2[1], bro.cl.auc07.pman$aov.tab$R2[1], bro.cl.auc08.pman$aov.tab$R2[1], 
                                        bro.cl.auc09.pman$aov.tab$R2[1], bro.cl.auc10.pman$aov.tab$R2[1], bro.cl.auc11.pman$aov.tab$R2[1], bro.cl.auc12.pman$aov.tab$R2[1], 
                                        bro.cl.auc13.pman$aov.tab$R2[1], bro.cl.auc14.pman$aov.tab$R2[1], bro.cl.auc15.pman$aov.tab$R2[1], bro.cl.auc16.pman$aov.tab$R2[1], 
                                        bro.cl.auc17.pman$aov.tab$R2[1], bro.cl.auc18.pman$aov.tab$R2[1], bro.cl.auc19.pman$aov.tab$R2[1], bro.cl.auc20.pman$aov.tab$R2[1], 
                                        bro.cl.auc21.pman$aov.tab$R2[1], bro.cl.auc22.pman$aov.tab$R2[1], bro.cl.auc23.pman$aov.tab$R2[1], bro.cl.auc24.pman$aov.tab$R2[1], 
                                        bro.cl.auc25.pman$aov.tab$R2[1], bro.cl.auc26.pman$aov.tab$R2[1], bro.cl.auc27.pman$aov.tab$R2[1], bro.cl.auc28.pman$aov.tab$R2[1], 
                                        bro.cl.auc29.pman$aov.tab$R2[1], bro.cl.auc30.pman$aov.tab$R2[1], bro.cl.auc31.pman$aov.tab$R2[1], bro.cl.auc32.pman$aov.tab$R2[1], 
                                        bro.cl.auc33.pman$aov.tab$R2[1], bro.cl.auc34.pman$aov.tab$R2[1], bro.cl.auc35.pman$aov.tab$R2[1], bro.cl.auc36.pman$aov.tab$R2[1], 
                                        bro.cl.auc37.pman$aov.tab$R2[1], bro.cl.auc38.pman$aov.tab$R2[1], bro.cl.auc39.pman$aov.tab$R2[1], bro.cl.auc40.pman$aov.tab$R2[1], 
                                        bro.cl.auc41.pman$aov.tab$R2[1], bro.cl.auc42.pman$aov.tab$R2[1], bro.cl.auc43.pman$aov.tab$R2[1], bro.cl.auc44.pman$aov.tab$R2[1], 
                                        bro.cl.auc45.pman$aov.tab$R2[1], bro.cl.auc46.pman$aov.tab$R2[1], bro.cl.auc47.pman$aov.tab$R2[1], bro.cl.auc48.pman$aov.tab$R2[1], 
                                        bro.cl.auc49.pman$aov.tab$R2[1], bro.cl.auc50.pman$aov.tab$R2[1], bro.cl.auc51.pman$aov.tab$R2[1], bro.cl.auc52.pman$aov.tab$R2[1], 
                                        bro.cl.auc53.pman$aov.tab$R2[1], bro.cl.auc54.pman$aov.tab$R2[1], bro.cl.auc55.pman$aov.tab$R2[1], bro.cl.auc56.pman$aov.tab$R2[1], 
                                        bro.cl.auc57.pman$aov.tab$R2[1], bro.cl.auc58.pman$aov.tab$R2[1], bro.cl.auc59.pman$aov.tab$R2[1], bro.cl.auc60.pman$aov.tab$R2[1], 
                                        bro.cl.auc61.pman$aov.tab$R2[1], bro.cl.auc62.pman$aov.tab$R2[1], bro.cl.auc63.pman$aov.tab$R2[1], bro.cl.auc64.pman$aov.tab$R2[1], 
                                        bro.cl.auc65.pman$aov.tab$R2[1], bro.cl.auc66.pman$aov.tab$R2[1], bro.cl.auc67.pman$aov.tab$R2[1], bro.cl.auc68.pman$aov.tab$R2[1], 
                                        bro.cl.auc69.pman$aov.tab$R2[1], bro.cl.auc70.pman$aov.tab$R2[1], bro.cl.auc71.pman$aov.tab$R2[1], bro.cl.auc72.pman$aov.tab$R2[1], 
                                        bro.cl.auc73.pman$aov.tab$R2[1], bro.cl.auc74.pman$aov.tab$R2[1], bro.cl.auc75.pman$aov.tab$R2[1], bro.cl.auc76.pman$aov.tab$R2[1], 
                                        bro.cl.auc77.pman$aov.tab$R2[1], bro.cl.auc78.pman$aov.tab$R2[1], bro.cl.auc79.pman$aov.tab$R2[1], bro.cl.auc80.pman$aov.tab$R2[1],
                                        bro.cl.auc81.pman$aov.tab$R2[1], bro.cl.auc82.pman$aov.tab$R2[1], bro.cl.auc83.pman$aov.tab$R2[1], bro.cl.auc84.pman$aov.tab$R2[1], 
                                        bro.cl.auc85.pman$aov.tab$R2[1], bro.cl.auc86.pman$aov.tab$R2[1], bro.cl.auc87.pman$aov.tab$R2[1], bro.cl.auc88.pman$aov.tab$R2[1], 
                                        bro.cl.auc89.pman$aov.tab$R2[1], bro.cl.auc90.pman$aov.tab$R2[1], bro.cl.auc91.pman$aov.tab$R2[1], bro.cl.auc92.pman$aov.tab$R2[1], 
                                        bro.cl.auc93.pman$aov.tab$R2[1], bro.cl.auc94.pman$aov.tab$R2[1], bro.cl.auc95.pman$aov.tab$R2[1], bro.cl.auc96.pman$aov.tab$R2[1], 
                                        bro.cl.auc97.pman$aov.tab$R2[1], bro.cl.auc98.pman$aov.tab$R2[1], bro.cl.auc99.pman$aov.tab$R2[1], bro.cl.auc100.pman$aov.tab$R2[1]),
                                 
                                 Pval = c(bro.cl.auc01.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc02.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc03.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc04.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc05.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc06.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc07.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc08.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc09.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc10.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc11.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc12.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc13.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc14.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc15.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc16.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc17.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc18.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc19.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc20.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc21.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc22.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc23.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc24.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc25.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc26.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc27.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc28.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc29.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc30.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc31.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc32.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc33.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc34.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc35.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc36.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc37.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc38.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc39.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc40.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc41.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc42.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc43.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc44.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc45.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc46.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc47.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc48.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc49.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc50.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc51.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc52.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc53.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc54.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc55.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc56.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc57.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc58.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc59.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc60.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc61.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc62.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc63.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc64.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc65.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc66.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc67.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc68.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc69.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc70.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc71.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc72.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc73.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc74.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc75.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc76.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc77.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc78.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc79.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc80.pman$aov.tab$`Pr(>F)`[1],
                                          bro.cl.auc81.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc82.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc83.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc84.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc85.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc86.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc87.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc88.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc89.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc90.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc91.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc92.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc93.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc94.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc95.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc96.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.cl.auc97.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc98.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc99.pman$aov.tab$`Pr(>F)`[1], bro.cl.auc100.pman$aov.tab$`Pr(>F)`[1]),
                                 
                                 N.taxa = c(ncol(bro.cl.auc01), ncol(bro.cl.auc02), ncol(bro.cl.auc03), ncol(bro.cl.auc04), ncol(bro.cl.auc05), ncol(bro.cl.auc06), 
                                            ncol(bro.cl.auc07), ncol(bro.cl.auc08), ncol(bro.cl.auc09), ncol(bro.cl.auc10), ncol(bro.cl.auc11), ncol(bro.cl.auc12), 
                                            ncol(bro.cl.auc13), ncol(bro.cl.auc14), ncol(bro.cl.auc15), ncol(bro.cl.auc16), ncol(bro.cl.auc17), ncol(bro.cl.auc18), 
                                            ncol(bro.cl.auc19), ncol(bro.cl.auc20), ncol(bro.cl.auc21), ncol(bro.cl.auc22), ncol(bro.cl.auc23), ncol(bro.cl.auc24), 
                                            ncol(bro.cl.auc25), ncol(bro.cl.auc26), ncol(bro.cl.auc27), ncol(bro.cl.auc28), ncol(bro.cl.auc29), ncol(bro.cl.auc30), 
                                            ncol(bro.cl.auc31), ncol(bro.cl.auc32), ncol(bro.cl.auc33), ncol(bro.cl.auc34), ncol(bro.cl.auc35), ncol(bro.cl.auc36), 
                                            ncol(bro.cl.auc37), ncol(bro.cl.auc38), ncol(bro.cl.auc39), ncol(bro.cl.auc40), ncol(bro.cl.auc41), ncol(bro.cl.auc42), 
                                            ncol(bro.cl.auc43), ncol(bro.cl.auc44), ncol(bro.cl.auc45), ncol(bro.cl.auc46), ncol(bro.cl.auc47), ncol(bro.cl.auc48), 
                                            ncol(bro.cl.auc49), ncol(bro.cl.auc50), ncol(bro.cl.auc51), ncol(bro.cl.auc52), ncol(bro.cl.auc53), ncol(bro.cl.auc54), 
                                            ncol(bro.cl.auc55), ncol(bro.cl.auc56), ncol(bro.cl.auc57), ncol(bro.cl.auc58), ncol(bro.cl.auc59), ncol(bro.cl.auc60), 
                                            ncol(bro.cl.auc61), ncol(bro.cl.auc62), ncol(bro.cl.auc63), ncol(bro.cl.auc64), ncol(bro.cl.auc65), ncol(bro.cl.auc66), 
                                            ncol(bro.cl.auc67), ncol(bro.cl.auc68), ncol(bro.cl.auc69), ncol(bro.cl.auc70), ncol(bro.cl.auc71), ncol(bro.cl.auc72), 
                                            ncol(bro.cl.auc73), ncol(bro.cl.auc74), ncol(bro.cl.auc75), ncol(bro.cl.auc76), ncol(bro.cl.auc77), ncol(bro.cl.auc78), 
                                            ncol(bro.cl.auc79), ncol(bro.cl.auc80), ncol(bro.cl.auc81), ncol(bro.cl.auc82), ncol(bro.cl.auc83), ncol(bro.cl.auc84), 
                                            ncol(bro.cl.auc85), ncol(bro.cl.auc86), ncol(bro.cl.auc87), ncol(bro.cl.auc88), ncol(bro.cl.auc89), ncol(bro.cl.auc90), 
                                            ncol(bro.cl.auc91), ncol(bro.cl.auc92), ncol(bro.cl.auc93), ncol(bro.cl.auc94), ncol(bro.cl.auc95), ncol(bro.cl.auc96), 
                                            ncol(bro.cl.auc97), ncol(bro.cl.auc98), ncol(bro.cl.auc99), ncol(bro.cl.auc100)))

# Scale the F-values to 0-1
brome.cl.Fscaled <- (brome.cl.permanova$F.model - min(brome.cl.permanova$F.model)) / (max(brome.cl.permanova$F.model) - min(brome.cl.permanova$F.model))
brome.cl.permanova$F.model.scale <- NA
brome.cl.permanova$F.model.scale <- brome.cl.Fscaled

write.csv(brome.cl.permanova, "~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/Brome.cl.permanova.csv")

###############################################################
###############################################################
###############################################################

brome.bw.auc <- read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/brome.bw.auc.csv", header = T)

# Hellinger
bro.bw.auc100 <- 
  read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/ADD1_AUC100_MIN3_MIC0.2__Brome_bacfunarc_dw_otu_table-graph_centrality-betweenness-selectallbyall-abundances.csv", header = T)
bro.bw.auc99 <- bro.bw.auc100[,c(1:brome.bw.auc[99,3])]; bro.bw.auc98 <- bro.bw.auc100[,c(1:brome.bw.auc[98,3])]; bro.bw.auc97 <- bro.bw.auc100[,c(1:brome.bw.auc[97,3])]; 
bro.bw.auc96 <- bro.bw.auc100[,c(1:brome.bw.auc[96,3])]; bro.bw.auc95 <- bro.bw.auc100[,c(1:brome.bw.auc[95,3])]; bro.bw.auc94 <- bro.bw.auc100[,c(1:brome.bw.auc[94,3])]; 
bro.bw.auc93 <- bro.bw.auc100[,c(1:brome.bw.auc[93,3])]; bro.bw.auc92 <- bro.bw.auc100[,c(1:brome.bw.auc[92,3])]; bro.bw.auc91 <- bro.bw.auc100[,c(1:brome.bw.auc[91,3])]; 
bro.bw.auc90 <- bro.bw.auc100[,c(1:brome.bw.auc[90,3])]; bro.bw.auc89 <- bro.bw.auc100[,c(1:brome.bw.auc[89,3])]; bro.bw.auc88 <- bro.bw.auc100[,c(1:brome.bw.auc[88,3])]; 
bro.bw.auc87 <- bro.bw.auc100[,c(1:brome.bw.auc[87,3])]; bro.bw.auc86 <- bro.bw.auc100[,c(1:brome.bw.auc[86,3])]; bro.bw.auc85 <- bro.bw.auc100[,c(1:brome.bw.auc[85,3])]; 
bro.bw.auc84 <- bro.bw.auc100[,c(1:brome.bw.auc[84,3])]; bro.bw.auc83 <- bro.bw.auc100[,c(1:brome.bw.auc[83,3])]; bro.bw.auc82 <- bro.bw.auc100[,c(1:brome.bw.auc[82,3])]; 
bro.bw.auc81 <- bro.bw.auc100[,c(1:brome.bw.auc[81,3])]; bro.bw.auc80 <- bro.bw.auc100[,c(1:brome.bw.auc[80,3])]; bro.bw.auc79 <- bro.bw.auc100[,c(1:brome.bw.auc[79,3])]; 
bro.bw.auc78 <- bro.bw.auc100[,c(1:brome.bw.auc[78,3])]; bro.bw.auc77 <- bro.bw.auc100[,c(1:brome.bw.auc[77,3])]; bro.bw.auc76 <- bro.bw.auc100[,c(1:brome.bw.auc[76,3])]; 
bro.bw.auc75 <- bro.bw.auc100[,c(1:brome.bw.auc[75,3])]; bro.bw.auc74 <- bro.bw.auc100[,c(1:brome.bw.auc[74,3])]; bro.bw.auc73 <- bro.bw.auc100[,c(1:brome.bw.auc[73,3])]; 
bro.bw.auc72 <- bro.bw.auc100[,c(1:brome.bw.auc[72,3])]; bro.bw.auc71 <- bro.bw.auc100[,c(1:brome.bw.auc[71,3])]; bro.bw.auc70 <- bro.bw.auc100[,c(1:brome.bw.auc[70,3])]; 
bro.bw.auc69 <- bro.bw.auc100[,c(1:brome.bw.auc[69,3])]; bro.bw.auc68 <- bro.bw.auc100[,c(1:brome.bw.auc[68,3])]; bro.bw.auc67 <- bro.bw.auc100[,c(1:brome.bw.auc[67,3])]; 
bro.bw.auc66 <- bro.bw.auc100[,c(1:brome.bw.auc[66,3])]; bro.bw.auc65 <- bro.bw.auc100[,c(1:brome.bw.auc[65,3])]; bro.bw.auc64 <- bro.bw.auc100[,c(1:brome.bw.auc[64,3])]; 
bro.bw.auc63 <- bro.bw.auc100[,c(1:brome.bw.auc[63,3])]; bro.bw.auc62 <- bro.bw.auc100[,c(1:brome.bw.auc[62,3])]; bro.bw.auc61 <- bro.bw.auc100[,c(1:brome.bw.auc[61,3])]; 
bro.bw.auc60 <- bro.bw.auc100[,c(1:brome.bw.auc[60,3])]; bro.bw.auc59 <- bro.bw.auc100[,c(1:brome.bw.auc[59,3])]; bro.bw.auc58 <- bro.bw.auc100[,c(1:brome.bw.auc[58,3])]; 
bro.bw.auc57 <- bro.bw.auc100[,c(1:brome.bw.auc[57,3])]; bro.bw.auc56 <- bro.bw.auc100[,c(1:brome.bw.auc[56,3])]; bro.bw.auc55 <- bro.bw.auc100[,c(1:brome.bw.auc[55,3])]; 
bro.bw.auc54 <- bro.bw.auc100[,c(1:brome.bw.auc[54,3])]; bro.bw.auc53 <- bro.bw.auc100[,c(1:brome.bw.auc[53,3])]; bro.bw.auc52 <- bro.bw.auc100[,c(1:brome.bw.auc[52,3])]; 
bro.bw.auc51 <- bro.bw.auc100[,c(1:brome.bw.auc[51,3])]; bro.bw.auc50 <- bro.bw.auc100[,c(1:brome.bw.auc[50,3])]; bro.bw.auc49 <- bro.bw.auc100[,c(1:brome.bw.auc[49,3])]; 
bro.bw.auc48 <- bro.bw.auc100[,c(1:brome.bw.auc[48,3])]; bro.bw.auc47 <- bro.bw.auc100[,c(1:brome.bw.auc[47,3])]; bro.bw.auc46 <- bro.bw.auc100[,c(1:brome.bw.auc[46,3])]; 
bro.bw.auc45 <- bro.bw.auc100[,c(1:brome.bw.auc[45,3])]; bro.bw.auc44 <- bro.bw.auc100[,c(1:brome.bw.auc[44,3])]; bro.bw.auc43 <- bro.bw.auc100[,c(1:brome.bw.auc[43,3])]; 
bro.bw.auc42 <- bro.bw.auc100[,c(1:brome.bw.auc[42,3])]; bro.bw.auc41 <- bro.bw.auc100[,c(1:brome.bw.auc[41,3])]; bro.bw.auc40 <- bro.bw.auc100[,c(1:brome.bw.auc[40,3])]; 
bro.bw.auc39 <- bro.bw.auc100[,c(1:brome.bw.auc[39,3])]; bro.bw.auc38 <- bro.bw.auc100[,c(1:brome.bw.auc[38,3])]; bro.bw.auc37 <- bro.bw.auc100[,c(1:brome.bw.auc[37,3])]; 
bro.bw.auc36 <- bro.bw.auc100[,c(1:brome.bw.auc[36,3])]; bro.bw.auc35 <- bro.bw.auc100[,c(1:brome.bw.auc[35,3])]; bro.bw.auc34 <- bro.bw.auc100[,c(1:brome.bw.auc[34,3])]; 
bro.bw.auc33 <- bro.bw.auc100[,c(1:brome.bw.auc[33,3])]; bro.bw.auc32 <- bro.bw.auc100[,c(1:brome.bw.auc[32,3])]; bro.bw.auc31 <- bro.bw.auc100[,c(1:brome.bw.auc[31,3])]; 
bro.bw.auc30 <- bro.bw.auc100[,c(1:brome.bw.auc[30,3])]; bro.bw.auc29 <- bro.bw.auc100[,c(1:brome.bw.auc[29,3])]; bro.bw.auc28 <- bro.bw.auc100[,c(1:brome.bw.auc[28,3])]; 
bro.bw.auc27 <- bro.bw.auc100[,c(1:brome.bw.auc[27,3])]; bro.bw.auc26 <- bro.bw.auc100[,c(1:brome.bw.auc[26,3])]; bro.bw.auc25 <- bro.bw.auc100[,c(1:brome.bw.auc[25,3])]; 
bro.bw.auc24 <- bro.bw.auc100[,c(1:brome.bw.auc[24,3])]; bro.bw.auc23 <- bro.bw.auc100[,c(1:brome.bw.auc[23,3])]; bro.bw.auc22 <- bro.bw.auc100[,c(1:brome.bw.auc[22,3])]; 
bro.bw.auc21 <- bro.bw.auc100[,c(1:brome.bw.auc[21,3])]; bro.bw.auc20 <- bro.bw.auc100[,c(1:brome.bw.auc[20,3])]; bro.bw.auc19 <- bro.bw.auc100[,c(1:brome.bw.auc[19,3])]; 
bro.bw.auc18 <- bro.bw.auc100[,c(1:brome.bw.auc[18,3])]; bro.bw.auc17 <- bro.bw.auc100[,c(1:brome.bw.auc[17,3])]; bro.bw.auc16 <- bro.bw.auc100[,c(1:brome.bw.auc[16,3])]; 
bro.bw.auc15 <- bro.bw.auc100[,c(1:brome.bw.auc[15,3])]; bro.bw.auc14 <- bro.bw.auc100[,c(1:brome.bw.auc[14,3])]; bro.bw.auc13 <- bro.bw.auc100[,c(1:brome.bw.auc[13,3])]; 
bro.bw.auc12 <- bro.bw.auc100[,c(1:brome.bw.auc[12,3])]; bro.bw.auc11 <- bro.bw.auc100[,c(1:brome.bw.auc[11,3])]; bro.bw.auc10 <- bro.bw.auc100[,c(1:brome.bw.auc[10,3])]; 
bro.bw.auc09 <- bro.bw.auc100[,c(1:brome.bw.auc[9,3])]; bro.bw.auc08 <- bro.bw.auc100[,c(1:brome.bw.auc[8,3])]; bro.bw.auc07 <- bro.bw.auc100[,c(1:brome.bw.auc[7,3])]; 
bro.bw.auc06 <- bro.bw.auc100[,c(1:brome.bw.auc[6,3])]; bro.bw.auc05 <- bro.bw.auc100[,c(1:brome.bw.auc[5,3])]; bro.bw.auc04 <- bro.bw.auc100[,c(1:brome.bw.auc[4,3])]; 
bro.bw.auc03 <- bro.bw.auc100[,c(1:brome.bw.auc[3,3])]; bro.bw.auc02 <- bro.bw.auc100[,c(1:brome.bw.auc[2,3])]; bro.bw.auc01 <- bro.bw.auc100[,c(1:brome.bw.auc[1,3])];

# Convert to Hellinger distance matrix
bro.bw.auc01.hel <- vegdist(decostand(bro.bw.auc01, "hellinger"), "euclidean"); bro.bw.auc02.hel <- vegdist(decostand(bro.bw.auc02, "hellinger"), "euclidean"); 
bro.bw.auc03.hel <- vegdist(decostand(bro.bw.auc03, "hellinger"), "euclidean"); bro.bw.auc04.hel <- vegdist(decostand(bro.bw.auc04, "hellinger"), "euclidean"); 
bro.bw.auc05.hel <- vegdist(decostand(bro.bw.auc05, "hellinger"), "euclidean"); bro.bw.auc06.hel <- vegdist(decostand(bro.bw.auc06, "hellinger"), "euclidean");
bro.bw.auc07.hel <- vegdist(decostand(bro.bw.auc07, "hellinger"), "euclidean"); bro.bw.auc08.hel <- vegdist(decostand(bro.bw.auc08, "hellinger"), "euclidean"); 
bro.bw.auc09.hel <- vegdist(decostand(bro.bw.auc09, "hellinger"), "euclidean"); bro.bw.auc10.hel <- vegdist(decostand(bro.bw.auc10, "hellinger"), "euclidean"); 
bro.bw.auc11.hel <- vegdist(decostand(bro.bw.auc11, "hellinger"), "euclidean"); bro.bw.auc12.hel <- vegdist(decostand(bro.bw.auc12, "hellinger"), "euclidean");
bro.bw.auc13.hel <- vegdist(decostand(bro.bw.auc13, "hellinger"), "euclidean"); bro.bw.auc14.hel <- vegdist(decostand(bro.bw.auc14, "hellinger"), "euclidean"); 
bro.bw.auc15.hel <- vegdist(decostand(bro.bw.auc15, "hellinger"), "euclidean"); bro.bw.auc16.hel <- vegdist(decostand(bro.bw.auc16, "hellinger"), "euclidean"); 
bro.bw.auc17.hel <- vegdist(decostand(bro.bw.auc17, "hellinger"), "euclidean"); bro.bw.auc18.hel <- vegdist(decostand(bro.bw.auc18, "hellinger"), "euclidean");
bro.bw.auc19.hel <- vegdist(decostand(bro.bw.auc19, "hellinger"), "euclidean"); bro.bw.auc20.hel <- vegdist(decostand(bro.bw.auc20, "hellinger"), "euclidean"); 
bro.bw.auc21.hel <- vegdist(decostand(bro.bw.auc21, "hellinger"), "euclidean"); bro.bw.auc22.hel <- vegdist(decostand(bro.bw.auc22, "hellinger"), "euclidean"); 
bro.bw.auc23.hel <- vegdist(decostand(bro.bw.auc23, "hellinger"), "euclidean"); bro.bw.auc24.hel <- vegdist(decostand(bro.bw.auc24, "hellinger"), "euclidean");
bro.bw.auc25.hel <- vegdist(decostand(bro.bw.auc25, "hellinger"), "euclidean"); bro.bw.auc26.hel <- vegdist(decostand(bro.bw.auc26, "hellinger"), "euclidean"); 
bro.bw.auc27.hel <- vegdist(decostand(bro.bw.auc27, "hellinger"), "euclidean"); bro.bw.auc28.hel <- vegdist(decostand(bro.bw.auc28, "hellinger"), "euclidean"); 
bro.bw.auc29.hel <- vegdist(decostand(bro.bw.auc29, "hellinger"), "euclidean"); bro.bw.auc30.hel <- vegdist(decostand(bro.bw.auc30, "hellinger"), "euclidean");
bro.bw.auc31.hel <- vegdist(decostand(bro.bw.auc31, "hellinger"), "euclidean"); bro.bw.auc32.hel <- vegdist(decostand(bro.bw.auc32, "hellinger"), "euclidean"); 
bro.bw.auc33.hel <- vegdist(decostand(bro.bw.auc33, "hellinger"), "euclidean"); bro.bw.auc34.hel <- vegdist(decostand(bro.bw.auc34, "hellinger"), "euclidean"); 
bro.bw.auc35.hel <- vegdist(decostand(bro.bw.auc35, "hellinger"), "euclidean"); bro.bw.auc36.hel <- vegdist(decostand(bro.bw.auc36, "hellinger"), "euclidean");
bro.bw.auc37.hel <- vegdist(decostand(bro.bw.auc37, "hellinger"), "euclidean"); bro.bw.auc38.hel <- vegdist(decostand(bro.bw.auc38, "hellinger"), "euclidean"); 
bro.bw.auc39.hel <- vegdist(decostand(bro.bw.auc39, "hellinger"), "euclidean"); bro.bw.auc40.hel <- vegdist(decostand(bro.bw.auc40, "hellinger"), "euclidean"); 
bro.bw.auc41.hel <- vegdist(decostand(bro.bw.auc41, "hellinger"), "euclidean"); bro.bw.auc42.hel <- vegdist(decostand(bro.bw.auc42, "hellinger"), "euclidean");
bro.bw.auc43.hel <- vegdist(decostand(bro.bw.auc43, "hellinger"), "euclidean"); bro.bw.auc44.hel <- vegdist(decostand(bro.bw.auc44, "hellinger"), "euclidean"); 
bro.bw.auc45.hel <- vegdist(decostand(bro.bw.auc45, "hellinger"), "euclidean"); bro.bw.auc46.hel <- vegdist(decostand(bro.bw.auc46, "hellinger"), "euclidean"); 
bro.bw.auc47.hel <- vegdist(decostand(bro.bw.auc47, "hellinger"), "euclidean"); bro.bw.auc48.hel <- vegdist(decostand(bro.bw.auc48, "hellinger"), "euclidean");
bro.bw.auc49.hel <- vegdist(decostand(bro.bw.auc49, "hellinger"), "euclidean"); bro.bw.auc50.hel <- vegdist(decostand(bro.bw.auc50, "hellinger"), "euclidean"); 
bro.bw.auc51.hel <- vegdist(decostand(bro.bw.auc51, "hellinger"), "euclidean"); bro.bw.auc52.hel <- vegdist(decostand(bro.bw.auc52, "hellinger"), "euclidean"); 
bro.bw.auc53.hel <- vegdist(decostand(bro.bw.auc53, "hellinger"), "euclidean"); bro.bw.auc54.hel <- vegdist(decostand(bro.bw.auc54, "hellinger"), "euclidean");
bro.bw.auc55.hel <- vegdist(decostand(bro.bw.auc55, "hellinger"), "euclidean"); bro.bw.auc56.hel <- vegdist(decostand(bro.bw.auc56, "hellinger"), "euclidean"); 
bro.bw.auc57.hel <- vegdist(decostand(bro.bw.auc57, "hellinger"), "euclidean"); bro.bw.auc58.hel <- vegdist(decostand(bro.bw.auc58, "hellinger"), "euclidean"); 
bro.bw.auc59.hel <- vegdist(decostand(bro.bw.auc59, "hellinger"), "euclidean"); bro.bw.auc60.hel <- vegdist(decostand(bro.bw.auc60, "hellinger"), "euclidean");
bro.bw.auc61.hel <- vegdist(decostand(bro.bw.auc61, "hellinger"), "euclidean"); bro.bw.auc62.hel <- vegdist(decostand(bro.bw.auc62, "hellinger"), "euclidean"); 
bro.bw.auc63.hel <- vegdist(decostand(bro.bw.auc63, "hellinger"), "euclidean"); bro.bw.auc64.hel <- vegdist(decostand(bro.bw.auc64, "hellinger"), "euclidean"); 
bro.bw.auc65.hel <- vegdist(decostand(bro.bw.auc65, "hellinger"), "euclidean"); bro.bw.auc66.hel <- vegdist(decostand(bro.bw.auc66, "hellinger"), "euclidean");
bro.bw.auc67.hel <- vegdist(decostand(bro.bw.auc67, "hellinger"), "euclidean"); bro.bw.auc68.hel <- vegdist(decostand(bro.bw.auc68, "hellinger"), "euclidean"); 
bro.bw.auc69.hel <- vegdist(decostand(bro.bw.auc69, "hellinger"), "euclidean"); bro.bw.auc70.hel <- vegdist(decostand(bro.bw.auc70, "hellinger"), "euclidean"); 
bro.bw.auc71.hel <- vegdist(decostand(bro.bw.auc71, "hellinger"), "euclidean"); bro.bw.auc72.hel <- vegdist(decostand(bro.bw.auc72, "hellinger"), "euclidean");
bro.bw.auc73.hel <- vegdist(decostand(bro.bw.auc73, "hellinger"), "euclidean"); bro.bw.auc74.hel <- vegdist(decostand(bro.bw.auc74, "hellinger"), "euclidean"); 
bro.bw.auc75.hel <- vegdist(decostand(bro.bw.auc75, "hellinger"), "euclidean"); bro.bw.auc76.hel <- vegdist(decostand(bro.bw.auc76, "hellinger"), "euclidean"); 
bro.bw.auc77.hel <- vegdist(decostand(bro.bw.auc77, "hellinger"), "euclidean"); bro.bw.auc78.hel <- vegdist(decostand(bro.bw.auc78, "hellinger"), "euclidean");
bro.bw.auc79.hel <- vegdist(decostand(bro.bw.auc79, "hellinger"), "euclidean"); bro.bw.auc80.hel <- vegdist(decostand(bro.bw.auc80, "hellinger"), "euclidean"); 
bro.bw.auc81.hel <- vegdist(decostand(bro.bw.auc81, "hellinger"), "euclidean"); bro.bw.auc82.hel <- vegdist(decostand(bro.bw.auc82, "hellinger"), "euclidean"); 
bro.bw.auc83.hel <- vegdist(decostand(bro.bw.auc83, "hellinger"), "euclidean"); bro.bw.auc84.hel <- vegdist(decostand(bro.bw.auc84, "hellinger"), "euclidean");
bro.bw.auc85.hel <- vegdist(decostand(bro.bw.auc85, "hellinger"), "euclidean"); bro.bw.auc86.hel <- vegdist(decostand(bro.bw.auc86, "hellinger"), "euclidean"); 
bro.bw.auc87.hel <- vegdist(decostand(bro.bw.auc87, "hellinger"), "euclidean"); bro.bw.auc88.hel <- vegdist(decostand(bro.bw.auc88, "hellinger"), "euclidean"); 
bro.bw.auc89.hel <- vegdist(decostand(bro.bw.auc89, "hellinger"), "euclidean"); bro.bw.auc90.hel <- vegdist(decostand(bro.bw.auc90, "hellinger"), "euclidean");
bro.bw.auc91.hel <- vegdist(decostand(bro.bw.auc91, "hellinger"), "euclidean"); bro.bw.auc92.hel <- vegdist(decostand(bro.bw.auc92, "hellinger"), "euclidean"); 
bro.bw.auc93.hel <- vegdist(decostand(bro.bw.auc93, "hellinger"), "euclidean"); bro.bw.auc94.hel <- vegdist(decostand(bro.bw.auc94, "hellinger"), "euclidean"); 
bro.bw.auc95.hel <- vegdist(decostand(bro.bw.auc95, "hellinger"), "euclidean"); bro.bw.auc96.hel <- vegdist(decostand(bro.bw.auc96, "hellinger"), "euclidean");
bro.bw.auc97.hel <- vegdist(decostand(bro.bw.auc97, "hellinger"), "euclidean"); bro.bw.auc98.hel <- vegdist(decostand(bro.bw.auc98, "hellinger"), "euclidean"); 
bro.bw.auc99.hel <- vegdist(decostand(bro.bw.auc99, "hellinger"), "euclidean"); bro.bw.auc100.hel <- vegdist(decostand(bro.bw.auc100, "hellinger"), "euclidean");

#############
# PERMANOVA
set.seed(1); bro.bw.auc01.pman <- adonis(bro.bw.auc01.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc02.pman <- adonis(bro.bw.auc02.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc03.pman <- adonis(bro.bw.auc03.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc04.pman <- adonis(bro.bw.auc04.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc05.pman <- adonis(bro.bw.auc05.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc06.pman <- adonis(bro.bw.auc06.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc07.pman <- adonis(bro.bw.auc07.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc08.pman <- adonis(bro.bw.auc08.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc09.pman <- adonis(bro.bw.auc09.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc10.pman <- adonis(bro.bw.auc10.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc11.pman <- adonis(bro.bw.auc11.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc12.pman <- adonis(bro.bw.auc12.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc13.pman <- adonis(bro.bw.auc13.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc14.pman <- adonis(bro.bw.auc14.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc15.pman <- adonis(bro.bw.auc15.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc16.pman <- adonis(bro.bw.auc16.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc17.pman <- adonis(bro.bw.auc17.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc18.pman <- adonis(bro.bw.auc18.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc19.pman <- adonis(bro.bw.auc19.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc20.pman <- adonis(bro.bw.auc20.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc21.pman <- adonis(bro.bw.auc21.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc22.pman <- adonis(bro.bw.auc22.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc23.pman <- adonis(bro.bw.auc23.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc24.pman <- adonis(bro.bw.auc24.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc25.pman <- adonis(bro.bw.auc25.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc26.pman <- adonis(bro.bw.auc26.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc27.pman <- adonis(bro.bw.auc27.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc28.pman <- adonis(bro.bw.auc28.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc29.pman <- adonis(bro.bw.auc29.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc30.pman <- adonis(bro.bw.auc30.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc31.pman <- adonis(bro.bw.auc31.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc32.pman <- adonis(bro.bw.auc32.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc33.pman <- adonis(bro.bw.auc33.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc34.pman <- adonis(bro.bw.auc34.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc35.pman <- adonis(bro.bw.auc35.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc36.pman <- adonis(bro.bw.auc36.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc37.pman <- adonis(bro.bw.auc37.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc38.pman <- adonis(bro.bw.auc38.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc39.pman <- adonis(bro.bw.auc39.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc40.pman <- adonis(bro.bw.auc40.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc41.pman <- adonis(bro.bw.auc41.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc42.pman <- adonis(bro.bw.auc42.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc43.pman <- adonis(bro.bw.auc43.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc44.pman <- adonis(bro.bw.auc44.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc45.pman <- adonis(bro.bw.auc45.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc46.pman <- adonis(bro.bw.auc46.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc47.pman <- adonis(bro.bw.auc47.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc48.pman <- adonis(bro.bw.auc48.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc49.pman <- adonis(bro.bw.auc49.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc50.pman <- adonis(bro.bw.auc50.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc51.pman <- adonis(bro.bw.auc51.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc52.pman <- adonis(bro.bw.auc52.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc53.pman <- adonis(bro.bw.auc53.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc54.pman <- adonis(bro.bw.auc54.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc55.pman <- adonis(bro.bw.auc55.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc56.pman <- adonis(bro.bw.auc56.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc57.pman <- adonis(bro.bw.auc57.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc58.pman <- adonis(bro.bw.auc58.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc59.pman <- adonis(bro.bw.auc59.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc60.pman <- adonis(bro.bw.auc60.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc61.pman <- adonis(bro.bw.auc61.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc62.pman <- adonis(bro.bw.auc62.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc63.pman <- adonis(bro.bw.auc63.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc64.pman <- adonis(bro.bw.auc64.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc65.pman <- adonis(bro.bw.auc65.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc66.pman <- adonis(bro.bw.auc66.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc67.pman <- adonis(bro.bw.auc67.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc68.pman <- adonis(bro.bw.auc68.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc69.pman <- adonis(bro.bw.auc69.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc70.pman <- adonis(bro.bw.auc70.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc71.pman <- adonis(bro.bw.auc71.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc72.pman <- adonis(bro.bw.auc72.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc73.pman <- adonis(bro.bw.auc73.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc74.pman <- adonis(bro.bw.auc74.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc75.pman <- adonis(bro.bw.auc75.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc76.pman <- adonis(bro.bw.auc76.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc77.pman <- adonis(bro.bw.auc77.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc78.pman <- adonis(bro.bw.auc78.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc79.pman <- adonis(bro.bw.auc79.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc80.pman <- adonis(bro.bw.auc80.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc81.pman <- adonis(bro.bw.auc81.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc82.pman <- adonis(bro.bw.auc82.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc83.pman <- adonis(bro.bw.auc83.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc84.pman <- adonis(bro.bw.auc84.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc85.pman <- adonis(bro.bw.auc85.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc86.pman <- adonis(bro.bw.auc86.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc87.pman <- adonis(bro.bw.auc87.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc88.pman <- adonis(bro.bw.auc88.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc89.pman <- adonis(bro.bw.auc89.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc90.pman <- adonis(bro.bw.auc90.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc91.pman <- adonis(bro.bw.auc91.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc92.pman <- adonis(bro.bw.auc92.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc93.pman <- adonis(bro.bw.auc93.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc94.pman <- adonis(bro.bw.auc94.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc95.pman <- adonis(bro.bw.auc95.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc96.pman <- adonis(bro.bw.auc96.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.bw.auc97.pman <- adonis(bro.bw.auc97.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc98.pman <- adonis(bro.bw.auc98.hel ~ brome.sample$type, permutations=999);
set.seed(1); bro.bw.auc99.pman <- adonis(bro.bw.auc99.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.bw.auc100.pman <- adonis(bro.bw.auc100.hel ~ brome.sample$type, permutations=999) 

# Create a table of the outputs
brome.bw.permanova <- data.frame(test = c(paste("auc0", seq(1,9,1), sep = ""),
                                          paste("auc", seq(10,99,1), sep = ""),
                                          "auc100"),
                                 order = rep(c(1:100), 1),
                                 auc = c(seq(1,100,1)),
                                 SumsOfSqs = c(bro.bw.auc01.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc02.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc03.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc04.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc05.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc06.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc07.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc08.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc09.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc10.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc11.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc12.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc13.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc14.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc15.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc16.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc17.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc18.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc19.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc20.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc21.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc22.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc23.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc24.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc25.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc26.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc27.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc28.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc29.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc30.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc31.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc32.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc33.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc34.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc35.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc36.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc37.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc38.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc39.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc40.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc41.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc42.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc43.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc44.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc45.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc46.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc47.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc48.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc49.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc50.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc51.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc52.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc53.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc54.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc55.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc56.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc57.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc58.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc59.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc60.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc61.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc62.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc63.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc64.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc65.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc66.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc67.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc68.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc69.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc70.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc71.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc72.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc73.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc74.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc75.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc76.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc77.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc78.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc79.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc80.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc81.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc82.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc83.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc84.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc85.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc86.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc87.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc88.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc89.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc90.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc91.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc92.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc93.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc94.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc95.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc96.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.bw.auc97.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc98.pman$aov.tab$SumsOfSqs[[1]], bro.bw.auc99.pman$aov.tab$SumsOfSqs[[1]],
                                               bro.bw.auc100.pman$aov.tab$SumsOfSqs[[1]]),
                                 
                                 MeanSqs = c(bro.bw.auc01.pman$aov.tab$MeanSqs[[1]], bro.bw.auc02.pman$aov.tab$MeanSqs[[1]], bro.bw.auc03.pman$aov.tab$MeanSqs[[1]], bro.bw.auc04.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc05.pman$aov.tab$MeanSqs[[1]], bro.bw.auc06.pman$aov.tab$MeanSqs[[1]], bro.bw.auc07.pman$aov.tab$MeanSqs[[1]], bro.bw.auc08.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc09.pman$aov.tab$MeanSqs[[1]], bro.bw.auc10.pman$aov.tab$MeanSqs[[1]], bro.bw.auc11.pman$aov.tab$MeanSqs[[1]], bro.bw.auc12.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc13.pman$aov.tab$MeanSqs[[1]], bro.bw.auc14.pman$aov.tab$MeanSqs[[1]], bro.bw.auc15.pman$aov.tab$MeanSqs[[1]], bro.bw.auc16.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc17.pman$aov.tab$MeanSqs[[1]], bro.bw.auc18.pman$aov.tab$MeanSqs[[1]], bro.bw.auc19.pman$aov.tab$MeanSqs[[1]], bro.bw.auc20.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc21.pman$aov.tab$MeanSqs[[1]], bro.bw.auc22.pman$aov.tab$MeanSqs[[1]], bro.bw.auc23.pman$aov.tab$MeanSqs[[1]], bro.bw.auc24.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc25.pman$aov.tab$MeanSqs[[1]], bro.bw.auc26.pman$aov.tab$MeanSqs[[1]], bro.bw.auc27.pman$aov.tab$MeanSqs[[1]], bro.bw.auc28.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc29.pman$aov.tab$MeanSqs[[1]], bro.bw.auc30.pman$aov.tab$MeanSqs[[1]], bro.bw.auc31.pman$aov.tab$MeanSqs[[1]], bro.bw.auc32.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc33.pman$aov.tab$MeanSqs[[1]], bro.bw.auc34.pman$aov.tab$MeanSqs[[1]], bro.bw.auc35.pman$aov.tab$MeanSqs[[1]], bro.bw.auc36.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc37.pman$aov.tab$MeanSqs[[1]], bro.bw.auc38.pman$aov.tab$MeanSqs[[1]], bro.bw.auc39.pman$aov.tab$MeanSqs[[1]], bro.bw.auc40.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc41.pman$aov.tab$MeanSqs[[1]], bro.bw.auc42.pman$aov.tab$MeanSqs[[1]], bro.bw.auc43.pman$aov.tab$MeanSqs[[1]], bro.bw.auc44.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc45.pman$aov.tab$MeanSqs[[1]], bro.bw.auc46.pman$aov.tab$MeanSqs[[1]], bro.bw.auc47.pman$aov.tab$MeanSqs[[1]], bro.bw.auc48.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc49.pman$aov.tab$MeanSqs[[1]], bro.bw.auc50.pman$aov.tab$MeanSqs[[1]], bro.bw.auc51.pman$aov.tab$MeanSqs[[1]], bro.bw.auc52.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc53.pman$aov.tab$MeanSqs[[1]], bro.bw.auc54.pman$aov.tab$MeanSqs[[1]], bro.bw.auc55.pman$aov.tab$MeanSqs[[1]], bro.bw.auc56.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc57.pman$aov.tab$MeanSqs[[1]], bro.bw.auc58.pman$aov.tab$MeanSqs[[1]], bro.bw.auc59.pman$aov.tab$MeanSqs[[1]], bro.bw.auc60.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc61.pman$aov.tab$MeanSqs[[1]], bro.bw.auc62.pman$aov.tab$MeanSqs[[1]], bro.bw.auc63.pman$aov.tab$MeanSqs[[1]], bro.bw.auc64.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc65.pman$aov.tab$MeanSqs[[1]], bro.bw.auc66.pman$aov.tab$MeanSqs[[1]], bro.bw.auc67.pman$aov.tab$MeanSqs[[1]], bro.bw.auc68.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc69.pman$aov.tab$MeanSqs[[1]], bro.bw.auc70.pman$aov.tab$MeanSqs[[1]], bro.bw.auc71.pman$aov.tab$MeanSqs[[1]], bro.bw.auc72.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc73.pman$aov.tab$MeanSqs[[1]], bro.bw.auc74.pman$aov.tab$MeanSqs[[1]], bro.bw.auc75.pman$aov.tab$MeanSqs[[1]], bro.bw.auc76.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc77.pman$aov.tab$MeanSqs[[1]], bro.bw.auc78.pman$aov.tab$MeanSqs[[1]], bro.bw.auc79.pman$aov.tab$MeanSqs[[1]], bro.bw.auc80.pman$aov.tab$MeanSqs[[1]],
                                             bro.bw.auc81.pman$aov.tab$MeanSqs[[1]], bro.bw.auc82.pman$aov.tab$MeanSqs[[1]], bro.bw.auc83.pman$aov.tab$MeanSqs[[1]], bro.bw.auc84.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc85.pman$aov.tab$MeanSqs[[1]], bro.bw.auc86.pman$aov.tab$MeanSqs[[1]], bro.bw.auc87.pman$aov.tab$MeanSqs[[1]], bro.bw.auc88.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc89.pman$aov.tab$MeanSqs[[1]], bro.bw.auc90.pman$aov.tab$MeanSqs[[1]], bro.bw.auc91.pman$aov.tab$MeanSqs[[1]], bro.bw.auc92.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc93.pman$aov.tab$MeanSqs[[1]], bro.bw.auc94.pman$aov.tab$MeanSqs[[1]], bro.bw.auc95.pman$aov.tab$MeanSqs[[1]], bro.bw.auc96.pman$aov.tab$MeanSqs[[1]], 
                                             bro.bw.auc97.pman$aov.tab$MeanSqs[[1]], bro.bw.auc98.pman$aov.tab$MeanSqs[[1]], bro.bw.auc99.pman$aov.tab$MeanSqs[[1]], bro.bw.auc100.pman$aov.tab$MeanSqs[[1]]),
                                 
                                 F.model = c(bro.bw.auc01.pman$aov.tab$F.Model[1], bro.bw.auc02.pman$aov.tab$F.Model[1], bro.bw.auc03.pman$aov.tab$F.Model[1], bro.bw.auc04.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc05.pman$aov.tab$F.Model[1], bro.bw.auc06.pman$aov.tab$F.Model[1], bro.bw.auc07.pman$aov.tab$F.Model[1], bro.bw.auc08.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc09.pman$aov.tab$F.Model[1], bro.bw.auc10.pman$aov.tab$F.Model[1], bro.bw.auc11.pman$aov.tab$F.Model[1], bro.bw.auc12.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc13.pman$aov.tab$F.Model[1], bro.bw.auc14.pman$aov.tab$F.Model[1], bro.bw.auc15.pman$aov.tab$F.Model[1], bro.bw.auc16.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc17.pman$aov.tab$F.Model[1], bro.bw.auc18.pman$aov.tab$F.Model[1], bro.bw.auc19.pman$aov.tab$F.Model[1], bro.bw.auc20.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc21.pman$aov.tab$F.Model[1], bro.bw.auc22.pman$aov.tab$F.Model[1], bro.bw.auc23.pman$aov.tab$F.Model[1], bro.bw.auc24.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc25.pman$aov.tab$F.Model[1], bro.bw.auc26.pman$aov.tab$F.Model[1], bro.bw.auc27.pman$aov.tab$F.Model[1], bro.bw.auc28.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc29.pman$aov.tab$F.Model[1], bro.bw.auc30.pman$aov.tab$F.Model[1], bro.bw.auc31.pman$aov.tab$F.Model[1], bro.bw.auc32.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc33.pman$aov.tab$F.Model[1], bro.bw.auc34.pman$aov.tab$F.Model[1], bro.bw.auc35.pman$aov.tab$F.Model[1], bro.bw.auc36.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc37.pman$aov.tab$F.Model[1], bro.bw.auc38.pman$aov.tab$F.Model[1], bro.bw.auc39.pman$aov.tab$F.Model[1], bro.bw.auc40.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc41.pman$aov.tab$F.Model[1], bro.bw.auc42.pman$aov.tab$F.Model[1], bro.bw.auc43.pman$aov.tab$F.Model[1], bro.bw.auc44.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc45.pman$aov.tab$F.Model[1], bro.bw.auc46.pman$aov.tab$F.Model[1], bro.bw.auc47.pman$aov.tab$F.Model[1], bro.bw.auc48.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc49.pman$aov.tab$F.Model[1], bro.bw.auc50.pman$aov.tab$F.Model[1], bro.bw.auc51.pman$aov.tab$F.Model[1], bro.bw.auc52.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc53.pman$aov.tab$F.Model[1], bro.bw.auc54.pman$aov.tab$F.Model[1], bro.bw.auc55.pman$aov.tab$F.Model[1], bro.bw.auc56.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc57.pman$aov.tab$F.Model[1], bro.bw.auc58.pman$aov.tab$F.Model[1], bro.bw.auc59.pman$aov.tab$F.Model[1], bro.bw.auc60.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc61.pman$aov.tab$F.Model[1], bro.bw.auc62.pman$aov.tab$F.Model[1], bro.bw.auc63.pman$aov.tab$F.Model[1], bro.bw.auc64.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc65.pman$aov.tab$F.Model[1], bro.bw.auc66.pman$aov.tab$F.Model[1], bro.bw.auc67.pman$aov.tab$F.Model[1], bro.bw.auc68.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc69.pman$aov.tab$F.Model[1], bro.bw.auc70.pman$aov.tab$F.Model[1], bro.bw.auc71.pman$aov.tab$F.Model[1], bro.bw.auc72.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc73.pman$aov.tab$F.Model[1], bro.bw.auc74.pman$aov.tab$F.Model[1], bro.bw.auc75.pman$aov.tab$F.Model[1], bro.bw.auc76.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc77.pman$aov.tab$F.Model[1], bro.bw.auc78.pman$aov.tab$F.Model[1], bro.bw.auc79.pman$aov.tab$F.Model[1], bro.bw.auc80.pman$aov.tab$F.Model[1],
                                             bro.bw.auc81.pman$aov.tab$F.Model[1], bro.bw.auc82.pman$aov.tab$F.Model[1], bro.bw.auc83.pman$aov.tab$F.Model[1], bro.bw.auc84.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc85.pman$aov.tab$F.Model[1], bro.bw.auc86.pman$aov.tab$F.Model[1], bro.bw.auc87.pman$aov.tab$F.Model[1], bro.bw.auc88.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc89.pman$aov.tab$F.Model[1], bro.bw.auc90.pman$aov.tab$F.Model[1], bro.bw.auc91.pman$aov.tab$F.Model[1], bro.bw.auc92.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc93.pman$aov.tab$F.Model[1], bro.bw.auc94.pman$aov.tab$F.Model[1], bro.bw.auc95.pman$aov.tab$F.Model[1], bro.bw.auc96.pman$aov.tab$F.Model[1], 
                                             bro.bw.auc97.pman$aov.tab$F.Model[1], bro.bw.auc98.pman$aov.tab$F.Model[1], bro.bw.auc99.pman$aov.tab$F.Model[1], bro.bw.auc100.pman$aov.tab$F.Model[1]),
                                 
                                 R2 = c(bro.bw.auc01.pman$aov.tab$R2[1], bro.bw.auc02.pman$aov.tab$R2[1], bro.bw.auc03.pman$aov.tab$R2[1], bro.bw.auc04.pman$aov.tab$R2[1], 
                                        bro.bw.auc05.pman$aov.tab$R2[1], bro.bw.auc06.pman$aov.tab$R2[1], bro.bw.auc07.pman$aov.tab$R2[1], bro.bw.auc08.pman$aov.tab$R2[1], 
                                        bro.bw.auc09.pman$aov.tab$R2[1], bro.bw.auc10.pman$aov.tab$R2[1], bro.bw.auc11.pman$aov.tab$R2[1], bro.bw.auc12.pman$aov.tab$R2[1], 
                                        bro.bw.auc13.pman$aov.tab$R2[1], bro.bw.auc14.pman$aov.tab$R2[1], bro.bw.auc15.pman$aov.tab$R2[1], bro.bw.auc16.pman$aov.tab$R2[1], 
                                        bro.bw.auc17.pman$aov.tab$R2[1], bro.bw.auc18.pman$aov.tab$R2[1], bro.bw.auc19.pman$aov.tab$R2[1], bro.bw.auc20.pman$aov.tab$R2[1], 
                                        bro.bw.auc21.pman$aov.tab$R2[1], bro.bw.auc22.pman$aov.tab$R2[1], bro.bw.auc23.pman$aov.tab$R2[1], bro.bw.auc24.pman$aov.tab$R2[1], 
                                        bro.bw.auc25.pman$aov.tab$R2[1], bro.bw.auc26.pman$aov.tab$R2[1], bro.bw.auc27.pman$aov.tab$R2[1], bro.bw.auc28.pman$aov.tab$R2[1], 
                                        bro.bw.auc29.pman$aov.tab$R2[1], bro.bw.auc30.pman$aov.tab$R2[1], bro.bw.auc31.pman$aov.tab$R2[1], bro.bw.auc32.pman$aov.tab$R2[1], 
                                        bro.bw.auc33.pman$aov.tab$R2[1], bro.bw.auc34.pman$aov.tab$R2[1], bro.bw.auc35.pman$aov.tab$R2[1], bro.bw.auc36.pman$aov.tab$R2[1], 
                                        bro.bw.auc37.pman$aov.tab$R2[1], bro.bw.auc38.pman$aov.tab$R2[1], bro.bw.auc39.pman$aov.tab$R2[1], bro.bw.auc40.pman$aov.tab$R2[1], 
                                        bro.bw.auc41.pman$aov.tab$R2[1], bro.bw.auc42.pman$aov.tab$R2[1], bro.bw.auc43.pman$aov.tab$R2[1], bro.bw.auc44.pman$aov.tab$R2[1], 
                                        bro.bw.auc45.pman$aov.tab$R2[1], bro.bw.auc46.pman$aov.tab$R2[1], bro.bw.auc47.pman$aov.tab$R2[1], bro.bw.auc48.pman$aov.tab$R2[1], 
                                        bro.bw.auc49.pman$aov.tab$R2[1], bro.bw.auc50.pman$aov.tab$R2[1], bro.bw.auc51.pman$aov.tab$R2[1], bro.bw.auc52.pman$aov.tab$R2[1], 
                                        bro.bw.auc53.pman$aov.tab$R2[1], bro.bw.auc54.pman$aov.tab$R2[1], bro.bw.auc55.pman$aov.tab$R2[1], bro.bw.auc56.pman$aov.tab$R2[1], 
                                        bro.bw.auc57.pman$aov.tab$R2[1], bro.bw.auc58.pman$aov.tab$R2[1], bro.bw.auc59.pman$aov.tab$R2[1], bro.bw.auc60.pman$aov.tab$R2[1], 
                                        bro.bw.auc61.pman$aov.tab$R2[1], bro.bw.auc62.pman$aov.tab$R2[1], bro.bw.auc63.pman$aov.tab$R2[1], bro.bw.auc64.pman$aov.tab$R2[1], 
                                        bro.bw.auc65.pman$aov.tab$R2[1], bro.bw.auc66.pman$aov.tab$R2[1], bro.bw.auc67.pman$aov.tab$R2[1], bro.bw.auc68.pman$aov.tab$R2[1], 
                                        bro.bw.auc69.pman$aov.tab$R2[1], bro.bw.auc70.pman$aov.tab$R2[1], bro.bw.auc71.pman$aov.tab$R2[1], bro.bw.auc72.pman$aov.tab$R2[1], 
                                        bro.bw.auc73.pman$aov.tab$R2[1], bro.bw.auc74.pman$aov.tab$R2[1], bro.bw.auc75.pman$aov.tab$R2[1], bro.bw.auc76.pman$aov.tab$R2[1], 
                                        bro.bw.auc77.pman$aov.tab$R2[1], bro.bw.auc78.pman$aov.tab$R2[1], bro.bw.auc79.pman$aov.tab$R2[1], bro.bw.auc80.pman$aov.tab$R2[1],
                                        bro.bw.auc81.pman$aov.tab$R2[1], bro.bw.auc82.pman$aov.tab$R2[1], bro.bw.auc83.pman$aov.tab$R2[1], bro.bw.auc84.pman$aov.tab$R2[1], 
                                        bro.bw.auc85.pman$aov.tab$R2[1], bro.bw.auc86.pman$aov.tab$R2[1], bro.bw.auc87.pman$aov.tab$R2[1], bro.bw.auc88.pman$aov.tab$R2[1], 
                                        bro.bw.auc89.pman$aov.tab$R2[1], bro.bw.auc90.pman$aov.tab$R2[1], bro.bw.auc91.pman$aov.tab$R2[1], bro.bw.auc92.pman$aov.tab$R2[1], 
                                        bro.bw.auc93.pman$aov.tab$R2[1], bro.bw.auc94.pman$aov.tab$R2[1], bro.bw.auc95.pman$aov.tab$R2[1], bro.bw.auc96.pman$aov.tab$R2[1], 
                                        bro.bw.auc97.pman$aov.tab$R2[1], bro.bw.auc98.pman$aov.tab$R2[1], bro.bw.auc99.pman$aov.tab$R2[1], bro.bw.auc100.pman$aov.tab$R2[1]),
                                 
                                 Pval = c(bro.bw.auc01.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc02.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc03.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc04.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc05.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc06.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc07.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc08.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc09.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc10.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc11.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc12.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc13.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc14.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc15.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc16.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc17.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc18.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc19.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc20.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc21.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc22.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc23.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc24.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc25.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc26.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc27.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc28.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc29.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc30.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc31.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc32.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc33.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc34.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc35.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc36.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc37.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc38.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc39.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc40.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc41.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc42.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc43.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc44.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc45.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc46.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc47.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc48.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc49.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc50.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc51.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc52.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc53.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc54.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc55.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc56.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc57.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc58.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc59.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc60.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc61.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc62.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc63.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc64.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc65.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc66.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc67.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc68.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc69.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc70.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc71.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc72.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc73.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc74.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc75.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc76.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc77.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc78.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc79.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc80.pman$aov.tab$`Pr(>F)`[1],
                                          bro.bw.auc81.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc82.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc83.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc84.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc85.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc86.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc87.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc88.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc89.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc90.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc91.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc92.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc93.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc94.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc95.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc96.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.bw.auc97.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc98.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc99.pman$aov.tab$`Pr(>F)`[1], bro.bw.auc100.pman$aov.tab$`Pr(>F)`[1]),
                                 
                                 N.taxa = c(ncol(bro.bw.auc01), ncol(bro.bw.auc02), ncol(bro.bw.auc03), ncol(bro.bw.auc04), ncol(bro.bw.auc05), ncol(bro.bw.auc06), 
                                            ncol(bro.bw.auc07), ncol(bro.bw.auc08), ncol(bro.bw.auc09), ncol(bro.bw.auc10), ncol(bro.bw.auc11), ncol(bro.bw.auc12), 
                                            ncol(bro.bw.auc13), ncol(bro.bw.auc14), ncol(bro.bw.auc15), ncol(bro.bw.auc16), ncol(bro.bw.auc17), ncol(bro.bw.auc18), 
                                            ncol(bro.bw.auc19), ncol(bro.bw.auc20), ncol(bro.bw.auc21), ncol(bro.bw.auc22), ncol(bro.bw.auc23), ncol(bro.bw.auc24), 
                                            ncol(bro.bw.auc25), ncol(bro.bw.auc26), ncol(bro.bw.auc27), ncol(bro.bw.auc28), ncol(bro.bw.auc29), ncol(bro.bw.auc30), 
                                            ncol(bro.bw.auc31), ncol(bro.bw.auc32), ncol(bro.bw.auc33), ncol(bro.bw.auc34), ncol(bro.bw.auc35), ncol(bro.bw.auc36), 
                                            ncol(bro.bw.auc37), ncol(bro.bw.auc38), ncol(bro.bw.auc39), ncol(bro.bw.auc40), ncol(bro.bw.auc41), ncol(bro.bw.auc42), 
                                            ncol(bro.bw.auc43), ncol(bro.bw.auc44), ncol(bro.bw.auc45), ncol(bro.bw.auc46), ncol(bro.bw.auc47), ncol(bro.bw.auc48), 
                                            ncol(bro.bw.auc49), ncol(bro.bw.auc50), ncol(bro.bw.auc51), ncol(bro.bw.auc52), ncol(bro.bw.auc53), ncol(bro.bw.auc54), 
                                            ncol(bro.bw.auc55), ncol(bro.bw.auc56), ncol(bro.bw.auc57), ncol(bro.bw.auc58), ncol(bro.bw.auc59), ncol(bro.bw.auc60), 
                                            ncol(bro.bw.auc61), ncol(bro.bw.auc62), ncol(bro.bw.auc63), ncol(bro.bw.auc64), ncol(bro.bw.auc65), ncol(bro.bw.auc66), 
                                            ncol(bro.bw.auc67), ncol(bro.bw.auc68), ncol(bro.bw.auc69), ncol(bro.bw.auc70), ncol(bro.bw.auc71), ncol(bro.bw.auc72), 
                                            ncol(bro.bw.auc73), ncol(bro.bw.auc74), ncol(bro.bw.auc75), ncol(bro.bw.auc76), ncol(bro.bw.auc77), ncol(bro.bw.auc78), 
                                            ncol(bro.bw.auc79), ncol(bro.bw.auc80), ncol(bro.bw.auc81), ncol(bro.bw.auc82), ncol(bro.bw.auc83), ncol(bro.bw.auc84), 
                                            ncol(bro.bw.auc85), ncol(bro.bw.auc86), ncol(bro.bw.auc87), ncol(bro.bw.auc88), ncol(bro.bw.auc89), ncol(bro.bw.auc90), 
                                            ncol(bro.bw.auc91), ncol(bro.bw.auc92), ncol(bro.bw.auc93), ncol(bro.bw.auc94), ncol(bro.bw.auc95), ncol(bro.bw.auc96), 
                                            ncol(bro.bw.auc97), ncol(bro.bw.auc98), ncol(bro.bw.auc99), ncol(bro.bw.auc100)))

# Scale the F-values to 0-1
brome.bw.Fscaled <- (brome.bw.permanova$F.model - min(brome.bw.permanova$F.model)) / (max(brome.bw.permanova$F.model) - min(brome.bw.permanova$F.model))
brome.bw.permanova$F.model.scale <- NA
brome.bw.permanova$F.model.scale <- brome.bw.Fscaled

write.csv(brome.bw.permanova, "~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/Brome.bw.permanova.csv")

###############################################################
###############################################################
###############################################################

brome.ei.auc <- read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/brome.ei.auc.csv", header = T)

# Hellinger
bro.ei.auc100 <- 
  read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/BROME/Network/Eigenvector/ADD1_AUC100_MIN3Brome_bacfunarc_dw_otu_table-graph_centrality-eigenvector-selectallbyall-abundances.csv", header = T)
bro.ei.auc99 <- bro.ei.auc100[,c(1:brome.ei.auc[99,3])]; bro.ei.auc98 <- bro.ei.auc100[,c(1:brome.ei.auc[98,3])]; bro.ei.auc97 <- bro.ei.auc100[,c(1:brome.ei.auc[97,3])]; 
bro.ei.auc96 <- bro.ei.auc100[,c(1:brome.ei.auc[96,3])]; bro.ei.auc95 <- bro.ei.auc100[,c(1:brome.ei.auc[95,3])]; bro.ei.auc94 <- bro.ei.auc100[,c(1:brome.ei.auc[94,3])]; 
bro.ei.auc93 <- bro.ei.auc100[,c(1:brome.ei.auc[93,3])]; bro.ei.auc92 <- bro.ei.auc100[,c(1:brome.ei.auc[92,3])]; bro.ei.auc91 <- bro.ei.auc100[,c(1:brome.ei.auc[91,3])]; 
bro.ei.auc90 <- bro.ei.auc100[,c(1:brome.ei.auc[90,3])]; bro.ei.auc89 <- bro.ei.auc100[,c(1:brome.ei.auc[89,3])]; bro.ei.auc88 <- bro.ei.auc100[,c(1:brome.ei.auc[88,3])]; 
bro.ei.auc87 <- bro.ei.auc100[,c(1:brome.ei.auc[87,3])]; bro.ei.auc86 <- bro.ei.auc100[,c(1:brome.ei.auc[86,3])]; bro.ei.auc85 <- bro.ei.auc100[,c(1:brome.ei.auc[85,3])]; 
bro.ei.auc84 <- bro.ei.auc100[,c(1:brome.ei.auc[84,3])]; bro.ei.auc83 <- bro.ei.auc100[,c(1:brome.ei.auc[83,3])]; bro.ei.auc82 <- bro.ei.auc100[,c(1:brome.ei.auc[82,3])]; 
bro.ei.auc81 <- bro.ei.auc100[,c(1:brome.ei.auc[81,3])]; bro.ei.auc80 <- bro.ei.auc100[,c(1:brome.ei.auc[80,3])]; bro.ei.auc79 <- bro.ei.auc100[,c(1:brome.ei.auc[79,3])]; 
bro.ei.auc78 <- bro.ei.auc100[,c(1:brome.ei.auc[78,3])]; bro.ei.auc77 <- bro.ei.auc100[,c(1:brome.ei.auc[77,3])]; bro.ei.auc76 <- bro.ei.auc100[,c(1:brome.ei.auc[76,3])]; 
bro.ei.auc75 <- bro.ei.auc100[,c(1:brome.ei.auc[75,3])]; bro.ei.auc74 <- bro.ei.auc100[,c(1:brome.ei.auc[74,3])]; bro.ei.auc73 <- bro.ei.auc100[,c(1:brome.ei.auc[73,3])]; 
bro.ei.auc72 <- bro.ei.auc100[,c(1:brome.ei.auc[72,3])]; bro.ei.auc71 <- bro.ei.auc100[,c(1:brome.ei.auc[71,3])]; bro.ei.auc70 <- bro.ei.auc100[,c(1:brome.ei.auc[70,3])]; 
bro.ei.auc69 <- bro.ei.auc100[,c(1:brome.ei.auc[69,3])]; bro.ei.auc68 <- bro.ei.auc100[,c(1:brome.ei.auc[68,3])]; bro.ei.auc67 <- bro.ei.auc100[,c(1:brome.ei.auc[67,3])]; 
bro.ei.auc66 <- bro.ei.auc100[,c(1:brome.ei.auc[66,3])]; bro.ei.auc65 <- bro.ei.auc100[,c(1:brome.ei.auc[65,3])]; bro.ei.auc64 <- bro.ei.auc100[,c(1:brome.ei.auc[64,3])]; 
bro.ei.auc63 <- bro.ei.auc100[,c(1:brome.ei.auc[63,3])]; bro.ei.auc62 <- bro.ei.auc100[,c(1:brome.ei.auc[62,3])]; bro.ei.auc61 <- bro.ei.auc100[,c(1:brome.ei.auc[61,3])]; 
bro.ei.auc60 <- bro.ei.auc100[,c(1:brome.ei.auc[60,3])]; bro.ei.auc59 <- bro.ei.auc100[,c(1:brome.ei.auc[59,3])]; bro.ei.auc58 <- bro.ei.auc100[,c(1:brome.ei.auc[58,3])]; 
bro.ei.auc57 <- bro.ei.auc100[,c(1:brome.ei.auc[57,3])]; bro.ei.auc56 <- bro.ei.auc100[,c(1:brome.ei.auc[56,3])]; bro.ei.auc55 <- bro.ei.auc100[,c(1:brome.ei.auc[55,3])]; 
bro.ei.auc54 <- bro.ei.auc100[,c(1:brome.ei.auc[54,3])]; bro.ei.auc53 <- bro.ei.auc100[,c(1:brome.ei.auc[53,3])]; bro.ei.auc52 <- bro.ei.auc100[,c(1:brome.ei.auc[52,3])]; 
bro.ei.auc51 <- bro.ei.auc100[,c(1:brome.ei.auc[51,3])]; bro.ei.auc50 <- bro.ei.auc100[,c(1:brome.ei.auc[50,3])]; bro.ei.auc49 <- bro.ei.auc100[,c(1:brome.ei.auc[49,3])]; 
bro.ei.auc48 <- bro.ei.auc100[,c(1:brome.ei.auc[48,3])]; bro.ei.auc47 <- bro.ei.auc100[,c(1:brome.ei.auc[47,3])]; bro.ei.auc46 <- bro.ei.auc100[,c(1:brome.ei.auc[46,3])]; 
bro.ei.auc45 <- bro.ei.auc100[,c(1:brome.ei.auc[45,3])]; bro.ei.auc44 <- bro.ei.auc100[,c(1:brome.ei.auc[44,3])]; bro.ei.auc43 <- bro.ei.auc100[,c(1:brome.ei.auc[43,3])]; 
bro.ei.auc42 <- bro.ei.auc100[,c(1:brome.ei.auc[42,3])]; bro.ei.auc41 <- bro.ei.auc100[,c(1:brome.ei.auc[41,3])]; bro.ei.auc40 <- bro.ei.auc100[,c(1:brome.ei.auc[40,3])]; 
bro.ei.auc39 <- bro.ei.auc100[,c(1:brome.ei.auc[39,3])]; bro.ei.auc38 <- bro.ei.auc100[,c(1:brome.ei.auc[38,3])]; bro.ei.auc37 <- bro.ei.auc100[,c(1:brome.ei.auc[37,3])]; 
bro.ei.auc36 <- bro.ei.auc100[,c(1:brome.ei.auc[36,3])]; bro.ei.auc35 <- bro.ei.auc100[,c(1:brome.ei.auc[35,3])]; bro.ei.auc34 <- bro.ei.auc100[,c(1:brome.ei.auc[34,3])]; 
bro.ei.auc33 <- bro.ei.auc100[,c(1:brome.ei.auc[33,3])]; bro.ei.auc32 <- bro.ei.auc100[,c(1:brome.ei.auc[32,3])]; bro.ei.auc31 <- bro.ei.auc100[,c(1:brome.ei.auc[31,3])]; 
bro.ei.auc30 <- bro.ei.auc100[,c(1:brome.ei.auc[30,3])]; bro.ei.auc29 <- bro.ei.auc100[,c(1:brome.ei.auc[29,3])]; bro.ei.auc28 <- bro.ei.auc100[,c(1:brome.ei.auc[28,3])]; 
bro.ei.auc27 <- bro.ei.auc100[,c(1:brome.ei.auc[27,3])]; bro.ei.auc26 <- bro.ei.auc100[,c(1:brome.ei.auc[26,3])]; bro.ei.auc25 <- bro.ei.auc100[,c(1:brome.ei.auc[25,3])]; 
bro.ei.auc24 <- bro.ei.auc100[,c(1:brome.ei.auc[24,3])]; bro.ei.auc23 <- bro.ei.auc100[,c(1:brome.ei.auc[23,3])]; bro.ei.auc22 <- bro.ei.auc100[,c(1:brome.ei.auc[22,3])]; 
bro.ei.auc21 <- bro.ei.auc100[,c(1:brome.ei.auc[21,3])]; bro.ei.auc20 <- bro.ei.auc100[,c(1:brome.ei.auc[20,3])]; bro.ei.auc19 <- bro.ei.auc100[,c(1:brome.ei.auc[19,3])]; 
bro.ei.auc18 <- bro.ei.auc100[,c(1:brome.ei.auc[18,3])]; bro.ei.auc17 <- bro.ei.auc100[,c(1:brome.ei.auc[17,3])]; bro.ei.auc16 <- bro.ei.auc100[,c(1:brome.ei.auc[16,3])]; 
bro.ei.auc15 <- bro.ei.auc100[,c(1:brome.ei.auc[15,3])]; bro.ei.auc14 <- bro.ei.auc100[,c(1:brome.ei.auc[14,3])]; bro.ei.auc13 <- bro.ei.auc100[,c(1:brome.ei.auc[13,3])]; 
bro.ei.auc12 <- bro.ei.auc100[,c(1:brome.ei.auc[12,3])]; bro.ei.auc11 <- bro.ei.auc100[,c(1:brome.ei.auc[11,3])]; bro.ei.auc10 <- bro.ei.auc100[,c(1:brome.ei.auc[10,3])]; 
bro.ei.auc09 <- bro.ei.auc100[,c(1:brome.ei.auc[9,3])]; bro.ei.auc08 <- bro.ei.auc100[,c(1:brome.ei.auc[8,3])]; bro.ei.auc07 <- bro.ei.auc100[,c(1:brome.ei.auc[7,3])]; 
bro.ei.auc06 <- bro.ei.auc100[,c(1:brome.ei.auc[6,3])]; bro.ei.auc05 <- bro.ei.auc100[,c(1:brome.ei.auc[5,3])]; bro.ei.auc04 <- bro.ei.auc100[,c(1:brome.ei.auc[4,3])]; 
bro.ei.auc03 <- bro.ei.auc100[,c(1:brome.ei.auc[3,3])]; bro.ei.auc02 <- bro.ei.auc100[,c(1:brome.ei.auc[2,3])]; bro.ei.auc01 <- bro.ei.auc100[,c(1:brome.ei.auc[1,3])];

# Convert to Hellinger distance matrix
bro.ei.auc01.hel <- vegdist(decostand(bro.ei.auc01, "hellinger"), "euclidean"); bro.ei.auc02.hel <- vegdist(decostand(bro.ei.auc02, "hellinger"), "euclidean"); 
bro.ei.auc03.hel <- vegdist(decostand(bro.ei.auc03, "hellinger"), "euclidean"); bro.ei.auc04.hel <- vegdist(decostand(bro.ei.auc04, "hellinger"), "euclidean"); 
bro.ei.auc05.hel <- vegdist(decostand(bro.ei.auc05, "hellinger"), "euclidean"); bro.ei.auc06.hel <- vegdist(decostand(bro.ei.auc06, "hellinger"), "euclidean");
bro.ei.auc07.hel <- vegdist(decostand(bro.ei.auc07, "hellinger"), "euclidean"); bro.ei.auc08.hel <- vegdist(decostand(bro.ei.auc08, "hellinger"), "euclidean"); 
bro.ei.auc09.hel <- vegdist(decostand(bro.ei.auc09, "hellinger"), "euclidean"); bro.ei.auc10.hel <- vegdist(decostand(bro.ei.auc10, "hellinger"), "euclidean"); 
bro.ei.auc11.hel <- vegdist(decostand(bro.ei.auc11, "hellinger"), "euclidean"); bro.ei.auc12.hel <- vegdist(decostand(bro.ei.auc12, "hellinger"), "euclidean");
bro.ei.auc13.hel <- vegdist(decostand(bro.ei.auc13, "hellinger"), "euclidean"); bro.ei.auc14.hel <- vegdist(decostand(bro.ei.auc14, "hellinger"), "euclidean"); 
bro.ei.auc15.hel <- vegdist(decostand(bro.ei.auc15, "hellinger"), "euclidean"); bro.ei.auc16.hel <- vegdist(decostand(bro.ei.auc16, "hellinger"), "euclidean"); 
bro.ei.auc17.hel <- vegdist(decostand(bro.ei.auc17, "hellinger"), "euclidean"); bro.ei.auc18.hel <- vegdist(decostand(bro.ei.auc18, "hellinger"), "euclidean");
bro.ei.auc19.hel <- vegdist(decostand(bro.ei.auc19, "hellinger"), "euclidean"); bro.ei.auc20.hel <- vegdist(decostand(bro.ei.auc20, "hellinger"), "euclidean"); 
bro.ei.auc21.hel <- vegdist(decostand(bro.ei.auc21, "hellinger"), "euclidean"); bro.ei.auc22.hel <- vegdist(decostand(bro.ei.auc22, "hellinger"), "euclidean"); 
bro.ei.auc23.hel <- vegdist(decostand(bro.ei.auc23, "hellinger"), "euclidean"); bro.ei.auc24.hel <- vegdist(decostand(bro.ei.auc24, "hellinger"), "euclidean");
bro.ei.auc25.hel <- vegdist(decostand(bro.ei.auc25, "hellinger"), "euclidean"); bro.ei.auc26.hel <- vegdist(decostand(bro.ei.auc26, "hellinger"), "euclidean"); 
bro.ei.auc27.hel <- vegdist(decostand(bro.ei.auc27, "hellinger"), "euclidean"); bro.ei.auc28.hel <- vegdist(decostand(bro.ei.auc28, "hellinger"), "euclidean"); 
bro.ei.auc29.hel <- vegdist(decostand(bro.ei.auc29, "hellinger"), "euclidean"); bro.ei.auc30.hel <- vegdist(decostand(bro.ei.auc30, "hellinger"), "euclidean");
bro.ei.auc31.hel <- vegdist(decostand(bro.ei.auc31, "hellinger"), "euclidean"); bro.ei.auc32.hel <- vegdist(decostand(bro.ei.auc32, "hellinger"), "euclidean"); 
bro.ei.auc33.hel <- vegdist(decostand(bro.ei.auc33, "hellinger"), "euclidean"); bro.ei.auc34.hel <- vegdist(decostand(bro.ei.auc34, "hellinger"), "euclidean"); 
bro.ei.auc35.hel <- vegdist(decostand(bro.ei.auc35, "hellinger"), "euclidean"); bro.ei.auc36.hel <- vegdist(decostand(bro.ei.auc36, "hellinger"), "euclidean");
bro.ei.auc37.hel <- vegdist(decostand(bro.ei.auc37, "hellinger"), "euclidean"); bro.ei.auc38.hel <- vegdist(decostand(bro.ei.auc38, "hellinger"), "euclidean"); 
bro.ei.auc39.hel <- vegdist(decostand(bro.ei.auc39, "hellinger"), "euclidean"); bro.ei.auc40.hel <- vegdist(decostand(bro.ei.auc40, "hellinger"), "euclidean"); 
bro.ei.auc41.hel <- vegdist(decostand(bro.ei.auc41, "hellinger"), "euclidean"); bro.ei.auc42.hel <- vegdist(decostand(bro.ei.auc42, "hellinger"), "euclidean");
bro.ei.auc43.hel <- vegdist(decostand(bro.ei.auc43, "hellinger"), "euclidean"); bro.ei.auc44.hel <- vegdist(decostand(bro.ei.auc44, "hellinger"), "euclidean"); 
bro.ei.auc45.hel <- vegdist(decostand(bro.ei.auc45, "hellinger"), "euclidean"); bro.ei.auc46.hel <- vegdist(decostand(bro.ei.auc46, "hellinger"), "euclidean"); 
bro.ei.auc47.hel <- vegdist(decostand(bro.ei.auc47, "hellinger"), "euclidean"); bro.ei.auc48.hel <- vegdist(decostand(bro.ei.auc48, "hellinger"), "euclidean");
bro.ei.auc49.hel <- vegdist(decostand(bro.ei.auc49, "hellinger"), "euclidean"); bro.ei.auc50.hel <- vegdist(decostand(bro.ei.auc50, "hellinger"), "euclidean"); 
bro.ei.auc51.hel <- vegdist(decostand(bro.ei.auc51, "hellinger"), "euclidean"); bro.ei.auc52.hel <- vegdist(decostand(bro.ei.auc52, "hellinger"), "euclidean"); 
bro.ei.auc53.hel <- vegdist(decostand(bro.ei.auc53, "hellinger"), "euclidean"); bro.ei.auc54.hel <- vegdist(decostand(bro.ei.auc54, "hellinger"), "euclidean");
bro.ei.auc55.hel <- vegdist(decostand(bro.ei.auc55, "hellinger"), "euclidean"); bro.ei.auc56.hel <- vegdist(decostand(bro.ei.auc56, "hellinger"), "euclidean"); 
bro.ei.auc57.hel <- vegdist(decostand(bro.ei.auc57, "hellinger"), "euclidean"); bro.ei.auc58.hel <- vegdist(decostand(bro.ei.auc58, "hellinger"), "euclidean"); 
bro.ei.auc59.hel <- vegdist(decostand(bro.ei.auc59, "hellinger"), "euclidean"); bro.ei.auc60.hel <- vegdist(decostand(bro.ei.auc60, "hellinger"), "euclidean");
bro.ei.auc61.hel <- vegdist(decostand(bro.ei.auc61, "hellinger"), "euclidean"); bro.ei.auc62.hel <- vegdist(decostand(bro.ei.auc62, "hellinger"), "euclidean"); 
bro.ei.auc63.hel <- vegdist(decostand(bro.ei.auc63, "hellinger"), "euclidean"); bro.ei.auc64.hel <- vegdist(decostand(bro.ei.auc64, "hellinger"), "euclidean"); 
bro.ei.auc65.hel <- vegdist(decostand(bro.ei.auc65, "hellinger"), "euclidean"); bro.ei.auc66.hel <- vegdist(decostand(bro.ei.auc66, "hellinger"), "euclidean");
bro.ei.auc67.hel <- vegdist(decostand(bro.ei.auc67, "hellinger"), "euclidean"); bro.ei.auc68.hel <- vegdist(decostand(bro.ei.auc68, "hellinger"), "euclidean"); 
bro.ei.auc69.hel <- vegdist(decostand(bro.ei.auc69, "hellinger"), "euclidean"); bro.ei.auc70.hel <- vegdist(decostand(bro.ei.auc70, "hellinger"), "euclidean"); 
bro.ei.auc71.hel <- vegdist(decostand(bro.ei.auc71, "hellinger"), "euclidean"); bro.ei.auc72.hel <- vegdist(decostand(bro.ei.auc72, "hellinger"), "euclidean");
bro.ei.auc73.hel <- vegdist(decostand(bro.ei.auc73, "hellinger"), "euclidean"); bro.ei.auc74.hel <- vegdist(decostand(bro.ei.auc74, "hellinger"), "euclidean"); 
bro.ei.auc75.hel <- vegdist(decostand(bro.ei.auc75, "hellinger"), "euclidean"); bro.ei.auc76.hel <- vegdist(decostand(bro.ei.auc76, "hellinger"), "euclidean"); 
bro.ei.auc77.hel <- vegdist(decostand(bro.ei.auc77, "hellinger"), "euclidean"); bro.ei.auc78.hel <- vegdist(decostand(bro.ei.auc78, "hellinger"), "euclidean");
bro.ei.auc79.hel <- vegdist(decostand(bro.ei.auc79, "hellinger"), "euclidean"); bro.ei.auc80.hel <- vegdist(decostand(bro.ei.auc80, "hellinger"), "euclidean"); 
bro.ei.auc81.hel <- vegdist(decostand(bro.ei.auc81, "hellinger"), "euclidean"); bro.ei.auc82.hel <- vegdist(decostand(bro.ei.auc82, "hellinger"), "euclidean"); 
bro.ei.auc83.hel <- vegdist(decostand(bro.ei.auc83, "hellinger"), "euclidean"); bro.ei.auc84.hel <- vegdist(decostand(bro.ei.auc84, "hellinger"), "euclidean");
bro.ei.auc85.hel <- vegdist(decostand(bro.ei.auc85, "hellinger"), "euclidean"); bro.ei.auc86.hel <- vegdist(decostand(bro.ei.auc86, "hellinger"), "euclidean"); 
bro.ei.auc87.hel <- vegdist(decostand(bro.ei.auc87, "hellinger"), "euclidean"); bro.ei.auc88.hel <- vegdist(decostand(bro.ei.auc88, "hellinger"), "euclidean"); 
bro.ei.auc89.hel <- vegdist(decostand(bro.ei.auc89, "hellinger"), "euclidean"); bro.ei.auc90.hel <- vegdist(decostand(bro.ei.auc90, "hellinger"), "euclidean");
bro.ei.auc91.hel <- vegdist(decostand(bro.ei.auc91, "hellinger"), "euclidean"); bro.ei.auc92.hel <- vegdist(decostand(bro.ei.auc92, "hellinger"), "euclidean"); 
bro.ei.auc93.hel <- vegdist(decostand(bro.ei.auc93, "hellinger"), "euclidean"); bro.ei.auc94.hel <- vegdist(decostand(bro.ei.auc94, "hellinger"), "euclidean"); 
bro.ei.auc95.hel <- vegdist(decostand(bro.ei.auc95, "hellinger"), "euclidean"); bro.ei.auc96.hel <- vegdist(decostand(bro.ei.auc96, "hellinger"), "euclidean");
bro.ei.auc97.hel <- vegdist(decostand(bro.ei.auc97, "hellinger"), "euclidean"); bro.ei.auc98.hel <- vegdist(decostand(bro.ei.auc98, "hellinger"), "euclidean"); 
bro.ei.auc99.hel <- vegdist(decostand(bro.ei.auc99, "hellinger"), "euclidean"); bro.ei.auc100.hel <- vegdist(decostand(bro.ei.auc100, "hellinger"), "euclidean");

#############
# PERMANOVA
set.seed(1); bro.ei.auc01.pman <- adonis(bro.ei.auc01.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc02.pman <- adonis(bro.ei.auc02.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc03.pman <- adonis(bro.ei.auc03.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc04.pman <- adonis(bro.ei.auc04.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc05.pman <- adonis(bro.ei.auc05.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc06.pman <- adonis(bro.ei.auc06.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc07.pman <- adonis(bro.ei.auc07.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc08.pman <- adonis(bro.ei.auc08.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc09.pman <- adonis(bro.ei.auc09.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc10.pman <- adonis(bro.ei.auc10.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc11.pman <- adonis(bro.ei.auc11.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc12.pman <- adonis(bro.ei.auc12.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc13.pman <- adonis(bro.ei.auc13.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc14.pman <- adonis(bro.ei.auc14.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc15.pman <- adonis(bro.ei.auc15.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc16.pman <- adonis(bro.ei.auc16.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc17.pman <- adonis(bro.ei.auc17.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc18.pman <- adonis(bro.ei.auc18.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc19.pman <- adonis(bro.ei.auc19.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc20.pman <- adonis(bro.ei.auc20.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc21.pman <- adonis(bro.ei.auc21.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc22.pman <- adonis(bro.ei.auc22.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc23.pman <- adonis(bro.ei.auc23.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc24.pman <- adonis(bro.ei.auc24.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc25.pman <- adonis(bro.ei.auc25.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc26.pman <- adonis(bro.ei.auc26.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc27.pman <- adonis(bro.ei.auc27.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc28.pman <- adonis(bro.ei.auc28.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc29.pman <- adonis(bro.ei.auc29.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc30.pman <- adonis(bro.ei.auc30.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc31.pman <- adonis(bro.ei.auc31.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc32.pman <- adonis(bro.ei.auc32.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc33.pman <- adonis(bro.ei.auc33.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc34.pman <- adonis(bro.ei.auc34.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc35.pman <- adonis(bro.ei.auc35.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc36.pman <- adonis(bro.ei.auc36.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc37.pman <- adonis(bro.ei.auc37.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc38.pman <- adonis(bro.ei.auc38.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc39.pman <- adonis(bro.ei.auc39.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc40.pman <- adonis(bro.ei.auc40.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc41.pman <- adonis(bro.ei.auc41.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc42.pman <- adonis(bro.ei.auc42.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc43.pman <- adonis(bro.ei.auc43.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc44.pman <- adonis(bro.ei.auc44.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc45.pman <- adonis(bro.ei.auc45.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc46.pman <- adonis(bro.ei.auc46.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc47.pman <- adonis(bro.ei.auc47.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc48.pman <- adonis(bro.ei.auc48.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc49.pman <- adonis(bro.ei.auc49.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc50.pman <- adonis(bro.ei.auc50.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc51.pman <- adonis(bro.ei.auc51.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc52.pman <- adonis(bro.ei.auc52.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc53.pman <- adonis(bro.ei.auc53.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc54.pman <- adonis(bro.ei.auc54.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc55.pman <- adonis(bro.ei.auc55.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc56.pman <- adonis(bro.ei.auc56.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc57.pman <- adonis(bro.ei.auc57.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc58.pman <- adonis(bro.ei.auc58.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc59.pman <- adonis(bro.ei.auc59.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc60.pman <- adonis(bro.ei.auc60.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc61.pman <- adonis(bro.ei.auc61.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc62.pman <- adonis(bro.ei.auc62.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc63.pman <- adonis(bro.ei.auc63.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc64.pman <- adonis(bro.ei.auc64.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc65.pman <- adonis(bro.ei.auc65.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc66.pman <- adonis(bro.ei.auc66.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc67.pman <- adonis(bro.ei.auc67.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc68.pman <- adonis(bro.ei.auc68.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc69.pman <- adonis(bro.ei.auc69.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc70.pman <- adonis(bro.ei.auc70.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc71.pman <- adonis(bro.ei.auc71.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc72.pman <- adonis(bro.ei.auc72.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc73.pman <- adonis(bro.ei.auc73.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc74.pman <- adonis(bro.ei.auc74.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc75.pman <- adonis(bro.ei.auc75.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc76.pman <- adonis(bro.ei.auc76.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc77.pman <- adonis(bro.ei.auc77.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc78.pman <- adonis(bro.ei.auc78.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc79.pman <- adonis(bro.ei.auc79.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc80.pman <- adonis(bro.ei.auc80.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc81.pman <- adonis(bro.ei.auc81.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc82.pman <- adonis(bro.ei.auc82.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc83.pman <- adonis(bro.ei.auc83.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc84.pman <- adonis(bro.ei.auc84.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc85.pman <- adonis(bro.ei.auc85.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc86.pman <- adonis(bro.ei.auc86.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc87.pman <- adonis(bro.ei.auc87.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc88.pman <- adonis(bro.ei.auc88.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc89.pman <- adonis(bro.ei.auc89.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc90.pman <- adonis(bro.ei.auc90.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc91.pman <- adonis(bro.ei.auc91.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc92.pman <- adonis(bro.ei.auc92.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc93.pman <- adonis(bro.ei.auc93.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc94.pman <- adonis(bro.ei.auc94.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc95.pman <- adonis(bro.ei.auc95.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc96.pman <- adonis(bro.ei.auc96.hel ~ brome.sample$type, permutations=999); 
set.seed(1); bro.ei.auc97.pman <- adonis(bro.ei.auc97.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc98.pman <- adonis(bro.ei.auc98.hel ~ brome.sample$type, permutations=999);
set.seed(1); bro.ei.auc99.pman <- adonis(bro.ei.auc99.hel ~ brome.sample$type, permutations=999); set.seed(1); bro.ei.auc100.pman <- adonis(bro.ei.auc100.hel ~ brome.sample$type, permutations=999) 

# Create a table of the outputs
brome.ei.permanova <- data.frame(test = c(paste("auc0", seq(1,9,1), sep = ""),
                                          paste("auc", seq(10,99,1), sep = ""),
                                          "auc100"),
                                 order = rep(c(1:100), 1),
                                 auc = c(seq(1,100,1)),
                                 SumsOfSqs = c(bro.ei.auc01.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc02.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc03.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc04.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc05.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc06.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc07.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc08.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc09.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc10.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc11.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc12.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc13.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc14.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc15.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc16.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc17.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc18.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc19.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc20.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc21.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc22.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc23.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc24.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc25.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc26.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc27.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc28.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc29.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc30.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc31.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc32.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc33.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc34.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc35.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc36.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc37.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc38.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc39.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc40.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc41.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc42.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc43.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc44.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc45.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc46.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc47.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc48.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc49.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc50.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc51.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc52.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc53.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc54.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc55.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc56.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc57.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc58.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc59.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc60.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc61.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc62.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc63.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc64.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc65.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc66.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc67.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc68.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc69.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc70.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc71.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc72.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc73.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc74.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc75.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc76.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc77.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc78.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc79.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc80.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc81.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc82.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc83.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc84.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc85.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc86.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc87.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc88.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc89.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc90.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc91.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc92.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc93.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc94.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc95.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc96.pman$aov.tab$SumsOfSqs[[1]], 
                                               bro.ei.auc97.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc98.pman$aov.tab$SumsOfSqs[[1]], bro.ei.auc99.pman$aov.tab$SumsOfSqs[[1]],
                                               bro.ei.auc100.pman$aov.tab$SumsOfSqs[[1]]),
                                 
                                 MeanSqs = c(bro.ei.auc01.pman$aov.tab$MeanSqs[[1]], bro.ei.auc02.pman$aov.tab$MeanSqs[[1]], bro.ei.auc03.pman$aov.tab$MeanSqs[[1]], bro.ei.auc04.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc05.pman$aov.tab$MeanSqs[[1]], bro.ei.auc06.pman$aov.tab$MeanSqs[[1]], bro.ei.auc07.pman$aov.tab$MeanSqs[[1]], bro.ei.auc08.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc09.pman$aov.tab$MeanSqs[[1]], bro.ei.auc10.pman$aov.tab$MeanSqs[[1]], bro.ei.auc11.pman$aov.tab$MeanSqs[[1]], bro.ei.auc12.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc13.pman$aov.tab$MeanSqs[[1]], bro.ei.auc14.pman$aov.tab$MeanSqs[[1]], bro.ei.auc15.pman$aov.tab$MeanSqs[[1]], bro.ei.auc16.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc17.pman$aov.tab$MeanSqs[[1]], bro.ei.auc18.pman$aov.tab$MeanSqs[[1]], bro.ei.auc19.pman$aov.tab$MeanSqs[[1]], bro.ei.auc20.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc21.pman$aov.tab$MeanSqs[[1]], bro.ei.auc22.pman$aov.tab$MeanSqs[[1]], bro.ei.auc23.pman$aov.tab$MeanSqs[[1]], bro.ei.auc24.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc25.pman$aov.tab$MeanSqs[[1]], bro.ei.auc26.pman$aov.tab$MeanSqs[[1]], bro.ei.auc27.pman$aov.tab$MeanSqs[[1]], bro.ei.auc28.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc29.pman$aov.tab$MeanSqs[[1]], bro.ei.auc30.pman$aov.tab$MeanSqs[[1]], bro.ei.auc31.pman$aov.tab$MeanSqs[[1]], bro.ei.auc32.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc33.pman$aov.tab$MeanSqs[[1]], bro.ei.auc34.pman$aov.tab$MeanSqs[[1]], bro.ei.auc35.pman$aov.tab$MeanSqs[[1]], bro.ei.auc36.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc37.pman$aov.tab$MeanSqs[[1]], bro.ei.auc38.pman$aov.tab$MeanSqs[[1]], bro.ei.auc39.pman$aov.tab$MeanSqs[[1]], bro.ei.auc40.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc41.pman$aov.tab$MeanSqs[[1]], bro.ei.auc42.pman$aov.tab$MeanSqs[[1]], bro.ei.auc43.pman$aov.tab$MeanSqs[[1]], bro.ei.auc44.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc45.pman$aov.tab$MeanSqs[[1]], bro.ei.auc46.pman$aov.tab$MeanSqs[[1]], bro.ei.auc47.pman$aov.tab$MeanSqs[[1]], bro.ei.auc48.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc49.pman$aov.tab$MeanSqs[[1]], bro.ei.auc50.pman$aov.tab$MeanSqs[[1]], bro.ei.auc51.pman$aov.tab$MeanSqs[[1]], bro.ei.auc52.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc53.pman$aov.tab$MeanSqs[[1]], bro.ei.auc54.pman$aov.tab$MeanSqs[[1]], bro.ei.auc55.pman$aov.tab$MeanSqs[[1]], bro.ei.auc56.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc57.pman$aov.tab$MeanSqs[[1]], bro.ei.auc58.pman$aov.tab$MeanSqs[[1]], bro.ei.auc59.pman$aov.tab$MeanSqs[[1]], bro.ei.auc60.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc61.pman$aov.tab$MeanSqs[[1]], bro.ei.auc62.pman$aov.tab$MeanSqs[[1]], bro.ei.auc63.pman$aov.tab$MeanSqs[[1]], bro.ei.auc64.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc65.pman$aov.tab$MeanSqs[[1]], bro.ei.auc66.pman$aov.tab$MeanSqs[[1]], bro.ei.auc67.pman$aov.tab$MeanSqs[[1]], bro.ei.auc68.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc69.pman$aov.tab$MeanSqs[[1]], bro.ei.auc70.pman$aov.tab$MeanSqs[[1]], bro.ei.auc71.pman$aov.tab$MeanSqs[[1]], bro.ei.auc72.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc73.pman$aov.tab$MeanSqs[[1]], bro.ei.auc74.pman$aov.tab$MeanSqs[[1]], bro.ei.auc75.pman$aov.tab$MeanSqs[[1]], bro.ei.auc76.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc77.pman$aov.tab$MeanSqs[[1]], bro.ei.auc78.pman$aov.tab$MeanSqs[[1]], bro.ei.auc79.pman$aov.tab$MeanSqs[[1]], bro.ei.auc80.pman$aov.tab$MeanSqs[[1]],
                                             bro.ei.auc81.pman$aov.tab$MeanSqs[[1]], bro.ei.auc82.pman$aov.tab$MeanSqs[[1]], bro.ei.auc83.pman$aov.tab$MeanSqs[[1]], bro.ei.auc84.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc85.pman$aov.tab$MeanSqs[[1]], bro.ei.auc86.pman$aov.tab$MeanSqs[[1]], bro.ei.auc87.pman$aov.tab$MeanSqs[[1]], bro.ei.auc88.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc89.pman$aov.tab$MeanSqs[[1]], bro.ei.auc90.pman$aov.tab$MeanSqs[[1]], bro.ei.auc91.pman$aov.tab$MeanSqs[[1]], bro.ei.auc92.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc93.pman$aov.tab$MeanSqs[[1]], bro.ei.auc94.pman$aov.tab$MeanSqs[[1]], bro.ei.auc95.pman$aov.tab$MeanSqs[[1]], bro.ei.auc96.pman$aov.tab$MeanSqs[[1]], 
                                             bro.ei.auc97.pman$aov.tab$MeanSqs[[1]], bro.ei.auc98.pman$aov.tab$MeanSqs[[1]], bro.ei.auc99.pman$aov.tab$MeanSqs[[1]], bro.ei.auc100.pman$aov.tab$MeanSqs[[1]]),
                                 
                                 F.model = c(bro.ei.auc01.pman$aov.tab$F.Model[1], bro.ei.auc02.pman$aov.tab$F.Model[1], bro.ei.auc03.pman$aov.tab$F.Model[1], bro.ei.auc04.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc05.pman$aov.tab$F.Model[1], bro.ei.auc06.pman$aov.tab$F.Model[1], bro.ei.auc07.pman$aov.tab$F.Model[1], bro.ei.auc08.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc09.pman$aov.tab$F.Model[1], bro.ei.auc10.pman$aov.tab$F.Model[1], bro.ei.auc11.pman$aov.tab$F.Model[1], bro.ei.auc12.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc13.pman$aov.tab$F.Model[1], bro.ei.auc14.pman$aov.tab$F.Model[1], bro.ei.auc15.pman$aov.tab$F.Model[1], bro.ei.auc16.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc17.pman$aov.tab$F.Model[1], bro.ei.auc18.pman$aov.tab$F.Model[1], bro.ei.auc19.pman$aov.tab$F.Model[1], bro.ei.auc20.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc21.pman$aov.tab$F.Model[1], bro.ei.auc22.pman$aov.tab$F.Model[1], bro.ei.auc23.pman$aov.tab$F.Model[1], bro.ei.auc24.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc25.pman$aov.tab$F.Model[1], bro.ei.auc26.pman$aov.tab$F.Model[1], bro.ei.auc27.pman$aov.tab$F.Model[1], bro.ei.auc28.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc29.pman$aov.tab$F.Model[1], bro.ei.auc30.pman$aov.tab$F.Model[1], bro.ei.auc31.pman$aov.tab$F.Model[1], bro.ei.auc32.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc33.pman$aov.tab$F.Model[1], bro.ei.auc34.pman$aov.tab$F.Model[1], bro.ei.auc35.pman$aov.tab$F.Model[1], bro.ei.auc36.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc37.pman$aov.tab$F.Model[1], bro.ei.auc38.pman$aov.tab$F.Model[1], bro.ei.auc39.pman$aov.tab$F.Model[1], bro.ei.auc40.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc41.pman$aov.tab$F.Model[1], bro.ei.auc42.pman$aov.tab$F.Model[1], bro.ei.auc43.pman$aov.tab$F.Model[1], bro.ei.auc44.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc45.pman$aov.tab$F.Model[1], bro.ei.auc46.pman$aov.tab$F.Model[1], bro.ei.auc47.pman$aov.tab$F.Model[1], bro.ei.auc48.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc49.pman$aov.tab$F.Model[1], bro.ei.auc50.pman$aov.tab$F.Model[1], bro.ei.auc51.pman$aov.tab$F.Model[1], bro.ei.auc52.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc53.pman$aov.tab$F.Model[1], bro.ei.auc54.pman$aov.tab$F.Model[1], bro.ei.auc55.pman$aov.tab$F.Model[1], bro.ei.auc56.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc57.pman$aov.tab$F.Model[1], bro.ei.auc58.pman$aov.tab$F.Model[1], bro.ei.auc59.pman$aov.tab$F.Model[1], bro.ei.auc60.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc61.pman$aov.tab$F.Model[1], bro.ei.auc62.pman$aov.tab$F.Model[1], bro.ei.auc63.pman$aov.tab$F.Model[1], bro.ei.auc64.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc65.pman$aov.tab$F.Model[1], bro.ei.auc66.pman$aov.tab$F.Model[1], bro.ei.auc67.pman$aov.tab$F.Model[1], bro.ei.auc68.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc69.pman$aov.tab$F.Model[1], bro.ei.auc70.pman$aov.tab$F.Model[1], bro.ei.auc71.pman$aov.tab$F.Model[1], bro.ei.auc72.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc73.pman$aov.tab$F.Model[1], bro.ei.auc74.pman$aov.tab$F.Model[1], bro.ei.auc75.pman$aov.tab$F.Model[1], bro.ei.auc76.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc77.pman$aov.tab$F.Model[1], bro.ei.auc78.pman$aov.tab$F.Model[1], bro.ei.auc79.pman$aov.tab$F.Model[1], bro.ei.auc80.pman$aov.tab$F.Model[1],
                                             bro.ei.auc81.pman$aov.tab$F.Model[1], bro.ei.auc82.pman$aov.tab$F.Model[1], bro.ei.auc83.pman$aov.tab$F.Model[1], bro.ei.auc84.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc85.pman$aov.tab$F.Model[1], bro.ei.auc86.pman$aov.tab$F.Model[1], bro.ei.auc87.pman$aov.tab$F.Model[1], bro.ei.auc88.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc89.pman$aov.tab$F.Model[1], bro.ei.auc90.pman$aov.tab$F.Model[1], bro.ei.auc91.pman$aov.tab$F.Model[1], bro.ei.auc92.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc93.pman$aov.tab$F.Model[1], bro.ei.auc94.pman$aov.tab$F.Model[1], bro.ei.auc95.pman$aov.tab$F.Model[1], bro.ei.auc96.pman$aov.tab$F.Model[1], 
                                             bro.ei.auc97.pman$aov.tab$F.Model[1], bro.ei.auc98.pman$aov.tab$F.Model[1], bro.ei.auc99.pman$aov.tab$F.Model[1], bro.ei.auc100.pman$aov.tab$F.Model[1]),
                                 
                                 R2 = c(bro.ei.auc01.pman$aov.tab$R2[1], bro.ei.auc02.pman$aov.tab$R2[1], bro.ei.auc03.pman$aov.tab$R2[1], bro.ei.auc04.pman$aov.tab$R2[1], 
                                        bro.ei.auc05.pman$aov.tab$R2[1], bro.ei.auc06.pman$aov.tab$R2[1], bro.ei.auc07.pman$aov.tab$R2[1], bro.ei.auc08.pman$aov.tab$R2[1], 
                                        bro.ei.auc09.pman$aov.tab$R2[1], bro.ei.auc10.pman$aov.tab$R2[1], bro.ei.auc11.pman$aov.tab$R2[1], bro.ei.auc12.pman$aov.tab$R2[1], 
                                        bro.ei.auc13.pman$aov.tab$R2[1], bro.ei.auc14.pman$aov.tab$R2[1], bro.ei.auc15.pman$aov.tab$R2[1], bro.ei.auc16.pman$aov.tab$R2[1], 
                                        bro.ei.auc17.pman$aov.tab$R2[1], bro.ei.auc18.pman$aov.tab$R2[1], bro.ei.auc19.pman$aov.tab$R2[1], bro.ei.auc20.pman$aov.tab$R2[1], 
                                        bro.ei.auc21.pman$aov.tab$R2[1], bro.ei.auc22.pman$aov.tab$R2[1], bro.ei.auc23.pman$aov.tab$R2[1], bro.ei.auc24.pman$aov.tab$R2[1], 
                                        bro.ei.auc25.pman$aov.tab$R2[1], bro.ei.auc26.pman$aov.tab$R2[1], bro.ei.auc27.pman$aov.tab$R2[1], bro.ei.auc28.pman$aov.tab$R2[1], 
                                        bro.ei.auc29.pman$aov.tab$R2[1], bro.ei.auc30.pman$aov.tab$R2[1], bro.ei.auc31.pman$aov.tab$R2[1], bro.ei.auc32.pman$aov.tab$R2[1], 
                                        bro.ei.auc33.pman$aov.tab$R2[1], bro.ei.auc34.pman$aov.tab$R2[1], bro.ei.auc35.pman$aov.tab$R2[1], bro.ei.auc36.pman$aov.tab$R2[1], 
                                        bro.ei.auc37.pman$aov.tab$R2[1], bro.ei.auc38.pman$aov.tab$R2[1], bro.ei.auc39.pman$aov.tab$R2[1], bro.ei.auc40.pman$aov.tab$R2[1], 
                                        bro.ei.auc41.pman$aov.tab$R2[1], bro.ei.auc42.pman$aov.tab$R2[1], bro.ei.auc43.pman$aov.tab$R2[1], bro.ei.auc44.pman$aov.tab$R2[1], 
                                        bro.ei.auc45.pman$aov.tab$R2[1], bro.ei.auc46.pman$aov.tab$R2[1], bro.ei.auc47.pman$aov.tab$R2[1], bro.ei.auc48.pman$aov.tab$R2[1], 
                                        bro.ei.auc49.pman$aov.tab$R2[1], bro.ei.auc50.pman$aov.tab$R2[1], bro.ei.auc51.pman$aov.tab$R2[1], bro.ei.auc52.pman$aov.tab$R2[1], 
                                        bro.ei.auc53.pman$aov.tab$R2[1], bro.ei.auc54.pman$aov.tab$R2[1], bro.ei.auc55.pman$aov.tab$R2[1], bro.ei.auc56.pman$aov.tab$R2[1], 
                                        bro.ei.auc57.pman$aov.tab$R2[1], bro.ei.auc58.pman$aov.tab$R2[1], bro.ei.auc59.pman$aov.tab$R2[1], bro.ei.auc60.pman$aov.tab$R2[1], 
                                        bro.ei.auc61.pman$aov.tab$R2[1], bro.ei.auc62.pman$aov.tab$R2[1], bro.ei.auc63.pman$aov.tab$R2[1], bro.ei.auc64.pman$aov.tab$R2[1], 
                                        bro.ei.auc65.pman$aov.tab$R2[1], bro.ei.auc66.pman$aov.tab$R2[1], bro.ei.auc67.pman$aov.tab$R2[1], bro.ei.auc68.pman$aov.tab$R2[1], 
                                        bro.ei.auc69.pman$aov.tab$R2[1], bro.ei.auc70.pman$aov.tab$R2[1], bro.ei.auc71.pman$aov.tab$R2[1], bro.ei.auc72.pman$aov.tab$R2[1], 
                                        bro.ei.auc73.pman$aov.tab$R2[1], bro.ei.auc74.pman$aov.tab$R2[1], bro.ei.auc75.pman$aov.tab$R2[1], bro.ei.auc76.pman$aov.tab$R2[1], 
                                        bro.ei.auc77.pman$aov.tab$R2[1], bro.ei.auc78.pman$aov.tab$R2[1], bro.ei.auc79.pman$aov.tab$R2[1], bro.ei.auc80.pman$aov.tab$R2[1],
                                        bro.ei.auc81.pman$aov.tab$R2[1], bro.ei.auc82.pman$aov.tab$R2[1], bro.ei.auc83.pman$aov.tab$R2[1], bro.ei.auc84.pman$aov.tab$R2[1], 
                                        bro.ei.auc85.pman$aov.tab$R2[1], bro.ei.auc86.pman$aov.tab$R2[1], bro.ei.auc87.pman$aov.tab$R2[1], bro.ei.auc88.pman$aov.tab$R2[1], 
                                        bro.ei.auc89.pman$aov.tab$R2[1], bro.ei.auc90.pman$aov.tab$R2[1], bro.ei.auc91.pman$aov.tab$R2[1], bro.ei.auc92.pman$aov.tab$R2[1], 
                                        bro.ei.auc93.pman$aov.tab$R2[1], bro.ei.auc94.pman$aov.tab$R2[1], bro.ei.auc95.pman$aov.tab$R2[1], bro.ei.auc96.pman$aov.tab$R2[1], 
                                        bro.ei.auc97.pman$aov.tab$R2[1], bro.ei.auc98.pman$aov.tab$R2[1], bro.ei.auc99.pman$aov.tab$R2[1], bro.ei.auc100.pman$aov.tab$R2[1]),
                                 
                                 Pval = c(bro.ei.auc01.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc02.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc03.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc04.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc05.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc06.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc07.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc08.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc09.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc10.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc11.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc12.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc13.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc14.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc15.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc16.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc17.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc18.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc19.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc20.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc21.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc22.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc23.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc24.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc25.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc26.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc27.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc28.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc29.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc30.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc31.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc32.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc33.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc34.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc35.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc36.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc37.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc38.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc39.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc40.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc41.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc42.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc43.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc44.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc45.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc46.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc47.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc48.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc49.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc50.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc51.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc52.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc53.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc54.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc55.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc56.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc57.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc58.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc59.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc60.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc61.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc62.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc63.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc64.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc65.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc66.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc67.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc68.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc69.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc70.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc71.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc72.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc73.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc74.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc75.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc76.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc77.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc78.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc79.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc80.pman$aov.tab$`Pr(>F)`[1],
                                          bro.ei.auc81.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc82.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc83.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc84.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc85.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc86.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc87.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc88.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc89.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc90.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc91.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc92.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc93.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc94.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc95.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc96.pman$aov.tab$`Pr(>F)`[1], 
                                          bro.ei.auc97.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc98.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc99.pman$aov.tab$`Pr(>F)`[1], bro.ei.auc100.pman$aov.tab$`Pr(>F)`[1]),
                                 
                                 N.taxa = c(ncol(bro.ei.auc01), ncol(bro.ei.auc02), ncol(bro.ei.auc03), ncol(bro.ei.auc04), ncol(bro.ei.auc05), ncol(bro.ei.auc06), 
                                            ncol(bro.ei.auc07), ncol(bro.ei.auc08), ncol(bro.ei.auc09), ncol(bro.ei.auc10), ncol(bro.ei.auc11), ncol(bro.ei.auc12), 
                                            ncol(bro.ei.auc13), ncol(bro.ei.auc14), ncol(bro.ei.auc15), ncol(bro.ei.auc16), ncol(bro.ei.auc17), ncol(bro.ei.auc18), 
                                            ncol(bro.ei.auc19), ncol(bro.ei.auc20), ncol(bro.ei.auc21), ncol(bro.ei.auc22), ncol(bro.ei.auc23), ncol(bro.ei.auc24), 
                                            ncol(bro.ei.auc25), ncol(bro.ei.auc26), ncol(bro.ei.auc27), ncol(bro.ei.auc28), ncol(bro.ei.auc29), ncol(bro.ei.auc30), 
                                            ncol(bro.ei.auc31), ncol(bro.ei.auc32), ncol(bro.ei.auc33), ncol(bro.ei.auc34), ncol(bro.ei.auc35), ncol(bro.ei.auc36), 
                                            ncol(bro.ei.auc37), ncol(bro.ei.auc38), ncol(bro.ei.auc39), ncol(bro.ei.auc40), ncol(bro.ei.auc41), ncol(bro.ei.auc42), 
                                            ncol(bro.ei.auc43), ncol(bro.ei.auc44), ncol(bro.ei.auc45), ncol(bro.ei.auc46), ncol(bro.ei.auc47), ncol(bro.ei.auc48), 
                                            ncol(bro.ei.auc49), ncol(bro.ei.auc50), ncol(bro.ei.auc51), ncol(bro.ei.auc52), ncol(bro.ei.auc53), ncol(bro.ei.auc54), 
                                            ncol(bro.ei.auc55), ncol(bro.ei.auc56), ncol(bro.ei.auc57), ncol(bro.ei.auc58), ncol(bro.ei.auc59), ncol(bro.ei.auc60), 
                                            ncol(bro.ei.auc61), ncol(bro.ei.auc62), ncol(bro.ei.auc63), ncol(bro.ei.auc64), ncol(bro.ei.auc65), ncol(bro.ei.auc66), 
                                            ncol(bro.ei.auc67), ncol(bro.ei.auc68), ncol(bro.ei.auc69), ncol(bro.ei.auc70), ncol(bro.ei.auc71), ncol(bro.ei.auc72), 
                                            ncol(bro.ei.auc73), ncol(bro.ei.auc74), ncol(bro.ei.auc75), ncol(bro.ei.auc76), ncol(bro.ei.auc77), ncol(bro.ei.auc78), 
                                            ncol(bro.ei.auc79), ncol(bro.ei.auc80), ncol(bro.ei.auc81), ncol(bro.ei.auc82), ncol(bro.ei.auc83), ncol(bro.ei.auc84), 
                                            ncol(bro.ei.auc85), ncol(bro.ei.auc86), ncol(bro.ei.auc87), ncol(bro.ei.auc88), ncol(bro.ei.auc89), ncol(bro.ei.auc90), 
                                            ncol(bro.ei.auc91), ncol(bro.ei.auc92), ncol(bro.ei.auc93), ncol(bro.ei.auc94), ncol(bro.ei.auc95), ncol(bro.ei.auc96), 
                                            ncol(bro.ei.auc97), ncol(bro.ei.auc98), ncol(bro.ei.auc99), ncol(bro.ei.auc100)))

# Scale the F-values to 0-1
brome.ei.Fscaled <- (brome.ei.permanova$F.model - min(brome.ei.permanova$F.model)) / (max(brome.ei.permanova$F.model) - min(brome.ei.permanova$F.model))
brome.ei.permanova$F.model.scale <- NA
brome.ei.permanova$F.model.scale <- brome.ei.Fscaled

write.csv(brome.ei.permanova, "~/Dropbox/CFREF Work/Winnowing/SteveM/Brome/Network/Brome.ei.permanova.csv")


# Build the lists and df that will be combined into one data frame
bro.tmp <- data.frame(cbind(test = "auc00", order = 0, auc = 0, SumsOfSqs = 0, MeanSqs = 0, F.model = 0, R2 = 0, Pval = 0, N.taxa = 0, F.model.scale = 0)) # Make the y-vals start at zero
bro.tmp$order <- as.numeric(as.character(bro.tmp$order))
bro.tmp$auc <- as.numeric(as.character(bro.tmp$auc))
bro.tmp$SumsOfSqs <- as.numeric(as.character(bro.tmp$SumsOfSqs))
bro.tmp$MeanSqs <- as.numeric(as.character(bro.tmp$MeanSqs))
bro.tmp$F.model <- as.numeric(as.character(bro.tmp$F.model))
bro.tmp$R2 <- as.numeric(as.character(bro.tmp$R2))
bro.tmp$Pval <- as.numeric(as.character(bro.tmp$Pval))
bro.tmp$N.taxa <- as.numeric(as.character(bro.tmp$N.taxa))
bro.tmp$F.model.scale <- as.numeric(as.character(bro.tmp$F.model.scale))

# Merge into one df
brome.dg.permanova <- rbind(bro.tmp, brome.dg.permanova)
brome.ei.permanova <- rbind(bro.tmp, brome.ei.permanova)
brome.cl.permanova <- rbind(bro.tmp, brome.cl.permanova)
brome.bw.permanova <- rbind(bro.tmp, brome.bw.permanova)

# Calculate the sliding sd for the scaled F-stat
dg.sd <- rollapply(brome.dg.permanova$F.model.scale, width = 5, FUN = sd, fill = NA)
ei.sd <- rollapply(brome.ei.permanova$F.model.scale, width = 5, FUN = sd, fill = NA)
cl.sd <- rollapply(brome.cl.permanova$F.model.scale, width = 5, FUN = sd, fill = NA)
bw.sd <- rollapply(brome.bw.permanova$F.model.scale, width = 5, FUN = sd, fill = NA)

require(splines)

## Plotting the metrics. Export at 3 x 10
# Degree
par(mfrow = c(4,1))
par(mar = c(2,3,1,1), oma = c(2,1,0.5,0.5))
plot(brome.dg.permanova[c(40:45,seq(46,101,5)),3], brome.dg.permanova[c(40:45,seq(46,101,5)),10],  type = "p", col = "black", cex = 1.2, 
     pch = 21, ylim = c(0,1.05), xlim = c(-1,101), xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
par(new=T)
plot(brome.dg.permanova[c(seq(1,36,5),37:39),3], brome.dg.permanova[c(seq(1,36,5),37:39),10],  type = "p", col = "black", cex = 1.2,
     pch = 16, ylim = c(0,1.05), xlim = c(-1,101), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
brome.dg.spline5 <- smooth.spline(brome.dg.permanova$auc, brome.dg.permanova$F.model.scale, spar=0.6)
lines(brome.dg.spline5, col = "black", lwd = 1.5)
lines(dg.sd, col = alpha("black", 0.6), lwd = 1, lty = 2)
points(brome.dg.permanova$auc[[37]], brome.dg.permanova$F.model.scale[[37]], col = "blue", pch = 16, cex = 1.2)
text(brome.dg.permanova$auc[[37]]+40, brome.dg.permanova$F.model.scale[[37]],  paste(brome.dg.permanova$N.taxa[[37]], "OTUs", sep = " "), col = "blue", adj = c(1,0.5))
legend("topright", expression(bold("E")), bty = "n", cex = 1, inset = c(0.05,0))
axis(side = 1, at = seq(0,100,20), labels = seq(0,100,20))
axis(side = 2, at = seq(0,1.0,0.25), labels = c("0","0.25","0.50","0.75","1"))


# Closeness
plot(brome.cl.permanova[c(8:10,seq(11,101,5)),3], brome.cl.permanova[c(8:10,seq(11,101,5)),10],  type = "p", col = "black", cex = 1.2, 
     pch = 21, ylim = c(0,1.05), xlim = c(-1,101), xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
par(new=T)
plot(brome.cl.permanova[c(1:6),3], brome.cl.permanova[c(1:6),10],  type = "p", col = "black", cex = 1.2,
     pch = 16, ylim = c(0,1.05), xlim = c(-1,101), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
brome.cl.spline5 <- smooth.spline(brome.cl.permanova$auc, brome.cl.permanova$F.model.scale, spar=0.6)
lines(brome.cl.spline5, col = "black", lwd = 1.5)
lines(cl.sd, col = alpha("black", 0.6), lwd = 1, lty = 2)
points(brome.cl.permanova$auc[[5]], brome.cl.permanova$F.model.scale[[5]], col = "blue", pch = 16, cex = 1.2)
text(brome.cl.permanova$auc[[5]]+40, brome.cl.permanova$F.model.scale[[5]],  paste(brome.cl.permanova$N.taxa[[5]], "OTUs", sep = " "), col = "blue", adj = c(1,0.5))
legend("topright", expression(bold("F")), bty = "n", cex = 1, inset = c(0.05,0))
axis(side = 1, at = seq(0,100,20), labels = seq(0,100,20))
axis(side = 2, at = seq(0,1.0,0.25), labels = c("0","0.25","0.50","0.75","1"))

# Betweenness
plot(brome.bw.permanova[c(37:40,seq(41,101,5)),3], brome.bw.permanova[c(37:40,seq(41,101,5)),10],  type = "p", col = "black", cex = 1.2, 
     pch = 21, ylim = c(0,1.05), xlim = c(-1,101), xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
par(new=T)
plot(brome.bw.permanova[c(seq(1,31,5),32:35),3], brome.bw.permanova[c(seq(1,31,5),32:35),10],  type = "p", col = "black", cex = 1.2,
     pch = 16, ylim = c(0,1.05), xlim = c(-1,101), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
brome.bw.spline5 <- smooth.spline(brome.bw.permanova$auc, brome.bw.permanova$F.model.scale, spar=0.6)
lines(brome.bw.spline5, col = "black", lwd = 1.5)
lines(bw.sd, col = alpha("black", 0.6), lwd = 1, lty = 2)
points(brome.bw.permanova$auc[[36]], brome.bw.permanova$F.model.scale[[36]], col = "blue", pch = 16, cex = 1.2)
text(brome.bw.permanova$auc[[36]]+40, brome.bw.permanova$F.model.scale[[36]]+0.05,  paste(brome.bw.permanova$N.taxa[[36]], "OTUs", sep = " "), col = "blue", adj = c(1,0.5))
legend("topright", expression(bold("G")), bty = "n", cex = 1, inset = c(0.05,0))
axis(side = 1, at = seq(0,100,20), labels = seq(0,100,20))
axis(side = 2, at = seq(0,1.0,0.25), labels = c("0","0.25","0.50","0.75","1"))

# Eigenvector
plot(brome.ei.permanova[c(26:30,seq(31,101,5)),3], brome.ei.permanova[c(26:30,seq(31,101,5)),10],  type = "p", col = "black", cex = 1.2, 
     pch = 21, ylim = c(0,1.05), xlim = c(-1,101), xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
par(new=T)
plot(brome.ei.permanova[c(seq(1,21,5),22:23),3], brome.ei.permanova[c(seq(1,21,5),22:23),10],  type = "p", col = "black", cex = 1.2,
     pch = 16, ylim = c(0,1.05), xlim = c(-1,101), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
brome.ei.spline5 <- smooth.spline(brome.ei.permanova$auc, brome.ei.permanova$F.model.scale, spar=0.6)
lines(brome.ei.spline5, col = "black", lwd = 1.5)
lines(ei.sd, col = alpha("black", 0.6), lwd = 1, lty = 2)
points(brome.ei.permanova$auc[[22]], brome.ei.permanova$F.model.scale[[22]], col = "blue", pch = 16, cex = 1.2)
text(brome.ei.permanova$auc[[22]]+40, brome.ei.permanova$F.model.scale[[22]],  paste(brome.ei.permanova$N.taxa[[22]], "OTUs", sep = " "), col = "blue", adj = c(1,0.5))
legend("topright", expression(bold("H")), bty = "n", cex = 1, inset = c(0.05,0))
axis(side = 1, at = seq(0,100,20), labels = seq(0,100,20))
axis(side = 2, at = seq(0,1.0,0.25), labels = c("0","0.25","0.50","0.75","1"))

mtext("AUC (%)", side = 1, cex = 0.7, line = 2.5)
mtext("F-stat (scaled)", side = 2, cex = 0.7, line = 2.5, at = 2.85)
