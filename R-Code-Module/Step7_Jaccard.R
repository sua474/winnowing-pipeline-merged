library(DescTools) # Cramér's V
library(dplyr)
library(irr)
library(scales)
library(vegan)

# Clear the workspace
rm(list=ls())

# Read in the full abundance table for archaea, bacteria, and fungi
bfa <- read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/arc_bac_fun_abundances.csv", header = T)
names(bfa)[1] <- "OTUid"

# Read in the leave-one-out data frame
loo <- read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/results-brome_total_bad_removed.csv", stringsAsFactors = F)

# Replace blank cells with NA
loo <- apply(loo, 2, function(x) gsub("^$|^ $", NA, x))

# Convert to matrix
loo <- as.matrix(loo[,c(16:178)])

# Transpose the data
loo2 <- t(loo)
loo3 <- as.data.frame(loo2, stringsAsFactors = F)

## Calculate Cohen's kappa: If the raters are in complete agreement then kappa = 1. If there is no agreement among the raters other than what would be expected by chance kappa = 0. 

# Spearman, Hellinger, Betweenness, R > 0.2 
A1 <- kappa2(cbind(loo3$V1, loo3$V5)); A2 <- kappa2(cbind(loo3$V2, loo3$V5)); A3 <- kappa2(cbind(loo3$V3, loo3$V5)); A4 <- kappa2(cbind(loo3$V4, loo3$V5))
# Spearman, Hellinger, Closeness, R > 0.2 
A6 <- kappa2(cbind(loo3$V6, loo3$V10)); A7 <- kappa2(cbind(loo3$V7, loo3$V10)); A8 <- kappa2(cbind(loo3$V8, loo3$V10)); A9 <- kappa2(cbind(loo3$V9, loo3$V10))
# Spearman, Hellinger, Degree, R > 0.2 
A11 <- kappa2(cbind(loo3$V11, loo3$V15)); A12 <- kappa2(cbind(loo3$V12, loo3$V15)); A13 <- kappa2(cbind(loo3$V13, loo3$V15)); A14 <- kappa2(cbind(loo3$V14, loo3$V15))
# Spearman, Hellinger, Eigenvector, R > 0.2 
A16 <- kappa2(cbind(loo3$V16, loo3$V20)); A17 <- kappa2(cbind(loo3$V17, loo3$V20)); A18 <- kappa2(cbind(loo3$V18, loo3$V20)); A19 <- kappa2(cbind(loo3$V19, loo3$V20))

# MIC, Hellinger, Betweenness, R > 0.2 
A21 <- kappa2(cbind(loo3$V21, loo3$V25)); A22 <- kappa2(cbind(loo3$V22, loo3$V25)); A23 <- kappa2(cbind(loo3$V23, loo3$V25)); A24 <- kappa2(cbind(loo3$V24, loo3$V25))
# MIC, Hellinger, Closeness, R > 0.2 
A26 <- kappa2(cbind(loo3$V26, loo3$V30)); A27 <- kappa2(cbind(loo3$V27, loo3$V30)); A28 <- kappa2(cbind(loo3$V28, loo3$V30)); A29 <- kappa2(cbind(loo3$V29, loo3$V30))
# MIC, Hellinger, Degree, R > 0.2 
A31 <- kappa2(cbind(loo3$V31, loo3$V35)); A32 <- kappa2(cbind(loo3$V32, loo3$V35)); A33 <- kappa2(cbind(loo3$V33, loo3$V35)); A34 <- kappa2(cbind(loo3$V34, loo3$V35))
# MIC, Hellinger, Eigenvector, R > 0.2 
A36 <- kappa2(cbind(loo3$V36, loo3$V40)); A37 <- kappa2(cbind(loo3$V37, loo3$V40)); A38 <- kappa2(cbind(loo3$V38, loo3$V40)); A39 <- kappa2(cbind(loo3$V39, loo3$V40))

#
# Spearman, Add1, Betweenness, R > 0.2 
A41 <- kappa2(cbind(loo3$V41, loo3$V45)); A42 <- kappa2(cbind(loo3$V42, loo3$V45)); A43 <- kappa2(cbind(loo3$V43, loo3$V45)); A44 <- kappa2(cbind(loo3$V44, loo3$V45))
# Spearman, Hellinger, Closeness, R > 0.2 
A46 <- kappa2(cbind(loo3$V46, loo3$V50)); A47 <- kappa2(cbind(loo3$V47, loo3$V50)); A48 <- kappa2(cbind(loo3$V48, loo3$V50)); A49 <- kappa2(cbind(loo3$V49, loo3$V50))
# Spearman, Hellinger, Degree, R > 0.2 
A51 <- kappa2(cbind(loo3$V51, loo3$V55)); A52 <- kappa2(cbind(loo3$V52, loo3$V55)); A53 <- kappa2(cbind(loo3$V53, loo3$V55)); A54 <- kappa2(cbind(loo3$V54, loo3$V55))
# Spearman, Hellinger, Eigenvector, R > 0.2 
A56 <- kappa2(cbind(loo3$V56, loo3$V60)); A57 <- kappa2(cbind(loo3$V57, loo3$V60)); A58 <- kappa2(cbind(loo3$V58, loo3$V60)); A59 <- kappa2(cbind(loo3$V59, loo3$V60))

# MIC, Add1, Betweenness, R > 0.2
A61 <- kappa2(cbind(loo3$V61, loo3$V65)); A62 <- kappa2(cbind(loo3$V62, loo3$V65)); A63 <- kappa2(cbind(loo3$V63, loo3$V65)); A64 <- kappa2(cbind(loo3$V64, loo3$V65))
# MIC, Hellinger, Closeness, R > 0.2 
A66 <- kappa2(cbind(loo3$V66, loo3$V70)); A67 <- kappa2(cbind(loo3$V67, loo3$V70)); A68 <- kappa2(cbind(loo3$V68, loo3$V70)); A69 <- kappa2(cbind(loo3$V69, loo3$V70))
# MIC, Hellinger, Degree, R > 0.2 
A71 <- kappa2(cbind(loo3$V71, loo3$V75)); A72 <- kappa2(cbind(loo3$V72, loo3$V75)); A73 <- kappa2(cbind(loo3$V73, loo3$V75)); A74 <- kappa2(cbind(loo3$V74, loo3$V75))
# MIC, Hellinger, Eigenvector, R > 0.2 
A76 <- kappa2(cbind(loo3$V76, loo3$V80)); A77 <- kappa2(cbind(loo3$V77, loo3$V80)); A78 <- kappa2(cbind(loo3$V78, loo3$V80)); A79 <- kappa2(cbind(loo3$V79, loo3$V80))

#
# Spearman, Hellinger, Betweenness, R > 0.3
A81 <- kappa2(cbind(loo3$V81, loo3$V85)); A82 <- kappa2(cbind(loo3$V82, loo3$V85)); A83 <- kappa2(cbind(loo3$V83, loo3$V85)); A84 <- kappa2(cbind(loo3$V84, loo3$V85))
# Spearman, Hellinger, Closeness, R > 0.3
A86 <- kappa2(cbind(loo3$V86, loo3$V90)); A87 <- kappa2(cbind(loo3$V87, loo3$V90)); A88 <- kappa2(cbind(loo3$V88, loo3$V90)); A89 <- kappa2(cbind(loo3$V89, loo3$V90))
# Spearman, Hellinger, Degree, R > 0.3 
A91 <- kappa2(cbind(loo3$V91, loo3$V95)); A92 <- kappa2(cbind(loo3$V92, loo3$V95)); A93 <- kappa2(cbind(loo3$V93, loo3$V95)); A94 <- kappa2(cbind(loo3$V94, loo3$V95))
# Spearman, Hellinger, Eigenvector, R > 0.3
A96 <- kappa2(cbind(loo3$V96, loo3$V100)); A97 <- kappa2(cbind(loo3$V97, loo3$V100)); A98 <- kappa2(cbind(loo3$V98, loo3$V100)); A99 <- kappa2(cbind(loo3$V99, loo3$V100))

# MIC, Hellinger, Betweenness, R > 0.3
A101 <- kappa2(cbind(loo3$V101, loo3$V105)); A102 <- kappa2(cbind(loo3$V102, loo3$V105)); A103 <- kappa2(cbind(loo3$V103, loo3$V105)); A104 <- kappa2(cbind(loo3$V104, loo3$V105))
# MIC, Hellinger, Closeness, R > 0.3
A106 <- kappa2(cbind(loo3$V106, loo3$V110)); A107 <- kappa2(cbind(loo3$V107, loo3$V110)); A108 <- kappa2(cbind(loo3$V108, loo3$V110)); A109 <- kappa2(cbind(loo3$V109, loo3$V110))
# MIC, Hellinger, Degree, R > 0.3
A111 <- kappa2(cbind(loo3$V111, loo3$V115)); A112 <- kappa2(cbind(loo3$V112, loo3$V115)); A113 <- kappa2(cbind(loo3$V113, loo3$V115)); A114 <- kappa2(cbind(loo3$V114, loo3$V115))
# MIC, Hellinger, Eigenvector, R > 0.3
A116 <- kappa2(cbind(loo3$V116, loo3$V120)); A117 <- kappa2(cbind(loo3$V117, loo3$V120)); A118 <- kappa2(cbind(loo3$V118, loo3$V120)); A119 <- kappa2(cbind(loo3$V119, loo3$V120))

#
# Spearman, Add1, Betweenness, R > 0.3
A121 <- kappa2(cbind(loo3$V121, loo3$V125)); A122 <- kappa2(cbind(loo3$V122, loo3$V125)); A123 <- kappa2(cbind(loo3$V123, loo3$V125)); A124 <- kappa2(cbind(loo3$V124, loo3$V125))
# Spearman, Add1, Closeness, R > 0.3 
A126 <- kappa2(cbind(loo3$V126, loo3$V130)); A127 <- kappa2(cbind(loo3$V127, loo3$V130)); A128 <- kappa2(cbind(loo3$V128, loo3$V130)); A129 <- kappa2(cbind(loo3$V129, loo3$V130))
# Spearman, Add1, Degree, R > 0.3
A131 <- kappa2(cbind(loo3$V131, loo3$V135)); A132 <- kappa2(cbind(loo3$V132, loo3$V135)); A133 <- kappa2(cbind(loo3$V133, loo3$V135)); A134 <- kappa2(cbind(loo3$V134, loo3$V135))
# Spearman, Add1, Eigenvector, R > 0.3
A136 <- kappa2(cbind(loo3$V136, loo3$V140)); A137 <- kappa2(cbind(loo3$V137, loo3$V140)); A138 <- kappa2(cbind(loo3$V138, loo3$V140)); A139 <- kappa2(cbind(loo3$V139, loo3$V140))

# MIC, Add1, Betweenness, R > 0.3
A141 <- kappa2(cbind(loo3$V141, loo3$V145)); A142 <- kappa2(cbind(loo3$V142, loo3$V145)); A143 <- kappa2(cbind(loo3$V143, loo3$V145)); A144 <- kappa2(cbind(loo3$V144, loo3$V145))
# MIC, Add1, Closeness, R > 0.3 
A146 <- kappa2(cbind(loo3$V146, loo3$V130)); A147 <- kappa2(cbind(loo3$V147, loo3$V130)); A148 <- kappa2(cbind(loo3$V148, loo3$V130)); A149 <- kappa2(cbind(loo3$V149, loo3$V150))
# MIC, Add1, Degree, R > 0.3
A151 <- kappa2(cbind(loo3$V151, loo3$V155)); A152 <- kappa2(cbind(loo3$V152, loo3$V155)); A153 <- kappa2(cbind(loo3$V153, loo3$V155)); A154 <- kappa2(cbind(loo3$V154, loo3$V155))
# MIC, Add1, Eigenvector, R > 0.3
A156 <- kappa2(cbind(loo3$V156, loo3$V160)); A157 <- kappa2(cbind(loo3$V157, loo3$V160)); A158 <- kappa2(cbind(loo3$V158, loo3$V160)); A159 <- kappa2(cbind(loo3$V159, loo3$V160))

#
# Spearman, Hellinger, Betweenness, R > 0.4
A161 <- kappa2(cbind(loo3$V161, loo3$V165)); A162 <- kappa2(cbind(loo3$V162, loo3$V165)); A163 <- kappa2(cbind(loo3$V163, loo3$V165)); A164 <- kappa2(cbind(loo3$V164, loo3$V165))
# Spearman, Hellinger, Closeness, R > 0.4 
A166 <- kappa2(cbind(loo3$V166, loo3$V170)); A167 <- kappa2(cbind(loo3$V167, loo3$V170)); A168 <- kappa2(cbind(loo3$V168, loo3$V170)); A169 <- kappa2(cbind(loo3$V169, loo3$V170))
# Spearman, Hellinger, Degree, R > 0.4
A171 <- kappa2(cbind(loo3$V171, loo3$V175)); A172 <- kappa2(cbind(loo3$V172, loo3$V175)); A173 <- kappa2(cbind(loo3$V173, loo3$V175)); A174 <- kappa2(cbind(loo3$V174, loo3$V175))
# Spearman, Hellinger, Eigenvector, R > 0.4 #### These rows were blank
A176 <- kappa2(cbind(loo3$V176, loo3$V180)); A177 <- kappa2(cbind(loo3$V177, loo3$V180)); A178 <- kappa2(cbind(loo3$V178, loo3$V180)); A179 <- kappa2(cbind(loo3$V179, loo3$V180))

# MIC, Hellinger, Betweenness, R > 0.4
A181 <- kappa2(cbind(loo3$V181, loo3$V185)); A182 <- kappa2(cbind(loo3$V182, loo3$V185)); A183 <- kappa2(cbind(loo3$V183, loo3$V185)); A184 <- kappa2(cbind(loo3$V184, loo3$V185))
# MIC, Hellinger, Closeness, R > 0.4 
A186 <- kappa2(cbind(loo3$V186, loo3$V130)); A187 <- kappa2(cbind(loo3$V187, loo3$V130)); A188 <- kappa2(cbind(loo3$V188, loo3$V130)); A189 <- kappa2(cbind(loo3$V189, loo3$V190))
# MIC, Hellinger, Degree, R > 0.4
A191 <- kappa2(cbind(loo3$V191, loo3$V195)); A192 <- kappa2(cbind(loo3$V192, loo3$V195)); A193 <- kappa2(cbind(loo3$V193, loo3$V195)); A194 <- kappa2(cbind(loo3$V194, loo3$V195))
# MIC, Hellinger, Eigenvector, R > 0.4
A196 <- kappa2(cbind(loo3$V196, loo3$V200)); A197 <- kappa2(cbind(loo3$V197, loo3$V200)); A198 <- kappa2(cbind(loo3$V198, loo3$V200)); A199 <- kappa2(cbind(loo3$V199, loo3$V200)) # unequal sample sizes

#
# Spearman, Add1, Betweenness, R > 0.4
A201 <- kappa2(cbind(loo3$V201, loo3$V205)); A202 <- kappa2(cbind(loo3$V202, loo3$V205)); A203 <- kappa2(cbind(loo3$V203, loo3$V205)); A204 <- kappa2(cbind(loo3$V204, loo3$V205))
# Spearman, Add1, Closeness, R > 0.4 
A206 <- kappa2(cbind(loo3$V206, loo3$V210)); A207 <- kappa2(cbind(loo3$V207, loo3$V210)); A208 <- kappa2(cbind(loo3$V208, loo3$V210)); A209 <- kappa2(cbind(loo3$V209, loo3$V210))
# Spearman, Add1, Degree, R > 0.4
A211 <- kappa2(cbind(loo3$V211, loo3$V215)); A212 <- kappa2(cbind(loo3$V212, loo3$V215)); A213 <- kappa2(cbind(loo3$V213, loo3$V215)); A214 <- kappa2(cbind(loo3$V214, loo3$V215))
# Spearman, Add1, Eigenvector, R > 0.4 #### These rows were blank
A216 <- kappa2(cbind(loo3$V216, loo3$V220)); A217 <- kappa2(cbind(loo3$V217, loo3$V220)); A218 <- kappa2(cbind(loo3$V218, loo3$V220)); A219 <- kappa2(cbind(loo3$V219, loo3$V220))

# MIC, Add1, Betweenness, R > 0.4
A221 <- kappa2(cbind(loo3$V221, loo3$V225)); A222 <- kappa2(cbind(loo3$V222, loo3$V225)); A223 <- kappa2(cbind(loo3$V223, loo3$V225)); A224 <- kappa2(cbind(loo3$V224, loo3$V225))
# MIC, Add1, Closeness, R > 0.4 
A226 <- kappa2(cbind(loo3$V226, loo3$V230)); A227 <- kappa2(cbind(loo3$V227, loo3$V230)); A228 <- kappa2(cbind(loo3$V228, loo3$V230)); A229 <- kappa2(cbind(loo3$V229, loo3$V230))
# MIC, Add1, Degree, R > 0.4
A231 <- kappa2(cbind(loo3$V231, loo3$V235)); A232 <- kappa2(cbind(loo3$V232, loo3$V235)); A233 <- kappa2(cbind(loo3$V233, loo3$V235)); A234 <- kappa2(cbind(loo3$V234, loo3$V235))
# MIC, Add1, Eigenvector, R > 0.4
A236 <- kappa2(cbind(loo3$V236, loo3$V240)); A237 <- kappa2(cbind(loo3$V237, loo3$V240)); A238 <- kappa2(cbind(loo3$V238, loo3$V240)); A239 <- kappa2(cbind(loo3$V239, loo3$V240))

## Calculate percent agreement between the list combinations
# Jaccard index is the interection divided by the union of two lists
xtab_set <- function(A,B){
  both    <-  union(A,B)
  inA     <-  both %in% A
  inB     <-  both %in% B
  return(table(inA,inB))
}


# Spearman, Hellinger, Betweenness, R > 0.2 
B1 <- xtab_set(na.omit(loo3$V1), na.omit(loo3$V5)); B2 <- xtab_set(na.omit(loo3$V2), na.omit(loo3$V5)); B3 <- xtab_set(na.omit(loo3$V3), na.omit(loo3$V5)); 
B4 <- xtab_set(na.omit(loo3$V4), na.omit(loo3$V5))
# Spearman, Hellinger, Closeness, R > 0.2 
B6 <- xtab_set(na.omit(loo3$V6), na.omit(loo3$V10)); B7 <- xtab_set(na.omit(loo3$V7), na.omit(loo3$V10)); B8 <- xtab_set(na.omit(loo3$V8), na.omit(loo3$V10)); 
B9 <- xtab_set(na.omit(loo3$V9), na.omit(loo3$V10))
# Spearman), Hellinger), Degree), R > 0.2 
B11 <- xtab_set(na.omit(loo3$V11), na.omit(loo3$V15)); B12 <- xtab_set(na.omit(loo3$V12), na.omit(loo3$V15)); B13 <- xtab_set(na.omit(loo3$V13), na.omit(loo3$V15)); 
B14 <- xtab_set(na.omit(loo3$V14), na.omit(loo3$V15))
# Spearman), Hellinger), Eigenvector), R > 0.2 
B16 <- xtab_set(na.omit(loo3$V16), na.omit(loo3$V20)); B17 <- xtab_set(na.omit(loo3$V17), na.omit(loo3$V20)); B18 <- xtab_set(na.omit(loo3$V18), na.omit(loo3$V20)); 
B19 <- xtab_set(na.omit(loo3$V19), na.omit(loo3$V20))

# MIC), Hellinger), Betweenness), R > 0.2 
B21 <- xtab_set(na.omit(loo3$V21), na.omit(loo3$V25)); B22 <- xtab_set(na.omit(loo3$V22), na.omit(loo3$V25)); B23 <- xtab_set(na.omit(loo3$V23), na.omit(loo3$V25)); 
B24 <- xtab_set(na.omit(loo3$V24), na.omit(loo3$V25))
# MIC), Hellinger), Closeness), R > 0.2 
B26 <- xtab_set(na.omit(loo3$V26), na.omit(loo3$V30)); B27 <- xtab_set(na.omit(loo3$V27), na.omit(loo3$V30)); B28 <- xtab_set(na.omit(loo3$V28), na.omit(loo3$V30)); 
B29 <- xtab_set(na.omit(loo3$V29), na.omit(loo3$V30))
# MIC), Hellinger), Degree), R > 0.2 
B31 <- xtab_set(na.omit(loo3$V31), na.omit(loo3$V35)); B32 <- xtab_set(na.omit(loo3$V32), na.omit(loo3$V35)); B33 <- xtab_set(na.omit(loo3$V33), na.omit(loo3$V35)); 
B34 <- xtab_set(na.omit(loo3$V34), na.omit(loo3$V35))
# MIC), Hellinger), Eigenvector), R > 0.2 
B36 <- xtab_set(na.omit(loo3$V36), na.omit(loo3$V40)); B37 <- xtab_set(na.omit(loo3$V37), na.omit(loo3$V40)); B38 <- xtab_set(na.omit(loo3$V38), na.omit(loo3$V40)); 
B39 <- xtab_set(na.omit(loo3$V39), na.omit(loo3$V40))

#
# Spearman), Add1), Betweenness), R > 0.2 
B41 <- xtab_set(na.omit(loo3$V41), na.omit(loo3$V45)); B42 <- xtab_set(na.omit(loo3$V42), na.omit(loo3$V45)); B43 <- xtab_set(na.omit(loo3$V43), na.omit(loo3$V45)); 
B44 <- xtab_set(na.omit(loo3$V44), na.omit(loo3$V45))
# Spearman), Hellinger), Closeness), R > 0.2 
B46 <- xtab_set(na.omit(loo3$V46), na.omit(loo3$V50)); B47 <- xtab_set(na.omit(loo3$V47), na.omit(loo3$V50)); B48 <- xtab_set(na.omit(loo3$V48), na.omit(loo3$V50)); 
B49 <- xtab_set(na.omit(loo3$V49), na.omit(loo3$V50))
# Spearman), Hellinger), Degree), R > 0.2 
B51 <- xtab_set(na.omit(loo3$V51), na.omit(loo3$V55)); B52 <- xtab_set(na.omit(loo3$V52), na.omit(loo3$V55)); B53 <- xtab_set(na.omit(loo3$V53), na.omit(loo3$V55)); 
B54 <- xtab_set(na.omit(loo3$V54), na.omit(loo3$V55))
# Spearman), Hellinger), Eigenvector), R > 0.2 
B56 <- xtab_set(na.omit(loo3$V56), na.omit(loo3$V60)); B57 <- xtab_set(na.omit(loo3$V57), na.omit(loo3$V60)); B58 <- xtab_set(na.omit(loo3$V58), na.omit(loo3$V60)); 
B59 <- xtab_set(na.omit(loo3$V59), na.omit(loo3$V60))

# MIC), Add1), Betweenness), R > 0.2
B61 <- xtab_set(na.omit(loo3$V61), na.omit(loo3$V65)); B62 <- xtab_set(na.omit(loo3$V62), na.omit(loo3$V65)); B63 <- xtab_set(na.omit(loo3$V63), na.omit(loo3$V65)); 
B64 <- xtab_set(na.omit(loo3$V64), na.omit(loo3$V65))
# MIC), Hellinger), Closeness), R > 0.2 
B66 <- xtab_set(na.omit(loo3$V66), na.omit(loo3$V70)); B67 <- xtab_set(na.omit(loo3$V67), na.omit(loo3$V70)); B68 <- xtab_set(na.omit(loo3$V68), na.omit(loo3$V70)); 
B69 <- xtab_set(na.omit(loo3$V69), na.omit(loo3$V70))
# MIC), Hellinger), Degree), R > 0.2 
B71 <- xtab_set(na.omit(loo3$V71), na.omit(loo3$V75)); B72 <- xtab_set(na.omit(loo3$V72), na.omit(loo3$V75)); B73 <- xtab_set(na.omit(loo3$V73), na.omit(loo3$V75)); 
B74 <- xtab_set(na.omit(loo3$V74), na.omit(loo3$V75))
# MIC), Hellinger), Eigenvector), R > 0.2 
B76 <- xtab_set(na.omit(loo3$V76), na.omit(loo3$V80)); B77 <- xtab_set(na.omit(loo3$V77), na.omit(loo3$V80)); B78 <- xtab_set(na.omit(loo3$V78), na.omit(loo3$V80));
B79 <- xtab_set(na.omit(loo3$V79), na.omit(loo3$V80))

#
# Spearman), Hellinger), Betweenness), R > 0.3
B81 <- xtab_set(na.omit(loo3$V81), na.omit(loo3$V85)); B82 <- xtab_set(na.omit(loo3$V82), na.omit(loo3$V85)); B83 <- xtab_set(na.omit(loo3$V83), na.omit(loo3$V85)); 
B84 <- xtab_set(na.omit(loo3$V84), na.omit(loo3$V85))
# Spearman), Hellinger), Closeness), R > 0.3
B86 <- xtab_set(na.omit(loo3$V86), na.omit(loo3$V90)); B87 <- xtab_set(na.omit(loo3$V87), na.omit(loo3$V90)); B88 <- xtab_set(na.omit(loo3$V88), na.omit(loo3$V90)); 
B89 <- xtab_set(na.omit(loo3$V89), na.omit(loo3$V90))
# Spearman), Hellinger), Degree), R > 0.3 
B91 <- xtab_set(na.omit(loo3$V91), na.omit(loo3$V95)); B92 <- xtab_set(na.omit(loo3$V92), na.omit(loo3$V95)); B93 <- xtab_set(na.omit(loo3$V93), na.omit(loo3$V95)); 
B94 <- xtab_set(na.omit(loo3$V94), na.omit(loo3$V95))
# Spearman), Hellinger), Eigenvector), R > 0.3
B96 <- xtab_set(na.omit(loo3$V96), na.omit(loo3$V100)); B97 <- xtab_set(na.omit(loo3$V97), na.omit(loo3$V100)); B98 <- xtab_set(na.omit(loo3$V98), na.omit(loo3$V100)); 
B99 <- xtab_set(na.omit(loo3$V99), na.omit(loo3$V100))

# MIC), Hellinger), Betweenness), R > 0.3
B101 <- xtab_set(na.omit(loo3$V101), na.omit(loo3$V105)); B102 <- xtab_set(na.omit(loo3$V102), na.omit(loo3$V105)); B103 <- xtab_set(na.omit(loo3$V103), na.omit(loo3$V105)); 
B104 <- xtab_set(na.omit(loo3$V104), na.omit(loo3$V105))
# MIC), Hellinger), Closeness), R > 0.3
B106 <- xtab_set(na.omit(loo3$V106), na.omit(loo3$V110)); B107 <- xtab_set(na.omit(loo3$V107), na.omit(loo3$V110)); B108 <- xtab_set(na.omit(loo3$V108), na.omit(loo3$V110)); 
B109 <- xtab_set(na.omit(loo3$V109), na.omit(loo3$V110))
# MIC), Hellinger), Degree), R > 0.3
B111 <- xtab_set(na.omit(loo3$V111), na.omit(loo3$V115)); B112 <- xtab_set(na.omit(loo3$V112), na.omit(loo3$V115)); B113 <- xtab_set(na.omit(loo3$V113), na.omit(loo3$V115)); 
B114 <- xtab_set(na.omit(loo3$V114), na.omit(loo3$V115))
# MIC), Hellinger), Eigenvector), R > 0.3
B116 <- xtab_set(na.omit(loo3$V116), na.omit(loo3$V120)); B117 <- xtab_set(na.omit(loo3$V117), na.omit(loo3$V120)); B118 <- xtab_set(na.omit(loo3$V118), na.omit(loo3$V120)); 
B119 <- xtab_set(na.omit(loo3$V119), na.omit(loo3$V120))

#
# Spearman), Add1), Betweenness), R > 0.3
B121 <- xtab_set(na.omit(loo3$V121), na.omit(loo3$V125)); B122 <- xtab_set(na.omit(loo3$V122), na.omit(loo3$V125)); B123 <- xtab_set(na.omit(loo3$V123), na.omit(loo3$V125)); 
B124 <- xtab_set(na.omit(loo3$V124), na.omit(loo3$V125))
# Spearman), Add1), Closeness), R > 0.3 
B126 <- xtab_set(na.omit(loo3$V126), na.omit(loo3$V130)); B127 <- xtab_set(na.omit(loo3$V127), na.omit(loo3$V130)); B128 <- xtab_set(na.omit(loo3$V128), na.omit(loo3$V130)); 
B129 <- xtab_set(na.omit(loo3$V129), na.omit(loo3$V130))
# Spearman), Add1), Degree), R > 0.3
B131 <- xtab_set(na.omit(loo3$V131), na.omit(loo3$V135)); B132 <- xtab_set(na.omit(loo3$V132), na.omit(loo3$V135)); B133 <- xtab_set(na.omit(loo3$V133), na.omit(loo3$V135)); 
B134 <- xtab_set(na.omit(loo3$V134), na.omit(loo3$V135))
# Spearman), Add1), Eigenvector), R > 0.3
B136 <- xtab_set(na.omit(loo3$V136), na.omit(loo3$V140)); B137 <- xtab_set(na.omit(loo3$V137), na.omit(loo3$V140)); B138 <- xtab_set(na.omit(loo3$V138), na.omit(loo3$V140)); 
B139 <- xtab_set(na.omit(loo3$V139), na.omit(loo3$V140))

# MIC), Add1), Betweenness), R > 0.3
B141 <- xtab_set(na.omit(loo3$V141), na.omit(loo3$V145)); B142 <- xtab_set(na.omit(loo3$V142), na.omit(loo3$V145)); B143 <- xtab_set(na.omit(loo3$V143), na.omit(loo3$V145)); 
B144 <- xtab_set(na.omit(loo3$V144), na.omit(loo3$V145))
# MIC), Add1), Closeness), R > 0.3 
B146 <- xtab_set(na.omit(loo3$V146), na.omit(loo3$V130)); B147 <- xtab_set(na.omit(loo3$V147), na.omit(loo3$V130)); B148 <- xtab_set(na.omit(loo3$V148), na.omit(loo3$V130)); 
B149 <- xtab_set(na.omit(loo3$V149), na.omit(loo3$V150))
# MIC), Add1), Degree), R > 0.3
B151 <- xtab_set(na.omit(loo3$V151), na.omit(loo3$V155)); B152 <- xtab_set(na.omit(loo3$V152), na.omit(loo3$V155)); B153 <- xtab_set(na.omit(loo3$V153), na.omit(loo3$V155)); 
B154 <- xtab_set(na.omit(loo3$V154), na.omit(loo3$V155))
# MIC), Add1), Eigenvector), R > 0.3
B156 <- xtab_set(na.omit(loo3$V156), na.omit(loo3$V160)); B157 <- xtab_set(na.omit(loo3$V157), na.omit(loo3$V160)); B158 <- xtab_set(na.omit(loo3$V158), na.omit(loo3$V160)); 
B159 <- xtab_set(na.omit(loo3$V159), na.omit(loo3$V160))

#
# Spearman), Hellinger), Betweenness), R > 0.4
B161 <- xtab_set(na.omit(loo3$V161), na.omit(loo3$V165)); B162 <- xtab_set(na.omit(loo3$V162), na.omit(loo3$V165)); B163 <- xtab_set(na.omit(loo3$V163), na.omit(loo3$V165)); 
B164 <- xtab_set(na.omit(loo3$V164), na.omit(loo3$V165))
# Spearman), Hellinger), Closeness), R > 0.4 
B166 <- xtab_set(na.omit(loo3$V166), na.omit(loo3$V170)); B167 <- xtab_set(na.omit(loo3$V167), na.omit(loo3$V170)); B168 <- xtab_set(na.omit(loo3$V168), na.omit(loo3$V170)); 
B169 <- xtab_set(na.omit(loo3$V169), na.omit(loo3$V170))
# Spearman), Hellinger), Degree), R > 0.4
B171 <- xtab_set(na.omit(loo3$V171), na.omit(loo3$V175)); B172 <- xtab_set(na.omit(loo3$V172), na.omit(loo3$V175)); B173 <- xtab_set(na.omit(loo3$V173), na.omit(loo3$V175)); 
B174 <- xtab_set(na.omit(loo3$V174), na.omit(loo3$V175))
# Spearman), Hellinger), Eigenvector), R > 0.4 #### These rows were blank
B176 <- xtab_set(na.omit(loo3$V176), na.omit(loo3$V180)); B177 <- xtab_set(na.omit(loo3$V177), na.omit(loo3$V180)); B178 <- xtab_set(na.omit(loo3$V178), na.omit(loo3$V180)); 
B179 <- xtab_set(na.omit(loo3$V179), na.omit(loo3$V180))

# MIC), Hellinger), Betweenness), R > 0.4
B181 <- xtab_set(na.omit(loo3$V181), na.omit(loo3$V185)); B182 <- xtab_set(na.omit(loo3$V182), na.omit(loo3$V185)); B183 <- xtab_set(na.omit(loo3$V183), na.omit(loo3$V185)); 
B184 <- xtab_set(na.omit(loo3$V184), na.omit(loo3$V185))
# MIC), Hellinger), Closeness), R > 0.4 
B186 <- xtab_set(na.omit(loo3$V186), na.omit(loo3$V130)); B187 <- xtab_set(na.omit(loo3$V187), na.omit(loo3$V130)); B188 <- xtab_set(na.omit(loo3$V188), na.omit(loo3$V130)); 
B189 <- xtab_set(na.omit(loo3$V189), na.omit(loo3$V190))
# MIC), Hellinger), Degree), R > 0.4
B191 <- xtab_set(na.omit(loo3$V191), na.omit(loo3$V195)); B192 <- xtab_set(na.omit(loo3$V192), na.omit(loo3$V195)); B193 <- xtab_set(na.omit(loo3$V193), na.omit(loo3$V195)); 
B194 <- xtab_set(na.omit(loo3$V194), na.omit(loo3$V195))
# MIC), Hellinger), Eigenvector), R > 0.4
B196 <- xtab_set(na.omit(loo3$V196), na.omit(loo3$V200)); B197 <- xtab_set(na.omit(loo3$V197), na.omit(loo3$V200)); B198 <- xtab_set(na.omit(loo3$V198), na.omit(loo3$V200)); 
B199 <- xtab_set(na.omit(loo3$V199), na.omit(loo3$V200)) # unequal sample sizes

#
# Spearman), Add1), Betweenness), R > 0.4
B201 <- xtab_set(na.omit(loo3$V201), na.omit(loo3$V205)); B202 <- xtab_set(na.omit(loo3$V202), na.omit(loo3$V205)); B203 <- xtab_set(na.omit(loo3$V203), na.omit(loo3$V205)); 
B204 <- xtab_set(na.omit(loo3$V204), na.omit(loo3$V205))
# Spearman), Add1), Closeness), R > 0.4 
B206 <- xtab_set(na.omit(loo3$V206), na.omit(loo3$V210)); B207 <- xtab_set(na.omit(loo3$V207), na.omit(loo3$V210)); B208 <- xtab_set(na.omit(loo3$V208), na.omit(loo3$V210)); 
B209 <- xtab_set(na.omit(loo3$V209), na.omit(loo3$V210))
# Spearman), Add1), Degree), R > 0.4
B211 <- xtab_set(na.omit(loo3$V211), na.omit(loo3$V215)); B212 <- xtab_set(na.omit(loo3$V212), na.omit(loo3$V215)); B213 <- xtab_set(na.omit(loo3$V213), na.omit(loo3$V215)); 
B214 <- xtab_set(na.omit(loo3$V214), na.omit(loo3$V215))
# Spearman), Add1), Eigenvector), R > 0.4 #### These rows were blank
B216 <- xtab_set(na.omit(loo3$V216), na.omit(loo3$V220)); B217 <- xtab_set(na.omit(loo3$V217), na.omit(loo3$V220)); B218 <- xtab_set(na.omit(loo3$V218), na.omit(loo3$V220)); 
B219 <- xtab_set(na.omit(loo3$V219), na.omit(loo3$V220))

# MIC), Add1), Betweenness), R > 0.4
B221 <- xtab_set(na.omit(loo3$V221), na.omit(loo3$V225)); B222 <- xtab_set(na.omit(loo3$V222), na.omit(loo3$V225)); B223 <- xtab_set(na.omit(loo3$V223), na.omit(loo3$V225)); 
B224 <- xtab_set(na.omit(loo3$V224), na.omit(loo3$V225))
# MIC), Add1), Closeness), R > 0.4 
B226 <- xtab_set(na.omit(loo3$V226), na.omit(loo3$V230)); B227 <- xtab_set(na.omit(loo3$V227), na.omit(loo3$V230)); B228 <- xtab_set(na.omit(loo3$V228), na.omit(loo3$V230)); 
B229 <- xtab_set(na.omit(loo3$V229), na.omit(loo3$V230))
# MIC, Add1, Degree, R > 0.4
B231 <- xtab_set(na.omit(loo3$V231), na.omit(loo3$V235)); B232 <- xtab_set(na.omit(loo3$V232), na.omit(loo3$V235)); B233 <- xtab_set(na.omit(loo3$V233), na.omit(loo3$V235)); 
B234 <- xtab_set(na.omit(loo3$V234), na.omit(loo3$V235))
# MIC, Add1, Eigenvector, R > 0.4
B236 <- xtab_set(na.omit(loo3$V236), na.omit(loo3$V240)); B237 <- xtab_set(na.omit(loo3$V237), na.omit(loo3$V240)); B238 <- xtab_set(na.omit(loo3$V238), na.omit(loo3$V240)); 
B239 <- xtab_set(na.omit(loo3$V239), na.omit(loo3$V240))

B1.p <- B1[4]/sum(B1); B2.p <- B2[4]/sum(B2); B3.p <- B3[4]/sum(B3); B4.p <- B4[4]/sum(B4); 
B6.p <- B6[4]/sum(B6); B7.p <- B7[4]/sum(B7); B8.p <- B8[4]/sum(B8); B9.p <- B9[4]/sum(B9); 
B11.p <- B11[4]/sum(B11); B12.p <- B12[4]/sum(B12); B13.p <- B13[4]/sum(B13); B14.p <- B14[4]/sum(B14)
B16.p <- B16[4]/sum(B16); B17.p <- B17[4]/sum(B17); B18.p <- B18[4]/sum(B18); B19.p <- B19[4]/sum(B19) 
B21.p <- B21[4]/sum(B21); B22.p <- B22[4]/sum(B22); B23.p <- B23[4]/sum(B23); B24.p <- B24[4]/sum(B24) 
B26.p <- B26[4]/sum(B26); B27.p <- B27[4]/sum(B27); B28.p <- B28[4]/sum(B28); B29.p <- B29[4]/sum(B29) 
B31.p <- B31[4]/sum(B31); B32.p <- B32[4]/sum(B32); B33.p <- B33[4]/sum(B33); B34.p <- B34[4]/sum(B34) 
B36.p <- B36[4]/sum(B36); B37.p <- B37[4]/sum(B37); B38.p <- B38[4]/sum(B38); B39.p <- B39[4]/sum(B39) 
B41.p <- B41[4]/sum(B41); B42.p <- B42[4]/sum(B42); B43.p <- B43[4]/sum(B43); B44.p <- B44[4]/sum(B44) 
B46.p <- B46[4]/sum(B46); B47.p <- B47[4]/sum(B47); B48.p <- B48[4]/sum(B48); B49.p <- B49[4]/sum(B49) 
B51.p <- B51[4]/sum(B51); B52.p <- B52[4]/sum(B52); B53.p <- B53[4]/sum(B53); B54.p <- B54[4]/sum(B54) 
B56.p <- B56[4]/sum(B56); B57.p <- B57[4]/sum(B57); B58.p <- B58[4]/sum(B58); B59.p <- B59[4]/sum(B59) 
B61.p <- B61[4]/sum(B61); B62.p <- B62[4]/sum(B62); B63.p <- B63[4]/sum(B63); B64.p <- B64[4]/sum(B64) 
B66.p <- B66[4]/sum(B66); B67.p <- B67[4]/sum(B67); B68.p <- B68[4]/sum(B68); B69.p <- B69[4]/sum(B69) 
B71.p <- B71[4]/sum(B71); B72.p <- B72[4]/sum(B72); B73.p <- B73[4]/sum(B73); B74.p <- B74[4]/sum(B74) 
B76.p <- B76[4]/sum(B76); B77.p <- B77[4]/sum(B77); B78.p <- B78[4]/sum(B78); B79.p <- B79[4]/sum(B79) 
B81.p <- B81[4]/sum(B81); B82.p <- B82[4]/sum(B82); B83.p <- B83[4]/sum(B83); B84.p <- B84[4]/sum(B84) 
B86.p <- B86[4]/sum(B86); B87.p <- B87[4]/sum(B87); B88.p <- B88[4]/sum(B88); B89.p <- B89[4]/sum(B89) 
B91.p <- B91[4]/sum(B91); B92.p <- B92[4]/sum(B92); B93.p <- B93[4]/sum(B93); B94.p <- B94[4]/sum(B94) 
B96.p <- B96[4]/sum(B96); B97.p <- B97[4]/sum(B97); B98.p <- B98[4]/sum(B98); B99.p <- B99[4]/sum(B99) 
B101.p <- B101[4]/sum(B101); B102.p <- B102[4]/sum(B102); B103.p <- B103[4]/sum(B103); B104.p <- B104[4]/sum(B104) 
B106.p <- B106[4]/sum(B106); B107.p <- B107[4]/sum(B107); B108.p <- B108[4]/sum(B108); B109.p <- B109[4]/sum(B109) 
B111.p <- B111[4]/sum(B111); B112.p <- B112[4]/sum(B112); B113.p <- B113[4]/sum(B113); B114.p <- B114[4]/sum(B114) 
B116.p <- B116[4]/sum(B116); B117.p <- B117[4]/sum(B117); B118.p <- B118[4]/sum(B118); B119.p <- B119[4]/sum(B119) 
B121.p <- B121[4]/sum(B121); B122.p <- B122[4]/sum(B122); B123.p <- B123[4]/sum(B123); B124.p <- B124[4]/sum(B124) 
B126.p <- B126[4]/sum(B126); B127.p <- B127[4]/sum(B127); B128.p <- B128[4]/sum(B128); B129.p <- B129[4]/sum(B129) 
B131.p <- B131[4]/sum(B131); B132.p <- B132[4]/sum(B132); B133.p <- B133[4]/sum(B133); B134.p <- B134[4]/sum(B134) 
B136.p <- B136[4]/sum(B136); B137.p <- B137[4]/sum(B137); B138.p <- B138[4]/sum(B138); B139.p <- B139[4]/sum(B139) 
B141.p <- B141[4]/sum(B141); B142.p <- B142[4]/sum(B142); B143.p <- B143[4]/sum(B143); B144.p <- B144[4]/sum(B144) 
B146.p <- B146[4]/sum(B146); B147.p <- B147[4]/sum(B147); B148.p <- B148[4]/sum(B148); B149.p <- B149[4]/sum(B149) 
B151.p <- B151[4]/sum(B151); B152.p <- B152[4]/sum(B152); B153.p <- B153[4]/sum(B153); B154.p <- B154[4]/sum(B154) 
B156.p <- B156[4]/sum(B156); B157.p <- B157[4]/sum(B157); B158.p <- B158[4]/sum(B158); B159.p <- B159[4]/sum(B159) 
B161.p <- B161[4]/sum(B161); B162.p <- B162[4]/sum(B162); B163.p <- B163[4]/sum(B163); B164.p <- B164[4]/sum(B164) 
B166.p <- B166[4]/sum(B166); B167.p <- B167[4]/sum(B167); B168.p <- B168[4]/sum(B168); B169.p <- B169[4]/sum(B169) 
B171.p <- B171[4]/sum(B171); B172.p <- B172[4]/sum(B172); B173.p <- B173[4]/sum(B173); B174.p <- B174[4]/sum(B174) 
B176.p <- B176[4]/sum(B176); B177.p <- B177[4]/sum(B177); B178.p <- B178[4]/sum(B178); B179.p <- B179[4]/sum(B179) 
B181.p <- B181[4]/sum(B181); B182.p <- B182[4]/sum(B182); B183.p <- B183[4]/sum(B183); B184.p <- B184[4]/sum(B184) 
B186.p <- B186[4]/sum(B186); B187.p <- B187[4]/sum(B187); B188.p <- B188[4]/sum(B188); B189.p <- B189[4]/sum(B189) 
B191.p <- B191[4]/sum(B191); B192.p <- B192[4]/sum(B192); B193.p <- B193[4]/sum(B193); B194.p <- B194[4]/sum(B194) 
B196.p <- B196[4]/sum(B196); B197.p <- B197[4]/sum(B197); B198.p <- B198[2]/sum(B198); B199.p <- B199[2]/sum(B199) 
B201.p <- B201[4]/sum(B201); B202.p <- B202[4]/sum(B202); B203.p <- B203[4]/sum(B203); B204.p <- B204[4]/sum(B204) 
B206.p <- B206[4]/sum(B206); B207.p <- B207[4]/sum(B207); B208.p <- B208[4]/sum(B208); B209.p <- B209[4]/sum(B209) 
B211.p <- B211[4]/sum(B211); B212.p <- B212[4]/sum(B212); B213.p <- B213[4]/sum(B213); B214.p <- B214[4]/sum(B214) 
B216.p <- B216[2]/sum(B216); B217.p <- B217[2]/sum(B217); B218.p <- B218[2]/sum(B218); B219.p <- B219[4]/sum(B219) 
B221.p <- B221[4]/sum(B221); B222.p <- B222[4]/sum(B222); B223.p <- B223[4]/sum(B223); B224.p <- B224[4]/sum(B224) 
B226.p <- B226[4]/sum(B226); B227.p <- B227[4]/sum(B227); B228.p <- B228[4]/sum(B228); B229.p <- B229[4]/sum(B229) 
B231.p <- B231[4]/sum(B231); B232.p <- B232[4]/sum(B232); B233.p <- B233[4]/sum(B233); B234.p <- B234[4]/sum(B234) 
B236.p <- B236[2]/sum(B236); B237.p <- B237[2]/sum(B237); B238.p <- B238[2]/sum(B238); B239.p <- B239[4]/sum(B239)

## Here's a check of the first calculation in the following code
length(intersect(na.omit(loo3$V1), na.omit(loo3$V5))) / length(union(na.omit(loo3$V1), na.omit(loo3$V5))) # 0.741
B1.p <- B1[4]/sum(B1); B1.p # 0.741

## Make the dataframe to use for plotting
kappa.df <- data.frame(
  conditioning = c(rep(c("Hellinger","Add1","Hellinger","Add1","Hellinger","Add1"), each = 32)),
  centrality = c(rep(rep(c("bw","cl","dg","ev"), each = 4),12)),
  correl = c(rep(rep(c("Spearman","MIC"), each = 16),6)),
  threshold = rep(c(0.2,0.3,0.4), each = 64),
  select_iter = rep(c(1,4,16,64),48),
  kappa = rbind(A1$value, A2$value, A3$value, A4$value, A6$value, A7$value, A8$value, A9$value, 
                A11$value, A12$value, A13$value, A14$value, A16$value, A17$value, A18$value, A19$value, 
                A21$value, A22$value, A23$value, A24$value, A26$value, A27$value, A28$value, A29$value, 
                A31$value, A32$value, A33$value, A34$value, A36$value, A37$value, A38$value, A39$value, 
                A41$value, A42$value, A43$value, A44$value, A46$value, A47$value, A48$value, A49$value, 
                A51$value, A52$value, A53$value, A54$value, A56$value, A57$value, A58$value, A59$value, 
                A61$value, A62$value, A63$value, A64$value, A66$value, A67$value, A68$value, A69$value, 
                A71$value, A72$value, A73$value, A74$value, A76$value, A77$value, A78$value, A79$value, 
                A81$value, A82$value, A83$value, A84$value, A86$value, A87$value, A88$value, A89$value, 
                A91$value, A92$value, A93$value, A94$value, A96$value, A97$value, A98$value, A99$value, 
                A101$value, A102$value, A103$value, A104$value, A106$value, A107$value, A108$value, A109$value, 
                A111$value, A112$value, A113$value, A114$value, A116$value, A117$value, A118$value, A119$value, 
                A121$value, A122$value, A123$value, A124$value, A126$value, A127$value, A128$value, A129$value, 
                A131$value, A132$value, A133$value, A134$value, A136$value, A137$value, A138$value, A139$value, 
                A141$value, A142$value, A143$value, A144$value, A146$value, A147$value, A148$value, A149$value, 
                A151$value, A152$value, A153$value, A154$value, A156$value, A157$value, A158$value, A159$value, 
                A161$value, A162$value, A163$value, A164$value, A166$value, A167$value, A168$value, A169$value, 
                A171$value, A172$value, A173$value, A174$value, "NA", "NA", "NA", "NA", 
                A181$value, A182$value, A183$value, A184$value, A186$value, A187$value, A188$value, A189$value, 
                A191$value, A192$value, A193$value, A194$value, A196$value, A197$value, A198$value, A199$value, 
                A201$value, A202$value, A203$value, A204$value, A206$value, A207$value, A208$value, A209$value, 
                A211$value, A212$value, A213$value, A214$value, A216$value, A217$value, A218$value, A219$value, 
                A221$value, A222$value, A223$value, A224$value, A226$value, A227$value, A228$value, A229$value, 
                A231$value, A232$value, A233$value, A234$value, A236$value, A237$value, A238$value, A239$value),
  agreement = rbind(B1.p, B2.p, B3.p, B4.p, B6.p, B7.p, B8.p, B9.p, 
                B11.p, B12.p, B13.p, B14.p, B16.p, B17.p, B18.p, B19.p, 
                B21.p, B22.p, B23.p, B24.p, B26.p, B27.p, B28.p, B29.p, 
                B31.p, B32.p, B33.p, B34.p, B36.p, B37.p, B38.p, B39.p, 
                B41.p, B42.p, B43.p, B44.p, B46.p, B47.p, B48.p, B49.p, 
                B51.p, B52.p, B53.p, B54.p, B56.p, B57.p, B58.p, B59.p, 
                B61.p, B62.p, B63.p, B64.p, B66.p, B67.p, B68.p, B69.p, 
                B71.p, B72.p, B73.p, B74.p, B76.p, B77.p, B78.p, B79.p, 
                B81.p, B82.p, B83.p, B84.p, B86.p, B87.p, B88.p, B89.p, 
                B91.p, B92.p, B93.p, B94.p, B96.p, B97.p, B98.p, B99.p, 
                B101.p, B102.p, B103.p, B104.p, B106.p, B107.p, B108.p, B109.p, 
                B111.p, B112.p, B113.p, B114.p, B116.p, B117.p, B118.p, B119.p, 
                B121.p, B122.p, B123.p, B124.p, B126.p, B127.p, B128.p, B129.p, 
                B131.p, B132.p, B133.p, B134.p, B136.p, B137.p, B138.p, B139.p, 
                B141.p, B142.p, B143.p, B144.p, B146.p, B147.p, B148.p, B149.p, 
                B151.p, B152.p, B153.p, B154.p, B156.p, B157.p, B158.p, B159.p, 
                B161.p, B162.p, B163.p, B164.p, B166.p, B167.p, B168.p, B169.p, 
                B171.p, B172.p, B173.p, B174.p, "NA", "NA", "NA", "NA", 
                B181.p, B182.p, B183.p, B184.p, B186.p, B187.p, B188.p, B189.p, 
                B191.p, B192.p, B193.p, B194.p, B196.p, B197.p, B198.p, B199.p, 
                B201.p, B202.p, B203.p, B204.p, B206.p, B207.p, B208.p, B209.p, 
                B211.p, B212.p, B213.p, B214.p, B216.p, B217.p, B218.p, B219.p, 
                B221.p, B222.p, B223.p, B224.p, B226.p, B227.p, B228.p, B229.p, 
                B231.p, B232.p, B233.p, B234.p, B236.p, B237.p, B238.p, B239.p))

kappa.df$kappa[kappa.df$kappa == 1] <- NA
kappa.df$kappa <- as.numeric(as.character(kappa.df$kappa))
kappa.df$agreement <- as.numeric(as.character(kappa.df$agreement))
                
kappa.agg <- aggregate(agreement ~ conditioning + centrality + correl + threshold, mean, data = kappa.df)
kappa.agg <- aggregate(agreement ~ centrality + correl + threshold, mean, data = subset(kappa.df,conditioning == "Add1" & correl == "MIC"))

# Global ANOVA
summary(aov1 <- aov(agreement ~ conditioning + centrality + correl + as.factor(threshold), kappa.df))
TukeyHSD(aov1)

# Compare between conditioning types
kappa.add1 <- subset(kappa.df, conditioning == "Add1")
kappa.hell <- subset(kappa.df, conditioning == "Hellinger")
summary(aov.add1 <- aov(agreement ~ centrality + correl + as.factor(threshold), kappa.add1))
summary(aov.hell <- aov(agreement ~ centrality + correl + as.factor(threshold), kappa.hell))

# Compare within each threshold
kappa.02.add1.rho <- subset(kappa.df, threshold == 0.2 & conditioning == "Add1" & correl == "Spearman")
kappa.03.add1.rho <- subset(kappa.df, threshold == 0.3 & conditioning == "Add1" & correl == "Spearman")
kappa.04.add1.rho <- subset(kappa.df, threshold == 0.4 & conditioning == "Add1" & correl == "Spearman")
kappa.02.hell.rho <- subset(kappa.df, threshold == 0.2 & conditioning == "Hellinger" & correl == "Spearman")
kappa.03.hell.rho <- subset(kappa.df, threshold == 0.3 & conditioning == "Hellinger" & correl == "Spearman")
kappa.04.hell.rho <- subset(kappa.df, threshold == 0.4 & conditioning == "Hellinger" & correl == "Spearman")
kappa.02.add1.mic <- subset(kappa.df, threshold == 0.2 & conditioning == "Add1" & correl == "MIC")
kappa.03.add1.mic <- subset(kappa.df, threshold == 0.3 & conditioning == "Add1" & correl == "MIC")
kappa.04.add1.mic <- subset(kappa.df, threshold == 0.4 & conditioning == "Add1" & correl == "MIC")
kappa.02.hell.mic <- subset(kappa.df, threshold == 0.2 & conditioning == "Hellinger" & correl == "MIC")
kappa.03.hell.mic <- subset(kappa.df, threshold == 0.3 & conditioning == "Hellinger" & correl == "MIC")
kappa.04.hell.mic <- subset(kappa.df, threshold == 0.4 & conditioning == "Hellinger" & correl == "MIC")

# MIC > 0.2
summary(aov.02.add1.mic <- aov(agreement ~ centrality + select_iter, kappa.02.add1.mic)) # No interaction
summary(aov.02.hell.mic <- aov(agreement ~ centrality + select_iter, kappa.02.hell.mic)) # No interaction
summary(aov.02.add1.rho <- aov(agreement ~ centrality*select_iter, kappa.02.add1.rho)) # Sig interaction
summary(aov.02.hell.rho <- aov(agreement ~ centrality*select_iter, kappa.02.hell.rho)) # Sig interaction

# MIC > 0.3
summary(aov.03.add1.mic <- aov(agreement ~ centrality*select_iter, kappa.03.add1.mic)) # Sig interaction
summary(aov.03.hell.mic <- aov(agreement ~ centrality + select_iter, kappa.03.hell.mic)) # No interaction
summary(aov.03.add1.rho <- aov(agreement ~ centrality*select_iter, kappa.03.add1.rho)) # Sig interaction
summary(aov.03.hell.rho <- aov(agreement ~ centrality*select_iter, kappa.03.hell.rho)) # Sig interaction

# MIC > 0.4
summary(aov.04.add1.mic <- aov(agreement ~ centrality*select_iter, kappa.04.add1.mic)) # Sig interaction
summary(aov.04.hell.mic <- aov(agreement ~ centrality*select_iter, kappa.04.hell.mic)) # Sig interaction
summary(aov.04.add1.rho <- aov(agreement ~ centrality*select_iter, kappa.04.add1.rho)) # Sig interaction
summary(aov.04.hell.rho <- aov(agreement ~ centrality*select_iter, kappa.04.hell.rho)) # Sig interaction