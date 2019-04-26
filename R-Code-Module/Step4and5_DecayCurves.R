library(PKNCA) # AUC
library(colorRamps) # Get color schemes
library(fifer) # Gradient legend

# Clear the workspace
rm(list = ls())

###################

## Brome A - bacteria, fungi, and archaea

# Degree selectbyall, MIC thresold of 0.2, minimum = 3 OTUs
brome_dg <- read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/ADD1_AUC100_MIC0.2_Brome_bacfunarc_dw_otu_table-graph_centrality-degree-selectallbyall.csv")
brome_dg <- brome_dg[ order(-brome_dg$metric), ]    # Order the data just in case
brome_dg.x <- as.numeric(rownames(brome_dg))        # Pulling in index after ordering
brome_dg.y <- brome_dg$metric                       # metric of interest

## Area under the curve
## Brome A horizon
# Degree
bro3AUC100 <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, 2996)) 
bro3AUC01 <- 0.01*bro3AUC100; bro3AUC02 <- 0.02*bro3AUC100; bro3AUC03 <- 0.03*bro3AUC100; bro3AUC04 <- 0.04*bro3AUC100; bro3AUC05 <- 0.05*bro3AUC100
bro3AUC06 <- 0.06*bro3AUC100; bro3AUC07 <- 0.07*bro3AUC100; bro3AUC08 <- 0.08*bro3AUC100; bro3AUC09 <- 0.09*bro3AUC100; bro3AUC10 <- 0.10*bro3AUC100
bro3AUC11 <- 0.11*bro3AUC100; bro3AUC12 <- 0.12*bro3AUC100; bro3AUC13 <- 0.13*bro3AUC100; bro3AUC14 <- 0.14*bro3AUC100; bro3AUC15 <- 0.15*bro3AUC100
bro3AUC16 <- 0.16*bro3AUC100; bro3AUC17 <- 0.17*bro3AUC100; bro3AUC18 <- 0.18*bro3AUC100; bro3AUC19 <- 0.19*bro3AUC100; bro3AUC20 <- 0.20*bro3AUC100
bro3AUC21 <- 0.21*bro3AUC100; bro3AUC22 <- 0.22*bro3AUC100; bro3AUC23 <- 0.23*bro3AUC100; bro3AUC24 <- 0.24*bro3AUC100; bro3AUC25 <- 0.25*bro3AUC100
bro3AUC26 <- 0.26*bro3AUC100; bro3AUC27 <- 0.27*bro3AUC100; bro3AUC28 <- 0.28*bro3AUC100; bro3AUC29 <- 0.29*bro3AUC100; bro3AUC30 <- 0.30*bro3AUC100;
bro3AUC31 <- 0.31*bro3AUC100; bro3AUC32 <- 0.32*bro3AUC100; bro3AUC33 <- 0.33*bro3AUC100; bro3AUC34 <- 0.34*bro3AUC100; bro3AUC35 <- 0.35*bro3AUC100;
bro3AUC36 <- 0.36*bro3AUC100; bro3AUC37 <- 0.37*bro3AUC100; bro3AUC38 <- 0.38*bro3AUC100; bro3AUC39 <- 0.39*bro3AUC100; bro3AUC40 <- 0.40*bro3AUC100;
bro3AUC41 <- 0.41*bro3AUC100; bro3AUC42 <- 0.42*bro3AUC100; bro3AUC43 <- 0.43*bro3AUC100; bro3AUC44 <- 0.44*bro3AUC100; bro3AUC45 <- 0.45*bro3AUC100;
bro3AUC46 <- 0.46*bro3AUC100; bro3AUC47 <- 0.47*bro3AUC100; bro3AUC48 <- 0.48*bro3AUC100; bro3AUC49 <- 0.49*bro3AUC100; bro3AUC50 <- 0.50*bro3AUC100;
bro3AUC51 <- 0.51*bro3AUC100; bro3AUC52 <- 0.52*bro3AUC100; bro3AUC53 <- 0.53*bro3AUC100; bro3AUC54 <- 0.54*bro3AUC100; bro3AUC55 <- 0.55*bro3AUC100;
bro3AUC56 <- 0.56*bro3AUC100; bro3AUC57 <- 0.57*bro3AUC100; bro3AUC58 <- 0.58*bro3AUC100; bro3AUC59 <- 0.59*bro3AUC100; bro3AUC60 <- 0.60*bro3AUC100;
bro3AUC61 <- 0.61*bro3AUC100; bro3AUC62 <- 0.62*bro3AUC100; bro3AUC63 <- 0.63*bro3AUC100; bro3AUC64 <- 0.64*bro3AUC100; bro3AUC65 <- 0.65*bro3AUC100;
bro3AUC66 <- 0.66*bro3AUC100; bro3AUC67 <- 0.67*bro3AUC100; bro3AUC68 <- 0.68*bro3AUC100; bro3AUC69 <- 0.69*bro3AUC100; bro3AUC70 <- 0.70*bro3AUC100;
bro3AUC71 <- 0.71*bro3AUC100; bro3AUC72 <- 0.72*bro3AUC100; bro3AUC73 <- 0.73*bro3AUC100; bro3AUC74 <- 0.74*bro3AUC100; bro3AUC75 <- 0.75*bro3AUC100;
bro3AUC76 <- 0.76*bro3AUC100; bro3AUC77 <- 0.77*bro3AUC100; bro3AUC78 <- 0.78*bro3AUC100; bro3AUC79 <- 0.79*bro3AUC100; bro3AUC80 <- 0.80*bro3AUC100;
bro3AUC81 <- 0.81*bro3AUC100; bro3AUC82 <- 0.82*bro3AUC100; bro3AUC83 <- 0.83*bro3AUC100; bro3AUC84 <- 0.84*bro3AUC100; bro3AUC85 <- 0.85*bro3AUC100;
bro3AUC86 <- 0.86*bro3AUC100; bro3AUC87 <- 0.87*bro3AUC100; bro3AUC88 <- 0.88*bro3AUC100; bro3AUC89 <- 0.89*bro3AUC100; bro3AUC90 <- 0.90*bro3AUC100;
bro3AUC91 <- 0.91*bro3AUC100; bro3AUC92 <- 0.92*bro3AUC100; bro3AUC93 <- 0.93*bro3AUC100; bro3AUC94 <- 0.94*bro3AUC100; bro3AUC95 <- 0.95*bro3AUC100;
bro3AUC96 <- 0.96*bro3AUC100; bro3AUC97 <- 0.97*bro3AUC100; bro3AUC98 <- 0.98*bro3AUC100; bro3AUC99 <- 0.99*bro3AUC100; 

# AUC01
area <- 0
i <- 2
while (area <= bro3AUC01) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 1% of the AUC is =", i))
i01 <- i

# AUC02
area <- 0
i <- 2
while (area <= bro3AUC02) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 2% of the AUC is =", i))
i02 <- i

# AUC03
area <- 0
i <- 2
while (area <= bro3AUC03) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 3% of the AUC is =", i))
i03 <- i

# AUC04
area <- 0
i <- 2
while (area <= bro3AUC04) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 4% of the AUC is =", i))
i04 <- i

# AUC05
area <- 0
i <- 2
while (area <= bro3AUC05) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 5% of the AUC is =", i))
i05 <- i

# AUC06
area <- 0
i <- 2
while (area <= bro3AUC06) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 6% of the AUC is =", i))
i06 <- i

# AUC07
area <- 0
i <- 2
while (area <= bro3AUC07) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 7% of the AUC is =", i))
i07 <- i

# AUC08
area <- 0
i <- 2
while (area <= bro3AUC08) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 8% of the AUC is =", i))
i08 <- i

# AUC09
area <- 0
i <- 2
while (area <= bro3AUC09) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 9% of the AUC is =", i))
i09 <- i

# AUC10
area <- 0
i <- 2
while (area <= bro3AUC10) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 10% of the AUC is =", i))
i10 <- i

# AUC11
area <- 0
i <- 2
while (area <= bro3AUC11) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 11% of the AUC is =", i))
i11 <- i

# AUC12
area <- 0
i <- 2
while (area <= bro3AUC12) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 12% of the AUC is =", i))
i12 <- i

# AUC13
area <- 0
i <- 2
while (area <= bro3AUC13) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 13% of the AUC is =", i))
i13 <- i

# AUC14
area <- 0
i <- 2
while (area <= bro3AUC14) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 14% of the AUC is =", i))
i14 <- i

# AUC15
area <- 0
i <- 2
while (area <= bro3AUC15) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 15% of the AUC is =", i))
i15 <- i

# AUC16
area <- 0
i <- 2
while (area <= bro3AUC16) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 16% of the AUC is =", i))
i16 <- i

# AUC17
area <- 0
i <- 2
while (area <= bro3AUC17) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 17% of the AUC is =", i))
i17 <- i

# AUC18
area <- 0
i <- 2
while (area <= bro3AUC18) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 18% of the AUC is =", i))
i18 <- i

# AUC19
area <- 0
i <- 2
while (area <= bro3AUC19) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 19% of the AUC is =", i))
i19 <- i

# AUC20
area <- 0
i <- 2
while (area <= bro3AUC20) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 20% of the AUC is =", i))
i20 <- i

# AUC21
area <- 0
i <- 2
while (area <= bro3AUC21) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 21% of the AUC is =", i))
i21 <- i

# AUC22
area <- 0
i <- 2
while (area <= bro3AUC22) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 22% of the AUC is =", i))
i22 <- i

# AUC23
area <- 0
i <- 2
while (area <= bro3AUC23) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 23% of the AUC is =", i))
i23 <- i

# AUC24
area <- 0
i <- 2
while (area <= bro3AUC24) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 24% of the AUC is =", i))
i24 <- i

# AUC25
area <- 0
i <- 2
while (area <= bro3AUC25) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 25% of the AUC is =", i))
i25 <- i

# AUC26
area <- 0
i <- 2
while (area <= bro3AUC26) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 26% of the AUC is =", i))
i26 <- i

# AUC27
area <- 0
i <- 2
while (area <= bro3AUC27) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 27% of the AUC is =", i))
i27 <- i

# AUC28
area <- 0
i <- 2
while (area <= bro3AUC28) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 28% of the AUC is =", i))
i28 <- i

# AUC29
area <- 0
i <- 2
while (area <= bro3AUC29) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 29% of the AUC is =", i))
i29 <- i

# AUC30
area <- 0
i <- 2
while (area <= bro3AUC30) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 30% of the AUC is =", i))
i30 <- i

# AUC31
area <- 0
i <- 2
while (area <= bro3AUC31) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 31% of the AUC is =", i))
i31 <- i

# AUC32
area <- 0
i <- 2
while (area <= bro3AUC32) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 32% of the AUC is =", i))
i32 <- i

# AUC33
area <- 0
i <- 2
while (area <= bro3AUC33) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 33% of the AUC is =", i))
i33 <- i

# AUC34
area <- 0
i <- 2
while (area <= bro3AUC34) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 34% of the AUC is =", i))
i34 <- i

# AUC35
area <- 0
i <- 2
while (area <= bro3AUC35) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 35% of the AUC is =", i))
i35 <- i

# AUC36
area <- 0
i <- 2
while (area <= bro3AUC36) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 36% of the AUC is =", i))
i36 <- i

# AUC37
area <- 0
i <- 2
while (area <= bro3AUC37) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 37% of the AUC is =", i))
i37 <- i

# AUC38
area <- 0
i <- 2
while (area <= bro3AUC38) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 38% of the AUC is =", i))
i38 <- i

# AUC39
area <- 0
i <- 2
while (area <= bro3AUC39) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 39% of the AUC is =", i))
i39 <- i

# AUC40
area <- 0
i <- 2
while (area <= bro3AUC40) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 40% of the AUC is =", i))
i40 <- i

# AUC41
area <- 0
i <- 2
while (area <= bro3AUC41) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 41% of the AUC is =", i))
i41 <- i

# AUC42
area <- 0
i <- 2
while (area <= bro3AUC42) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 42% of the AUC is =", i))
i42 <- i

# AUC43
area <- 0
i <- 2
while (area <= bro3AUC43) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 43% of the AUC is =", i))
i43 <- i

# AUC44
area <- 0
i <- 2
while (area <= bro3AUC44) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 44% of the AUC is =", i))
i44 <- i

# AUC45
area <- 0
i <- 2
while (area <= bro3AUC45) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 45% of the AUC is =", i))
i45 <- i

# AUC46
area <- 0
i <- 2
while (area <= bro3AUC46) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 46% of the AUC is =", i))
i46 <- i

# AUC47
area <- 0
i <- 2
while (area <= bro3AUC47) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 47% of the AUC is =", i))
i47 <- i

# AUC48
area <- 0
i <- 2
while (area <= bro3AUC48) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 48% of the AUC is =", i))
i48 <- i

# AUC49
area <- 0
i <- 2
while (area <= bro3AUC49) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 49% of the AUC is =", i))
i49 <- i

# AUC50
area <- 0
i <- 2
while (area <= bro3AUC50) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 50% of the AUC is =", i))
i50 <- i

# AUC51
area <- 0
i <- 2
while (area <= bro3AUC51) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 51% of the AUC is =", i))
i51 <- i

# AUC52
area <- 0
i <- 2
while (area <= bro3AUC52) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 52% of the AUC is =", i))
i52 <- i

# AUC53
area <- 0
i <- 2
while (area <= bro3AUC53) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 53% of the AUC is =", i))
i53 <- i

# AUC54
area <- 0
i <- 2
while (area <= bro3AUC54) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 54% of the AUC is =", i))
i54 <- i

# AUC55
area <- 0
i <- 2
while (area <= bro3AUC55) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 55% of the AUC is =", i))
i55 <- i

# AUC56
area <- 0
i <- 2
while (area <= bro3AUC56) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 56% of the AUC is =", i))
i56 <- i

# AUC57
area <- 0
i <- 2
while (area <= bro3AUC57) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 57% of the AUC is =", i))
i57 <- i

# AUC58
area <- 0
i <- 2
while (area <= bro3AUC58) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 58% of the AUC is =", i))
i58 <- i

# AUC59
area <- 0
i <- 2
while (area <= bro3AUC59) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 59% of the AUC is =", i))
i59 <- i

# AUC60
area <- 0
i <- 2
while (area <= bro3AUC60) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 60% of the AUC is =", i))
i60 <- i

# AUC61
area <- 0
i <- 2
while (area <= bro3AUC61) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 61% of the AUC is =", i))
i61 <- i

# AUC62
area <- 0
i <- 2
while (area <= bro3AUC62) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 62% of the AUC is =", i))
i62 <- i

# AUC63
area <- 0
i <- 2
while (area <= bro3AUC63) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 63% of the AUC is =", i))
i63 <- i

# AUC64
area <- 0
i <- 2
while (area <= bro3AUC64) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 64% of the AUC is =", i))
i64 <- i

# AUC65
area <- 0
i <- 2
while (area <= bro3AUC65) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 65% of the AUC is =", i))
i65 <- i

# AUC66
area <- 0
i <- 2
while (area <= bro3AUC66) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 66% of the AUC is =", i))
i66 <- i

# AUC67
area <- 0
i <- 2
while (area <= bro3AUC67) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 67% of the AUC is =", i))
i67 <- i

# AUC68
area <- 0
i <- 2
while (area <= bro3AUC68) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 68% of the AUC is =", i))
i68 <- i

# AUC69
area <- 0
i <- 2
while (area <= bro3AUC69) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 69% of the AUC is =", i))
i69 <- i

# AUC70
area <- 0
i <- 2
while (area <= bro3AUC70) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 70% of the AUC is =", i))
i70 <- i

# AUC71
area <- 0
i <- 2
while (area <= bro3AUC71) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 71% of the AUC is =", i))
i71 <- i

# AUC72
area <- 0
i <- 2
while (area <= bro3AUC72) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 72% of the AUC is =", i))
i72 <- i

# AUC73
area <- 0
i <- 2
while (area <= bro3AUC73) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 73% of the AUC is =", i))
i73 <- i

# AUC74
area <- 0
i <- 2
while (area <= bro3AUC74) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 74% of the AUC is =", i))
i74 <- i

# AUC75
area <- 0
i <- 2
while (area <= bro3AUC75) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 75% of the AUC is =", i))
i75 <- i

# AUC76
area <- 0
i <- 2
while (area <= bro3AUC76) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 76% of the AUC is =", i))
i76 <- i

# AUC77
area <- 0
i <- 2
while (area <= bro3AUC77) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 77% of the AUC is =", i))
i77 <- i

# AUC78
area <- 0
i <- 2
while (area <= bro3AUC78) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 78% of the AUC is =", i))
i78 <- i

# AUC79
area <- 0
i <- 2
while (area <= bro3AUC79) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 79% of the AUC is =", i))
i79 <- i

# AUC80
area <- 0
i <- 2
while (area <= bro3AUC80) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 80% of the AUC is =", i))
i80 <- i

# AUC81
area <- 0
i <- 2
while (area <= bro3AUC81) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 81% of the AUC is =", i))
i81 <- i

# AUC82
area <- 0
i <- 2
while (area <= bro3AUC82) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 82% of the AUC is =", i))
i82 <- i

# AUC83
area <- 0
i <- 2
while (area <= bro3AUC83) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 83% of the AUC is =", i))
i83 <- i

# AUC84
area <- 0
i <- 2
while (area <= bro3AUC84) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 84% of the AUC is =", i))
i84 <- i

# AUC85
area <- 0
i <- 2
while (area <= bro3AUC85) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 85% of the AUC is =", i))
i85 <- i

# AUC86
area <- 0
i <- 2
while (area <= bro3AUC86) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 86% of the AUC is =", i))
i86 <- i

# AUC87
area <- 0
i <- 2
while (area <= bro3AUC87) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 87% of the AUC is =", i))
i87 <- i

# AUC88
area <- 0
i <- 2
while (area <= bro3AUC88) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 88% of the AUC is =", i))
i88 <- i

# AUC89
area <- 0
i <- 2
while (area <= bro3AUC89) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 89% of the AUC is =", i))
i89 <- i

# AUC90
area <- 0
i <- 2
while (area <= bro3AUC90) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 90% of the AUC is =", i))
i90 <- i

# AUC91
area <- 0
i <- 2
while (area <= bro3AUC91) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 91% of the AUC is =", i))
i91 <- i

# AUC92
area <- 0
i <- 2
while (area <= bro3AUC92) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 92% of the AUC is =", i))
i92 <- i

# AUC93
area <- 0
i <- 2
while (area <= bro3AUC93) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 93% of the AUC is =", i))
i93 <- i

# AUC94
area <- 0
i <- 2
while (area <= bro3AUC94) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 94% of the AUC is =", i))
i94 <- i

# AUC95
area <- 0
i <- 2
while (area <= bro3AUC95) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 95% of the AUC is =", i))
i95 <- i

# AUC96
area <- 0
i <- 2
while (area <= bro3AUC96) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 96% of the AUC is =", i))
i96 <- i

# AUC97
area <- 0
i <- 2
while (area <= bro3AUC97) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 97% of the AUC is =", i))
i97 <- i

# AUC98
area <- 0
i <- 2
while (area <= bro3AUC98) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 98% of the AUC is =", i))
i98 <- i

# AUC99
area <- 0
i <- 2
while (area <= bro3AUC99) {
  area <- pk.calc.auc(brome_dg.y, brome_dg.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 99% of the AUC is =", i))
i99 <- i

i100 <- length(brome_dg.y)

# Make a table of the AUC x-vals for quick reference
brome.dg.auc <- data.frame(auc = c(paste("auc0",1:9,sep=""),
                                   paste("auc",10:100,sep="")),
                           otu.num = c(i01,i02,i03,i04,i05,i06,i07,i08,i09,
                                       i10,i11,i12,i13,i14,i15,i16,i17,i18,i19,
                                       i20,i21,i22,i23,i24,i25,i26,i27,i28,i29,
                                       i30,i31,i32,i33,i34,i35,i36,i37,i38,i39,
                                       i40,i41,i42,i43,i44,i45,i46,i47,i48,i49,
                                       i50,i51,i52,i53,i54,i55,i56,i57,i58,i59,
                                       i60,i61,i62,i63,i64,i65,i66,i67,i68,i69,
                                       i70,i71,i72,i73,i74,i75,i76,i77,i78,i79,
                                       i80,i81,i82,i83,i84,i85,i86,i87,i88,i89,
                                       i90,i91,i92,i93,i94,i95,i96,i97,i98,i99,i100))
write.csv(brome.dg.auc, "~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/brome.dg.auc.csv")
write.csv(as.data.frame(cbind(x = brome_dg.x, y = brome_dg.y)), "~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/brome.add1.degree.fig2.csv")

############################################################################################################
############################################################################################################

# Closeness selectbyall, MIC thresold of 0.2, minimum = 3 OTUs
brome_cl <- read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/ADD1_AUC100_MIN3_Brome_bacfunarc_dw_otu_table-graph_centrality-closeness-selectallbyall.csv")
brome_cl <- brome_cl[ order(-brome_cl$metric), ]
brome_cl.x <- as.numeric(rownames(brome_cl))  ## pulling in index after ordering
brome_cl.y <- brome_cl$metric  ## metric of interest

## Area under the curve

## Brome A horizon
# Degree
bro3AUC100 <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, 2996)) 
bro3AUC01 <- 0.01*bro3AUC100; bro3AUC02 <- 0.02*bro3AUC100; bro3AUC03 <- 0.03*bro3AUC100; bro3AUC04 <- 0.04*bro3AUC100; bro3AUC05 <- 0.05*bro3AUC100
bro3AUC06 <- 0.06*bro3AUC100; bro3AUC07 <- 0.07*bro3AUC100; bro3AUC08 <- 0.08*bro3AUC100; bro3AUC09 <- 0.09*bro3AUC100; bro3AUC10 <- 0.10*bro3AUC100
bro3AUC11 <- 0.11*bro3AUC100; bro3AUC12 <- 0.12*bro3AUC100; bro3AUC13 <- 0.13*bro3AUC100; bro3AUC14 <- 0.14*bro3AUC100; bro3AUC15 <- 0.15*bro3AUC100
bro3AUC16 <- 0.16*bro3AUC100; bro3AUC17 <- 0.17*bro3AUC100; bro3AUC18 <- 0.18*bro3AUC100; bro3AUC19 <- 0.19*bro3AUC100; bro3AUC20 <- 0.20*bro3AUC100
bro3AUC21 <- 0.21*bro3AUC100; bro3AUC22 <- 0.22*bro3AUC100; bro3AUC23 <- 0.23*bro3AUC100; bro3AUC24 <- 0.24*bro3AUC100; bro3AUC25 <- 0.25*bro3AUC100
bro3AUC26 <- 0.26*bro3AUC100; bro3AUC27 <- 0.27*bro3AUC100; bro3AUC28 <- 0.28*bro3AUC100; bro3AUC29 <- 0.29*bro3AUC100; bro3AUC30 <- 0.30*bro3AUC100
bro3AUC31 <- 0.31*bro3AUC100; bro3AUC32 <- 0.32*bro3AUC100; bro3AUC33 <- 0.33*bro3AUC100; bro3AUC34 <- 0.34*bro3AUC100; bro3AUC35 <- 0.35*bro3AUC100
bro3AUC36 <- 0.36*bro3AUC100; bro3AUC37 <- 0.37*bro3AUC100; bro3AUC38 <- 0.38*bro3AUC100; bro3AUC39 <- 0.39*bro3AUC100; bro3AUC40 <- 0.40*bro3AUC100
bro3AUC41 <- 0.41*bro3AUC100; bro3AUC42 <- 0.42*bro3AUC100; bro3AUC43 <- 0.43*bro3AUC100; bro3AUC44 <- 0.44*bro3AUC100; bro3AUC45 <- 0.45*bro3AUC100
bro3AUC46 <- 0.46*bro3AUC100; bro3AUC47 <- 0.47*bro3AUC100; bro3AUC48 <- 0.48*bro3AUC100; bro3AUC49 <- 0.49*bro3AUC100; bro3AUC50 <- 0.50*bro3AUC100
bro3AUC51 <- 0.51*bro3AUC100; bro3AUC52 <- 0.52*bro3AUC100; bro3AUC53 <- 0.53*bro3AUC100; bro3AUC54 <- 0.54*bro3AUC100; bro3AUC55 <- 0.55*bro3AUC100
bro3AUC56 <- 0.56*bro3AUC100; bro3AUC57 <- 0.57*bro3AUC100; bro3AUC58 <- 0.58*bro3AUC100; bro3AUC59 <- 0.59*bro3AUC100; bro3AUC60 <- 0.60*bro3AUC100
bro3AUC61 <- 0.61*bro3AUC100; bro3AUC62 <- 0.62*bro3AUC100; bro3AUC63 <- 0.63*bro3AUC100; bro3AUC64 <- 0.64*bro3AUC100; bro3AUC65 <- 0.65*bro3AUC100
bro3AUC66 <- 0.66*bro3AUC100; bro3AUC67 <- 0.67*bro3AUC100; bro3AUC68 <- 0.68*bro3AUC100; bro3AUC69 <- 0.69*bro3AUC100; bro3AUC70 <- 0.70*bro3AUC100
bro3AUC71 <- 0.71*bro3AUC100; bro3AUC72 <- 0.72*bro3AUC100; bro3AUC73 <- 0.73*bro3AUC100; bro3AUC74 <- 0.74*bro3AUC100; bro3AUC75 <- 0.75*bro3AUC100
bro3AUC76 <- 0.76*bro3AUC100; bro3AUC77 <- 0.77*bro3AUC100; bro3AUC78 <- 0.78*bro3AUC100; bro3AUC79 <- 0.79*bro3AUC100; bro3AUC80 <- 0.80*bro3AUC100
bro3AUC81 <- 0.81*bro3AUC100; bro3AUC82 <- 0.82*bro3AUC100; bro3AUC83 <- 0.83*bro3AUC100; bro3AUC84 <- 0.84*bro3AUC100; bro3AUC85 <- 0.85*bro3AUC100
bro3AUC86 <- 0.86*bro3AUC100; bro3AUC87 <- 0.87*bro3AUC100; bro3AUC88 <- 0.88*bro3AUC100; bro3AUC89 <- 0.89*bro3AUC100; bro3AUC90 <- 0.90*bro3AUC100
bro3AUC91 <- 0.91*bro3AUC100; bro3AUC92 <- 0.92*bro3AUC100; bro3AUC93 <- 0.93*bro3AUC100; bro3AUC94 <- 0.94*bro3AUC100; bro3AUC95 <- 0.95*bro3AUC100
bro3AUC96 <- 0.96*bro3AUC100; bro3AUC97 <- 0.97*bro3AUC100; bro3AUC98 <- 0.98*bro3AUC100; bro3AUC99 <- 0.99*bro3AUC100

# AUC01
area <- 0
i <- 2
while (area <= bro3AUC01) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 1% of the AUC is =", i))
i01 <- i

# AUC02
area <- 0
i <- 2
while (area <= bro3AUC02) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 2% of the AUC is =", i))
i02 <- i

# AUC03
area <- 0
i <- 2
while (area <= bro3AUC03) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 3% of the AUC is =", i))
i03 <- i

# AUC04
area <- 0
i <- 2
while (area <= bro3AUC04) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 4% of the AUC is =", i))
i04 <- i

# AUC05
area <- 0
i <- 2
while (area <= bro3AUC05) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 5% of the AUC is =", i))
i05 <- i

# AUC06
area <- 0
i <- 2
while (area <= bro3AUC06) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 6% of the AUC is =", i))
i06 <- i

# AUC07
area <- 0
i <- 2
while (area <= bro3AUC07) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 7% of the AUC is =", i))
i07 <- i

# AUC08
area <- 0
i <- 2
while (area <= bro3AUC08) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 8% of the AUC is =", i))
i08 <- i

# AUC09
area <- 0
i <- 2
while (area <= bro3AUC09) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 9% of the AUC is =", i))
i09 <- i

# AUC10
area <- 0
i <- 2
while (area <= bro3AUC10) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 10% of the AUC is =", i))
i10 <- i

# AUC11
area <- 0
i <- 2
while (area <= bro3AUC11) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 11% of the AUC is =", i))
i11 <- i

# AUC12
area <- 0
i <- 2
while (area <= bro3AUC12) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 12% of the AUC is =", i))
i12 <- i

# AUC13
area <- 0
i <- 2
while (area <= bro3AUC13) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 13% of the AUC is =", i))
i13 <- i

# AUC14
area <- 0
i <- 2
while (area <= bro3AUC14) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 14% of the AUC is =", i))
i14 <- i

# AUC15
area <- 0
i <- 2
while (area <= bro3AUC15) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 15% of the AUC is =", i))
i15 <- i

# AUC16
area <- 0
i <- 2
while (area <= bro3AUC16) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 16% of the AUC is =", i))
i16 <- i

# AUC17
area <- 0
i <- 2
while (area <= bro3AUC17) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 17% of the AUC is =", i))
i17 <- i

# AUC18
area <- 0
i <- 2
while (area <= bro3AUC18) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 18% of the AUC is =", i))
i18 <- i

# AUC19
area <- 0
i <- 2
while (area <= bro3AUC19) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 19% of the AUC is =", i))
i19 <- i

# AUC20
area <- 0
i <- 2
while (area <= bro3AUC20) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 20% of the AUC is =", i))
i20 <- i

# AUC21
area <- 0
i <- 2
while (area <= bro3AUC21) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 21% of the AUC is =", i))
i21 <- i

# AUC22
area <- 0
i <- 2
while (area <= bro3AUC22) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 22% of the AUC is =", i))
i22 <- i

# AUC23
area <- 0
i <- 2
while (area <= bro3AUC23) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 23% of the AUC is =", i))
i23 <- i

# AUC24
area <- 0
i <- 2
while (area <= bro3AUC24) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 24% of the AUC is =", i))
i24 <- i

# AUC25
area <- 0
i <- 2
while (area <= bro3AUC25) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 25% of the AUC is =", i))
i25 <- i

# AUC26
area <- 0
i <- 2
while (area <= bro3AUC26) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 26% of the AUC is =", i))
i26 <- i

# AUC27
area <- 0
i <- 2
while (area <= bro3AUC27) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 27% of the AUC is =", i))
i27 <- i

# AUC28
area <- 0
i <- 2
while (area <= bro3AUC28) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 28% of the AUC is =", i))
i28 <- i

# AUC29
area <- 0
i <- 2
while (area <= bro3AUC29) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 29% of the AUC is =", i))
i29 <- i

# AUC30
area <- 0
i <- 2
while (area <= bro3AUC30) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 30% of the AUC is =", i))
i30 <- i

# AUC31
area <- 0
i <- 2
while (area <= bro3AUC31) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 31% of the AUC is =", i))
i31 <- i

# AUC32
area <- 0
i <- 2
while (area <= bro3AUC32) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 32% of the AUC is =", i))
i32 <- i

# AUC33
area <- 0
i <- 2
while (area <= bro3AUC33) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 33% of the AUC is =", i))
i33 <- i

# AUC34
area <- 0
i <- 2
while (area <= bro3AUC34) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 34% of the AUC is =", i))
i34 <- i

# AUC35
area <- 0
i <- 2
while (area <= bro3AUC35) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 35% of the AUC is =", i))
i35 <- i

# AUC36
area <- 0
i <- 2
while (area <= bro3AUC36) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 36% of the AUC is =", i))
i36 <- i

# AUC37
area <- 0
i <- 2
while (area <= bro3AUC37) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 37% of the AUC is =", i))
i37 <- i

# AUC38
area <- 0
i <- 2
while (area <= bro3AUC38) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 38% of the AUC is =", i))
i38 <- i

# AUC39
area <- 0
i <- 2
while (area <= bro3AUC39) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 39% of the AUC is =", i))
i39 <- i

# AUC40
area <- 0
i <- 2
while (area <= bro3AUC40) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 40% of the AUC is =", i))
i40 <- i

# AUC41
area <- 0
i <- 2
while (area <= bro3AUC41) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 41% of the AUC is =", i))
i41 <- i

# AUC42
area <- 0
i <- 2
while (area <= bro3AUC42) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 42% of the AUC is =", i))
i42 <- i

# AUC43
area <- 0
i <- 2
while (area <= bro3AUC43) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 43% of the AUC is =", i))
i43 <- i

# AUC44
area <- 0
i <- 2
while (area <= bro3AUC44) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 44% of the AUC is =", i))
i44 <- i

# AUC45
area <- 0
i <- 2
while (area <= bro3AUC45) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 45% of the AUC is =", i))
i45 <- i

# AUC46
area <- 0
i <- 2
while (area <= bro3AUC46) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 46% of the AUC is =", i))
i46 <- i

# AUC47
area <- 0
i <- 2
while (area <= bro3AUC47) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 47% of the AUC is =", i))
i47 <- i

# AUC48
area <- 0
i <- 2
while (area <= bro3AUC48) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 48% of the AUC is =", i))
i48 <- i

# AUC49
area <- 0
i <- 2
while (area <= bro3AUC49) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 49% of the AUC is =", i))
i49 <- i

# AUC50
area <- 0
i <- 2
while (area <= bro3AUC50) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 50% of the AUC is =", i))
i50 <- i

# AUC51
area <- 0
i <- 2
while (area <= bro3AUC51) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 51% of the AUC is =", i))
i51 <- i

# AUC52
area <- 0
i <- 2
while (area <= bro3AUC52) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 52% of the AUC is =", i))
i52 <- i

# AUC53
area <- 0
i <- 2
while (area <= bro3AUC53) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 53% of the AUC is =", i))
i53 <- i

# AUC54
area <- 0
i <- 2
while (area <= bro3AUC54) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 54% of the AUC is =", i))
i54 <- i

# AUC55
area <- 0
i <- 2
while (area <= bro3AUC55) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 55% of the AUC is =", i))
i55 <- i

# AUC56
area <- 0
i <- 2
while (area <= bro3AUC56) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 56% of the AUC is =", i))
i56 <- i

# AUC57
area <- 0
i <- 2
while (area <= bro3AUC57) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 57% of the AUC is =", i))
i57 <- i

# AUC58
area <- 0
i <- 2
while (area <= bro3AUC58) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 58% of the AUC is =", i))
i58 <- i

# AUC59
area <- 0
i <- 2
while (area <= bro3AUC59) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 59% of the AUC is =", i))
i59 <- i

# AUC60
area <- 0
i <- 2
while (area <= bro3AUC60) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 60% of the AUC is =", i))
i60 <- i

# AUC61
area <- 0
i <- 2
while (area <= bro3AUC61) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 61% of the AUC is =", i))
i61 <- i

# AUC62
area <- 0
i <- 2
while (area <= bro3AUC62) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 62% of the AUC is =", i))
i62 <- i

# AUC63
area <- 0
i <- 2
while (area <= bro3AUC63) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 63% of the AUC is =", i))
i63 <- i

# AUC64
area <- 0
i <- 2
while (area <= bro3AUC64) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 64% of the AUC is =", i))
i64 <- i

# AUC65
area <- 0
i <- 2
while (area <= bro3AUC65) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 65% of the AUC is =", i))
i65 <- i

# AUC66
area <- 0
i <- 2
while (area <= bro3AUC66) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 66% of the AUC is =", i))
i66 <- i

# AUC67
area <- 0
i <- 2
while (area <= bro3AUC67) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 67% of the AUC is =", i))
i67 <- i

# AUC68
area <- 0
i <- 2
while (area <= bro3AUC68) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 68% of the AUC is =", i))
i68 <- i

# AUC69
area <- 0
i <- 2
while (area <= bro3AUC69) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 69% of the AUC is =", i))
i69 <- i

# AUC70
area <- 0
i <- 2
while (area <= bro3AUC70) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 70% of the AUC is =", i))
i70 <- i

# AUC71
area <- 0
i <- 2
while (area <= bro3AUC71) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 71% of the AUC is =", i))
i71 <- i

# AUC72
area <- 0
i <- 2
while (area <= bro3AUC72) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 72% of the AUC is =", i))
i72 <- i

# AUC73
area <- 0
i <- 2
while (area <= bro3AUC73) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 73% of the AUC is =", i))
i73 <- i

# AUC74
area <- 0
i <- 2
while (area <= bro3AUC74) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 74% of the AUC is =", i))
i74 <- i

# AUC75
area <- 0
i <- 2
while (area <= bro3AUC75) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 75% of the AUC is =", i))
i75 <- i

# AUC76
area <- 0
i <- 2
while (area <= bro3AUC76) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 76% of the AUC is =", i))
i76 <- i

# AUC77
area <- 0
i <- 2
while (area <= bro3AUC77) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 77% of the AUC is =", i))
i77 <- i

# AUC78
area <- 0
i <- 2
while (area <= bro3AUC78) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 78% of the AUC is =", i))
i78 <- i

# AUC79
area <- 0
i <- 2
while (area <= bro3AUC79) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 79% of the AUC is =", i))
i79 <- i

# AUC80
area <- 0
i <- 2
while (area <= bro3AUC80) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 80% of the AUC is =", i))
i80 <- i

# AUC81
area <- 0
i <- 2
while (area <= bro3AUC81) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 81% of the AUC is =", i))
i81 <- i

# AUC82
area <- 0
i <- 2
while (area <= bro3AUC82) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 82% of the AUC is =", i))
i82 <- i

# AUC83
area <- 0
i <- 2
while (area <= bro3AUC83) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 83% of the AUC is =", i))
i83 <- i

# AUC84
area <- 0
i <- 2
while (area <= bro3AUC84) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 84% of the AUC is =", i))
i84 <- i

# AUC85
area <- 0
i <- 2
while (area <= bro3AUC85) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 85% of the AUC is =", i))
i85 <- i

# AUC86
area <- 0
i <- 2
while (area <= bro3AUC86) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 86% of the AUC is =", i))
i86 <- i

# AUC87
area <- 0
i <- 2
while (area <= bro3AUC87) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 87% of the AUC is =", i))
i87 <- i

# AUC88
area <- 0
i <- 2
while (area <= bro3AUC88) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 88% of the AUC is =", i))
i88 <- i

# AUC89
area <- 0
i <- 2
while (area <= bro3AUC89) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 89% of the AUC is =", i))
i89 <- i

# AUC90
area <- 0
i <- 2
while (area <= bro3AUC90) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 90% of the AUC is =", i))
i90 <- i

# AUC91
area <- 0
i <- 2
while (area <= bro3AUC91) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 91% of the AUC is =", i))
i91 <- i

# AUC92
area <- 0
i <- 2
while (area <= bro3AUC92) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 92% of the AUC is =", i))
i92 <- i

# AUC93
area <- 0
i <- 2
while (area <= bro3AUC93) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 93% of the AUC is =", i))
i93 <- i

# AUC94
area <- 0
i <- 2
while (area <= bro3AUC94) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 94% of the AUC is =", i))
i94 <- i

# AUC95
area <- 0
i <- 2
while (area <= bro3AUC95) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 95% of the AUC is =", i))
i95 <- i

# AUC96
area <- 0
i <- 2
while (area <= bro3AUC96) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 96% of the AUC is =", i))
i96 <- i

# AUC97
area <- 0
i <- 2
while (area <= bro3AUC97) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 97% of the AUC is =", i))
i97 <- i

# AUC98
area <- 0
i <- 2
while (area <= bro3AUC98) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 98% of the AUC is =", i))
i98 <- i

# AUC99
area <- 0
i <- 2
while (area <= bro3AUC99) {
  area <- pk.calc.auc(brome_cl.y, brome_cl.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 99% of the AUC is =", i))
i99 <- i

i100 <- length(brome_cl.y)

# Make a table of the AUC x-vals for quick reference
brome.cl.auc <- data.frame(auc = c(paste("auc0",1:9,sep=""),
                                   paste("auc",10:100,sep="")),
                           otu.num = c(i01,i02,i03,i04,i05,i06,i07,i08,i09,
                                       i10,i11,i12,i13,i14,i15,i16,i17,i18,i19,
                                       i20,i21,i22,i23,i24,i25,i26,i27,i28,i29,
                                       i30,i31,i32,i33,i34,i35,i36,i37,i38,i39,
                                       i40,i41,i42,i43,i44,i45,i46,i47,i48,i49,
                                       i50,i51,i52,i53,i54,i55,i56,i57,i58,i59,
                                       i60,i61,i62,i63,i64,i65,i66,i67,i68,i69,
                                       i70,i71,i72,i73,i74,i75,i76,i77,i78,i79,
                                       i80,i81,i82,i83,i84,i85,i86,i87,i88,i89,
                                       i90,i91,i92,i93,i94,i95,i96,i97,i98,i99,i100))
write.csv(brome.cl.auc, "~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/brome.cl.auc.csv")
write.csv(as.data.frame(cbind(x = brome_cl.x, y = brome_cl.y)), "~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/brome.add1.closeness.csv")

############################################################################################################
############################################################################################################

# Closeness selectbyall, MIC thresold of 0.2, minimum = 3 OTUs
brome_ei <- read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/ADD1_AUC100_MIN3Brome_bacfunarc_dw_otu_table-graph_centrality-eigenvector-selectallbyall.csv")
brome_ei <- brome_ei[ order(-brome_ei$metric), ]
brome_ei.x <- as.numeric(rownames(brome_ei))  ## pulling in index after ordering
brome_ei.y <- brome_ei$metric  ## metric of interest

## Area under the curve

## Brome A horizon
# Degree
bro3AUC100 <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, 2996)) 
bro3AUC01 <- 0.01*bro3AUC100; bro3AUC02 <- 0.02*bro3AUC100; bro3AUC03 <- 0.03*bro3AUC100; bro3AUC04 <- 0.04*bro3AUC100; bro3AUC05 <- 0.05*bro3AUC100
bro3AUC06 <- 0.06*bro3AUC100; bro3AUC07 <- 0.07*bro3AUC100; bro3AUC08 <- 0.08*bro3AUC100; bro3AUC09 <- 0.09*bro3AUC100; bro3AUC10 <- 0.10*bro3AUC100
bro3AUC11 <- 0.11*bro3AUC100; bro3AUC12 <- 0.12*bro3AUC100; bro3AUC13 <- 0.13*bro3AUC100; bro3AUC14 <- 0.14*bro3AUC100; bro3AUC15 <- 0.15*bro3AUC100
bro3AUC16 <- 0.16*bro3AUC100; bro3AUC17 <- 0.17*bro3AUC100; bro3AUC18 <- 0.18*bro3AUC100; bro3AUC19 <- 0.19*bro3AUC100; bro3AUC20 <- 0.20*bro3AUC100
bro3AUC21 <- 0.21*bro3AUC100; bro3AUC22 <- 0.22*bro3AUC100; bro3AUC23 <- 0.23*bro3AUC100; bro3AUC24 <- 0.24*bro3AUC100; bro3AUC25 <- 0.25*bro3AUC100
bro3AUC26 <- 0.26*bro3AUC100; bro3AUC27 <- 0.27*bro3AUC100; bro3AUC28 <- 0.28*bro3AUC100; bro3AUC29 <- 0.29*bro3AUC100; bro3AUC30 <- 0.30*bro3AUC100
bro3AUC31 <- 0.31*bro3AUC100; bro3AUC32 <- 0.32*bro3AUC100; bro3AUC33 <- 0.33*bro3AUC100; bro3AUC34 <- 0.34*bro3AUC100; bro3AUC35 <- 0.35*bro3AUC100
bro3AUC36 <- 0.36*bro3AUC100; bro3AUC37 <- 0.37*bro3AUC100; bro3AUC38 <- 0.38*bro3AUC100; bro3AUC39 <- 0.39*bro3AUC100; bro3AUC40 <- 0.40*bro3AUC100
bro3AUC41 <- 0.41*bro3AUC100; bro3AUC42 <- 0.42*bro3AUC100; bro3AUC43 <- 0.43*bro3AUC100; bro3AUC44 <- 0.44*bro3AUC100; bro3AUC45 <- 0.45*bro3AUC100
bro3AUC46 <- 0.46*bro3AUC100; bro3AUC47 <- 0.47*bro3AUC100; bro3AUC48 <- 0.48*bro3AUC100; bro3AUC49 <- 0.49*bro3AUC100; bro3AUC50 <- 0.50*bro3AUC100
bro3AUC51 <- 0.51*bro3AUC100; bro3AUC52 <- 0.52*bro3AUC100; bro3AUC53 <- 0.53*bro3AUC100; bro3AUC54 <- 0.54*bro3AUC100; bro3AUC55 <- 0.55*bro3AUC100
bro3AUC56 <- 0.56*bro3AUC100; bro3AUC57 <- 0.57*bro3AUC100; bro3AUC58 <- 0.58*bro3AUC100; bro3AUC59 <- 0.59*bro3AUC100; bro3AUC60 <- 0.60*bro3AUC100
bro3AUC61 <- 0.61*bro3AUC100; bro3AUC62 <- 0.62*bro3AUC100; bro3AUC63 <- 0.63*bro3AUC100; bro3AUC64 <- 0.64*bro3AUC100; bro3AUC65 <- 0.65*bro3AUC100
bro3AUC66 <- 0.66*bro3AUC100; bro3AUC67 <- 0.67*bro3AUC100; bro3AUC68 <- 0.68*bro3AUC100; bro3AUC69 <- 0.69*bro3AUC100; bro3AUC70 <- 0.70*bro3AUC100
bro3AUC71 <- 0.71*bro3AUC100; bro3AUC72 <- 0.72*bro3AUC100; bro3AUC73 <- 0.73*bro3AUC100; bro3AUC74 <- 0.74*bro3AUC100; bro3AUC75 <- 0.75*bro3AUC100
bro3AUC76 <- 0.76*bro3AUC100; bro3AUC77 <- 0.77*bro3AUC100; bro3AUC78 <- 0.78*bro3AUC100; bro3AUC79 <- 0.79*bro3AUC100; bro3AUC80 <- 0.80*bro3AUC100
bro3AUC81 <- 0.81*bro3AUC100; bro3AUC82 <- 0.82*bro3AUC100; bro3AUC83 <- 0.83*bro3AUC100; bro3AUC84 <- 0.84*bro3AUC100; bro3AUC85 <- 0.85*bro3AUC100
bro3AUC86 <- 0.86*bro3AUC100; bro3AUC87 <- 0.87*bro3AUC100; bro3AUC88 <- 0.88*bro3AUC100; bro3AUC89 <- 0.89*bro3AUC100; bro3AUC90 <- 0.90*bro3AUC100
bro3AUC91 <- 0.91*bro3AUC100; bro3AUC92 <- 0.92*bro3AUC100; bro3AUC93 <- 0.93*bro3AUC100; bro3AUC94 <- 0.94*bro3AUC100; bro3AUC95 <- 0.95*bro3AUC100
bro3AUC96 <- 0.96*bro3AUC100; bro3AUC97 <- 0.97*bro3AUC100; bro3AUC98 <- 0.98*bro3AUC100; bro3AUC99 <- 0.99*bro3AUC100

# AUC01
area <- 0
i <- 2
while (area <= bro3AUC01) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 1% of the AUC is =", i))
i01 <- i

# AUC02
area <- 0
i <- 2
while (area <= bro3AUC02) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 2% of the AUC is =", i))
i02 <- i

# AUC03
area <- 0
i <- 2
while (area <= bro3AUC03) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 3% of the AUC is =", i))
i03 <- i

# AUC04
area <- 0
i <- 2
while (area <= bro3AUC04) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 4% of the AUC is =", i))
i04 <- i

# AUC05
area <- 0
i <- 2
while (area <= bro3AUC05) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 5% of the AUC is =", i))
i05 <- i

# AUC06
area <- 0
i <- 2
while (area <= bro3AUC06) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 6% of the AUC is =", i))
i06 <- i

# AUC07
area <- 0
i <- 2
while (area <= bro3AUC07) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 7% of the AUC is =", i))
i07 <- i

# AUC08
area <- 0
i <- 2
while (area <= bro3AUC08) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 8% of the AUC is =", i))
i08 <- i

# AUC09
area <- 0
i <- 2
while (area <= bro3AUC09) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 9% of the AUC is =", i))
i09 <- i

# AUC10
area <- 0
i <- 2
while (area <= bro3AUC10) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 10% of the AUC is =", i))
i10 <- i

# AUC11
area <- 0
i <- 2
while (area <= bro3AUC11) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 11% of the AUC is =", i))
i11 <- i

# AUC12
area <- 0
i <- 2
while (area <= bro3AUC12) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 12% of the AUC is =", i))
i12 <- i

# AUC13
area <- 0
i <- 2
while (area <= bro3AUC13) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 13% of the AUC is =", i))
i13 <- i

# AUC14
area <- 0
i <- 2
while (area <= bro3AUC14) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 14% of the AUC is =", i))
i14 <- i

# AUC15
area <- 0
i <- 2
while (area <= bro3AUC15) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 15% of the AUC is =", i))
i15 <- i

# AUC16
area <- 0
i <- 2
while (area <= bro3AUC16) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 16% of the AUC is =", i))
i16 <- i

# AUC17
area <- 0
i <- 2
while (area <= bro3AUC17) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 17% of the AUC is =", i))
i17 <- i

# AUC18
area <- 0
i <- 2
while (area <= bro3AUC18) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 18% of the AUC is =", i))
i18 <- i

# AUC19
area <- 0
i <- 2
while (area <= bro3AUC19) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 19% of the AUC is =", i))
i19 <- i

# AUC20
area <- 0
i <- 2
while (area <= bro3AUC20) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 20% of the AUC is =", i))
i20 <- i

# AUC21
area <- 0
i <- 2
while (area <= bro3AUC21) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 21% of the AUC is =", i))
i21 <- i

# AUC22
area <- 0
i <- 2
while (area <= bro3AUC22) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 22% of the AUC is =", i))
i22 <- i

# AUC23
area <- 0
i <- 2
while (area <= bro3AUC23) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 23% of the AUC is =", i))
i23 <- i

# AUC24
area <- 0
i <- 2
while (area <= bro3AUC24) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 24% of the AUC is =", i))
i24 <- i

# AUC25
area <- 0
i <- 2
while (area <= bro3AUC25) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 25% of the AUC is =", i))
i25 <- i

# AUC26
area <- 0
i <- 2
while (area <= bro3AUC26) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 26% of the AUC is =", i))
i26 <- i

# AUC27
area <- 0
i <- 2
while (area <= bro3AUC27) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 27% of the AUC is =", i))
i27 <- i

# AUC28
area <- 0
i <- 2
while (area <= bro3AUC28) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 28% of the AUC is =", i))
i28 <- i

# AUC29
area <- 0
i <- 2
while (area <= bro3AUC29) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 29% of the AUC is =", i))
i29 <- i

# AUC30
area <- 0
i <- 2
while (area <= bro3AUC30) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 30% of the AUC is =", i))
i30 <- i

# AUC31
area <- 0
i <- 2
while (area <= bro3AUC31) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 31% of the AUC is =", i))
i31 <- i

# AUC32
area <- 0
i <- 2
while (area <= bro3AUC32) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 32% of the AUC is =", i))
i32 <- i

# AUC33
area <- 0
i <- 2
while (area <= bro3AUC33) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 33% of the AUC is =", i))
i33 <- i

# AUC34
area <- 0
i <- 2
while (area <= bro3AUC34) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 34% of the AUC is =", i))
i34 <- i

# AUC35
area <- 0
i <- 2
while (area <= bro3AUC35) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 35% of the AUC is =", i))
i35 <- i

# AUC36
area <- 0
i <- 2
while (area <= bro3AUC36) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 36% of the AUC is =", i))
i36 <- i

# AUC37
area <- 0
i <- 2
while (area <= bro3AUC37) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 37% of the AUC is =", i))
i37 <- i

# AUC38
area <- 0
i <- 2
while (area <= bro3AUC38) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 38% of the AUC is =", i))
i38 <- i

# AUC39
area <- 0
i <- 2
while (area <= bro3AUC39) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 39% of the AUC is =", i))
i39 <- i

# AUC40
area <- 0
i <- 2
while (area <= bro3AUC40) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 40% of the AUC is =", i))
i40 <- i

# AUC41
area <- 0
i <- 2
while (area <= bro3AUC41) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 41% of the AUC is =", i))
i41 <- i

# AUC42
area <- 0
i <- 2
while (area <= bro3AUC42) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 42% of the AUC is =", i))
i42 <- i

# AUC43
area <- 0
i <- 2
while (area <= bro3AUC43) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 43% of the AUC is =", i))
i43 <- i

# AUC44
area <- 0
i <- 2
while (area <= bro3AUC44) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 44% of the AUC is =", i))
i44 <- i

# AUC45
area <- 0
i <- 2
while (area <= bro3AUC45) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 45% of the AUC is =", i))
i45 <- i

# AUC46
area <- 0
i <- 2
while (area <= bro3AUC46) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 46% of the AUC is =", i))
i46 <- i

# AUC47
area <- 0
i <- 2
while (area <= bro3AUC47) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 47% of the AUC is =", i))
i47 <- i

# AUC48
area <- 0
i <- 2
while (area <= bro3AUC48) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 48% of the AUC is =", i))
i48 <- i

# AUC49
area <- 0
i <- 2
while (area <= bro3AUC49) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 49% of the AUC is =", i))
i49 <- i

# AUC50
area <- 0
i <- 2
while (area <= bro3AUC50) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 50% of the AUC is =", i))
i50 <- i

# AUC51
area <- 0
i <- 2
while (area <= bro3AUC51) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 51% of the AUC is =", i))
i51 <- i

# AUC52
area <- 0
i <- 2
while (area <= bro3AUC52) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 52% of the AUC is =", i))
i52 <- i

# AUC53
area <- 0
i <- 2
while (area <= bro3AUC53) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 53% of the AUC is =", i))
i53 <- i

# AUC54
area <- 0
i <- 2
while (area <= bro3AUC54) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 54% of the AUC is =", i))
i54 <- i

# AUC55
area <- 0
i <- 2
while (area <= bro3AUC55) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 55% of the AUC is =", i))
i55 <- i

# AUC56
area <- 0
i <- 2
while (area <= bro3AUC56) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 56% of the AUC is =", i))
i56 <- i

# AUC57
area <- 0
i <- 2
while (area <= bro3AUC57) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 57% of the AUC is =", i))
i57 <- i

# AUC58
area <- 0
i <- 2
while (area <= bro3AUC58) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 58% of the AUC is =", i))
i58 <- i

# AUC59
area <- 0
i <- 2
while (area <= bro3AUC59) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 59% of the AUC is =", i))
i59 <- i

# AUC60
area <- 0
i <- 2
while (area <= bro3AUC60) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 60% of the AUC is =", i))
i60 <- i

# AUC61
area <- 0
i <- 2
while (area <= bro3AUC61) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 61% of the AUC is =", i))
i61 <- i

# AUC62
area <- 0
i <- 2
while (area <= bro3AUC62) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 62% of the AUC is =", i))
i62 <- i

# AUC63
area <- 0
i <- 2
while (area <= bro3AUC63) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 63% of the AUC is =", i))
i63 <- i

# AUC64
area <- 0
i <- 2
while (area <= bro3AUC64) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 64% of the AUC is =", i))
i64 <- i

# AUC65
area <- 0
i <- 2
while (area <= bro3AUC65) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 65% of the AUC is =", i))
i65 <- i

# AUC66
area <- 0
i <- 2
while (area <= bro3AUC66) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 66% of the AUC is =", i))
i66 <- i

# AUC67
area <- 0
i <- 2
while (area <= bro3AUC67) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 67% of the AUC is =", i))
i67 <- i

# AUC68
area <- 0
i <- 2
while (area <= bro3AUC68) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 68% of the AUC is =", i))
i68 <- i

# AUC69
area <- 0
i <- 2
while (area <= bro3AUC69) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 69% of the AUC is =", i))
i69 <- i

# AUC70
area <- 0
i <- 2
while (area <= bro3AUC70) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 70% of the AUC is =", i))
i70 <- i

# AUC71
area <- 0
i <- 2
while (area <= bro3AUC71) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 71% of the AUC is =", i))
i71 <- i

# AUC72
area <- 0
i <- 2
while (area <= bro3AUC72) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 72% of the AUC is =", i))
i72 <- i

# AUC73
area <- 0
i <- 2
while (area <= bro3AUC73) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 73% of the AUC is =", i))
i73 <- i

# AUC74
area <- 0
i <- 2
while (area <= bro3AUC74) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 74% of the AUC is =", i))
i74 <- i

# AUC75
area <- 0
i <- 2
while (area <= bro3AUC75) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 75% of the AUC is =", i))
i75 <- i

# AUC76
area <- 0
i <- 2
while (area <= bro3AUC76) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 76% of the AUC is =", i))
i76 <- i

# AUC77
area <- 0
i <- 2
while (area <= bro3AUC77) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 77% of the AUC is =", i))
i77 <- i

# AUC78
area <- 0
i <- 2
while (area <= bro3AUC78) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 78% of the AUC is =", i))
i78 <- i

# AUC79
area <- 0
i <- 2
while (area <= bro3AUC79) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 79% of the AUC is =", i))
i79 <- i

# AUC80
area <- 0
i <- 2
while (area <= bro3AUC80) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 80% of the AUC is =", i))
i80 <- i

# AUC81
area <- 0
i <- 2
while (area <= bro3AUC81) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 81% of the AUC is =", i))
i81 <- i

# AUC82
area <- 0
i <- 2
while (area <= bro3AUC82) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 82% of the AUC is =", i))
i82 <- i

# AUC83
area <- 0
i <- 2
while (area <= bro3AUC83) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 83% of the AUC is =", i))
i83 <- i

# AUC84
area <- 0
i <- 2
while (area <= bro3AUC84) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 84% of the AUC is =", i))
i84 <- i

# AUC85
area <- 0
i <- 2
while (area <= bro3AUC85) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 85% of the AUC is =", i))
i85 <- i

# AUC86
area <- 0
i <- 2
while (area <= bro3AUC86) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 86% of the AUC is =", i))
i86 <- i

# AUC87
area <- 0
i <- 2
while (area <= bro3AUC87) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 87% of the AUC is =", i))
i87 <- i

# AUC88
area <- 0
i <- 2
while (area <= bro3AUC88) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 88% of the AUC is =", i))
i88 <- i

# AUC89
area <- 0
i <- 2
while (area <= bro3AUC89) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 89% of the AUC is =", i))
i89 <- i

# AUC90
area <- 0
i <- 2
while (area <= bro3AUC90) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 90% of the AUC is =", i))
i90 <- i

# AUC91
area <- 0
i <- 2
while (area <= bro3AUC91) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 91% of the AUC is =", i))
i91 <- i

# AUC92
area <- 0
i <- 2
while (area <= bro3AUC92) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 92% of the AUC is =", i))
i92 <- i

# AUC93
area <- 0
i <- 2
while (area <= bro3AUC93) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 93% of the AUC is =", i))
i93 <- i

# AUC94
area <- 0
i <- 2
while (area <= bro3AUC94) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 94% of the AUC is =", i))
i94 <- i

# AUC95
area <- 0
i <- 2
while (area <= bro3AUC95) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 95% of the AUC is =", i))
i95 <- i

# AUC96
area <- 0
i <- 2
while (area <= bro3AUC96) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 96% of the AUC is =", i))
i96 <- i

# AUC97
area <- 0
i <- 2
while (area <= bro3AUC97) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 97% of the AUC is =", i))
i97 <- i

# AUC98
area <- 0
i <- 2
while (area <= bro3AUC98) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 98% of the AUC is =", i))
i98 <- i

# AUC99
area <- 0
i <- 2
while (area <= bro3AUC99) {
  area <- pk.calc.auc(brome_ei.y, brome_ei.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 99% of the AUC is =", i))
i99 <- i

i100 <- length(brome_ei.y)

# Make a table of the AUC x-vals for quick reference
brome.ei.auc <- data.frame(auc = c(paste("auc0",1:9,sep=""),
                                   paste("auc",10:100,sep="")),
                           otu.num = c(i01,i02,i03,i04,i05,i06,i07,i08,i09,
                                       i10,i11,i12,i13,i14,i15,i16,i17,i18,i19,
                                       i20,i21,i22,i23,i24,i25,i26,i27,i28,i29,
                                       i30,i31,i32,i33,i34,i35,i36,i37,i38,i39,
                                       i40,i41,i42,i43,i44,i45,i46,i47,i48,i49,
                                       i50,i51,i52,i53,i54,i55,i56,i57,i58,i59,
                                       i60,i61,i62,i63,i64,i65,i66,i67,i68,i69,
                                       i70,i71,i72,i73,i74,i75,i76,i77,i78,i79,
                                       i80,i81,i82,i83,i84,i85,i86,i87,i88,i89,
                                       i90,i91,i92,i93,i94,i95,i96,i97,i98,i99,i100))
write.csv(brome.ei.auc, "~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/brome.ei.auc.csv")
write.csv(as.data.frame(cbind(x = brome_ei.x, y = brome_ei.y)), "~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/brome.add1.eigenvector.csv")











# Betweenness selectbyall, MIC thresold of 0.2, minimum = 3 OTUs
brome_bw <- read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/ADD1_AUC100_MIN3_MIC0.2_Brome_bacfunarc_dw_otu_table-graph_centrality-betweenness-selectallbyall.csv")
brome_bw <- brome_bw[ order(-brome_bw$metric), ]
brome_bw.x <- as.numeric(rownames(brome_bw))  ## pulling in index after ordering
brome_bw.y <- brome_bw$metric  ## metric of interest

## Area under the curve

## Brome A horizon
# Degree
bro3AUC100 <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, 2996)) 
bro3AUC01 <- 0.01*bro3AUC100; bro3AUC02 <- 0.02*bro3AUC100; bro3AUC03 <- 0.03*bro3AUC100; bro3AUC04 <- 0.04*bro3AUC100; bro3AUC05 <- 0.05*bro3AUC100
bro3AUC06 <- 0.06*bro3AUC100; bro3AUC07 <- 0.07*bro3AUC100; bro3AUC08 <- 0.08*bro3AUC100; bro3AUC09 <- 0.09*bro3AUC100; bro3AUC10 <- 0.10*bro3AUC100
bro3AUC11 <- 0.11*bro3AUC100; bro3AUC12 <- 0.12*bro3AUC100; bro3AUC13 <- 0.13*bro3AUC100; bro3AUC14 <- 0.14*bro3AUC100; bro3AUC15 <- 0.15*bro3AUC100
bro3AUC16 <- 0.16*bro3AUC100; bro3AUC17 <- 0.17*bro3AUC100; bro3AUC18 <- 0.18*bro3AUC100; bro3AUC19 <- 0.19*bro3AUC100; bro3AUC20 <- 0.20*bro3AUC100
bro3AUC21 <- 0.21*bro3AUC100; bro3AUC22 <- 0.22*bro3AUC100; bro3AUC23 <- 0.23*bro3AUC100; bro3AUC24 <- 0.24*bro3AUC100; bro3AUC25 <- 0.25*bro3AUC100
bro3AUC26 <- 0.26*bro3AUC100; bro3AUC27 <- 0.27*bro3AUC100; bro3AUC28 <- 0.28*bro3AUC100; bro3AUC29 <- 0.29*bro3AUC100; bro3AUC30 <- 0.30*bro3AUC100
bro3AUC31 <- 0.31*bro3AUC100; bro3AUC32 <- 0.32*bro3AUC100; bro3AUC33 <- 0.33*bro3AUC100; bro3AUC34 <- 0.34*bro3AUC100; bro3AUC35 <- 0.35*bro3AUC100
bro3AUC36 <- 0.36*bro3AUC100; bro3AUC37 <- 0.37*bro3AUC100; bro3AUC38 <- 0.38*bro3AUC100; bro3AUC39 <- 0.39*bro3AUC100; bro3AUC40 <- 0.40*bro3AUC100
bro3AUC41 <- 0.41*bro3AUC100; bro3AUC42 <- 0.42*bro3AUC100; bro3AUC43 <- 0.43*bro3AUC100; bro3AUC44 <- 0.44*bro3AUC100; bro3AUC45 <- 0.45*bro3AUC100
bro3AUC46 <- 0.46*bro3AUC100; bro3AUC47 <- 0.47*bro3AUC100; bro3AUC48 <- 0.48*bro3AUC100; bro3AUC49 <- 0.49*bro3AUC100; bro3AUC50 <- 0.50*bro3AUC100
bro3AUC51 <- 0.51*bro3AUC100; bro3AUC52 <- 0.52*bro3AUC100; bro3AUC53 <- 0.53*bro3AUC100; bro3AUC54 <- 0.54*bro3AUC100; bro3AUC55 <- 0.55*bro3AUC100
bro3AUC56 <- 0.56*bro3AUC100; bro3AUC57 <- 0.57*bro3AUC100; bro3AUC58 <- 0.58*bro3AUC100; bro3AUC59 <- 0.59*bro3AUC100; bro3AUC60 <- 0.60*bro3AUC100
bro3AUC61 <- 0.61*bro3AUC100; bro3AUC62 <- 0.62*bro3AUC100; bro3AUC63 <- 0.63*bro3AUC100; bro3AUC64 <- 0.64*bro3AUC100; bro3AUC65 <- 0.65*bro3AUC100
bro3AUC66 <- 0.66*bro3AUC100; bro3AUC67 <- 0.67*bro3AUC100; bro3AUC68 <- 0.68*bro3AUC100; bro3AUC69 <- 0.69*bro3AUC100; bro3AUC70 <- 0.70*bro3AUC100
bro3AUC71 <- 0.71*bro3AUC100; bro3AUC72 <- 0.72*bro3AUC100; bro3AUC73 <- 0.73*bro3AUC100; bro3AUC74 <- 0.74*bro3AUC100; bro3AUC75 <- 0.75*bro3AUC100
bro3AUC76 <- 0.76*bro3AUC100; bro3AUC77 <- 0.77*bro3AUC100; bro3AUC78 <- 0.78*bro3AUC100; bro3AUC79 <- 0.79*bro3AUC100; bro3AUC80 <- 0.80*bro3AUC100
bro3AUC81 <- 0.81*bro3AUC100; bro3AUC82 <- 0.82*bro3AUC100; bro3AUC83 <- 0.83*bro3AUC100; bro3AUC84 <- 0.84*bro3AUC100; bro3AUC85 <- 0.85*bro3AUC100
bro3AUC86 <- 0.86*bro3AUC100; bro3AUC87 <- 0.87*bro3AUC100; bro3AUC88 <- 0.88*bro3AUC100; bro3AUC89 <- 0.89*bro3AUC100; bro3AUC90 <- 0.90*bro3AUC100
bro3AUC91 <- 0.91*bro3AUC100; bro3AUC92 <- 0.92*bro3AUC100; bro3AUC93 <- 0.93*bro3AUC100; bro3AUC94 <- 0.94*bro3AUC100; bro3AUC95 <- 0.95*bro3AUC100
bro3AUC96 <- 0.96*bro3AUC100; bro3AUC97 <- 0.97*bro3AUC100; bro3AUC98 <- 0.98*bro3AUC100; bro3AUC99 <- 0.99*bro3AUC100

# AUC01
area <- 0
i <- 2
while (area <= bro3AUC01) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 1% of the AUC is =", i))
i01 <- i

# AUC02
area <- 0
i <- 2
while (area <= bro3AUC02) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 2% of the AUC is =", i))
i02 <- i

# AUC03
area <- 0
i <- 2
while (area <= bro3AUC03) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 3% of the AUC is =", i))
i03 <- i

# AUC04
area <- 0
i <- 2
while (area <= bro3AUC04) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 4% of the AUC is =", i))
i04 <- i

# AUC05
area <- 0
i <- 2
while (area <= bro3AUC05) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 5% of the AUC is =", i))
i05 <- i

# AUC06
area <- 0
i <- 2
while (area <= bro3AUC06) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 6% of the AUC is =", i))
i06 <- i

# AUC07
area <- 0
i <- 2
while (area <= bro3AUC07) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 7% of the AUC is =", i))
i07 <- i

# AUC08
area <- 0
i <- 2
while (area <= bro3AUC08) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 8% of the AUC is =", i))
i08 <- i

# AUC09
area <- 0
i <- 2
while (area <= bro3AUC09) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 9% of the AUC is =", i))
i09 <- i

# AUC10
area <- 0
i <- 2
while (area <= bro3AUC10) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 10% of the AUC is =", i))
i10 <- i

# AUC11
area <- 0
i <- 2
while (area <= bro3AUC11) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 11% of the AUC is =", i))
i11 <- i

# AUC12
area <- 0
i <- 2
while (area <= bro3AUC12) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 12% of the AUC is =", i))
i12 <- i

# AUC13
area <- 0
i <- 2
while (area <= bro3AUC13) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 13% of the AUC is =", i))
i13 <- i

# AUC14
area <- 0
i <- 2
while (area <= bro3AUC14) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 14% of the AUC is =", i))
i14 <- i

# AUC15
area <- 0
i <- 2
while (area <= bro3AUC15) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 15% of the AUC is =", i))
i15 <- i

# AUC16
area <- 0
i <- 2
while (area <= bro3AUC16) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 16% of the AUC is =", i))
i16 <- i

# AUC17
area <- 0
i <- 2
while (area <= bro3AUC17) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 17% of the AUC is =", i))
i17 <- i

# AUC18
area <- 0
i <- 2
while (area <= bro3AUC18) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 18% of the AUC is =", i))
i18 <- i

# AUC19
area <- 0
i <- 2
while (area <= bro3AUC19) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 19% of the AUC is =", i))
i19 <- i

# AUC20
area <- 0
i <- 2
while (area <= bro3AUC20) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 20% of the AUC is =", i))
i20 <- i

# AUC21
area <- 0
i <- 2
while (area <= bro3AUC21) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 21% of the AUC is =", i))
i21 <- i

# AUC22
area <- 0
i <- 2
while (area <= bro3AUC22) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 22% of the AUC is =", i))
i22 <- i

# AUC23
area <- 0
i <- 2
while (area <= bro3AUC23) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 23% of the AUC is =", i))
i23 <- i

# AUC24
area <- 0
i <- 2
while (area <= bro3AUC24) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 24% of the AUC is =", i))
i24 <- i

# AUC25
area <- 0
i <- 2
while (area <= bro3AUC25) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 25% of the AUC is =", i))
i25 <- i

# AUC26
area <- 0
i <- 2
while (area <= bro3AUC26) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 26% of the AUC is =", i))
i26 <- i

# AUC27
area <- 0
i <- 2
while (area <= bro3AUC27) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 27% of the AUC is =", i))
i27 <- i

# AUC28
area <- 0
i <- 2
while (area <= bro3AUC28) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 28% of the AUC is =", i))
i28 <- i

# AUC29
area <- 0
i <- 2
while (area <= bro3AUC29) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 29% of the AUC is =", i))
i29 <- i

# AUC30
area <- 0
i <- 2
while (area <= bro3AUC30) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 30% of the AUC is =", i))
i30 <- i

# AUC31
area <- 0
i <- 2
while (area <= bro3AUC31) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 31% of the AUC is =", i))
i31 <- i

# AUC32
area <- 0
i <- 2
while (area <= bro3AUC32) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 32% of the AUC is =", i))
i32 <- i

# AUC33
area <- 0
i <- 2
while (area <= bro3AUC33) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 33% of the AUC is =", i))
i33 <- i

# AUC34
area <- 0
i <- 2
while (area <= bro3AUC34) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 34% of the AUC is =", i))
i34 <- i

# AUC35
area <- 0
i <- 2
while (area <= bro3AUC35) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 35% of the AUC is =", i))
i35 <- i

# AUC36
area <- 0
i <- 2
while (area <= bro3AUC36) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 36% of the AUC is =", i))
i36 <- i

# AUC37
area <- 0
i <- 2
while (area <= bro3AUC37) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 37% of the AUC is =", i))
i37 <- i

# AUC38
area <- 0
i <- 2
while (area <= bro3AUC38) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 38% of the AUC is =", i))
i38 <- i

# AUC39
area <- 0
i <- 2
while (area <= bro3AUC39) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 39% of the AUC is =", i))
i39 <- i

# AUC40
area <- 0
i <- 2
while (area <= bro3AUC40) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 40% of the AUC is =", i))
i40 <- i

# AUC41
area <- 0
i <- 2
while (area <= bro3AUC41) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 41% of the AUC is =", i))
i41 <- i

# AUC42
area <- 0
i <- 2
while (area <= bro3AUC42) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 42% of the AUC is =", i))
i42 <- i

# AUC43
area <- 0
i <- 2
while (area <= bro3AUC43) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 43% of the AUC is =", i))
i43 <- i

# AUC44
area <- 0
i <- 2
while (area <= bro3AUC44) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 44% of the AUC is =", i))
i44 <- i

# AUC45
area <- 0
i <- 2
while (area <= bro3AUC45) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 45% of the AUC is =", i))
i45 <- i

# AUC46
area <- 0
i <- 2
while (area <= bro3AUC46) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 46% of the AUC is =", i))
i46 <- i

# AUC47
area <- 0
i <- 2
while (area <= bro3AUC47) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 47% of the AUC is =", i))
i47 <- i

# AUC48
area <- 0
i <- 2
while (area <= bro3AUC48) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 48% of the AUC is =", i))
i48 <- i

# AUC49
area <- 0
i <- 2
while (area <= bro3AUC49) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 49% of the AUC is =", i))
i49 <- i

# AUC50
area <- 0
i <- 2
while (area <= bro3AUC50) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 50% of the AUC is =", i))
i50 <- i

# AUC51
area <- 0
i <- 2
while (area <= bro3AUC51) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 51% of the AUC is =", i))
i51 <- i

# AUC52
area <- 0
i <- 2
while (area <= bro3AUC52) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 52% of the AUC is =", i))
i52 <- i

# AUC53
area <- 0
i <- 2
while (area <= bro3AUC53) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 53% of the AUC is =", i))
i53 <- i

# AUC54
area <- 0
i <- 2
while (area <= bro3AUC54) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 54% of the AUC is =", i))
i54 <- i

# AUC55
area <- 0
i <- 2
while (area <= bro3AUC55) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 55% of the AUC is =", i))
i55 <- i

# AUC56
area <- 0
i <- 2
while (area <= bro3AUC56) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 56% of the AUC is =", i))
i56 <- i

# AUC57
area <- 0
i <- 2
while (area <= bro3AUC57) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 57% of the AUC is =", i))
i57 <- i

# AUC58
area <- 0
i <- 2
while (area <= bro3AUC58) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 58% of the AUC is =", i))
i58 <- i

# AUC59
area <- 0
i <- 2
while (area <= bro3AUC59) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 59% of the AUC is =", i))
i59 <- i

# AUC60
area <- 0
i <- 2
while (area <= bro3AUC60) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 60% of the AUC is =", i))
i60 <- i

# AUC61
area <- 0
i <- 2
while (area <= bro3AUC61) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 61% of the AUC is =", i))
i61 <- i

# AUC62
area <- 0
i <- 2
while (area <= bro3AUC62) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 62% of the AUC is =", i))
i62 <- i

# AUC63
area <- 0
i <- 2
while (area <= bro3AUC63) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 63% of the AUC is =", i))
i63 <- i

# AUC64
area <- 0
i <- 2
while (area <= bro3AUC64) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 64% of the AUC is =", i))
i64 <- i

# AUC65
area <- 0
i <- 2
while (area <= bro3AUC65) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 65% of the AUC is =", i))
i65 <- i

# AUC66
area <- 0
i <- 2
while (area <= bro3AUC66) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 66% of the AUC is =", i))
i66 <- i

# AUC67
area <- 0
i <- 2
while (area <= bro3AUC67) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 67% of the AUC is =", i))
i67 <- i

# AUC68
area <- 0
i <- 2
while (area <= bro3AUC68) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 68% of the AUC is =", i))
i68 <- i

# AUC69
area <- 0
i <- 2
while (area <= bro3AUC69) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 69% of the AUC is =", i))
i69 <- i

# AUC70
area <- 0
i <- 2
while (area <= bro3AUC70) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 70% of the AUC is =", i))
i70 <- i

# AUC71
area <- 0
i <- 2
while (area <= bro3AUC71) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 71% of the AUC is =", i))
i71 <- i

# AUC72
area <- 0
i <- 2
while (area <= bro3AUC72) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 72% of the AUC is =", i))
i72 <- i

# AUC73
area <- 0
i <- 2
while (area <= bro3AUC73) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 73% of the AUC is =", i))
i73 <- i

# AUC74
area <- 0
i <- 2
while (area <= bro3AUC74) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 74% of the AUC is =", i))
i74 <- i

# AUC75
area <- 0
i <- 2
while (area <= bro3AUC75) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 75% of the AUC is =", i))
i75 <- i

# AUC76
area <- 0
i <- 2
while (area <= bro3AUC76) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 76% of the AUC is =", i))
i76 <- i

# AUC77
area <- 0
i <- 2
while (area <= bro3AUC77) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 77% of the AUC is =", i))
i77 <- i

# AUC78
area <- 0
i <- 2
while (area <= bro3AUC78) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 78% of the AUC is =", i))
i78 <- i

# AUC79
area <- 0
i <- 2
while (area <= bro3AUC79) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 79% of the AUC is =", i))
i79 <- i

# AUC80
area <- 0
i <- 2
while (area <= bro3AUC80) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 80% of the AUC is =", i))
i80 <- i

# AUC81
area <- 0
i <- 2
while (area <= bro3AUC81) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 81% of the AUC is =", i))
i81 <- i

# AUC82
area <- 0
i <- 2
while (area <= bro3AUC82) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 82% of the AUC is =", i))
i82 <- i

# AUC83
area <- 0
i <- 2
while (area <= bro3AUC83) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 83% of the AUC is =", i))
i83 <- i

# AUC84
area <- 0
i <- 2
while (area <= bro3AUC84) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 84% of the AUC is =", i))
i84 <- i

# AUC85
area <- 0
i <- 2
while (area <= bro3AUC85) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 85% of the AUC is =", i))
i85 <- i

# AUC86
area <- 0
i <- 2
while (area <= bro3AUC86) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 86% of the AUC is =", i))
i86 <- i

# AUC87
area <- 0
i <- 2
while (area <= bro3AUC87) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 87% of the AUC is =", i))
i87 <- i

# AUC88
area <- 0
i <- 2
while (area <= bro3AUC88) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 88% of the AUC is =", i))
i88 <- i

# AUC89
area <- 0
i <- 2
while (area <= bro3AUC89) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 89% of the AUC is =", i))
i89 <- i

# AUC90
area <- 0
i <- 2
while (area <= bro3AUC90) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 90% of the AUC is =", i))
i90 <- i

# AUC91
area <- 0
i <- 2
while (area <= bro3AUC91) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 91% of the AUC is =", i))
i91 <- i

# AUC92
area <- 0
i <- 2
while (area <= bro3AUC92) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 92% of the AUC is =", i))
i92 <- i

# AUC93
area <- 0
i <- 2
while (area <= bro3AUC93) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 93% of the AUC is =", i))
i93 <- i

# AUC94
area <- 0
i <- 2
while (area <= bro3AUC94) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 94% of the AUC is =", i))
i94 <- i

# AUC95
area <- 0
i <- 2
while (area <= bro3AUC95) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 95% of the AUC is =", i))
i95 <- i

# AUC96
area <- 0
i <- 2
while (area <= bro3AUC96) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 96% of the AUC is =", i))
i96 <- i

# AUC97
area <- 0
i <- 2
while (area <= bro3AUC97) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 97% of the AUC is =", i))
i97 <- i

# AUC98
area <- 0
i <- 2
while (area <= bro3AUC98) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 98% of the AUC is =", i))
i98 <- i

# AUC99
area <- 0
i <- 2
while (area <= bro3AUC99) {
  area <- pk.calc.auc(brome_bw.y, brome_bw.x, interval=c(1, i))
  i = i + 1
}
print(paste("The point at which we reach 99% of the AUC is =", i))
i99 <- i

i100 <- length(brome_bw.y)

# Make a table of the AUC x-vals for quick reference
brome.bw.auc <- data.frame(auc = c(paste("auc0",1:9,sep=""),
                                   paste("auc",10:100,sep="")),
                           otu.num = c(i01,i02,i03,i04,i05,i06,i07,i08,i09,
                                       i10,i11,i12,i13,i14,i15,i16,i17,i18,i19,
                                       i20,i21,i22,i23,i24,i25,i26,i27,i28,i29,
                                       i30,i31,i32,i33,i34,i35,i36,i37,i38,i39,
                                       i40,i41,i42,i43,i44,i45,i46,i47,i48,i49,
                                       i50,i51,i52,i53,i54,i55,i56,i57,i58,i59,
                                       i60,i61,i62,i63,i64,i65,i66,i67,i68,i69,
                                       i70,i71,i72,i73,i74,i75,i76,i77,i78,i79,
                                       i80,i81,i82,i83,i84,i85,i86,i87,i88,i89,
                                       i90,i91,i92,i93,i94,i95,i96,i97,i98,i99,i100))
write.csv(brome.bw.auc, "~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/brome.bw.auc.csv")
write.csv(as.data.frame(cbind(x = brome_bw.x, y = brome_bw.y)), "~/Dropbox/CFREF Work/Winnowing/SteveM/Documents/Manuscript/Publication Code/brome.add1.betweenness.csv")
