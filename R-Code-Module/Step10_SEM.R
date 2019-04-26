library(lavaan) # Latent variable analysis for SEM
library(psych) # Correlations and scatterplots
library(vegan) # Scaling and standardizing data

# Clear the workspace
rm(list = ls())

# Read in the data
sem.data <- read.csv("~/Dropbox/CFREF Work/Winnowing/SteveM/BROME/Brome.SEM.data.csv", header = T)

# Transforming the inputs
AE <- sem.data$Archaea.Evenness*10
AR <- sem.data$Archaea.Richness/10
BE <- sem.data$Bacterial.Evenness*10
BR <- sem.data$Bacterial.Richness/100
FE <- sem.data$Fungal.Evenness*10
FR <- sem.data$Fungal.Richness/100
PR <- sem.data$Plant.R/10
Root.Biomass <- log(sem.data$Root_gm2)
Litter.Biomass <- sem.data$Litter_gm2/100
Litter.CN.ratio <- sem.data$LitterCN_ratio/10
BromeShoots <- sem.data$Brome_gm2/100
BromeRoots <- log(sem.data$Bromus_inermis+1)
Degree.Sum <- (sem.data$Degree.Sum - min(sem.data$Degree.Sum))*10
Eigenvalue.Sum <- (sem.data$Eigenvalue.Sum - min(sem.data$Eigenvalue.Sum))*10
Closeness.Sum <- (sem.data$Closeness.Sum - min(sem.data$Closeness.Sum))*10
Betweenness.Sum <- (sem.data$Betweenness.Sum - min(sem.data$Betweenness.Sum))*10

# Compile the transformed inputs
sem.data2 <- as.data.frame(cbind(as.factor(as.character(sem.data$Horizon)), sem.data$A.horizon, sem.data$Org_C, sem.data$Soil_TotN, PR, AE, AR, BE, BR, FE, FR, 
                                 Root.Biomass, Litter.Biomass, Litter.CN.ratio, BromeShoots, BromeRoots, Degree.Sum, Eigenvalue.Sum, Closeness.Sum, Betweenness.Sum, sem.data$pH))
sem.data2$V1 <- sem.data$Horizon
names(sem.data2) <- c("Horizon", "A.Horizon", "SOC", "Soil.N", names(sem.data2[,c(5:20)]), "pH")

# Degree.Sum <- 0 + (sem.data$Degree.Sum-min(sem.data$Degree.Sum))*(1-0)/(max(sem.data$Degree.Sum)-min(sem.data$Degree.Sum))

# Check for non-linearities in the data
summary(glm(Degree.Sum ~ BromeRoots + I(BromeRoots^2), data = sem.data2, family = gaussian))     # Linear
summary(glm(Degree.Sum ~ BromeShoots + I(BromeShoots^2), data = sem.data2, family = gaussian))   # Linear
summary(glm(BE ~ Degree.Sum + I(Degree.Sum^2), data = sem.data2, family = gaussian))             # Linear
summary(glm(BE ~ Eigenvalue.Sum + I(Eigenvalue.Sum^2), data = sem.data2, family = gaussian))     # Linear
summary(glm(BE ~ Closeness.Sum + I(Closeness.Sum^2), data = sem.data2, family = gaussian))       # Linear
summary(glm(BE ~ Betweenness.Sum + I(Betweenness.Sum^2), data = sem.data2, family = gaussian))   # Linear
summary(glm(FR ~ AR + I(AR^2), data = sem.data2, family = gaussian))                             # Linear
summary(glm(FR ~ BromeShoots + I(BromeShoots^2), data = sem.data2, family = gaussian))           # Linear
summary(glm(FR ~ BromeRoots + I(BromeRoots^2), data = sem.data2, family = gaussian))             # Linear
summary(glm(AR ~ BromeShoots + I(BromeShoots^2), data = sem.data2, family = gaussian))           # Linear
summary(glm(AR ~ BromeRoots + I(BromeRoots^2), data = sem.data2, family = gaussian))             # Linear
summary(glm(FE ~ AE + I(AE^2), data = sem.data2, family = gaussian))                             # Linear
summary(glm(FE ~ BromeShoots + I(BromeShoots^2), data = sem.data2, family = gaussian))           # Linear
summary(glm(FE ~ BromeRoots + I(BromeRoots^2), data = sem.data2, family = gaussian))             # Linear
summary(glm(AE ~ BromeShoots + I(BromeShoots^2), data = sem.data2, family = gaussian))           # Linear
summary(glm(AE ~ BromeRoots + I(BromeRoots^2), data = sem.data2, family = gaussian))             # Linear
summary(glm(AR ~ pH + I(pH^2), data = sem.data2, family = gaussian))                             # Linear
summary(glm(FR ~ pH + I(pH^2), data = sem.data2, family = gaussian))                             # Linear

# Look for significant relationships
summary(lm(Degree.Sum ~ BromeRoots, data = sem.data2))
summary(lm(Degree.Sum ~ BromeShoots, data = sem.data2))
summary(lm(BE ~ Degree.Sum, data = sem.data2)) # P = 0.000855
summary(lm(BE ~ Eigenvalue.Sum, data = sem.data2)) # P = 0.000856
summary(lm(BE ~ Closeness.Sum, data = sem.data2)) # P = 7e-04
summary(lm(BE ~ Betweenness.Sum, data = sem.data2)) # P = 0.000857
summary(lm(FR ~ AR, data = sem.data2))
summary(lm(FR ~ BromeShoots, data = sem.data2))
summary(lm(FR ~ BromeRoots, data = sem.data2))
summary(lm(AR ~ BromeShoots, data = sem.data2)) # P = 0.0172
summary(lm(AR ~ BromeRoots, data = sem.data2))
summary(lm(FE ~ AE, data = sem.data2))
summary(lm(FE ~ BromeShoots, data = sem.data2))
summary(lm(FE ~ BromeRoots, data = sem.data2))
summary(lm(AE ~ BromeShoots, data = sem.data2))
summary(lm(AE ~ BromeRoots, data = sem.data2))
summary(lm(AR ~ pH, data = sem.data2))
summary(lm(FR ~ pH, data = sem.data2))

# Plot the various comparisons and add trend lines to significant relationships
plot(Degree.Sum ~ BromeRoots, data = sem.data2)
plot(Degree.Sum ~ BromeShoots, data = sem.data2)
plot(BE ~ Degree.Sum, data = sem.data2)
abline(lm(BE ~ Degree.Sum, data = sem.data2))
plot(BE ~ Eigenvalue.Sum, data = sem.data2)
abline(lm(BE ~ Eigenvalue.Sum, data = sem.data2))
plot(BE ~ Closeness.Sum, data = sem.data2)
abline(lm(BE ~ Closeness.Sum, data = sem.data2))
plot(BE ~ Betweenness.Sum, data = sem.data2)
abline(lm(BE ~ Betweenness.Sum, data = sem.data2))
plot(FR ~ AR, data = sem.data2)
plot(FR ~ BromeShoots, data = sem.data2)
plot(FR ~ BromeRoots, data = sem.data2)
plot(AR ~ BromeShoots, data = sem.data2)
abline(lm(AR ~ BromeShoots, data = sem.data2))
plot(AR ~ BromeRoots, data = sem.data2)
plot(FE ~ AE, data = sem.data2)
plot(FE ~ BromeShoots, data = sem.data2)
plot(FE ~ BromeRoots, data = sem.data2)
plot(AE ~ BromeShoots, data = sem.data2)
plot(AE ~ BromeRoots, data = sem.data2)
plot(AR ~ pH, data = sem.data2)
plot(FR ~ pH, data = sem.data2)

# Look at the distributions and relationships ('psych' package)
pairs.panels(sem.data2[c("BE","BR","Degree.Sum","Closeness.Sum","Eigenvalue.Sum","Betweenness.Sum")], lm = TRUE, stars = TRUE)

sem.data2A <- subset(sem.data2, Horizon == "A")
sem.data2B <- subset(sem.data2, Horizon == "B")

###########################
###########################
## Evenness

## Notes: TLI and CFI are relative indexes of model fit--they compare the fit of your model to the 
## fit of your (worst fitting) null model

####################################################################################
####################################################################################
####################################################################################

### Running Candace's SEMs, tweaked to include fungi, archaea, and degree - running horizons separately on degree centrality

####### Evenness (Table S2)

#****************************************
# Model A1: Microbiome with BE (null) - A horizon 

sem.1a <- '
# regressions
Degree.Sum ~ BE
BE ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.1a <- sem(sem.1a, data = sem.data2A, missing = 'fiml')
varTable(fit.1a)
summary(fit.1a, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) 
fitMeasures(fit.1a, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic    chisq       df   pvalue      cfi    rmsea     srmr 
# 720.093  24.925  32.000   0.809   1.000   0.000   0.068 
residuals(fit.1a,type="raw")

#****************************************
# Model B1: Microbiome with BE - B horizon

sem.1b <- '
# regressions
Degree.Sum ~ BE
BE ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.1b <- sem(sem.1b, data = sem.data2B, missing = 'fiml')
varTable(fit.1b)
summary(fit.1b, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) 
fitMeasures(fit.1b, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic    chisq       df   pvalue      cfi    rmsea     srmr 
# 601.135  50.576  32.000   0.020   0.914   0.106   0.104 
residuals(fit.1b,type="raw")


#****************************************
# Model A2: Plant with BE - A horizon
# Degree.Sum ~ BE + BromeShoots + PR + Litter.Biomass + Litter.CN.ratio + Root.Biomass

sem.2a <- '
# regressions
Degree.Sum ~ BE + BromeShoots + Root.Biomass
BE ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.2a <- sem(sem.2a, data = sem.data2A, missing = 'fiml')
varTable(fit.2a)
summary(fit.2a, fit.measures = TRUE, standardized = TRUE, rsq = TRUE)
fitMeasures(fit.2a, c("aic","chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic   chisq      df  pvalue     cfi   rmsea    srmr 
# 718.604  19.436  30.000   0.930   1.000   0.000   0.060 
residuals(fit.2a,type="raw")

#****************************************
# Model B2: Plant with BE - B horizon

sem.2b <- '
# regressions
Degree.Sum ~ BE + BromeShoots + Root.Biomass
BE ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.2b <- sem(sem.2b, data = sem.data2B, missing = 'fiml')
varTable(fit.2b)
summary(fit.2b, fit.measures = TRUE, standardized = TRUE, rsq = TRUE)
fitMeasures(fit.2b, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic   chisq      df  pvalue     cfi   rmsea    srmr 
# 603.627  49.068  30.000   0.015   0.912   0.111   0.102 
residuals(fit.2b,type="raw")

#****************************************
# Model A3: Soil with BE - A horizon

sem.3a <- '
# regressions
Degree.Sum ~ BE + SOC + pH
BE ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.3a <- sem(sem.3a, data = sem.data2A, missing = 'fiml')
varTable(fit.3a)
summary(fit.3a, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) 
fitMeasures(fit.3a, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic   chisq      df  pvalue     cfi   rmsea    srmr 
# 723.629  24.461  30.000   0.751   1.000   0.000   0.066 
residuals(fit.3a,type="raw")

#****************************************
# Model B3: Soil with BE - B horizon

sem.3b <- '
# regressions
Degree.Sum ~ BE + SOC + pH
BE ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.3b <- sem(sem.3b, data = sem.data2B, missing = 'fiml')
varTable(fit.3b)
summary(fit.3b, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) 
fitMeasures(fit.3b, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic   chisq      df  pvalue     cfi   rmsea    srmr 
# 603.364  48.806  30.000   0.016   0.913   0.110   0.102 
residuals(fit.3b,type="raw")

#****************************************
# Model A4: Plant-soil with BE - A horizon

sem.4a <- '
# regressions
Degree.Sum ~ BE + BromeShoots + Root.Biomass + SOC + pH
BE ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.4a <- sem(sem.4a, data = sem.data2A, missing = 'fiml')
varTable(fit.4a)
summary(fit.4a, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) 
fitMeasures(fit.4a, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic   chisq      df  pvalue     cfi   rmsea    srmr 
# 722.453  19.285  28.000   0.889   1.000   0.000   0.060 
residuals(fit.4a,type="raw")

#****************************************
# Model B4: Plant-soil with BE - B horizon

sem.4b <- '
# regressions
Degree.Sum ~ BE + BromeShoots + Root.Biomass + SOC + pH
BE ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.4b <- sem(sem.4b, data = sem.data2B, missing = 'fiml')
varTable(fit.4b)
summary(fit.4b, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) 
fitMeasures(fit.4b, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic   chisq      df  pvalue     cfi   rmsea    srmr 
# 606.112  47.553  28.000   0.012   0.910   0.116   0.100 
residuals(fit.4b,type="raw")





####### Richness (Table S3)

#****************************************
# Model A1: Microbiome with BR (null) - A horizon

sem.1a <- '
# regressions
Degree.Sum ~ BR
BR ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.1a <- sem(sem.1a, data = sem.data2A, missing = 'fiml')
varTable(fit.1a)
summary(fit.1a, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) 
fitMeasures(fit.1a, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic    chisq       df   pvalue      cfi    rmsea     srmr 
# 820.311  23.539  32.000   0.861   1.000   0.000   0.067 
residuals(fit.1a,type="raw")

#****************************************
# Model B1: Microbiome with BR - B horizon

sem.1b <- '
# regressions
Degree.Sum ~ BR
BR ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.1b <- sem(sem.1b, data = sem.data2B, missing = 'fiml')
varTable(fit.1b)
summary(fit.1b, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) 
fitMeasures(fit.1b, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic    chisq       df   pvalue      cfi    rmsea     srmr 
# 684.773  51.528  32.000   0.016   0.910   0.108   0.107
residuals(fit.1b,type="raw")


#****************************************
# Model A2: Plant with BR - A horizon
# Degree.Sum ~ BR + BromeShoots + PR + Litter.Biomass + Litter.CN.ratio + Root.Biomass

sem.2a <- '
# regressions
Degree.Sum ~ BR + BromeShoots + Root.Biomass
BR ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.2a <- sem(sem.2a, data = sem.data2A, missing = 'fiml')
varTable(fit.2a)
summary(fit.2a, fit.measures = TRUE, standardized = TRUE, rsq = TRUE)
fitMeasures(fit.2a, c("aic","chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic   chisq      df  pvalue     cfi   rmsea    srmr 
# 818.552  17.780  30.000   0.962   1.000   0.000   0.058
residuals(fit.2a,type="raw")

#****************************************
# Model B2: Plant with BR - B horizon

sem.2b <- '
# regressions
Degree.Sum ~ BR + BromeShoots + Root.Biomass
BR ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.2b <- sem(sem.2b, data = sem.data2B, missing = 'fiml')
varTable(fit.2b)
summary(fit.2b, fit.measures = TRUE, standardized = TRUE, rsq = TRUE)
fitMeasures(fit.2b, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic   chisq      df  pvalue     cfi   rmsea    srmr 
# 686.811  49.565  30.000   0.014   0.910   0.112   0.103 
residuals(fit.2b,type="raw")

#****************************************
# Model A3: Soil with BR - A horizon

sem.3a <- '
# regressions
Degree.Sum ~ BR + SOC + pH
BR ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.3a <- sem(sem.3a, data = sem.data2A, missing = 'fiml')
varTable(fit.3a)
summary(fit.3a, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) 
fitMeasures(fit.3a, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic   chisq      df  pvalue     cfi   rmsea    srmr 
# 823.899  23.127  30.000   0.810   1.000   0.000   0.065 
residuals(fit.3a,type="raw")

#****************************************
# Model B3: Soil with BR - B horizon

sem.3b <- '
# regressions
Degree.Sum ~ BR + SOC + pH
BR ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.3b <- sem(sem.3b, data = sem.data2B, missing = 'fiml')
varTable(fit.3b)
summary(fit.3b, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) 
fitMeasures(fit.3b, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic   chisq      df  pvalue     cfi   rmsea    srmr 
# 686.701  49.455  30.000   0.014   0.910   0.112   0.104
residuals(fit.3b,type="raw")

#****************************************
# Model A4: Plant-soil with BR - A horizon

sem.4a <- '
# regressions
Degree.Sum ~ BR + BromeShoots + Root.Biomass + SOC + pH
BR ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.4a <- sem(sem.4a, data = sem.data2A, missing = 'fiml')
varTable(fit.4a)
summary(fit.4a, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) 
fitMeasures(fit.4a, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic   chisq      df  pvalue     cfi   rmsea    srmr 
# 822.422  17.650  28.000   0.935   1.000   0.000   0.058 
residuals(fit.4a,type="raw")

#****************************************
# Model B4: Plant-soil with BR - B horizon

sem.4b <- '
# regressions
Degree.Sum ~ BR + BromeShoots + Root.Biomass + SOC + pH
BR ~ SOC + Soil.N + pH + BromeShoots
SOC ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
SOC ~~ Soil.N
Soil.N ~ Litter.Biomass + Litter.CN.ratio + Root.Biomass
Litter.Biomass ~ BromeShoots
Litter.CN.ratio ~ BromeShoots + PR
Root.Biomass ~ BromeShoots + PR
PR ~ BromeShoots + Litter.Biomass + A.Horizon
'
fit.4b <- sem(sem.4b, data = sem.data2B, missing = 'fiml')
varTable(fit.4b)
summary(fit.4b, fit.measures = TRUE, standardized = TRUE, rsq = TRUE) 
fitMeasures(fit.4b, c("aic", "chisq", "df", "pvalue", "cfi", "rmsea","srmr"))
# aic   chisq      df  pvalue     cfi   rmsea    srmr 
# 689.267  48.021  28.000   0.011   0.908   0.117   0.102 
residuals(fit.4b,type="raw")
