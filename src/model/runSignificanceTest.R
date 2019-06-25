# Perform stepwise on glmmTMB given explanatory variables T3 - T8, R3 - R8
#   if formula is NULL, and/or
#   run leave-one-out cross-validation given formula
# @Outputs
# - actual >< predicted case plots
rm(list=ls(all=TRUE))

#---------USER INPUTS-------------
## temperatureField: "mean", "absMin", "absMax"
temperatureField <- "mean"
## temperatureType: "mean", "max", "min"
temperatureType <- "mean" 
## rainfallType: "total", "max"
rainfallType <- "total"
minYear <- 2002
maxYear <- 2018
areas <- c("NTS", "NTN", "HKL")
fixedVarsList <- c("T3", "T4", "T5", "T7", "R4", "R5", "R6")
randomVarsList <- c("AREA")
family <- poisson # poisson or nbinom2
#---------------------------------

source("../lib/retrieveData.R")
library("glmmTMB")
source("../lib/LRT.R")

df <- extractAnnualClimateData(temperatureField, temperatureType, rainfallType,
                               areas, minYear=minYear, maxYear=maxYear)

maxs <- apply(df[,c(4:19)], 2, max)
mins <- apply(df[,c(4:19)], 2, min)
df[,c(4:19)] <- scale(df[,c(4:19)], center = mins, scale = maxs - mins)

runLRT("RISK", fixedVarsList, randomVarsList, data=df, glmmTMB, family=family, REML=F)