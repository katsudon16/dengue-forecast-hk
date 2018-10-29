rm(list=ls(all=TRUE))
library('MASS')
source('../lib/stepAICc.R')
startyear <- 2002

# collectData
Rain.CZ <- read.csv("../../dat/csv/changzhou_climate.csv")
Rain.TT <- read.csv("../../dat/csv/tate_climate.csv")
Cases.HK <- read.csv("../../dat/csv/HK_Cases.csv")
#Rain.CZ <- Rain.CZ[ , -which(names(Rain.CZ) %in% c("X"))]
#Rain.TT <- Rain.TT[ , -which(names(Rain.TT) %in% c("X"))]

MEANFLAG <- 0
#initialize data
T <- 0
R <- 0
df <- data.frame()

for (year in 2002:2018) {
  print(year)
  for (month in 1:7) {
    T[month] <- Rain.CZ[Rain.CZ$month==month & Rain.CZ$year==year,'avg']
    R[month] <- Rain.CZ[Rain.CZ$month==month & Rain.CZ$year==year,'totalrain']
  }
  df <- rbind(df, c(year, 1, T, R))
}
names(df)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df$CASES <- Cases.HK$CASES
dfnew <- df

# create dataframe for Tate
if (MEANFLAG == 1) {
T <- 0
R <- 0
df2 <- data.frame()
for (year in 2002:2018) {
  print(year)
  for (month in 1:7) {
    T[month] <- Rain.TT[Rain.TT$month==month & Rain.TT$year==year,'avg']
    R[month] <- Rain.TT[Rain.TT$month==month & Rain.TT$year==year,'totalrain']
  }
  df2 <- rbind(df2, c(year, 1, T, R))
}
names(df2)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df2$CASES <- Cases.HK$case
dfnew <- data.frame(data.matrix(df[,3:ncol(df)]) + data.matrix(df2[,3:ncol(df)])/2)
dfnew <- cbind(Cases.HK,dfnew)
}


#df$CASES <- df$CASES-11 #transforming the raw data to exclude zeros

## model fitting
glm_fin <- stepAIC(glm(CASES ~ 1, data = dfnew),
                   scope = CASES ~  T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7,
                   # scope = CASES ~   T4 + T5 + T6 + T7 + R4 + R5 + R6 + R7,
                   direction = "forward")
# glm(CASES ~ T3 + T4 + T5 + T6 + T7 + R3 + R4 + R5 + R6 + R7, data = dfnew)
# AIC=33.84 temp=avg

## Prediction in 2018
fit.glm <- glm_fin
year2018 <- df[17,3:16]
fit.glm.coeff <- summary(fit.glm)$coefficients[,1]
par <- rownames(as.data.frame(fit.glm.coeff))
year2018[par[2:15]]
pred2018 <- sum(fit.glm.coeff*cbind(1,year2018[par[2:15]]))



## ?u???o?f?ƻP???p?o?f?ƪ?????
y_actual <- Cases.HK[,2]
y_hat <- round(glm_fin$fitted)
y <- data.frame(y_actual,y_hat)
#save(y, file = "DenguePrediction.RData")