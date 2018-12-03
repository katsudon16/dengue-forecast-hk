
rm(list=ls(all=TRUE))

# 1. Cheun Chau(CC)  

library(openxlsx)
HKCDCC=read.xlsx("../../dat/climate/HKCD.xlsx",sheet = "HKCDCC",startRow = 1,colNames = TRUE,detectDates = TRUE)

for (fieldLabel in c("Daily.Mean", "Absolute.Daily.Min","Total.Rainfall.(mm)")) {
  HKCDCC[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", HKCDCC[fieldLabel][,]))
}


# Minimum Month Temprature  (CC)

MMTCC=aggregate(HKCDCC$Daily.Mean,list(HKCDCC$Month,HKCDCC$Year),min,na.rm=TRUE)
names(MMTCC)[1]="Month"
names(MMTCC)[2]="Year"
names(MMTCC)[3]="MMT"

# Average Monthly Temprature (CC)
AMTCC=aggregate(HKCDCC$Daily.Mean,list(HKCDCC$Month,HKCDCC$Year),mean,na.rm=TRUE)
names(AMTCC)[1]="Month"
names(AMTCC)[2]="Year"
names(AMTCC)[3]="AMT"

# Average Daily Minimum Temprature (CC)
ADMTCC=aggregate(HKCDCC$Absolute.Daily.Min,list(HKCDCC$Month,HKCDCC$Year),mean,na.rm=TRUE)
names(ADMTCC)[1]="Month"
names(ADMTCC)[2]="Year"
names(ADMTCC)[3]="ADMT"

RCC=read.xlsx("../../dat/climate/changzhou_climate(clean).xlsx",sheet = "Sheet1",startRow = 1,colNames = TRUE,detectDates = TRUE)
for (fieldLabel in c("totalrain")) {
  RCC[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", RCC[fieldLabel][,]))
}



RmaxCC=aggregate(HKCDCC$`Total.Rainfall.(mm)`,list(HKCDCC$Month,HKCDCC$Year),max,na.rm=TRUE)
names(RmaxCC)[1]="Month"
names(RmaxCC)[2]="Year"
names(RmaxCC)[3]="Rmax"


# AICC


library('MASS')
source('../lib/stepAICc.R')
startyear <- 2002


# collectData

Cases.HK <- read.xlsx("../../dat/cases/hk_annual_cases.xlsx",sheet = "Sheet1",startRow = 1,colNames = TRUE,detectDates = TRUE)


T <- 0
R <- 0
df <- data.frame()

for (Year in 2002:2018) {
  print(Year)
  for (Month in 1:7) {
    T[Month] <- ADMTCC[ADMTCC$Month==Month & ADMTCC$Year==Year,'ADMT']
    R[Month] <- RmaxCC[RmaxCC$Month==Month & RmaxCC$Year==Year,'Rmax'] 
  }
  df <- rbind(df, c(Year, 1,T , R))
}
names(df)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df$CASES <- Cases.HK$CASES
dfnew <- df

#Calculating AIC
glm_fin <- stepAIC(glm(CASES ~ 1, data = dfnew),
                   scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7,
                   direction = "forward")



# Minimum Month Temprature  (CC)
MMTCC=aggregate(HKCDCC$Daily.Mean,list(HKCDCC$Month,HKCDCC$Year),min,na.rm=TRUE)
names(MMTCC)[1]="Month"
names(MMTCC)[2]="Year"
names(MMTCC)[3]="MMT"

T <- 0
R <- 0
df2 <- data.frame()

for (Year in 2002:2018) {
  print(Year)
  for (Month in 1:7) {
    T[Month] <- MMTCC[MMTCC$Month==Month & MMTCC$Year==Year,'MMT']
    R[Month] <- RmaxCC[RmaxCC$Month==Month & RmaxCC$Year==Year,'Rmax'] 
  }
  df2 <- rbind(df2, c(Year, 1,T , R))
}
names(df2)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df2$CASES <- Cases.HK$CASES
df2new <- df2

#Calculating AIC
glm_fin2 <- stepAIC(glm(CASES ~ 1, data = df2new),
                    scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7,
                    direction = "forward")



# Average Monthly Temprature (CC)
AMTCC=aggregate(HKCDCC$Daily.Mean,list(HKCDCC$Month,HKCDCC$Year),mean,na.rm=TRUE)
names(AMTCC)[1]="Month"
names(AMTCC)[2]="Year"
names(AMTCC)[3]="AMT"

T <- 0
R <- 0
df3 <- data.frame()

for (Year in 2002:2018) {
  print(Year)
  for (Month in 1:7) {
    T[Month] <- AMTCC[AMTCC$Month==Month & AMTCC$Year==Year,'AMT']
    R[Month] <- RmaxCC[RmaxCC$Month==Month & RmaxCC$Year==Year,'Rmax'] 
  }
  df3 <- rbind(df3, c(Year, 1,T , R))
}
names(df3)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df3$CASES <- Cases.HK$CASES
df3new <- df3

#Calculating AIC
glm_fin3 <- stepAIC(glm(CASES ~ 1, data = df3new),
                    scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7,
                    direction = "forward")
summary(glm_fin)
summary(glm_fin2)
summary(glm_fin3)









# Tate's Kairn 

#Data collection 

# rm(list=ls(all=TRUE))


library(openxlsx)
HKCDTC=read.xlsx("../../dat/climate/HKCD.xlsx",sheet = "HKCDTC",startRow = 1,colNames = TRUE,detectDates = TRUE)

for (fieldLabel in c("Daily.Mean", "Absolute.Daily.Min","Total.Rainfall.(mm)")) {
  HKCDTC[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", HKCDTC[fieldLabel][,]))
}


# Minimum Month Temprature  (TC)
MMTTC=aggregate(HKCDTC$Daily.Mean,list(HKCDTC$Month,HKCDTC$Year),min,na.rm=TRUE)
names(MMTTC)[1]="Month"
names(MMTTC)[2]="Year"
names(MMTTC)[3]="MMT"

# Average Monthly Temprature (TC)
AMTTC=aggregate(HKCDTC$Daily.Mean,list(HKCDTC$Month,HKCDTC$Year),mean,na.rm=TRUE)
names(AMTTC)[1]="Month"
names(AMTTC)[2]="Year"
names(AMTTC)[3]="AMT"

# Average Daily Minimum Temprature (TC)
ADMTTC=aggregate(HKCDTC$Absolute.Daily.Min,list(HKCDTC$Month,HKCDTC$Year),mean,na.rm=TRUE)
names(ADMTTC)[1]="Month"
names(ADMTTC)[2]="Year"
names(ADMTTC)[3]="ADMT"

RTC=read.xlsx("../../dat/climate/tate_climate(clean).xlsx",sheet = "Sheet1",startRow = 1,colNames = TRUE,detectDates = TRUE)
for (fieldLabel in c("totalrain")) {
  RTC[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", RTC[fieldLabel][,]))
}


RmaxTC=aggregate(HKCDTC$`Total.Rainfall.(mm)`,list(HKCDTC$Month,HKCDTC$Year),max,na.rm=TRUE)
names(RmaxTC)[1]="Month"
names(RmaxTC)[2]="Year"
names(RmaxTC)[3]="Rmax"


# model fitting


library('MASS')
source('../lib/stepAICc.R')
startyear <- 2002

Cases.HK <- read.xlsx("../../dat/cases/hk_annual_cases.xlsx",sheet = "Sheet1",startRow = 1,colNames = TRUE,detectDates = TRUE)


T <- 0
R <- 0
df <- data.frame()

for (Year in 2002:2018) {
  print(Year)
  for (Month in 1:7) {
    T[Month] <- ADMTTC[ADMTTC$Month==Month & ADMTTC$Year==Year,'ADMT']
    R[Month] <- RmaxTC[RmaxTC$Month==Month & RmaxTC$Year==Year,'Rmax'] 
  }
  df <- rbind(df, c(Year, 1,T , R))
}
names(df)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df$CASES <- Cases.HK$CASES
dfnew <- df

#Calculating AIC
glm_fin4 <- stepAIC(glm(CASES ~ 1, data = dfnew),
                   scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7,
                   direction = "forward")



# Minimum Month Temprature  (TC)
MMTTC=aggregate(HKCDTC$Daily.Mean,list(HKCDTC$Month,HKCDTC$Year),min,na.rm=TRUE)
names(MMTTC)[1]="Month"
names(MMTTC)[2]="Year"
names(MMTTC)[3]="MMT"

#model fitting

library('MASS')
source('../lib/stepAICc.R')
startyear <- 2002

T <- 0
R <- 0
df2 <- data.frame()

for (Year in 2002:2018) {
  print(Year)
  for (Month in 1:7) {
    T[Month] <- MMTTC[MMTTC$Month==Month & MMTTC$Year==Year,'MMT']
    R[Month] <- RmaxTC[RmaxTC$Month==Month & RmaxTC$Year==Year,'Rmax'] 
  }
  df2 <- rbind(df2, c(Year, 1,T , R))
}
names(df2)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df2$CASES <- Cases.HK$CASES
df2new <- df2

#Calculating AIC
glm_fin5 <- stepAIC(glm(CASES ~ 1, data = df2new),
                    scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7,
                    direction = "forward")



# Average Monthly Temprature (TC)
AMTTC=aggregate(HKCDTC$Daily.Mean,list(HKCDTC$Month,HKCDTC$Year),mean,na.rm=TRUE)
names(AMTTC)[1]="Month"
names(AMTTC)[2]="Year"
names(AMTTC)[3]="AMT"

T <- 0
R <- 0
df3 <- data.frame()

for (Year in 2002:2018) {
  print(Year)
  for (Month in 1:7) {
    T[Month] <- AMTTC[AMTTC$Month==Month & AMTTC$Year==Year,'AMT']
    R[Month] <- RmaxTC[RmaxTC$Month==Month & RmaxTC$Year==Year,'Rmax'] 
  }
  df3 <- rbind(df3, c(Year, 1,T , R))
}
names(df3)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df3$CASES <- Cases.HK$CASES
df3new <- df3

#Calculating AIC
glm_fin6 <- stepAIC(glm(CASES ~ 1, data = df3new),
                    scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7,
                    direction = "forward")
summary(glm_fin4)
summary(glm_fin5)
summary(glm_fin6)









# King's Park 

# Data collection 

library(openxlsx)
HKCDKP=read.xlsx("../../dat/climate/HKCD.xlsx",sheet = "HKCDKP",startRow = 1,colNames = TRUE,detectDates = TRUE)

for (fieldLabel in c("Daily.Mean", "Absolute.Daily.Min","Total.Rainfall")) {
  HKCDKP[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", HKCDKP[fieldLabel][,]))
}

# Minimum Month Temprature  (KP)
MMTKP=aggregate(HKCDKP$Daily.Mean,list(HKCDKP$Month,HKCDKP$Year),min,na.rm=TRUE)
names(MMTKP)[1]="Month"
names(MMTKP)[2]="Year"
names(MMTKP)[3]="MMT"

# Average Monthly Temprature (KP)
AMTKP=aggregate(HKCDKP$Daily.Mean,list(HKCDKP$Month,HKCDKP$Year),mean,na.rm=TRUE)
names(AMTKP)[1]="Month"
names(AMTKP)[2]="Year"
names(AMTKP)[3]="AMT"

# Average Daily Minimum Temprature (KP)
ADMTKP=aggregate(HKCDKP$Absolute.Daily.Min,list(HKCDKP$Month,HKCDKP$Year),mean,na.rm=TRUE)
names(ADMTKP)[1]="Month"
names(ADMTKP)[2]="Year"
names(ADMTKP)[3]="ADMT"

RKP=read.xlsx("../../dat/climate/kingspark_climate.xlsx",sheet = "Sheet1",startRow = 1,colNames = TRUE,detectDates = TRUE)
for (fieldLabel in c("totalrain")) {
  RKP[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", RKP[fieldLabel][,]))
}

RmaxKP=aggregate(HKCDKP$Total.Rainfall,list(HKCDKP$Month,HKCDKP$Year),max,na.rm=TRUE)
names(RmaxKP)[1]="Month"
names(RmaxKP)[2]="Year"
names(RmaxKP)[3]="Rmax"


library('MASS')
source('../lib/stepAICc.R')
startyear <- 2002


# collectData

Cases.HK <- read.xlsx("../../dat/cases/hk_annual_cases.xlsx",sheet = "Sheet1",startRow = 1,colNames = TRUE,detectDates = TRUE)


T <- 0
R <- 0
df <- data.frame()

for (Year in 2002:2018) {
  print(Year)
  for (Month in 1:7) {
    T[Month] <- ADMTKP[ADMTKP$Month==Month & ADMTKP$Year==Year,'ADMT']
    R[Month] <- RmaxKP[RmaxKP$Month==Month & RmaxKP$Year==Year,'Rmax'] 
  }
  df <- rbind(df, c(Year, 1,T , R))
}
names(df)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df$CASES <- Cases.HK$CASES
dfnew <- df

#Calculating AIC
glm_fin7 <- stepAIC(glm(CASES ~ 1, data = dfnew),
                   scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7,
                   direction = "forward")



# Minimum Month Temprature  (CC)
MMTCC=aggregate(HKCDCC$Daily.Mean,list(HKCDCC$Month,HKCDCC$Year),min,na.rm=TRUE)
names(MMTKP)[1]="Month"
names(MMTKP)[2]="Year"
names(MMTKP)[3]="MMT"

T <- 0
R <- 0
df2 <- data.frame()

for (Year in 2002:2018) {
  print(Year)
  for (Month in 1:7) {
    T[Month] <- MMTKP[MMTKP$Month==Month & MMTKP$Year==Year,'MMT']
    R[Month] <- RmaxKP[RmaxKP$Month==Month & RmaxKP$Year==Year,'Rmax'] 
  }
  df2 <- rbind(df2, c(Year, 1,T , R))
}
names(df2)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df2$CASES <- Cases.HK$CASES
df2new <- df2

#Calculating AIC
glm_fin8 <- stepAIC(glm(CASES ~ 1, data = df2new),
                    scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7,
                    direction = "forward")



# Average Monthly Temprature (CC)
AMTKP=aggregate(HKCDKP$Daily.Mean,list(HKCDKP$Month,HKCDKP$Year),mean,na.rm=TRUE)
names(AMTKP)[1]="Month"
names(AMTKP)[2]="Year"
names(AMTKP)[3]="AMT"

T <- 0
R <- 0
df3 <- data.frame()

for (Year in 2002:2018) {
  print(Year)
  for (Month in 1:7) {
    T[Month] <- AMTKP[AMTKP$Month==Month & AMTKP$Year==Year,'AMT']
    R[Month] <- RmaxKP[RmaxKP$Month==Month & RmaxKP$Year==Year,'Rmax'] 
  }
  df3 <- rbind(df3, c(Year, 1,T , R))
}
names(df3)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df3$CASES <- Cases.HK$CASES
df3new <- df3

#Calculating AIC
glm_fin9 <- stepAIC(glm(CASES ~ 1, data = df3new),
                    scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7,
                    direction = "forward")
summary(glm_fin7)
summary(glm_fin8)
summary(glm_fin9)


