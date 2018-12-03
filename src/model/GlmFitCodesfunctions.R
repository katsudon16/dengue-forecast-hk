# 1. DATA COLLECTION

# Getting Temperature data 

source('../lib/getMonthlyTempData.R')

dfT <- getMonthlyTempData (type="mean", location="CC", colName="Daily.Mean", filepath="../../dat/climate/HKCD.xlsx")

# Geting Rainfal Data

source('../lib/getMonthlyRainData.R')

dfR <- getMonthlyRainData (type="max", filepath="../../dat/climate/HKCD.xlsx", location="CC")

# Merging temperature(dfT) and Rainfal(dfR) dataframes

df <- merge(dfT, dfR)

# Dengue Cases Data

Cases.HK <- read.xlsx("../../dat/cases/hk_annual_cases.xlsx",sheet = "Sheet1",startRow = 1,colNames = TRUE,detectDates = TRUE)

# Combining dfT,dfR and Cases data frames

names(df)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
df$CASES <- Cases.HK$CASES
dfnew <- df 


# 2. GLM and AIC

library('MASS')
source('../lib/stepAICc.R')

glm_fin <- stepAIC(glm(CASES ~ 1, data = dfnew),
                   scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7,
                   direction = "forward")

# 3. GET GLM SUMMARY AND AIC VALUES 

summary(glm_fin)
writeLines(capture.output(coefficients(glm_fin), AIC (glm_fin)),con="outfile.txt")


