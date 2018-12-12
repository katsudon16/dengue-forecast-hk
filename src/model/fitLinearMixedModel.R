rm(list=ls(all=TRUE))
source("../lib/retrieveData.R")

#---------USER INPUTS-------------
## temperatureField: "mean", "absMin", "absMax"
temperatureField <- "mean"
## temperatureType: "mean", "max", "min"
temperatureType <- "min" 
## rainfallType: "total", "max"
rainfallType <- "max"
minYear <- 2002
maxYear <- 2018
#---------------------------------

temperatureColLabels <- c(
  mean="Daily.Mean.Temperature",
  absMax="Absolute.Daily.Max.Temperature",
  absMin="Absolute.Daily.Min.Temperature"
)
temperatureColLabel <- temperatureColLabels[temperatureField]
rainfallColLabel <- "Total.Rainfall.(mm)"
climateFile <- "../../dat/climate/HKCD_areas.xlsx"

areaRisk <- getAnnualRiskByArea()
areas <- names(areaRisk)[-1]

areasT <- list()
areasR <- list()
for (area in areas) {
  data <- readDFFromFile(area, filePath=climateFile)
  areasT[[area]] <- getMonthlyTemperatureOnType(type=temperatureType,
                                                colName=temperatureColLabel,
                                                df=data)
  areasR[[area]] <- getMonthlyRainfallOnType(type=rainfallType,
                                             location=area,
                                             df=data)
}

df <- data.frame()
for (year in minYear:maxYear) {
  for (area_i in 1:length(areas)) {
    area <- areas[area_i]
    Tdata <- areasT[[area]]
    Rdata <- areasR[[area]]
    T <- 0
    R <- 0
    risk <- areaRisk[areaRisk$year == year, area]
    for (month in 1:7) {
      T[month] <- Tdata[Tdata$month==month & Tdata$year==year, "temperature"]
      R[month] <- Rdata[Rdata$month==month & Rdata$year==year, "rainfall"]
    }
    # missing data result in -Inf
    if (length(R[!is.finite(R)]) > 0) next
    df <- rbind(df, c(area_i, year, risk, T, R))
  }
}
names(df) <- c("AREA", "YEAR", "RISK",
               "T1", "T2", "T3", "T4",
               "T5", "T6", "T7",
               "R1", "R2", "R3", "R4",
               "R5", "R6", "R7")
df$AREA <- areas[df$AREA]
df$AREA <- as.factor(df$AREA)

if (!require("lme4")) install.packages("lme4")
source("../lib/stepAIC_custom.R")

res <- stepAIC(df, model=glmmTMB,
               explanatoryVars=c("T1", "T2", "T3", "T4", "T5", "T6", "T7",
                                 "R1", "R2", "R3", "R4", "R5", "R6", "R7"),
               randomFormula="(1 | AREA)",
               ziformula=~1, REML=TRUE)


# res <- lmer(RISK ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 +
#                    R1 + R2 + R3 + R4 + R5 + R6 + R7 + (1 | AREA),
#             data=df)
res <- lmer(RISK ~ T1 + (1 | AREA), data=df)
res <- glmmTMB(RISK ~ R5 + R4 + (1 | AREA), data=df, ziformula=~1, REML=TRUE)
