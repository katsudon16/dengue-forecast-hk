rm(list=ls(all=TRUE))
source("../lib/retrieveData.R")

#---------USER INPUTS-------------
## temperatureField: "mean", "absMin", "absMax"
temperatureField <- "mean"
## temperatureType: "mean", "max", "min"
temperatureType <- "mean" 
## rainfallType: "total", "max"
rainfallType <- "total"
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
# TODO: optimize retrieving data
for (area in areas) {
  areasT[[area]] <- getMonthlyTemperatureOnType(type=temperatureType,
                                                location=area,
                                                colName=temperatureColLabel,
                                                filepath=climateFile)
  areasR[[area]] <- getMonthlyRainfallOnType(type=rainfallType,
                                             filepath=climateFile,
                                             location=area)
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
res <- lmer(RISK ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 +
                   R1 + R2 + R3 + R4 + R5 + R6 + R7 + (1 | AREA),
            data=df)