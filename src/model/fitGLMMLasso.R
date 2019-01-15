# Fits linear mixed model of HK data given family input
#   Outputs length(areas) + 1 plots (actual >< predicted)
# Change explanatory / random variables on line 78 if needed
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
# if formula is specified, stepAIC will be skipped
# formula <- RISK ~ (1 | AREA)
formula <- RISK ~ (1 + R5| AREA)
family <- poisson() # e.g., poisson()
areas <- c("NTS", "NTN", "HKL")
predictType <- "response"
lambda <- 5
useAICc <- TRUE
# if showTruePrediction = TRUE, show model prediction result,
#   else, show cross validation prediction results
showTruePrediction <- FALSE
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
    for (month in 1:8) {
      T[month] <- Tdata[Tdata$month==month & Tdata$year==year, "temperature"]
      R[month] <- Rdata[Rdata$month==month & Rdata$year==year, "rainfall"]
    }
    # missing data result in -Inf
    if (length(R[!is.finite(R)]) > 0) next
    df <- rbind(df, c(area_i, year, risk, T, R))
  }
}
names(df) <- c("AREA", "YEAR", "RISK",
               "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8",
               "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8")
df$AREA <- as.factor(df$AREA)
rm(Rdata, Tdata, data, areaRisk, areasR, areasT)

if (!require("glmmLasso")) install.packages("glmmLasso")
if (!require("glmmTMB")) install.packages("glmmTMB")
if (!require("lme4")) install.packages("lme4")

if (is.null(formula)) {
  minBIC <- 10000
  res <- NULL
  resLambda <- NULL
  for (lambda in 51:60) {
    print(lambda)
    temp <- glmmLasso(RISK ~ T3 + T4 + T5 + T6 + T7 + T8 + R3 + R4 + R5 + R6 + R7 + R8,
                     rnd=list(AREA=~1 + R5), lambda=lambda,
                     data=df, family=family)
    print(summary(temp))
  }
} else {
  res <- glmmTMB(formula, data=df, family=family, REML=TRUE)
  # res <- glmer(formula, data=df, family=family, method="REML")
}

pred <- NULL
if (showTruePrediction) {
  pred <- df[c("AREA", "YEAR", "RISK")]
  pred$PRED <- round(predict(res, newdata=df, type=predictType))
  pred[pred$PRED < 0, "PRED"] <- 0
  pred$AREA <- as.numeric(pred$AREA)
} else {
  source("../lib/cross_validations.R")
  pred <- leaveOneOut(df, "RISK", res, modelType=glmmTMB,
                      resDfCols=c("AREA", "YEAR", "RISK"),
                      formula=formula(res), family=family,
                      predictType=predictType)
}
if (!"ALL" %in% areas) {
  areas <- c("ALL", areas)
}

byyear <- list()
for (area_i in areas) {
  print(area_i)
  byyear[[area_i]] <- data.frame()
  for (year in minYear:maxYear) {
    if (area_i == "ALL") {
      byyearRisk <- sum(pred[pred$YEAR == year, "RISK"])
      byyearPred <- sum(pred[pred$YEAR == year, "PRED"])
    } else {
      byyearRisk <- pred[pred$YEAR == year & areas[pred$AREA + 1] == area_i, "RISK"]
      byyearPred <- pred[pred$YEAR == year & areas[pred$AREA + 1] == area_i, "PRED"]
    }
    byyear[[area_i]] <- rbind(byyear[[area_i]], c(year, byyearRisk, byyearPred))
  }
  names(byyear[[area_i]]) <- c("YEAR", "RISK", "PRED")
  mse_val <- mean((byyear[[area_i]]$RISK - byyear[[area_i]]$PRED) ^ 2)
  cat("mse validation is", mse_val)
  
  # ensure the plot can contain max values, where
  #   predicted = blue dashed; actual = black solid
  if (max(byyear[[area_i]]$PRED) > max(byyear[[area_i]]$RISK)) {
    plot(byyear[[area_i]]$YEAR, byyear[[area_i]]$PRED,
         xlab="year", ylab="cases", type="l", col="blue", lty=2,
         main=paste(area_i,
                    " - MSE validation: ",
                    format(round(mse_val, 2), nsmall = 2), sep=""))
    lines(byyear[[area_i]]$YEAR, byyear[[area_i]]$RISK)
  } else {
    plot(byyear[[area_i]]$YEAR, byyear[[area_i]]$RISK,
         xlab="year", ylab="cases", type="l", 
         main=paste(area_i,
                    " - MSE validation: ",
                    format(round(mse_val, 2), nsmall = 2), sep=""))
    lines(byyear[[area_i]]$YEAR, byyear[[area_i]]$PRED, col="blue", lty=2)
  }
  legend("top", legend=c("actual", "predicted"), col=c("black", "blue"),
         lty=1:2, cex=0.8)
  invisible(readline(prompt="Press [enter] to continue"))
}
print("BIC")
print(BIC(res))
print("AIC")
print(AIC(res))
print("AICc")
print(AICc(res))