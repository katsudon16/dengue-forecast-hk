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
# if formula is specified, stepAICc will be skipped
formula <- RISK ~ (1 | AREA) + T3 + T4 + T5 + T7 + R4 + R5 + R6
# formula <- NULL
family <- poisson # poisson or nbinom2
areas <- c("NTS", "NTN", "HKL")
predictType <- "response"
useAICc <- TRUE
# if showTruePrediction = TRUE, show model prediction result,
#   else, show cross validation prediction results
showTruePrediction <- FALSE
#---------------------------------
source("../lib/retrieveData.R")
df <- extractAnnualClimateData(temperatureField, temperatureType, rainfallType,
                               areas, minYear=minYear, maxYear=maxYear)

library(pracma)
findNRMSE <- function(val) {
    return (nthroot(val, 2) / 19)
}

maxs <- apply(df[,c(4:19)], 2, max)
mins <- apply(df[,c(4:19)], 2, min)
df[,c(4:19)] <- scale(df[,c(4:19)], center = mins, scale = maxs - mins)

mean_cases <- mean(df$RISK)
# df$RISK <- df$RISK / mean_cases


if (!require("glmmTMB")) install.packages("glmmTMB")
library("glmmTMB")
library("MASS")
source("../lib/stepAICc.R")

if (is.null(formula)) {
  res <- stepAICc_MM(glmmTMB(RISK ~ (1 | AREA), data=df, family=poisson, REML=F),
                     scope=RISK ~ T3 + T4 + T5 + T6 + T7 + T8 + R3 + R4 + R5 + R6 + R7 + R8,
                     direction="forward", useAICc=TRUE)
} else {
  res <- glmmTMB(formula, data=df, family=family, REML=T, se=TRUE)
}

# library("ggeffects")
# marginalVar <- "R4"
# p <- ggpredict(res, c(marginalVar))
# p$predicted <- p$predicted / mean_cases
# p$std.error <- p$std.error / nthroot(mean_cases, 2)
# p$conf.low <- p$conf.low / mean_cases
# p$conf.high <- p$conf.high / mean_cases
# p <- plot(p) + labs(x = paste(marginalVar, "(mm)", sep=" "), y="Relative Risk", title="")
# ggsave("../../marginal_effect_R4.tiff", units="in", width=5, height=4.2, dpi=300, compression = "lzw")


exit() # continue to run LOOCV

# summary(res)
# AICc(res)
# BIC(res)

pred <- NULL
minYear <- 2003
df <- df[df$YEAR != 2002,]
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
         xlab="Year", ylab="Local Cases", type="l", col="blue", lty=2,
         # main=paste(area_i,
         #            " - MSE validation: ",
         #            format(round(mse_val, 2), nsmall = 2), sep=""))
         main=area_i)
    lines(byyear[[area_i]]$YEAR, byyear[[area_i]]$RISK)
  } else {
    plot(byyear[[area_i]]$YEAR, byyear[[area_i]]$RISK,
         xlab="Year", ylab="Local Cases", type="l",
         main=area_i)
         # main=paste(area_i,
         #            " - MSE validation: ",
         #            format(round(mse_val, 2), nsmall = 2), sep=""))
    lines(byyear[[area_i]]$YEAR, byyear[[area_i]]$PRED, col="blue", lty=2)
  }
  legend("top", legend=c("actual", "predicted"), col=c("black", "blue"),
         lty=1:2, cex=0.8)
  invisible(readline(prompt="Press [enter] to continue"))
}

