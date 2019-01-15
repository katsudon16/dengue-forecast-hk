# Perform LASSO for variable selection if performLasso = TRUE with glmmLasso
# else, run leave-one-out cross validation given the formula on glmmTMB
# @output:
# - plot predicted >< actual
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
performLasso <- T
formula <- NULL
family <- poisson() # e.g., poisson()
areas <- c("NTS", "NTN", "HKL")
predictType <- "response"
maxLambda <- 20
useAICc <- TRUE
# if showTruePrediction = TRUE, show model prediction result,
#   else, show cross validation prediction results
showTruePrediction <- FALSE
#---------------------------------

if (is.null(formula) & !performLasso) {
  stop("performLasso should be TRUE if formula is not provided")
}

source("../lib/retrieveData.R")
df <- extractAnnualClimateData(temperatureField, temperatureType, rainfallType, areas)

if (performLasso) {
  predictors <- c("T3", "T4", "T5", "T6", "T7", "T8", "R3", "R4", "R5", "R6", "R7", "R8")
  maxs <- apply(df[,c(4:19)], 2, max)
  mins <- apply(df[,c(4:19)], 2, min)
  df[,c(4:19)] <- scale(df[,c(4:19)], center = mins, scale = maxs - mins)
  
  library("MASS")
  library("nlme")
  library("glmmTMB")
  # fit starting model
  pql<-glmmPQL(RISK ~ 1, random = ~1|AREA, family=family, data=df)
  delta.start<-c(as.numeric(pql$coef$fixed), rep(0, 12), as.numeric(t(pql$coef$random$AREA)))
  Q.start<-as.numeric(VarCorr(pql)[1,1])
  
  minAICc <- 10000
  minAICcFormula <- ""
  minLambda <- -1
  
  if (!require("glmmLasso")) install.packages("glmmLasso")
  source("../lib/cross_validations.R")
  
  coef_table <- matrix(NA, maxLambda, 12)
  
  resTable <- data.frame()
  for (i in 1:maxLambda) {
    print("--------------------------")
    print(paste("Lambda: ", i, sep=""))
    tempModel <- try(glmmLasso(RISK ~ T3 + T4 + T5 + T6 + T7 + T8 + R3 + R4 + R5 + R6 + R7 + R8,
                               rnd=list(AREA=~1), family=family,
                               data = df, lambda=i, final.re=TRUE,switch.NR=T, 
                               control=list(start=delta.start, q_start=Q.start)))
    coefs <- tempModel$coefficients
    newFormula <- "RISK ~ (1 | AREA)"
    notIncluded <- "Missing: "
    for (j in 2:13) {
      if (coefs[j] != 0) {
        newFormula <- paste(newFormula, predictors[j-1], sep=" + ")
      } else {
        notIncluded <- paste(notIncluded, predictors[j-1])
      }
    }
    
    res <- glmer(as.formula(newFormula), data=df, family=family)
    for (j in 2:13) {
      if (coefs[j] != 0) {
        coef_table[i, j-1] <- coef(res)$AREA[[predictors[j-1]]][1]
      } else {
        coef_table[i, j-1] <- 0
      }
    }
    modelAICc <- AICc(res)
    if (modelAICc < minAICc) {
      minAICc <- modelAICc
      minAICcFormula <- newFormula
      minLambda <- i
    }
    print(newFormula)
    print(paste("BIC:", tempModel$bic, " AIC:", tempModel$aic))
  }
  print(minAICcFormula)
  print(minAICc)
  
  
  coef_df <- as.data.frame(coef_table)
  names(coef_df) <- c("T3", "T4", "T5", "T6", "T7", "T8", "R3", "R4", "R5", "R6", "R7", "R8")
  
  maxVal <- max(coef_df)
  minVal <- min(coef_df)
  coef_df$lambda <- seq(1, maxLambda, 1)
  
  # Plot coefficient size for the increasing lambda
  plot(coef_df$lambda, coef_df$T3, lwd=2,
       xlab="Lambda", ylab="Coefficient", type="l", col="blue", lty=1, ylim=c(minVal, maxVal),
       main="Minimum Temperature & Maximum Rainfall")
  # lines(coef_df$lambda, coef_df$T4, lwd=2, lty=2, col="#d6d4d4")
  lines(coef_df$lambda, coef_df$T4, lwd=2, col="red")
  # lines(coef_df$lambda, coef_df$T5, lwd=2, lty=2, col="#d6d4d4")
  lines(coef_df$lambda, coef_df$T5, lwd=2, col="brown")
  # lines(coef_df$lambda, coef_df$T6, lwd=2, lty=2, col="#d6d4d4")
  lines(coef_df$lambda, coef_df$T6, lwd=2, col="black")
  # lines(coef_df$lambda, coef_df$T7, lwd=2, lty=2, col="#d6d4d4")
  lines(coef_df$lambda, coef_df$T7, lwd=2, col="green")
  # lines(coef_df$lambda, coef_df$T8, lwd=2, lty=2, col="#d6d4d4")
  lines(coef_df$lambda, coef_df$T8, lwd=2, col="yellow")
  # lines(coef_df$lambda, coef_df$R3, lwd=2, lty=2, col="#d6d4d4")
  lines(coef_df$lambda, coef_df$R3, lwd=2, col="pink")
  # lines(coef_df$lambda, coef_df$R4, lwd=2, lty=2, col="#d6d4d4")
  lines(coef_df$lambda, coef_df$R4, lwd=2, col="orange")
  # lines(coef_df$lambda, coef_df$R5, lwd=2, lty=2, col="#d6d4d4")
  lines(coef_df$lambda, coef_df$R5, lwd=2, col="purple")
  lines(coef_df$lambda, coef_df$R6, lwd=2, lty=2, col="#d6d4d4")
  # lines(coef_df$lambda, coef_df$R6, lwd=2, col="#39d8dd")
  lines(coef_df$lambda, coef_df$R7, lwd=2, lty=2, col="#d6d4d4")
  # lines(coef_df$lambda, coef_df$R7, lwd=2, col="#2b1054")
  # lines(coef_df$lambda, coef_df$R8, lwd=2, lty=2, col="#d6d4d4")
  lines(coef_df$lambda, coef_df$R8, lwd=2, col="#3a3a3a")
  abline(v=minLambda)
 
  legend("right", legend=c("T3", "T4", "T5", "T6", "T7", "T8",
                           "R3", "R4", "R5", "R8"),
         col=c("blue", "red", "brown", "black", "green", "yellow",
               "pink", "orange", "purple", "#3a3a3a"),
         lty=1, cex=1, lwd=2)
  
  # T3 = blue
  # T4 = red
  # T5 = brown
  # T6 = black
  # T7 = green
  # T8 = yellow
  # R3 = pink
  # R4 = orange
  # R5 = purple
  # R6 = #39d8dd
  # R7 = #2b1054
  # R8 = #3a3a3a
  
  
} else {
  if (!require("glmmTMB")) install.packages("glmmTMB")
  res <- glmmTMB(formula, data=df, family=family, REML=T)
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
}
