# Perform Lasso ONLY on glm using glmnet for variable selection
#   the final model is stored in finalModel variable
# @output:
# - MSE Validation
# - MSE Training
# - final model coefficients
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
formula <- RISK ~ R5
family <- poisson # poisson or nbinom2
areas <- c("NTS", "NTN", "HKL")
predictType <- "response"
useAICc <- TRUE
# if showTruePrediction = TRUE, show model prediction result,
#   else, show cross validation prediction results
showTruePrediction <- FALSE
#---------------------------------
source("../lib/retrieveData.R")
df <- extractAnnualClimateData(temperatureField, temperatureType, rainfallType, areas)

# extract columns for glmnet
risk <- df$RISK
T3 <- df$T3
T4 <- df$T4
T5 <- df$T5
T6 <- df$T6
T7 <- df$T7
T8 <- df$T8
R3 <- df$R3
R4 <- df$R4
R5 <- df$R5
R6 <- df$R6
R7 <- df$R7
R8 <- df$R8
area <- df$AREA

if (!require("glmnet")) install.packages("glmnet")
x <- as.matrix(data.frame(T3, T4, T5, T6, T7, T8, R3, R4, R5, R6, R7, R8))
cvres <- cv.glmnet(x, y=risk, alpha=1, family="poisson")
# store the optimal lambda
best.lambda <- cvres$lambda.min
# final model
finalModel <- glmnet(x, y=risk, alpha=1, family="poisson", lambda=best.lambda)

# run leavel-one-out cross validation
sampleLength <- dim(x)[1]
loo_pred <- rep(NA, sampleLength)
loo_fitted <- matrix(NA, sampleLength, sampleLength)
loo_train_mse <- rep(NA, sampleLength)

MSE_Tr <- 0
MSE_Va <- 0
MSE_Ratio <- 0
for (i in 1:sampleLength) {
  x <- as.matrix(data.frame(T3[-i], T4[-i], T5[-i], T6[-i], T7[-i], T8[-i],
                            R3[-i], R4[-i], R5[-i], R6[-i], R7[-i], R8[-i]))
  newx <- as.matrix(data.frame(T3[i], T4[i], T5[i], T6[i], T7[i], T8[i],
                               R3[i], R4[i], R5[i], R6[i], R7[i], R8[i]))
  model_i <- glmnet(x, y=risk[-i], alpha=1, family="poisson", lambda=best.lambda)
  loo_pred[i] <- round(predict(model_i, newx=newx, type="response"))
  fitting <- round(predict(model_i, newx=x, type="response"))
  loo_fitted[-i, i] <- fitting # fitting row
  loo_fitted[i,i] <- loo_pred[i] # predicting row
  loo_train_mse[i] <- mean((risk[-i] - fitting) ^ 2)
}
MSE_Tr <- mean(loo_train_mse)

# MSE for validating set
loo_mse <- mean((risk - loo_pred) ^ 2)
MSE_Va <- loo_mse
MSE_Ratio <- MSE_Tr/MSE_Va

cat("MSE validation is ", MSE_Va)
cat("MSE training is ", MSE_Tr)
coef(finalModel)