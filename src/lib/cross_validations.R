# leaveOneOut function
# params:
# - df (data frame): dataset
# - yLabel (string): response column label, e.g., "RISK"
# - model (model)  : e.g., a glmmTMB model
# - modelType      : prediction model, e.g., glmmTMB
# - resDfCols (list of strings): columns to be added to the return df
# - formula        : model formula. If not specified, model$formula is used
# - the rest is parameters for predict function
# returns prediction result data frame with the resDfCols columns and PRED
leaveOneOut <- function(df, yLabel, model, modelType=glm, resDfCols=c(), predictType="link", formula=NULL, returnPred=TRUE, ...) {
  time1 <- Sys.time()
  sampleLength <- dim(df)[1]
  y_actual <- df[[yLabel]]
  loo_pred <- rep(NA, sampleLength)
  loo_fitted <- matrix(NA, sampleLength, sampleLength)
  loo_train_mse <- rep(NA, sampleLength)
  if (is.null(formula)) {
    formula <- model$formula
  }
  
  MSE_Tr <- 0
  MSE_Va <- 0
  MSE_Ratio <- 0
  pred_df <- data.frame()
  print(Sys.time() - time1)
  for (i in 1:sampleLength) {
    model_i <- modelType(formula, data=df[-i,], ...)
    # hack around glmmLasso bug when only 1 row is given
    if (i == sampleLength) {
      tempPred <- round(predict(model_i, newdata=df[(i-1):i,], type=predictType))
      loo_pred[i] <- tempPred[2]
    } else {
      tempPred <- round(predict(model_i, newdata=df[i:(i+1),], type=predictType))
      loo_pred[i] <- tempPred[1]
    }
    row <- c()
    for (j in 1:length(resDfCols)) {
      row[j] <- df[i, resDfCols[j]]
    }
    row[length(resDfCols) + 1] <- loo_pred[i]
    pred_df <- rbind(pred_df, row)
    fitting <- max(round(fitted(model_i)), 0)
    loo_fitted[-i, i] <- fitting # fitting row
    loo_fitted[i,i] <- loo_pred[i] # predicting row
    loo_train_mse[i] <- mean((df[[yLabel]][-i] - fitted(model_i)) ^ 2)
  }
  print(Sys.time() - time1)
  names(pred_df) <- c(resDfCols, "PRED")
  MSE_Tr <- mean(loo_train_mse)
  # loo_matrix <- t(loo_fitted)
  # diag(loo_matrix) <- loo_pred
  
  ## MSE for validating set
  loo_mse <- mean((y_actual - loo_pred) ^ 2)
  MSE_Va <- loo_mse
  
  print(MSE_Va)
  print(MSE_Tr)
  
  MSE_Ratio <- MSE_Tr/MSE_Va
  if (returnPred)
    pred_df
  else
    list(mse_train=MSE_Tr, mse_valid=MSE_Va, mse_ratio=MSE_Ratio)
}
