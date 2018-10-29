## l-o-o cross validation
fit.glm <- glm_fin
n <- dim(Cases.HK)[1] # number of sampels
glm_fin.coeff <- summary(fit.glm)$coefficients[,1]
p <- length(glm_fin.coeff)-1
y_actual <- Cases.HK$CASES
  
loo_pred <- rep(NA, n)
loo_fitted <- matrix(NA, n, n)
loo_train_mse <- rep(NA, n)
MSE_Tr <- 0
MSE_Va <- 0
MSE_Ratio <- 0
Expected_Ratio <-0

for(i in 1:n)
{
  glm_i <- glm(glm_fin$formula, data = df[-i,])#glm with out ith row
  loo_pred[i] <- round(predict(glm_i, type = "response", newdata = df[i,]))
  if (loo_pred[i]<0) loo_pred[i] <- 0
  fitting <- round(glm_i$fitted) # fitting year
  fitting[fitting<0] <- 0
  loo_fitted[-i, i] <- round(fitting) # fitting year
  loo_fitted[i,i] <- loo_pred[i] # predicting year
  loo_train_mse[i] <- mean((glm_i$y - glm_i$fitted) ^ 2)
}
MSE_Tr <- mean(loo_train_mse)

loo_matrix <- t(loo_fitted)
diag(loo_matrix) <- loo_pred
#write.csv(loo_matrix, file = "loo_matrix.csv", row.names = FALSE)

length(glm_fin$coeff)
## MSE for validating set
loo_mse <- mean((y_actual - loo_pred) ^ 2)
MSE_Va <- loo_mse

MSE_Ratio <- MSE_Tr/MSE_Va
Expected_Ratio <- (n-p-1)/(n+p+1)

## Actual Cases number?BEstimated number, leave-one-out estimated number
Y_Pred <- data.frame(y_actual,y_hat,loo_pred) 
Validate <- data.frame(MSE_Ratio, Expected_Ratio <- (n-p-1)/(n+p+1))
