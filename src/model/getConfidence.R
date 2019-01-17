## confidence interval
## produce estimated mean and 95% confidence interval
nrow <- 1
n_rep <- 1000 # repeated simulation n_rep times 
y_hat_pool <- matrix(0, nrow, n_rep) # nrow by n_rep matrix with 0 
#get climate data from 2nd to ninth month 
new.Kao <- cbind(Temp.Kao[1:12,2:9], Rain.Kao[1:12,2:9])
for(i in 1:n_rep)
{
  n <- 1
  y_sim <- rpois(n, y_hat) # draw n samples from Poisson distribution using estimated y_hat
  glm_i <- glm(y_sim ~ T6 + R5 + R1 + R8 + R2 + R4 + T7 + T5 + T2,
               family = poisson(log), 
               data=new.Kao)
  y_hat_pool[, i] <- glm_i$fitted # y_hat with uncertainty
}
y_hat_lwr <- round(apply(y_hat_pool, 1, quantile, probs = 0.025))
y_hat_upr <- round(apply(y_hat_pool, 1, quantile, probs = 0.975))
y_ci <- data.frame(y_hat_lwr,y_hat_upr)
save(y_ci, file = "DenguePredictionCI.RData") # prediction results with 95%CI