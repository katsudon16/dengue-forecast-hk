divideByArea <- T
area <- 2

lambda <- round(fitted(res))
n <- dim(df)[1]

y_hat_pool <- matrix(NA, 1000, n)

if (!require("glmmTMB")) install.packages("glmmTMB")
library("glmmTMB")

for (i in 1:1000) {
  y_sim <- rpois(n, lambda)
  df$y_sim <- y_sim
  model_sim <- glmmTMB(y_sim ~ (1 | AREA) + T3 + T4 + T5 + T7 + R4 + R5 + R6,
                       data=df, family=family, REML=TRUE)
  y_hat_pool[i,] <- round(fitted(model_sim))
}

if (divideByArea == T) {
  estimated <- lambda
  y_hat_lwr <- round(apply(y_hat_pool, 2, quantile, probs = 0.025, na.rm=T))
  y_hat_upr <- round(apply(y_hat_pool, 2, quantile, probs = 0.975, na.rm=T))
  observed <- df$RISK
  
  y_ci <- data.frame()
  year <- 2002
  
  for (i in 0:(n/3 - 1)) {
    row <- area + i*3
    y_ci <- rbind(y_ci, c(observed[row], estimated[row], y_hat_lwr[row], y_hat_upr[row], year))
    year <- year + 1    
  }
  
  names(y_ci) <- c("observed", "estimated", "ci_lower", "ci_higher", "year")
  
} else {
  y_hat_fin <- matrix(NA, 1000, n / 3)
  estimated <- c()
  for (i in 1:(n/3)) {
    start_col <- (i-1) * 3 + 1
    y_hat_fin[,i] <- rowSums(y_hat_pool[,start_col:(start_col + 2)])
  }
  
  for (i in 1:(n/3)) {
    start_col <- (i-1) * 3 + 1
    estimated[i] <- sum(lambda[start_col:(start_col + 2)])
  }
  y_hat_lwr <- round(apply(y_hat_fin, 2, quantile, probs = 0.025, na.rm=T))
  y_hat_upr <- round(apply(y_hat_fin, 2, quantile, probs = 0.975, na.rm=T))
  
  observed <- c(19, 1, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 3, 3, 4, 1, 29)
  
  y_ci <- data.frame(observed, estimated, y_hat_lwr,y_hat_upr)
  names(y_ci) <- c("observed", "estimated", "ci_lower", "ci_higher")
  y_ci$year <- seq(2002, 2018, 1)
}

plot(y_ci$year, y_ci$estimated,
     xlab="Year", ylab="Local Cases", type="l", col="blue", lty=2, ylim=c(0, 40),
     main="NTN")
lines(y_ci$year, y_ci$observed)
arrows(y_ci$year, y_ci$ci_lower, y_ci$year, y_ci$ci_higher, length=0.05, angle=90, code=3)
legend("top", legend=c("observed", "estimated"), col=c("black", "blue"),
       lty=1:2, cex=0.8)
