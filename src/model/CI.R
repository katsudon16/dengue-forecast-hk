areas <- c("NTS", "NTN", "HKL")

#---------USER INPUTS-------------
divideByArea <- T
area_i <- 3
#---------------------------------

mainTitle <- ifelse(divideByArea, areas[area_i], "All Areas")

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
    row <- area_i + i*3
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

exit() # check y_ci for consistency with the paper if needed

library(plotrix)

if (divideByArea) {
  if (area_i == 3) {
    # special case: HKL
    # modify for graph only (axis break)
    par(xaxt="n")
    plot(y_ci$year, y_ci$observed, xaxt="n",
             panel.first=c(abline(v=seq(2002, 2018), h=seq(0, 27), col="#d3d3d3", lty=2)),
             xlab="Year", ylab="Local Cases", type="l", col="red", lwd=2, ylim=c(0, 27),
             main=mainTitle)
    axis(2, seq(0, 25, 5))
  } else {
    plot(y_ci$year, y_ci$observed, xaxt="n",
             panel.first=c(abline(v=seq(2002, 2018), h=seq(0, 20), col="#d3d3d3", lty=2)),
             xlab="Year", ylab="Local Cases", type="l", col="red", lwd=2, ylim=c(0, 20),
             main=mainTitle)
    axis(2, at=20, labels=20)
  }
  legend("topright", legend=c("observed", "estimated"), col=c("red", "blue"),
         lty=1:2, lwd=2:2, cex=1.1, inset=c(0.02, 0.02))
} else {
  # all fields
  # modify for graph only (axis break)
  par(xaxt="n")
  plot(y_ci$year, y_ci$observed, xaxt="n",
           panel.first=c(abline(v=seq(2002, 2018), h=seq(0, 37), col="#d3d3d3", lty=2)),
           xlab="Year", ylab="Local Cases", type="l", col="red", lwd=2, ylim=c(0, 37),
           main=mainTitle)
  axis(2, seq(0, 35, 5))
  legend("topright", legend=c("observed", "estimated"), col=c("red", "blue"),
         lty=1:2, lwd=2:2, cex=1.1, inset=c(0.06, 0.02))
}
par(xaxt="s")
axis(1, at=seq(2002, 2018, by=2))
lines(y_ci$year, y_ci$estimated, col="blue", lty=5, lwd=2)
arrows(y_ci$year, y_ci$ci_lower, y_ci$year, y_ci$ci_higher, length=0.05, angle=90, code=3, lwd=2)

