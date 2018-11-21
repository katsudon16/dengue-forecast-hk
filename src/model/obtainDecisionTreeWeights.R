rm(list=ls(all=TRUE))
normalize <- function(ar) {
  maxVal <- max(ar)
  minVal <- min(ar)
  return((ar - minVal) / (maxVal - minVal))
}

createDF <- function(ar) {
  ar <- abs(ar)
  ar <- as.data.frame(ar)
  ar$index <- 0
  for (i in 1:7) {
    ar$index[i] <- i
  }
  names(ar) <- c("value", "index")
  return(ar)
}

createModel <- function(df) {
  model <- lm(value ~ poly(index, 2), data=df)
  return(model)
}

drawChart <- function(model, df, label) {
  plot(fitted(model), type="l", col="red", ylab=label, xlab="Month")
  lines(df$index, df$value, col="blue")
}

tempOri <- c(2.20334,	-3.13133,	2.75299,	-3.64685, 8.43069, -2.44335, 10.84529)
temp <- createDF(tempOri)

rainOri <- c(-0.02854, -0.02039, 0.06764,	-0.04017, -0.03543, -0.01671, 0.0164)
rain <- createDF(rainOri)

tempModel <- createModel(temp)
drawChart(tempModel, temp, "Temperature")
tempWeight <- as.vector(fitted(tempModel))
tempWeight[8] <- predict(tempModel, as.data.frame(list(index=8)))
tempWeight <- normalize(tempWeight)

rainModel <- createModel(rain)
rainWeight <- as.vector(fitted(rainModel))
rainWeight[8] <- predict(rainModel, as.data.frame(list(index=8)))
rainWeight <- normalize(rainWeight)
# plot(tempWeight, col="blue", type="l")
# drawChart(rainModel, rain, "Rainfall")

# weightTemp <- c(0.00000000, 0.00683782, 0.05901541, 0.15653278,
#                 0.29938993, 0.48758684, 0.72112353, 1.00000000)
# weightRain <- c(0.6850413, 0.8914522, 0.9964384, 1.0000000,
#                 0.9021370, 0.7028493, 0.4021370, 0.0000000)
