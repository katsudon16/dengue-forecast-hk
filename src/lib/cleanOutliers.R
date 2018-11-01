cleanOutliersByMonth <- function(df, field, minYear, maxYear, monthField="month", yearField="year") {
  for (month in 1:12) {
    monthGroup <- data.frame()
    for (year in minYear:maxYear) {
      row <- df[df[yearField] == year & df[monthField] == month,]
      monthGroup <- rbind(monthGroup, row)
    }
    stats <- boxplot.stats(monthGroup[[field]])
    avg <- mean(monthGroup[[field]])
    out <- stats$out
    med <- stats$stats[3]
    df[df[[monthField]] == month & df[[field]] %in% out, field] <- med
  }
  return(df)
}



# test <- cleanOutliersByMonth(ccData, "totalrain", 2002, 2018)
# 
# ccData$index <- 0
# test$index <- 0
# curr <- 0
# for (i in 1:length(ccData$totalrain)) {
#   curr <- curr + 1
#   test$index[i] <- curr
#   ccData$index[i] <- curr
# }
# 
# ggplot() +
#   geom_line(data=ccData, aes(x=index, y=totalrain), colour="blue") +
#   geom_line(data=test, aes(x=index, y=totalrain), colour="red")