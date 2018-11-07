retrieveWeeklyData <- function(field1, field1Type, field2, field2Type, location="CC") {
  if (!require(openxlsx)) install.packages("openxlsx")
  
  fieldLabels <- list(minTemp="Absolute.Daily.Min.(deg.C)",
                      meanTemp="DM",
                      maxTemp="Absolute.Daily.Max.(deg.C)",
                      rainfall="Total.Rainfall.(mm)",
                      humidity="Mean.Relative.Humidity.(%)")
  
  climate <- read.xlsx("../../dat/climate/HKCD.xlsx",
                       sheet=paste("HKCD", location, sep=""),
                       startRow=1, colNames=TRUE, detectDates=TRUE)
  fieldLab1 <- as.character(fieldLabels[field1])
  fieldLab2 <- as.character(fieldLabels[field2])
  
  # Cleaning: remove "#" on every value
  climate[fieldLab1] <- as.numeric(gsub("[^.0-9]", "", climate[fieldLab1][,]))
  climate[fieldLab2] <- as.numeric(gsub("[^.0-9]", "", climate[fieldLab2][,]))
  
  getTotalDays <- function(month, year) {
    oddDaysMonths <- c(1, 3, 5, 7, 8, 10, 12)
    if (month == 2) {
      if (year %% 4 != 0) return(28)
      if (year %% 100 == 0) {
        if (year %% 400 == 0) return(29)
        return(28)
      }
      return(29)
    }
    if (month %in% oddDaysMonths) return(31)
    return(30)
  }
  
  obtainData <- function(df, field, type) {
    val <- 0
    if (type == "min") {
      val <- min(df[[field]], na.rm=TRUE)
    } else if (type == "mean") {
      val <- mean(df[[field]], na.rm=TRUE)
    } else if (type == "max") {
      val <- max(df[[field]], na.rm=TRUE)
    } else {
      val <- sum(df[[field]], na.rm=TRUE)
    }
    return(ifelse(is.na(val), 0, val)) # better way to replace NA?
  }
  
  # convert daily to weekly data (Jan - Aug)
  minYear <- min(climate$Year)
  maxYear <- max(climate$Year)
  dataset <- data.frame()
  
  for (year in minYear:maxYear) {
    curr <- 0
    week <- 0
    weekData <- data.frame()
    for (month in 1:8) {
      days <- getTotalDays(month, year)
      for (day in 1:days) {
        curr <- curr + 1
        row <- climate[climate$Year == year & climate$Month == month & climate$Day == day,]
        weekData <- rbind(weekData, row)
        if (curr %% 7 == 0) { # another week
          week <- week + 1
          field1Data <- obtainData(weekData, fieldLab1, field1Type)
          field2Data <- obtainData(weekData, fieldLab2, field2Type)
          dataset <- rbind(dataset, c(year, week, field1Data, field2Data))
          weekData <- data.frame()
        }
      }
    }
    if (dim(weekData)[1] > 0) {
      week <- week + 1
      field1Data <- obtainData(weekData, fieldLab1, field1Type)
      field2Data <- obtainData(weekData, fieldLab2, field2Type)
      dataset <- rbind(dataset, c(year, week, field1Data, field2Data))
    }
  }
  
  names(dataset) <- c("year", "week", field1, field2)
  return(dataset)
}
