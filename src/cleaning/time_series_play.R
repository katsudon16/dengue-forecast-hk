rm(list=ls(all=TRUE))
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("TSA")) install.packages("TSA")

districts <- c("SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK")

#---------USER INPUTS-------------
district <- "CC"
field <- "tempMean"
#---------------------------------

data <- read.xlsx("../../dat/climate/HKCD(clean).xlsx",
                  sheet=paste("HKCD", district, sep=""),
                  startRow=1, colNames=TRUE, detectDates=TRUE)
fieldLabels <- list(tempMin="Absolute.Daily.Min.(deg.C)",
                    tempMean="DM",
                    tempMax="Absolute.Daily.Max.(deg.C)",
                    humidity="Mean.Relative.Humidity.(%)",
                    rainfall="Total.Rainfall.(mm)")

fieldLabel <- as.character(fieldLabels[field])

getTotalDays <- function(month, year = 0) {
  oddMonths <- c(1, 3, 5, 7, 8, 10, 12)
  if (month %in% oddMonths) return(31)
  if (month == 2) {
    if (year %% 100 == 0) {
      if (year %% 400 == 0) return(29)
      return(28)
    }
    if (year %% 4 == 0) return(29)
    return(28)
  }
  return(30)
}

minYear <- min(data$Year)
maxYear <- max(data$Year)
data$fullDate <- ""

for (year in minYear:maxYear) {
  for (month in 1:12) {
    days <- getTotalDays(month, year)
    for (day in 1:days) {
      fullDate <- paste(day, month, year, sep="/")
      data[data$Year == year & data$Month == month & data$Day == day, "fullDate"] <- fullDate
    }
  }
}

tsdata <- data[, c("fullDate", fieldLabel)]

library(zoo)
test <- read.zoo(tsdata, format="%d/%m/%Y")
# xtsdata <- xts(tsdata[fieldLabel], order.by=as.Date(tsdata$fullDate, "%d/%m/%Y"), frequency=7)


