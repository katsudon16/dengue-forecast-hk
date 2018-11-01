rm(list=ls(all=TRUE))
if (!require(openxlsx)) install.packages("openxlsx")
library("crayon")

#---------USER INPUTS-------------
district <- "SLW"
correlatedDistrict <- "IA"
minYear <- 2002
maxYear <- 2018
#---------------------------------

districts <- c("SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK")

data <- read.xlsx("../../dat/climate/HKCD.xlsx",
                  sheet=paste("HKCD", district, sep=""),
                  startRow=1, colNames=TRUE, detectDates=TRUE)
cData <- read.xlsx("../../dat/climate/HKCD.xlsx",
                   sheet=paste("HKCD", correlatedDistrict, sep=""),
                   startRow=1, colNames=TRUE, detectDates=TRUE)
fieldLabels <- list(tempMin="Absolute.Daily.Min.(deg.C)",
                    tempMean="DM",
                    tempMax="Absolute.Daily.Max.(deg.C)",
                    humidity="Mean.Relative.Humidity.(%)",
                    rainfall="Total.Rainfall.(mm)")

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

for (field in c("tempMin", "tempMean", "tempMax", "humidity", "rainfall")) {
  fieldLabel <- as.character(fieldLabels[field])
  
  # Cleaning: remove "#" on every value
  data[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", data[fieldLabel][,]))
  cData[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", cData[fieldLabel][,]))
  
  data$correlated <- cData[,fieldLabel]
  
  model <- lm(get(fieldLabel) ~ correlated, data=data, na.action=na.omit)
  coef <- model$coefficients[2]
  er <- model$coefficients[1]
  
  cat(green(cor(data[[fieldLabel]], data$correlated, use="complete.obs")))
  
  dataMinYear <- min(data$Year)
  dataMaxYear <- max(data$Year)
  
  if (dataMinYear != minYear) {
    cat(red("The minimum year in the data is", dataMinYear))
  }
  
  firstFound <- FALSE
  firstFoundData <- c()
  lostValue <- 0
  cat(yellow("\n *", field, "\n"))
  for (year in dataMinYear:dataMaxYear) {
    maxMonth <- 12
    if (year == dataMaxYear) {
      maxMonth <- 8
    }
    for (month in 1:maxMonth) {
      totalDays <- getTotalDays(month, year)
      for (day in 1:totalDays) {
        row <- data[data$Year == year & data$Month == month & data$Day == day,]
        if (is.na(row[fieldLabel]) == FALSE) next
        if (is.na(row$correlated)) next
        # cat(yellow(row$Year, row$Month, row$Day, "\n"))
        data[data$Year == year & data$Month == month & data$Day == day, fieldLabel] <- coef * row$correlated + er
      }
    }
  }
}

# ------ summary ------
# SLW = 
# TY  = 
# TKL = 
# SK  = 
# ST  = 
# TP  = 
# TM
# YL
# CC
# TC
# HK
