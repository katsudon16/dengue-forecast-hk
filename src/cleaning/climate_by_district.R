rm(list=ls(all=TRUE))
if (!require(openxlsx)) install.packages("openxlsx")

#---------USER INPUTS-------------
district <- "TY"
minYear <- 2002
maxYear <- 2018
#---------------------------------

districts <- c("SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK")

data <- read.xlsx("../../dat/climate/HKCD(clean).xlsx",
                  sheet=paste("HKCD", district, sep=""),
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

examine <- function(field) {
  fieldLabel <- as.character(fieldLabels[field])
  
  # Cleaning: remove "#" on every value
  data[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", data[fieldLabel][,]))
  
  dataMinYear <- min(data$Year)
  dataMaxYear <- max(data$Year)
  
  if (dataMinYear != minYear) {
    cat(red("The minimum year in the data is", dataMinYear))
  }
  
  firstFound <- FALSE
  firstFoundData <- c()
  lostValue <- 0
  
  for (year in dataMinYear:dataMaxYear) {
    maxMonth <- 12
    if (year == dataMaxYear) {
      maxMonth <- 8
    }
    for (month in 1:maxMonth) {
      totalDays <- getTotalDays(month, year)
      for (day in 1:totalDays) {
        row <- data[data$Year == year & data$Month == month & data$Day == day,]
        if (dim(row)[1] == 0 | is.na(row[fieldLabel])) {
          if (firstFound) {
            lostValue <- lostValue + 1
            cat(red("Lost...", row[1], row[2], row[3], "\n"))}
        } else {
          if (firstFound == FALSE) {
            firstFound <- TRUE
            firstFoundData <- c(year=year, month=month, day=day)
          }
        }
      }
    }
  }
  cat(green("\n *", field, "\n"))
  cat(yellow("First found data is", firstFoundData[1], firstFoundData[2], firstFoundData[3], "\n"))
  cat(yellow("Total lost values are", lostValue, "\n"))
}



for (field in c("tempMin", "tempMean", "tempMax", "humidity", "rainfall")) {
  examine(field)
}


# ------ summary ------
# SLW = clean (correlation), rainfall - 3
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
