# by default: use changzhou data

getAnnualRelativeRisk <- function(filePath="../../dat/cases/hk_monthly_cases.csv") {
  allCases <- read.csv(filePath, header=T)
  allCases <- allCases[, c("Month", "Year", "Local.Cases")]
  # clean column titles
  names(allCases)[1] <- "month"
  names(allCases)[2] <- "year"
  names(allCases)[3] <- "localCases"
  allCases$relativeRisk <- 0
  
  avgCases <- 0
  minYear <- min(allCases$year, na.rm=TRUE)
  maxYear <- max(allCases$year, na.rm=TRUE)
  for (year in minYear:maxYear) {
    totalCases <- sum(allCases[allCases$year == year, "localCases"], na.rm=TRUE)
    allCases[allCases$year == year, "relativeRisk"] <- totalCases
    avgCases <- avgCases + totalCases
  }
  avgCases = avgCases / (maxYear - minYear + 1)
  allCases$relativeRisk = allCases$relativeRisk / avgCases
  return(allCases)
}

getCasesByDistrict <- function(district, filePath="../../dat/cases/hk_annual_cases_district.csv") {
  cases <- read.csv(filePath, header=T)
  cases <- cases[, c("year", district)]
  return(cases)
}

getMonthlyTemperatureOnType <- function(type="mean", location="CC", filepath="../../dat/climate/HKCD.xlsx") {
  library(openxlsx)
  allClimates <- read.xlsx(filepath, sheet=paste("HKCD", location, sep=""), startRow=1, colNames=TRUE, detectDates=TRUE)
  allClimates$DM <- as.numeric(gsub("[^.0-9]", "", allClimates$DM))
  if (type == "min") {
    temp <- aggregate(allClimates$DM, list(allClimates$Month, allClimates$Year), min, na.rm=TRUE)
  } else if (type == "mean") {
    temp <- aggregate(allClimates$DM, list(allClimates$Month, allClimates$Year), mean, na.rm=TRUE)
  } else {
    temp <- aggregate(allClimates$DM, list(allClimates$Month, allClimates$Year), max, na.rm=TRUE)
  }
  names(temp)[1] <- "month"
  names(temp)[2] <- "year"
  names(temp)[3] <- "temperature"
  temp$month_txt <- month.abb[temp$month]
  rm(list = c("allClimates"))
  return(temp)
}

getMonthlyRainFall <- function(filepath="../../dat/climate/changzhou_climate(clean).csv") {
  monthlyClimates <- read.csv(filepath, header=T)
  rainfall <- monthlyClimates[,c("month", "year", "totalrain")]
  rainfall$totalrain <- as.numeric(gsub("[^.0-9]", "", rainfall$totalrain))
  rm(list = c("monthlyClimates"))
  return(rainfall)
}

getOvitrapIndex <- function(filepath="data/ovitrap_CC.csv") {
  ovitrap <- read.csv(filepath, header=T)
  names(ovitrap) <- c("year", "month", "percentage")
  ovitrap$month_txt <- month.abb[ovitrap$month]
  return(ovitrap)
}

extractSeasonalData <- function(dataset, field="totalrain", isPreseason=TRUE) {
  # pre season: jan - jun, in season = jul - aug
  temp <- data.frame()
  if (isPreseason) {
    temp <- dataset[dataset$month <= 6,]
  } else {
    temp <- dataset[dataset$month >= 7 & dataset$month <= 8,]
  }
  seasonData <- list()
  seasonData$mean <- mean(temp[[field]], na.rm=TRUE)
  lgFormula <- formula(paste(field, "~ month", sep=""))
  lg <- lm(formula=lgFormula, data=temp)
  seasonData$grad <- lg$coefficients[2]
  return(seasonData)
}