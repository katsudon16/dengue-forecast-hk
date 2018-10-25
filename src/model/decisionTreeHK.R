# Decision tree by merging all variables by month
rm(list=ls(all=TRUE))

# Local variables
CLIMATE_CSV_FILE <- "../../dat/climate/changzhou_climate(clean).csv"
CASES_CSV_FILE <- "../../dat/cases/hk_monthly_cases.csv"
MIN_HIGH_RISK_VALUE <- 0.5
HIGH_RISK <- 1
MIN_MED_RISK_VALUE <- 0.2
MED_RISK <- 0

normalizeAbs <- function(array) {
  # method 1: if val < 0 or NA, val = 0 else val
  # for (i in 1:length(array)) {
  #   if (is.na(array[i]) | array[i] < 0) {
  #     array[i] = 0
  #   }
  # }
  # method 2: val = |val|
  array <- abs(array)
  sum <- sum(array)
  return(array / sum)
}

if (!require("party")) install.packages("party")
# 1. gather data
source("../lib/retrieveData.R")
# collect (monthly): annual relative risk, avg temperature, total rainfall
##   i) collect annual relative risk per month
annualRisk <- getAnnualRelativeRisk(CASES_CSV_FILE)

##  ii) collect and clean avg monthly temperature and rainfall
ccData <- read.csv(CLIMATE_CSV_FILE)
ccData$totalrain <- as.numeric(gsub("[^.0-9]", "", ccData$totalrain))
ccData$avg <- as.numeric(gsub("[^.0-9]", "", ccData$avg))

## iii) collect weights
weightTemp <- c(0.1, 0.3, 0.5, 1, 1, 1, 1)
weightTemp <- normalizeAbs(weightTemp)
weightRain <- c(0.1, 0.3, 0.5, 1, 1, 1, 1)
weightRain <- normalizeAbs(weightRain)

##  iv) merge them into 1 table
minYear <- min(annualRisk$year, na.rm=TRUE)
maxYear <- max(annualRisk$year, na.rm=TRUE)
dataset <- data.frame(stringsAsFactors=FALSE)
curr <- 0

for (year in minYear:maxYear) {
  for (month in 1:7) {
    curr <- curr + 1
    if (length(annualRisk[annualRisk$month == month & annualRisk$year == year,]) == 0) break
    avgWeight <- (weightTemp[month] + weightRain[month]) / 2
    weightedRisk <- avgWeight * annualRisk[annualRisk$month == month & annualRisk$year == year, "relativeRisk"]
    discreteRisk <- 0
    if (weightedRisk > MIN_HIGH_RISK_VALUE) {
      discreteRisk <- HIGH_RISK
    } else if (weightedRisk > MIN_MED_RISK_VALUE) {
      discreteRisk <- MED_RISK
    }
    row <- c(curr,
             ccData[ccData$month==month & ccData$year==year, "avg"],
             ccData[ccData$month==month & ccData$year==year, "totalrain"],
             weightTemp[month],
             weightRain[month],
             avgWeight,
             annualRisk[annualRisk$month == month & annualRisk$year == year, "relativeRisk"],
             weightedRisk,
             discreteRisk)
    print(row)
    dataset <- rbind(dataset, row)
    
  }
}

namesVector <- c("month", "temperature", "rainfall", "weightTemp", "weightRain", "avgWeight", "relativeRisk", "weightedRisk", "discreteRisk")
names(dataset) <- namesVector
rm(list = c("row", "avgWeight", "weightedRisk", "discreteRisk",
            "weightRain", "weightTemp", "minYear", "maxYear"))

# 2a. create decision tree
library(rpart)
library(rpart.plot)
dataset$discreteRisk <- ifelse(dataset$discreteRisk == 0, "low", "high")
dataset <- dataset[dataset$rainfall >= 10,]

tree <- rpart(discreteRisk ~ temperature + rainfall,
              data=dataset[c(1:dim(dataset)[1]),],
              method="class",
              control=rpart.control(minsplit=3, maxdepth=4))
# plotting tree; NOTE: comment the code out when building distribution plot
rpart.plot(tree, yesno=2, roundint=FALSE, extra=8,
           box.palette=list("Reds", "Blues"), main="Degree of Risk Distribution")

# plotting distribution; Note: comment the code out when building decision tree plot
# library(plotmo)
# plotmo(tree, type= "prob", nresponse = "low", # right graph
#        type2 = "image", ngrid2 = 200, image.col=grey(7:9/10), # type2 = "image" for an image plot
#        pt.col = ifelse(dataset$discreteRisk == "low", "blue", "red"),
#        main="Degree of Risk Distribution",
#        xlab="Temperature  (°C)",
#        ylab="Rainfall (mm)")