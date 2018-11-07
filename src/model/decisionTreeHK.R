# Decision tree by merging all variables by month
rm(list=ls(all=TRUE))

#---------USER INPUTS-------------
CLIMATE_CSV_FILE <- "../../dat/climate/changzhou_climate(clean).csv"
CASES_CSV_FILE <- "../../dat/cases/hk_monthly_cases.csv"
MIN_HIGH_RISK_VALUE <- 0.48
HIGH_RISK <- 2
MIN_MED_RISK_VALUE <- 0.2
MED_RISK <- 0

# action = - "point" (to plot points in 2D)
#          - "tree" (draw a tree)
#          - "tree-dist" (draw the tree distribution in 2D)
action <- "tree-dist"
#---------------------------------

normalizeAbs <- function(array) {
  # method 1: if val < 0 or NA, val = 0 else val
  # for (i in 1:length(array)) {
  #   if (is.na(array[i]) | array[i] < 0) {
  #     array[i] = 0
  #   }
  # }
  
  # method 2: val = |val|
  # array <- abs(array)
  # sum <- sum(array)
  # return(array / sum)
  
  # method 3: min-max scaling
  maxVal <- max(array)
  minVal <- min(array)
  return((array - minVal) / (maxVal - minVal))
}

if (!require("party")) install.packages("party")
# 1. gather data
source("../lib/retrieveData.R")
# collect (monthly): annual relative risk, avg temperature, total rainfall
##   i) collect annual relative risk per month
annualRisk <- getAnnualRelativeRisk(CASES_CSV_FILE)

##  ii) collect and clean avg monthly temperature and rainfall
ccData <- read.csv(CLIMATE_CSV_FILE)
# ccData$totalrain <- as.numeric(gsub("[^.0-9]", "", ccData$totalrain))
totalRain <- getMonthlyRainfallOnType("max")

# for (i in 1:length(ccData$avg) - 1) {
#   ccData$totalrain[i] <- totalRain[totalRain$month == ccData$month[i] & totalRain$year == ccData$year[i], "rainfall"]
# }

ccData$avg <- as.numeric(gsub("[^.0-9]", "", ccData$avg))


## iii) collect weights
# get temperature weight
annualTempAvg <- aggregate(ccData$avg, list(ccData$month), mean, na.rm=TRUE)
weightTemp <- normalizeAbs(annualTempAvg[,2])
annualRainAvg <- aggregate(ccData$totalrain, list(ccData$month), mean, na.rm=TRUE)
weightRain <- normalizeAbs(annualRainAvg[,2])

weightTemp <- c(0.00000000, 0.00683782, 0.05901541, 0.15653278,
                0.29938993, 0.48758684, 0.72112353, 1.00000000)
weightRain <- c(0.6850413, 0.8914522, 0.9964384, 1.0000000,
                0.9021370, 0.7028493, 0.4021370, 0.0000000)

##  iv) merge them into 1 table
minYear <- min(annualRisk$year, na.rm=TRUE)
maxYear <- max(annualRisk$year, na.rm=TRUE)
dataset <- data.frame(stringsAsFactors=FALSE)
curr <- 0

for (year in minYear:maxYear) {
  for (month in 1:8) {
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
    row <- c(year, month, curr,
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

namesVector <- c("year", "realMonth", "month", "temperature", "rainfall", "weightTemp", "weightRain", "avgWeight", "relativeRisk", "weightedRisk", "discreteRisk")
names(dataset) <- namesVector
# rm(list = c("row", "avgWeight", "weightedRisk", "discreteRisk",
#             "weightRain", "weightTemp", "minYear", "maxYear"))

dataset$discreteRisk <- ifelse(dataset$discreteRisk == 0, "low",
                               ifelse(dataset$discreteRisk == 1, "med", "high"))

# 2a. create decision tree
if (!require("rpart")) install.packages("rpart")
if (!require("rpart.plot")) install.packages("rpart.plot")
dataset <- dataset[dataset$rainfall >= 10,]

if (action == "point") {
  require(ggplot2)
  ggplot(data=dataset, aes(x=temperature, y=rainfall)) +
    geom_point(aes(x=temperature, y=rainfall, size=weightedRisk, colour=cut(year, c(2001, 2002, 2017, 2018))), alpha=0.5) +
    labs(size="Relative Risk") +
    geom_text(aes(label=ifelse(weightedRisk>0, round(weightedRisk, digits=2), '')), size=3, hjust=0, vjust=1) +
    scale_color_manual(name = "Years",
                       values = c("(2001,2002]" = "Green",
                                  "(2002,2017]" = "Black",
                                  "(2017,2018]" = "Red"),
                       labels = c("2002", "2003-2017", "2018")) +
    labs(x="Temperature") +
    labs(y="Rainfall (mm)")
} else {
  tree <- rpart(discreteRisk ~ temperature + rainfall,
                data=dataset[c(1:dim(dataset)[1]),],
                method="class",
                control=rpart.control(minsplit=3, maxdepth=3))
  
  if (action == "tree") {
    rpart.plot(tree, yesno=2, roundint=FALSE, extra=101,
               box.palette=list("Reds", "Blues", "Greens"), main="Degree of Risk Distribution")
  } else {
    if (!require("plotmo")) install.packages("plotmo")
    plotmo(tree, type= "prob", nresponse = "low", # right graph
           type2 = "image", ngrid2 = 200, image.col=grey(7:9/10), # type2 = "image" for an image plot
           pt.col = ifelse(dataset$discreteRisk == "low", "blue", ifelse(dataset$discreteRisk == "med", "green", "red")),
           main="Degree of Risk Distribution",
           xlab="Temperature  (°C)",
           ylab="Rainfall (mm)")
  }
}