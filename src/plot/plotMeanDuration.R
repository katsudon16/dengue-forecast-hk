rm(list=ls(all=TRUE))

#------- user input ------
field1 <- "maxTemp"
field1Type <- "mean"
field2 <- "humidity"
field2Type <- "max"
location <- "CC"
#-------------------------

source("../lib/convertDailyToWeekly.R")
source("../lib/retrieveData.R")

data <- retrieveWeeklyData(field1, field1Type, field2, field2Type)
cases <- getAnnualRelativeRisk()

# temperature (mean)= 9.2 - 29.72
# rainfall   (total)= 0 - 527.5
# humidity    (mean)= 55 - 98
# rainfall    (mean)= 0 - 75

# choosing the threshold
tempThreshList <- c(9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29)
humiThreshList <- c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95)
rainThreshList <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500)
rainMeanThreshList <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)

#------- user input ------
temp_i <- 11
humi_i <- 10
rain_i <- 3
rainMean_i <- 3
#-------------------------

tempThresh <- tempThreshList[temp_i]
humiThresh <- humiThreshList[humi_i]
rainThresh <- rainThreshList[rain_i]
rainMeanThresh <- rainMeanThreshList[rainMean_i]

#----- manual input ------
# TODO: make it automatic
field1Thresh <- tempThresh
field2Thresh <- humiThresh
#-------------------------

eval <- data.frame()
minYear <- min(data$year)
maxYear <- max(data$year)

for (year in minYear:maxYear) {
  durationList <- c()
  maxWeek <- max(data[data$year == year, "week"])
  temp <- 0
  for (week in 1:maxWeek) {
    row <- data[data$year == year & data$week == week,]
    if (row[[field1]] >= field1Thresh & row[[field2]] >= field2Thresh) {
      temp <- temp + 1
      if (week == maxWeek) {
        durationList[length(durationList) + 1] <- temp
      }
      next
    } else {
      if (temp > 0) {
        durationList[length(durationList) + 1] <- temp
      }
      temp <- 0
    }
  }
  avgDuration <- ifelse(length(durationList) == 0, 0, mean(durationList))
  # totalCases <- sum(cases[cases$year == year, "relativeRisk"])
  totalCases <- cases[cases$year == year, "relativeRisk"][1]
  eval <- rbind(eval, c(year, avgDuration, totalCases))
}

names(eval) <- c("year", "meanDuration", "totalCases")

plot(eval$meanDuration, eval$totalCases)

