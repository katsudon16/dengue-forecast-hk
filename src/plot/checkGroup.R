rm(list=ls(all=TRUE))
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(ggplot2)) install.packages("ggplot2")

#---------USER INPUTS-------------
year <- 2011
field <- "tempMean"
district1 <- "WCH" # blue
district2 <- "HP" # red
maxDiff <- 1
#---------------------------------

getData <- function(district) {
  return (read.xlsx("../../dat/climate/HKCD.xlsx",
                    sheet=paste("HKCD", district, sep=""),
                    startRow=1, colNames=TRUE, detectDates=TRUE))
}

fieldLabels <- list(tempMin="Absolute.Daily.Min.(deg.C)",
                    tempMean="DM",
                    tempMax="Absolute.Daily.Max.(deg.C)",
                    humidity="Mean.Relative.Humidity.(%)",
                    rainfall="Total.Rainfall")
fieldLabel <- as.character(fieldLabels[field])

data1 <- getData(district1)
data2 <- getData(district2)

# extractData1 <- data1[data1$Year == year,]
# extractData2 <- data2[data2$Year == year,]
# 
# extractData1[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", extractData1[fieldLabel][,])) 
# extractData2[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", extractData2[fieldLabel][,])) 
# 
# totalDays <- dim(extractData1)[1]
# if (totalDays != dim(extractData2)[1]) {
#   print("Invalid data; #days are different...")
#   print(dim(extractData1)[1])
#   print(dim(extractData2)[1])
#   quit()
# }
# 
# days <- c(1:totalDays)
# 
# cbind(extractData1, days)
# cbind(extractData2, days)
# 
# ggplot() +
#   geom_line(data=extractData1, aes(x=days, y=get(fieldLabel), group=1), colour="blue") +
#   geom_line(data=extractData2, aes(x=days, y=get(fieldLabel), group=1), colour="red") +
#   labs(x="Day") +
#   labs(y=fieldLabel)

cleanValue <- function(x) {
  return(as.numeric(gsub("[^.0-9]", "", x)))
}

getAverage <- function(a, b) {
  a = cleanValue(a)
  b = cleanValue(b)
  if (is.na(a)) return(b)
  if (is.na(b)) return(a)
  return ((a + b)/2)
}

minYear <- min(data1$Year, data2$Year, na.rm=TRUE)
maxYear <- max(data1$Year, data2$Year, na.rm=TRUE)
dataset <- data.frame(stringsAsFactors=FALSE)

totalDiff <- 0
test <- 0

for (year in minYear:maxYear) {
  for (month in 1:12) {
    for (day in 1:31) {
      data1row <- data1[data1$Year == year & data1$Month == month & data1$Day == day, ]
      data2row <- data2[data2$Year == year & data2$Month == month & data2$Day == day, ]
      if (dim(data1row)[1] == 0 | dim(data2row)[1] == 0) {
        next
      }
      val1 <- cleanValue(data1row[fieldLabel])
      val2 <- cleanValue(data2row[fieldLabel])
      absDiff <- abs(val1 - val2)
      if (is.na(absDiff)) next
      test <- test + 1
      if (absDiff >= maxDiff) {
        totalDiff <- totalDiff + 1
      }
    }
  }
}

print(totalDiff)
print(test)
quit()

# cleanValue <- function(x) {
#   return(as.numeric(gsub("[^.0-9]", "", x)))
# }
# 
# getAverage <- function(a, b) {
#   a = cleanValue(a)
#   b = cleanValue(b)
#   if (is.na(a)) return(b)
#   if (is.na(b)) return(a)
#   return ((a + b)/2)
# }
# 
# minYear <- min(data1$Year, data2$Year, na.rm=TRUE)
# maxYear <- max(data1$Year, data2$Year, na.rm=TRUE)
# dataset <- data.frame(stringsAsFactors=FALSE)
# 
# for (year in minYear:maxYear) {
#   for (month in 1:12) {
#     for (day in 1:31) {
#       data1row <- data1[data1$Year == year & data1$Month == month & data1$Day == day, ]
#       data2row <- data2[data2$Year == year & data2$Month == month & data2$Day == day, ]
#       if (dim(data1row)[1] == 0 & dim(data2row)[1] == 0) {
#         next
#       }
#       # if only one of them has the data
#       if (dim(data1row)[1] == 0) {
#         row <- cleanValue(data2row)
#       } else if (dim(data2row)[1] == 0) {
#         row <- cleanValue(data1row)
#       } else {
#         # both have data
#         row <- c(year, month, day,
#                  getAverage(data1row$`Mean.Pressure.(hPa)`, data2row$`Mean.Pressure.(hPa)`),
#                  getAverage(data1row$`Absolute.Daily.Max.(deg.C)`, data2row$`Absolute.Daily.Max.(deg.C)`),
#                  getAverage(data1row$DM, data2row$DM),
#                  getAverage(data1row$`Absolute.Daily.Min.(deg.C)`, data2row$`Absolute.Daily.Min.(deg.C)`),
#                  getAverage(data1row$`Mean.Dew.Point.(deg..C)`, data2row$`Mean.Dew.Point.(deg..C)`),
#                  getAverage(data1row$`Mean.Relative.Humidity.(%)`, data2row$`Mean.Relative.Humidity.(%)`),
#                  getAverage(data1row$`Total.Rainfall.(mm)`, data2row$`Total.Rainfall.(mm)`),
#                  getAverage(data1row$`Prevailing.Wind.Direction.(degrees)`, data2row$`Prevailing.Wind.Direction.(degrees)`),
#                  getAverage(data1row$`Mean.Wind.(km/h)`, data2row$`Mean.Wind.(km/h)`))
#       }
#       dataset <- rbind(dataset, row)
#     }
#   }
# }
# 
# names(dataset) <- c("Year", "Month", "Day",
#                     "Mean.Pressure.(hPa)",
#                     "Absolute.Daily.Max.(deg.C)",
#                     "DM",
#                     "Absolute.Daily.Min.(deg.C)",
#                     "Mean.Dew.Point.(deg..C)",
#                     "Mean.Relative.Humidity.(%)",
#                     "Total.Rainfall.(mm)",
#                     "Prevailing.Wind.Direction.(degrees)",
#                     "Mean.Wind.(km/h)")
