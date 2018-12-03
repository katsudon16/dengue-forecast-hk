# Compare climate data using time series plot

rm(list=ls(all=TRUE))
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(ggplot2)) install.packages("ggplot2")

#---------USER INPUTS-------------
isDistrict <- TRUE # district or area
## choose one location from:
# districts: "SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK"
# areas    : "NTW", "NTS", "NTE", "KL", "HK"
loc1 <- "SLW"  # blue
loc2 <- "TY" # red
title <- paste(loc1, " (blue) vs ", loc2, " (red)", sep="")
# field = "rainfall"/"meantemp"/"mintemp"/"maxtemp"
field <- "rainfall"
# method = "avg_manhattan"/"euclidean"
distanceMethod <- "avg_manhattan"
#---------------------------------

filename <- ifelse(field == "rainfall", "HKCM", "HKCD")
if (!isDistrict) {
  filename <- paste(filename, "_areas", sep="")
}
data1 <- read.xlsx(paste("../../dat/climate/", filename, ".xlsx", sep=""),
                   sheet=ifelse(filename == "HKCM" | filename == "HKCM_areas", loc1, paste("HKCD", loc1, sep="")),
                   startRow=1, colNames=TRUE, detectDates=TRUE)
data2 <- read.xlsx(paste("../../dat/climate/", filename, ".xlsx", sep=""),
                   sheet=ifelse(filename == "HKCM" | filename == "HKCM_areas", loc2, paste("HKCD", loc2, sep="")),
                   startRow=1, colNames=TRUE, detectDates=TRUE)
fieldLabels <- list(maxtemp="Absolute.Daily.Max.Temperature",
                    meantemp="Daily.Mean.Temperature",
                    mintemp="Absolute.Daily.Min.Temperature",
                    rainfall="totalrain")
fieldLabel <- as.character(fieldLabels[field])

# Cleaning: remove "#" on every value
data1[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", data1[fieldLabel][,]))
data2[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", data2[fieldLabel][,]))

data1$index <- 0
data2$index <- 0

totalDist <- 0
totalValid <- 0
for (i in 1:dim(data1)[1]) {
  data1[i, "index"] <- i
  data2[i, "index"] <- i
  x <- data1[i, fieldLabel]
  y <- data2[i, fieldLabel]
  if (is.na(x) | is.na(y)) next
  totalValid <- totalValid + 1
  if (distanceMethod == "avg_manhattan") {
    totalDist <- totalDist + abs(x - y)
  } else {
    totalDist <- totalDist + (x - y) * (x - y)
  }
}

ggplot() +
  ggtitle(title) +
  geom_line(data=data1, aes(x=index, y=get(fieldLabel), group=1), colour="blue") +
  geom_line(data=data2, aes(x=index, y=get(fieldLabel), group=1), colour="red") +
  labs(x="Time") +
  labs(y="Data")

if (distanceMethod == "avg_manhattan") {
  totalDist <- totalDist / totalValid
} else {
  totalDist <- sqrt(totalDist)
}
cat("Total distance: ", totalDist, "\n")