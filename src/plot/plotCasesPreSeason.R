# plot temperature against rainfall in pre-season months (Jan - Jun)
rm(list=ls(all=TRUE))
# install if ggplot2 is not found
if (!require(ggplot2)) install.packages(ggplot2)

source("../lib/retrieveData.R")

# plot using MIN temperature or AVERAGE temperature
temperatureType <- "mean"
temperatureLabel <- paste("Average Temperature (°C)")

# 1. Retrieve data
# ---temperature data---
temperature <- getMonthlyTemperatureOnType(temperatureType)

# ---rain data---
rainfall <- getMonthlyRainFall()

# ---cases data---
allCases <- getAnnualRelativeRisk()

# 2. Build plot_df, consisting of year, # cases, pre_season temperature, pre_season rainfall
plot_df <- data.frame()
minYear <- min(allCases$year, na.rm=TRUE)
maxYear <- max(allCases$year, na.rm=TRUE)
for (year in minYear:maxYear) {
  preseason_rainfall <- mean(rainfall[rainfall$year == year & rainfall$month <= 6,
                                 "totalrain"], na.rm=TRUE)
  preseason_temperature <- mean(temperature[temperature$year == year & temperature$month <= 6,
                                      "temperature"], na.rm=TRUE)
  relativeRisk <- allCases[allCases$year == year & allCases$month == 1, "relativeRisk"]
  plot_df <- rbind(plot_df, c(year, relativeRisk, preseason_rainfall, preseason_temperature))
}
names(plot_df)<-c("YEAR", "CASES", "RAINFALL", "TEMPERATURE")

# 3. Plot data
ggplot(data=plot_df, aes(x=RAINFALL, y=TEMPERATURE)) + 
  geom_point(aes(x=RAINFALL, y=TEMPERATURE, size=CASES), color="red", alpha=0.5) +
  labs(size="Relative Risk") +
  geom_text(aes(label=ifelse(CASES>0, round(CASES, digits=2), '')), size=3, hjust=0, vjust=1) +
  labs(x="Rainfall (mm)") +
  labs(y=temperatureLabel)