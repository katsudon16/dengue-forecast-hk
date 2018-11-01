# plot temperature against rainfall in in-season months (Jul - Aug)
rm(list=ls(all=TRUE))
# install if ggplot2 is not found
if (!require(ggplot2)) install.packages(ggplot2)

source("../lib/retrieveData.R")

# plot using MIN temperature or AVERAGE temperature
temperatureType <- "mean"
district <- "CC"
temperatureLabel <- paste("Average Temperature (°C)")

# 1. Retrieve data
# ---temperature data---
temperature <- getMonthlyTemperatureOnType(temperatureType, district)

# ---rain data---
rainfall <- getMonthlyRainFall()

# ---cases data---
cases <- getCasesByDistrict(district)

# 2. Build plot_df, consisting of year, # cases, pre_season temperature, pre_season rainfall
plot_df <- data.frame()
minYear <- min(cases$year, na.rm=TRUE)
maxYear <- max(cases$year, na.rm=TRUE)
for (year in minYear:maxYear) {
  inseason_rainfall <- mean(rainfall[rainfall$year == year & rainfall$month > 6 & rainfall$month <= 8,
                                      "totalrain"], na.rm=TRUE)
  inseason_temperature <- mean(temperature[temperature$year == year & temperature$month > 6 & temperature$month <= 8,
                                            "temperature"], na.rm=TRUE)
  totalCases <- cases[cases$year == year, district]
  plot_df <- rbind(plot_df, c(year, totalCases, inseason_rainfall, inseason_temperature))
}
names(plot_df)<-c("YEAR", "CASES", "RAINFALL", "TEMPERATURE")

# 3. Plot data
ggplot(data=plot_df, aes(x=RAINFALL, y=TEMPERATURE)) + 
  geom_point(aes(x=RAINFALL, y=TEMPERATURE, size=CASES), color="red", alpha=0.5) +
  labs(size="Relative Risk") +
  geom_text(aes(label=ifelse(CASES>0, round(CASES, digits=2), "")), size=3, hjust=0, vjust=1) +
  labs(x="Rainfall (mm)") +
  labs(y=temperatureLabel)
  
