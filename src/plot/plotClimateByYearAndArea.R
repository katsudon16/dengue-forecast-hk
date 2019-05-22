rm(list=ls(all=TRUE))

#---------USER INPUTS-------------
saveToFile <- F
outputFile <- "../../figure/compare_temp_2018.tiff"
## temperatureField: "mean", "absMin", "absMax"
temperatureField <- "mean"
## temperatureType: "mean", "max", "min"
temperatureType <- "mean" 
## rainfallType: "total", "max"
rainfallType <- "total"
minYear <- 2018
maxYear <- 2018
areas <- c("NTS", "NTN", "HKL")
showTemperature <- T
ylab <- "The Normalized Mean Temperature\n(2018)"
# ylab <- "The Normalized Total Rainfall\n(2018)"
#---------------------------------
source("../lib/retrieveData.R")
df <- extractAnnualClimateData(temperatureField, temperatureType, rainfallType, areas)

maxs <- apply(df[,c(4:19)], 2, max)
mins <- apply(df[,c(4:19)], 2, min)
df[,c(4:19)] <- scale(df[,c(4:19)], center = mins, scale = maxs - mins)

prefix <- ifelse(showTemperature, "T", "R")
plot_df <- data.frame()
for (area in 1:length(areas)) {
  for (year in minYear:maxYear) {
    row <- df[df$YEAR == year & df$AREA == area,]
    for (month in 3:8) {
      field <- paste(prefix, month, sep="")
      plot_df <- rbind(plot_df, c(area, year, month, row[[field]]))
    }
  }
}
names(plot_df) <- c("area", "year", "month", "value")
plot_df <- aggregate(plot_df$value, list(plot_df$area, plot_df$month), FUN=mean, na.rm=TRUE)
names(plot_df) <- c("area", "month", "value")
plot_df$year <- "other"
plot_df$area <- areas[plot_df$area]

g <- ggplot(data=plot_df, aes(x=month, y=value, group=area)) +
  geom_line(aes(color=area), size=1.2) +
  scale_fill_brewer(palette="Set1") +
  theme(legend.title=element_blank()) +
  ylim(0, 1) + 
  labs(x="Month") +
  labs(y=ylab)

if (saveToFile) {
  ggsave(outputFile, g, units="in", width=5,
         height=4, dpi=300, compression = "lzw")
} else {
  g
}