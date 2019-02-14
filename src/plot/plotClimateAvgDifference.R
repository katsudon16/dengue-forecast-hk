rm(list=ls(all=TRUE))
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(ggplot2)) install.packages("ggplot2")

#---------USER INPUTS-------------
shouldOutputFigure <- T
outputFile <- "../../figure/compare_rain_mid.tiff"
outputPlotWidth <- 4
outputPlotHeight <- 6
## choose temperature field between "absMin", "mean", "absMax", "rain"
field <- "rain"
fieldPlotLabel <- "Monthly Total Rain (mm)"
# fieldPlotLabel <- "Monthly Average Temperature (°C)"
## aggregate type, could be "min", "mean", "max", or "sum"
aggregateType <- "sum"
## choose a list of location from: (the code will automatically detect district/area)
# districts: "SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK"
# areas    : "NTN", "NTS", "KL", "HK", "HKL"
# the plots will be divided into grids
locations <- c("NTS", "NTN", "HKL")
gridRowNum <- 1 # number of grid rows
#---------------------------------

y_diff <- ifelse(field == "rain", 20, 1)

# plot's y axis limit
plotYmax <- -1
plotYmin <- 1000

df <- data.frame()
allDistricts <- c("SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK", "KP")
allAreas <- c("NTS", "NTN", "KL", "HK", "HKL", "ALL")

fieldLabels <- list(absMax="Absolute.Daily.Max.Temperature",
                    mean="Daily.Mean.Temperature",
                    absMin="Absolute.Daily.Min.Temperature",
                    rain="Total.Rainfall.(mm)")
fieldLabel <- as.character(fieldLabels[field])

for (loc in locations) {
  excelFilename <- ifelse(loc %in% allDistricts, "HKCD", "HKCD_areas")
  data <- read.xlsx(paste("../../dat/climate/", excelFilename, ".xlsx", sep=""),
                    sheet=paste("HKCD", loc, sep=""),
                    startRow=1, colNames=TRUE, detectDates=TRUE)
  
  # Cleaning: remove "#" on every value
  data[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", data[fieldLabel][,]))
  
  temp = aggregate(data[fieldLabel], list(data$Month,data$Year), FUN=aggregateType, na.rm=TRUE)
  names(temp)[1] = "Month"
  names(temp)[2] = "Year"
  names(temp)[3] = fieldLabel
  # temp <- temp[temp$Year == 2018,]
  temp <- temp[temp$Year > 2002 & temp$Year < 2018,]
  temp[[fieldLabel]] <- ifelse(is.infinite(temp[[fieldLabel]]), NA, temp[[fieldLabel]])
  temp = aggregate(temp[fieldLabel], list(temp$Month), FUN=mean, na.rm=TRUE)
  names(temp)[1] = "Month"
  names(temp)[2] = fieldLabel
  temp$Area <- loc
  
  df <- rbind(df, temp)
  rm(data, temp)
}
plotYmax <- ceiling(max(df[[fieldLabel]]))
plotYmin <- floor(min(df[[fieldLabel]]))

g <- ggplot(data=df, aes(x=Month, y=get(fieldLabel), group=Area)) +
  geom_line(aes(color=Area), size=1.2) +
  scale_x_continuous(breaks=seq(1, 8, 1), limits=c(1, 8)) +
  scale_y_continuous(breaks=seq(0, 750, 25), limits=c(0, 750)) +
  # scale_y_continuous(breaks=seq(15, 30, 1), limits=c(15, 30)) +
  # scale_y_continuous(breaks=seq(plotYmin, plotYmax, y_diff), limits=c(plotYmin, plotYmax)) +
  scale_fill_brewer(palette="Set1") +
  theme(legend.title=element_blank()) +
  labs(x="Month") +
  labs(y=fieldPlotLabel)

if (shouldOutputFigure) {
  ggsave(outputFile, g, units="in", width=outputPlotWidth,
         height=outputPlotHeight, dpi=300, compression = "lzw")
} else {
  g
}