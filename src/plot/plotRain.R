rm(list=ls(all=TRUE))
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(ggplot2)) install.packages(ggplot2)
if (!require(gridExtra)) install.packages("gridExtra")

#---------USER INPUTS-------------
shouldOutputFigure <- T
outputFile <- "../../figure/area_rain_all_max.tiff"
outputPlotWidth <- 4
outputPlotHeight <- 3
## choose one location from: (the code will automatically detect district/area)
# districts: "SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK"
# areas    : "NTN", "NTS", "KL", "HK", "HKL"
locations <- c("ALL")
## aggregate type, could be "max" or "sum"
aggregateType <- "max"
fieldPlotLabel <- "Monthly Total Rainfall (mm)"
gridRowNum <- 1 # number of grid rows
#---------------------------------
# plot's y axis limit
plotYmax <- -1
plotYmin <- 1000

grids <- c()
allDistricts <- c("SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK")
allAreas <- c("NTN", "NTS", "KL", "HK", "HKL", "ALL")
jitter <- position_jitter(width=0.25, height=0)

for (loc in locations) {
  excelFilename <- ifelse(loc %in% allDistricts, "HKCD", "HKCD_areas")
  data <- read.xlsx(paste("../../dat/climate/", excelFilename, ".xlsx", sep=""),
                    sheet=paste("HKCD", loc, sep=""),
                    startRow=1, colNames=TRUE, detectDates=TRUE)
  
  # Cleaning: remove "#" on every value in specific columns
  data$rain <- as.numeric(gsub("[^.0-9]", "", data$`Total.Rainfall.(mm)`))
  rain <- aggregate(data$rain, list(data$Month,data$Year), FUN=aggregateType, na.rm=TRUE)
  names(rain)[1] = "Month"
  names(rain)[2] = "Year"
  names(rain)[3] = "Rain"
  rain$month_txt <- month.abb[rain$Month]
  
  grids[[loc]] <- rain
  plotYmax <- max(plotYmax, rain$Rain)
  plotYmin <- min(plotYmin, rain$Rain)
}

commands <- ifelse(shouldOutputFigure, "arrangeGrob(", "grid.arrange(")
for (loc in locations) {
  grids[[loc]] <- ggplot(data=grids[[loc]], aes(x=month_txt, y=Rain)) +
    ylim(plotYmin, plotYmax) +
    ggtitle(loc) +
    geom_boxplot(aes(month_txt, Rain), outlier.shape=NA) +
    geom_point(aes(colour=cut(Year, c(2001, 2002, 2017, 2018)),
                   size=cut(Year, c(2001, 2002, 2017, 2018))),
               alpha=0.5,
               position=jitter) +
    scale_color_manual(name = "Years",
                       values = c("(2001,2002]" = "green4",
                                  "(2002,2017]" = "Black",
                                  "(2017,2018]" = "Red"),
                       labels = c("2002", "2003-2017", "2018")) +
    scale_size_manual(name = "Years",
                      values = c(2, 1, 2),
                       labels = c("2002", "2003-2017", "2018")) +
    scale_x_discrete(limits=month.abb[1:8]) +
    labs(x = "Month") +
    labs(y = fieldPlotLabel)
  commands <- paste(commands, "grids$", loc, ", ", sep="")
}
commands <- paste(commands, "nrow=", gridRowNum, ")", sep="")
g <- eval(parse(text=commands))
if (shouldOutputFigure) {
  ggsave(outputFile, g, units="in", width=outputPlotWidth,
         height=outputPlotHeight, dpi=300, compression = "lzw")
} else {
  g
}