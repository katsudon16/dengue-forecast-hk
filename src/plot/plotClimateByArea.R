rm(list=ls(all=TRUE))
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(ggplot2)) install.packages(ggplot2)
if (!require(gridExtra)) install.packages("gridExtra")

#---------USER INPUTS-------------
shouldOutputFigure <- F
outputFile <- "../../figure/normalized_temp_years.tiff"
outputPlotWidth <- 12
outputPlotHeight <- 3
showTemperature <- T
## temperatureField: "mean", "absMin", "absMax"
temperatureField <- "mean"
## temperatureType: "mean", "max", "min"
temperatureType <- "mean" 
## rainfallType: "total", "max"
rainfallType <- "total"
normalized <- T
# area option = 1 -> NTS, 2 -> NTN, 3 -> HKL
areas <- c("HKL")
# areas <- c("NTS", "NTN", "HKL")
# boxplot --> 2002 >< 2003 - 2017 >< 2018
# line    -->         2003 - 2017 >< 2018
plotBoxplot <- F
gridRowNum <- 1 # number of grid rows
ylab <- "The Normalized Ratio of\nMonthly Mean Temperature"
# ylab <- "The Normalized Ratio of\nMonthly Total Rainfall"
#---------------------------------
source("../lib/retrieveData.R")
df <- extractAnnualClimateData(temperatureField, temperatureType, rainfallType, areas)

# normalized
if (normalized) {
  maxs <- apply(df[,c(4:19)], 2, max)
  mins <- apply(df[,c(4:19)], 2, min)
  df[,c(4:19)] <- scale(df[,c(4:19)], center = mins, scale = maxs - mins)
}

prefix <- ifelse(showTemperature, "T", "R")
plot_df <- data.frame()
for (area in 1:length(areas)) {
  for (year in 2002:2018) {
    row <- df[df$YEAR == year & df$AREA == area,]
    for (month in 3:8) {
      field <- paste(prefix, month, sep="")
      plot_df <- rbind(plot_df, c(area, year, month, row[[field]]))
    }
  }
}
names(plot_df) <- c("area", "Year", "Month", "value")
plotYmax <- max(plot_df$value)
plotYmin <- min(plot_df$value)
plot_df$month_txt <- month.abb[plot_df$Month]

grids <- c()
jitter <- position_jitter(width=0.25, height=0)
commands <- ifelse(shouldOutputFigure, "arrangeGrob(", "grid.arrange(")
for (area_i in 1:length(areas)) {
  areaName <- areas[area_i]
  if (plotBoxplot) {
    grids[[areaName]] <- ggplot(data=plot_df[plot_df$area == area_i,], aes(x=month_txt,y=value)) +
      ylim(plotYmin, plotYmax) +
      ggtitle(areaName) +
      geom_boxplot(aes(month_txt, value), outlier.shape = NA) +
      geom_point(aes(colour = cut(Year, c(2001, 2002, 2017, 2018)),
                     size=cut(Year, c(2001, 2002, 2017, 2018))),
                 position=jitter, alpha=0.5) +
      scale_color_manual(name = "Years",
                         values = c("(2001,2002]" = "green4",
                                    "(2002,2017]" = "Black",
                                    "(2017,2018]" = "Red"),
                         labels = c("2002", "2003-2017", "2018")) +
      scale_size_manual(name = "Years",
                        values = c(2, 1, 2),
                        labels = c("2002", "2003-2017", "2018")) + 
      scale_x_discrete(limits=month.abb[3:8]) +
      labs(x = "Month") +
      labs(y = ylab)
  } else {
    area_df <- plot_df[plot_df$area == area_i,]
    # aggregate 2003 - 2017
    temp_df <- area_df[area_df$Year > 2002 & area_df$Year < 2018,]
    temp_df <- aggregate(temp_df$value, list(temp_df$Month), FUN=mean, na.rm=T)
    names(temp_df) <- c("Month", "value")
    temp_df$Year <- "Other"
    last_df <- area_df[area_df$Year == 2018,]
    area_df <- temp_df
    for (month in 3:8) {
      row <- last_df[last_df$Month == month,]
      area_df <- rbind(area_df, c(row$Month, row$value, row$Year))
    }
    
    grids[[areaName]] <- ggplot(data=area_df, aes(x=Month, y=value, group=Year)) +
      ggtitle(areaName) +
      geom_line(aes(color=Year), size=1.2) +
      scale_fill_brewer(palette="Set1") +
      theme(legend.title=element_blank()) +
      ylim(0, 1) + 
      labs(x="Month") +
      labs(y=ylab)
    
  }
  commands <- paste(commands, "grids$", areas[area_i], ", ", sep="")
}
commands <- paste(commands, "nrow=", gridRowNum, ")", sep="")
g <- eval(parse(text=commands))
if (shouldOutputFigure) {
  ggsave(outputFile, g, units="in", width=outputPlotWidth,
         height=outputPlotHeight, dpi=300, compression = "lzw")
} else {
  g
}
