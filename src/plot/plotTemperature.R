rm(list=ls(all=TRUE))
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(ggplot2)) install.packages("ggplot2")

#---------USER INPUTS-------------
shouldOutputFigure <- FALSE
outputFile <- "../../figure/CC_temp.tiff"
## choose field between "min", "mean", "max"
field <- "mean"
fieldPlotLabel <- "Average Monthly Temperature (°C)"
## choose one location from: (the code will automatically detect district/area)
 # districts: "SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK"
 # areas    : "NTW", "NTS", "NTE", "KL", "HK"
location <- "SLW"
#---------------------------------

allDistricts <- c("SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK")
allAreas <- c("NTW", "NTS", "NTE", "KL", "HK")

excelFilename <- ifelse(location %in% allDistricts, "HKCD", "HKCD_areas")
HKCDCC = read.xlsx(paste("../../dat/climate/", excelFilename, ".xlsx", sep=""),
                   sheet=paste("HKCD", location, sep=""),
                   startRow=1, colNames=TRUE, detectDates=TRUE)
fieldLabels <- list(min="Absolute.Daily.Min.(deg.C)",
                    mean="DM",
                    max="Absolute.Daily.Max.(deg.C)")
fieldLabel <- as.character(fieldLabels[field])

# Cleaning: remove "#" on every value
HKCDCC[fieldLabel] <- as.numeric(gsub("[^.0-9]", "", HKCDCC[fieldLabel][,]))

MMTCC = aggregate(HKCDCC[fieldLabel], list(HKCDCC$Month,HKCDCC$Year), FUN=field, na.rm=TRUE)

names(MMTCC)[1] = "Month"
names(MMTCC)[2] = "Year"
names(MMTCC)[3] = field

Temp.CC_df = MMTCC
Temp.CC_df$month_txt = month.abb[Temp.CC_df$Month]

if (shouldOutputFigure) {
  ggsave(outputFile, units="in", width=6, height=4.2, dpi=300, compression = "lzw")
}

jitter <- position_jitter(width=0.25, height=0)
ggplot(data=Temp.CC_df, aes(x=month_txt,y=get(field))) +
  geom_boxplot(aes(month_txt, get(field)), outlier.shape = NA) +
  geom_point(aes(colour = cut(Year, c(2001, 2002, 2017, 2018))), alpha=0.5, position=jitter, size=2) +
  scale_color_manual(name = "Years",
                     values = c("(2001,2002]" = "Green",
                                "(2002,2017]" = "Black",
                                "(2017,2018]" = "Red"),
                     labels = c("2002", "2003-2017", "2018")) +
  scale_x_discrete(limits=month.abb[1:8]) +
  labs(x = "Month") +
  labs(y = fieldPlotLabel)