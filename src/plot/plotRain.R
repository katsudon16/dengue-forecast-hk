rm(list=ls(all=TRUE))
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(ggplot2)) install.packages(ggplot2)

#---------USER INPUTS-------------
shouldOutputFigure <- FALSE
outputFile <- "../../figure/CC_temp.tiff"
## choose one location from: (the code will automatically detect district/area)
# districts: "SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK"
# areas    : "NTW", "NTS", "NTE", "KL", "HK"
location <- "SLW"
#---------------------------------

allDistricts <- c("SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK")
allAreas <- c("NTW", "NTS", "NTE", "KL", "HK")

excelFilename <- ifelse(location %in% allDistricts, "HKCM", "HKCM_areas")
weather_df2 <- read.xlsx(paste("../../dat/climate/", excelFilename, ".xlsx", sep=""),
                   sheet=location,
                   startRow=1, colNames=TRUE, detectDates=TRUE)

weather_df2$month_txt <- month.abb[weather_df2$month]

# Cleaning: remove "#" on every value in specific columns
weather_df2$totalrain <- as.numeric(gsub("[^.0-9]", "", weather_df2$totalrain))

if (shouldOutputFigure) {
  ggsave(outputFile, units="in", width=6, height=4.2, dpi=300, compression="lzw")
}

jitter <- position_jitter(width=0.25, height=0)
ggplot(data=weather_df2, aes(x=month_txt, y=totalrain)) +
  geom_boxplot(aes(month_txt,totalrain), outlier.shape=NA) +
  geom_point(aes(colour=cut(year, c(2001, 2002, 2017, 2018))), alpha=0.5, position=jitter, size=2) +
  scale_color_manual(name = "Years",
                     values = c("(2001,2002]" = "Green",
                                "(2002,2017]" = "Black",
                                "(2017,2018]" = "Red"),
                     labels = c("2002", "2003-2017", "2018")) +
  scale_x_discrete(limits=month.abb[1:8]) +
  labs(x = "Month") +
  labs(y = "Total rainfall (mm)")


