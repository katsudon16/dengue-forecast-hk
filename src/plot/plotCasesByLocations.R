rm(list=ls(all=TRUE))
source("../lib/retrieveData.R")

#---------USER INPUTS-------------
shouldOutputFigure <- TRUE
outputFile <- "../../figure/case_by_location.tiff"
outputPlotWidth <- 4
outputPlotHeight <- 4
#---------------------------------

areaRisk <- getAnnualRiskByArea()
areaRisk$ALL <- areaRisk$NTS + areaRisk$NTN + areaRisk$HKL

locations <- c("HKL", "NTN", "NTS", "ALL")
df <- data.frame(stringsAsFactors = FALSE)
for (i in 1:dim(areaRisk)[1]) {
  df <- rbind(df, c(areaRisk$year[i], 1, areaRisk$HKL[i])) # HKL
  df <- rbind(df, c(areaRisk$year[i], 2, areaRisk$NTN[i])) # NTN
  df <- rbind(df, c(areaRisk$year[i], 3, areaRisk$NTS[i])) # NTS
  df <- rbind(df, c(areaRisk$year[i], 4, areaRisk$ALL[i])) # ALL
}
names(df) <- c("year", "region", "cases")
df$region <- locations[df$region]

if (!require("ggplot2")) install.packages("ggplot2")
p <- ggplot(data=df, aes(x=year, y=cases, group=region)) +
  geom_line(aes(color=region), size=1.2) +
  geom_point(aes(color=region), size=3) +
  scale_color_brewer(palette="Paired") +
  theme(legend.title=element_blank()) +
  labs(x="Year") +
  labs(y="Cases")

if (shouldOutputFigure) {
  ggsave(outputFile, p, units="in", width=outputPlotWidth,
         height=outputPlotHeight, dpi=300, compression = "lzw")
} else {
  p
}