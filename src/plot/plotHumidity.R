rm(list=ls(all=TRUE))
if (!require(openxlsx)) install.packages("openxlsx")
if (!require(ggplot2)) install.packages("ggplot2")

#---------USER INPUTS-------------
shouldOutputFigure <- FALSE
outputFile <- "../../figure/CC_temp.tiff"
## choose humidity type: mean, min, max
type <- "mean"
fieldPlotLabel <- "Mean Relative Humidity (%)"
## choose location between "TC", "CC"
location <- "TY"
#---------------------------------

HKCDCC <- read.xlsx("../../dat/climate/HKCD.xlsx",
                   sheet=paste("HKCD", location, sep=""),
                   startRow=1, colNames=TRUE, detectDates=TRUE)

field <- "Mean.Relative.Humidity.(%)"

# Cleaning: remove "#" on every value
HKCDCC[field] <- as.numeric(gsub("[^.0-9]", "", HKCDCC[field][,]))

MMTCC <- aggregate(HKCDCC[field], list(HKCDCC$Month,HKCDCC$Year), FUN=type, na.rm=TRUE)

names(MMTCC)[1] = "Month"
names(MMTCC)[2] = "Year"
names(MMTCC)[3] = field

Temp.CC_df = MMTCC
Temp.CC_df$month_txt = month.abb[Temp.CC_df$Month]

if (shouldOutputFigure) {
  ggsave(outputFile, units="in", width=6, height=4.2, dpi=300, compression = "lzw")
}

# jitter <- position_jitter(width=0.25, height=0)
# ggplot(data=Temp.CC_df, aes(x=month_txt,y=get(field))) +
#   geom_boxplot(aes(month_txt, get(field)), outlier.shape = NA) +
#   geom_point(aes(colour = cut(Year, c(2001, 2002, 2017, 2018))), alpha=0.5, position=jitter, size=2) +
#   scale_color_manual(name = "Years",
#                      values = c("(2001,2002]" = "Green",
#                                 "(2002,2017]" = "Black",
#                                 "(2017,2018]" = "Red"),
#                      labels = c("2002", "2003-2017", "2018")) +
#   scale_x_discrete(limits=month.abb[1:8]) +
#   labs(x = "Month") +
#   labs(y = fieldPlotLabel)

if (!require("ggrepel")) install.packages("ggrepel")
ggplot(data=Temp.CC_df, aes(x=month_txt, y=get(field), colour=factor(Year), group=factor(Year))) +
  geom_line() +
  scale_x_discrete(limits=month.abb[1:8]) +
  geom_label_repel(aes(label=Year),
                   data=subset(Temp.CC_df, Month==8),
                   nudge_x=1,
                   na.rm=TRUE) +
  scale_color_discrete(guide=FALSE)