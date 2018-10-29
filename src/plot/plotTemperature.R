if (!require(openxlsx)) install.packages(openxlsx)
if (!require(ggplot2)) install.packages(ggplot2)
HKCDCC = read.xlsx("../../dat/climate/HKCD.xlsx", sheet="HKCDCC", startRow=1, colNames=TRUE, detectDates=TRUE)

outputFile <- "CC_temp.tiff"
shouldOutputFigure <- FALSE

# Cleaning: remove "#" on every value
HKCDCC$DM <- as.numeric(gsub("[^.0-9]", "", HKCDCC$DM))

MMTCC = aggregate(HKCDCC$DM, list(HKCDCC$Month,HKCDCC$Year), min, na.rm=TRUE)

names(MMTCC)[1] = "Month"
names(MMTCC)[2] = "Year"
names(MMTCC)[3] = "MMT"

Temp.CC_df = MMTCC
Temp.CC_df$month_txt = month.abb[Temp.CC_df$Month]

if (shouldOutputFigure) {
  ggsave(outputFile, units="in", width=6, height=4.2, dpi=300, compression = "lzw")
}

jitter <- position_jitter(width=0.25, height=0)
ggplot(data=Temp.CC_df, aes(x=month_txt,y=MMT)) +
  geom_boxplot(aes(month_txt, MMT), outlier.shape = NA) +
  geom_point(aes(colour = cut(Year, c(2001, 2002, 2017, 2018))), alpha=0.5, position=jitter, size=2) +
  scale_color_manual(name = "Years",
                     values = c("(2001,2002]" = "Green",
                                "(2002,2017]" = "Black",
                                "(2017,2018]" = "Red"),
                     labels = c("2002", "2003-2017", "2018")) +
  scale_x_discrete(limits=month.abb[1:8]) +
  labs(x = "Month") +
  labs(y = "Minimum Monthly Temperature (°C)")