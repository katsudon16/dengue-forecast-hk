rm(list=ls(all=TRUE))
if (!require(ggplot2)) install.packages(ggplot2)

shouldOutputFigure <- TRUE
outputFile <- "../../plot/rain.tiff"

weather_df2 <- read.csv("../../dat/climate/changzhou_climate(clean).csv", header=T)
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


