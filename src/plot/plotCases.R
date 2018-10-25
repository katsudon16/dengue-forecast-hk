rm(list=ls(all=TRUE))

outputFile <- "../../figure/local_cases.tiff"
shouldOutputFigure <- FALSE

# modify if needed
plotField <- "Local.Cases"
plotFieldLabel <- "Local Cases"

# install if ggplot2 is not found
if (!require(ggplot2)) install.packages(ggplot2)

cases <- read.csv("../../dat/cases/hk_monthly_cases.csv", header=T)
# remove year (month) with no incidence
cases <- cases[cases$Imported.Cases != cases$Total,]
lastYear <- -1
for (year in cases$Year) {
  if (lastYear == year) next
  lastYear <- year
  
  for (month in 1:12) {
    found = nrow(cases[cases$Year == year & cases$Month == month,]) != 0
    if (!found) {
      # note that the other field is now dummy value
      cases <- rbind(cases, c(year, month, 0, 0, 0))
    }
  }
}

cases$Month <- month.abb[cases$Month]

## combine extracted and average into 1 plot 
# 1. extract relevant fields from cases
extracted <- cases[,c("Month", plotField, "Year")]

# 2. create average data frame "monthAvg" by month
monthAvg <- aggregate(cases[plotField], list(cases$Month), mean, na.rm=TRUE)
names(monthAvg)[1] = "Month"
names(monthAvg)[2] = plotField
monthAvg$Year = -1 # "dummy" average

if (shouldOutputFigure) {
  ggsave(outputFile, units="in", width=6, height=4.2, dpi=300, compression = "lzw")
}

# 3. draw plot
ggplot() +
  # monthly average
  geom_line(data=monthAvg, aes(x=Month, y=get(plotField), group=1), colour="blue") +
  # extracted from cases
  geom_point(data=extracted, aes(Month, get(plotField), colour=factor(Year)), alpha=0.6, size=2) +
  scale_x_discrete(limits=month.abb[1:12]) +
  scale_color_manual(values=c("#000000", "#0000FF",
                              "#7D3C98", "#FF0000",
                              "#27AE60", "#FF00FF",
                              "#F1C40F", "#5DADE2")) +
  labs(color="Years") + 
  labs(x="Month") +
  labs(y=plotFieldLabel)