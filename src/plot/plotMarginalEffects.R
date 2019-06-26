rm(list=ls(all=TRUE))

# generate image file or not
#---------USER INPUTS-------------
shouldOutputFigure <- F
outputFile <- "../../marginal_effect_R5.tiff"
## temperatureField: "mean", "absMin", "absMax"
temperatureField <- "mean"
## temperatureType: "mean", "max", "min"
temperatureType <- "mean" 
## rainfallType: "total", "max"
rainfallType <- "total"
minYear <- 2002
maxYear <- 2018
formula <- RISK ~ (1 | AREA) + T3 + T4 + T5 + T7 + R4 + R5 + R6
# chosen variable to be plotted
marginalVar <- "R5"
family <- poisson # poisson or nbinom2
areas <- c("NTS", "NTN", "HKL")
#---------------------------------
source("../lib/retrieveData.R")
if (!require("glmmTMB")) install.packages("glmmTMB")
library("glmmTMB")
library("ggeffects")

df <- extractAnnualClimateData(temperatureField, temperatureType, rainfallType,
                               areas, minYear=minYear, maxYear=maxYear)

mean_cases <- mean(df$RISK)

res <- glmmTMB(formula, data=df, family=family, REML=F, se=TRUE)

# obtain relative risks
p <- ggpredict(res, c(marginalVar))
p$predicted <- p$predicted / mean_cases
p$std.error <- p$std.error / nthroot(mean_cases, 2)
p$conf.low <- p$conf.low / mean_cases
p$conf.high <- p$conf.high / mean_cases

labelSuffix <- ifelse(strcmp(substring(marginalVar, 1, 1), "T"), "(°C)", "(mm)")

p <- plot(p) + labs(x = paste(marginalVar, labelSuffix, sep=" "), y="Relative Risk", title="")
if (shouldOutputFigure) {
  ggsave(outputFile , units="in", width=5, height=4.2, dpi=300, compression = "lzw")
} else {
  p
}