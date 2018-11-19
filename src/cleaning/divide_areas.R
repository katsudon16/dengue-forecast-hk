rm(list=ls(all=TRUE))
if (!require(openxlsx)) install.packages("openxlsx")
library("crayon")

#---------USER INPUTS-------------
# districts <- c("SLW", "TY", "TKL", "SK", "ST", "TP", "TM", "YL", "CC", "TC", "HK")
districts <- c("TKL", "TM", "YL")
minYear <- 2002
maxYear <- 2018
excelFile <- "d:/workspace/dengue-forecast-hk/dat/climate/HKCD_test.xlsx"
sheetName <- "HKCDNTw"
#---------------------------------

data <- list()
for (d in districts) {
  df <- read.xlsx("../../dat/climate/HKCD.xlsx",
                  sheet=paste("HKCD", d, sep=""),
                  startRow=1, colNames=TRUE, detectDates=TRUE)
  data[[d]] <- df
}

isDateValid <- function(i) {
  isDateSame <- TRUE
  date <- list()
  for (d_i in 1:length(districts)) {
    district <- districts[d_i]
    if (d_i == 1) {
      date <- list(year=data[[district]]$Year[i],
                   month=data[[district]]$Month[i],
                   day=data[[district]]$Day[i])
      next
    }
    if (data[[district]]$Year[i] != date$year |
        data[[district]]$Month[i] != date$month |
        data[[district]]$Day[i] != date$day) {
      isDateSame <- FALSE
      break
    }
  }
  return(isDateSame)
}

getAverageRow <- function(i) {
  row <- list()
  for (col_i in 1:length(colnames(data[[districts[1]]]))) {
    col <- colnames(data[[districts[1]]])[col_i]
    if (col == "Year" | col == "Month" | col == "Day") {
      row[col_i] <- data[[districts[1]]][[col]][i]
      next
    }
    avg <- 0
    isNA <- TRUE
    for (d_i in 1:length(districts)) {
      if (is.na(data[[districts[d_i]]][[col]][i])) next
      isNA <- FALSE
      avg <- avg + data[[districts[d_i]]][[col]][i]
    }
    if (!isNA) {
      avg <- avg / length(districts)
    } else {
      avg <- NA
    }
    row[col_i] <- avg
  }
  return(row)
}

avg_df <- data.frame()
# clean
for (d in districts) {
  for (col in colnames(data[[districts[1]]])) {
    data[[d]][col] <- as.numeric(gsub("[^.0-9]", "", data[[d]][col][,]))
  }
}
for (i in 1:dim(data[[districts[1]]])[1]) {
  if (!isDateValid(i)) {
    cat(red("Date is not the same..."))
    break
  }
  row <- getAverageRow(i)
  avg_df <- rbind(avg_df, row)
}
names(avg_df) <- names(data[[districts[1]]])
write.xlsx(avg_df, excelFile, sheetName=sheetName, append=TRUE)
