# Function to fit Climate variables and Dengue incidence Data 
# input params:
# - Ttype (string)             : temperature type
# - Rtype (string)             : rainfall type
# - location (string)
# - temperatureColName (string): temperature excel column name
# - filepath (string)          : excel file path
# - glmFormula (formula)       : the glm formula if specified
# returns a glm model
getGLM <- function(Ttype="mean", Rtype="max", location="CC", temperatureColName="Daily.Mean.Temperature",
                   filepath="../../dat/climate/HKCD.xlsx", glmFormula=NULL, ...) {
  dirName <- "dengue-forecast-hk/src"
  srcDir <- substr(getwd(), 0, regexpr(dirName, getwd(), fixed=TRUE)[1] + nchar(dirName))
  source(paste(srcDir, "lib/retrieveData.R", sep=""))
  library("openxlsx")
  
  # 1. collect monthly temperature and rainfall data
  data <- readDFFromFile(location, filePath=filepath)
  temp <- getMonthlyTemperatureOnType(type=Ttype,
                                      location=location,
                                      colName=temperatureColName,
                                      df=data)
  rain <- getMonthlyRainfallOnType(type=Rtype,
                                   location=location,
                                   df=data)
  library("MASS")
  Cases.HK <- read.xlsx("../../dat/cases/hk_annual_cases.xlsx",sheet = "Sheet1",startRow = 1,colNames = TRUE,detectDates = TRUE)

  # 2. Creat Dataframe of temperature, rainfall, and dengue Cases 
  T <-0
  R <-0
  df <- data.frame()
  
  for (year in 2002:2018) {
    for (month in 1:8) {
      T[month] <- temp[temp$month==month & temp$year==year, 'temperature']
      R[month] <- rain[rain$month==month & rain$year==year, 'rainfall']
    }
    df <- rbind(df, c(year, 1, T, R))
  }
  names(df)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','T8','R1','R2','R3','R4','R5','R6','R7','R8')
  df$CASES <- Cases.HK$CASES
 
  # 3. Model fitting
  glm_fin <- NULL  
  if (!is.null(glmFormula)) {
    # 3a. General Linear Model
    glm_fin <- glm(glmFormula, data = df , ...)
  } else {
    # 3b. Selecting best model based on AIC criteria 
    glm_fin <- MASS::stepAIC(glm(CASES ~ 1, data = df, ...),
                             scope = CASES ~ T3 + T4 + T5 + T6 + T7 + T8 + R3 + R4 + R5 + R6 + R7 + R8,
                             direction = "forward")
  }
  return(glm_fin)
}

# Function to summarize and compare different models
# input params:
# - rank (function) : function used to rank different models e.g., AIC, AICc
# - model1, model 2, ... (models)
# returns the selection summary
compareGLMs <- function(..., rank=AICc) {
  library(MuMIn)
  selectionSummary <- model.sel(rank=rank, ...)
  return(selectionSummary)
}

