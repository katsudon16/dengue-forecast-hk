# GLM to fit Climate variables and Dengue incidence Data 

#.....Variable  inputs.......

 #  Input temprature variables in 'Ttype' and   Inpute Rainfall variables in 'Rtype'   

getGLM <- function(Ttype="mean", Rtype="max", location="CC", colName="Daily.Mean.Temperature", filepath="../../dat/climate/HKCD.xlsx") {
 
   # 1. collect and clean Monthly Temperature and  Rainfall data 
  
  library(openxlsx)
  
  allClimates <- read.xlsx(filepath, sheet=paste("HKCD", location, sep=""), startRow=1, colNames=TRUE, detectDates=TRUE)
  allClimates[[colName]] <- as.numeric(gsub("[^.0-9]", "", allClimates[[colName]]))
  allClimates$`Total.Rainfall.(mm)` <- as.numeric(gsub("[^.0-9]", "", allClimates$`Total.Rainfall.(mm)`))
  
  if (Ttype == "min") {
    
    temp <- aggregate(allClimates[[colName]], list(allClimates$Month, allClimates$Year), min, na.rm=TRUE)
    
  } else if (Ttype == "mean") {
    
    temp <- aggregate(allClimates[[colName]], list(allClimates$Month, allClimates$Year), mean, na.rm=TRUE)
    
  } else {
    
    temp <- aggregate(allClimates[[colName]], list(allClimates$Month, allClimates$Year), max, na.rm=TRUE)
   
  }
  if (Rtype == "max") {
      
    Rain <- aggregate(allClimates$`Total.Rainfall.(mm)`, list(allClimates$Month, allClimates$Year), max, na.rm=TRUE)
      
  } else if (Rtype == "sum") {
      
      Rain <- aggregate(allClimates$`Total.Rainfall.(mm)`, list(allClimates$Month, allClimates$Year), sum, na.rm=TRUE)  
  }
  names(temp)[1] <- "month"
  
  names(temp)[2] <- "year"
  
  names(temp)[3] <- "temperature"
  
  temp$month_txt <- month.abb[temp$month]
  
  names(Rain)[1] <- "month"
  
  names(Rain)[2] <- "year"
  
  names(Rain)[3] <- "rainfall"
  
  Rain$month_txt <- month.abb[Rain$month]
 
  
  library('MASS')

  startyear <- 2002
  Cases.HK <- read.xlsx("../../dat/cases/hk_annual_cases.xlsx",sheet = "Sheet1",startRow = 1,colNames = TRUE,detectDates = TRUE)
 
   # 2. Creat Dataframe of temprature ,rainfall and and dengue Cases 
 
  T <-0
  R <-0
  df <- data.frame()
  
  for (year in 2002:2018) {
    
    print(year)
    
    for (month in 1:7) {
      
      T[month] <-  temp[ temp$month==month &  temp$year==year,'temperature']
      R[month] <- Rain[Rain$month==month & Rain$year==year,'rainfall']
    }
    df <- rbind(df, c(year, 1,T , R))
  }
  names(df)<-c('YEAR','CASES','T1','T2','T3','T4','T5','T6','T7','R1','R2','R3','R4','R5','R6','R7')
  df$CASES <- Cases.HK$CASES
  dfnew <- df
 
  # 3. Model fitting
  
  # 3a. General Linear Model
  
  #glm_1 <- glm(CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7 , data = dfnew , family = poisson)
  
  #print( glm_1)
  
  # 3b. Selecting best model based on AIC criteria 

  glm_fin <- stepAIC(glm(CASES ~ 1, data = dfnew),
                   scope = CASES ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + R1 + R2 + R3 + R4 + R5 + R6 + R7,
                  direction = "forward")
  print(glm_fin)
}
