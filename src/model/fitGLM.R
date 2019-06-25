rm(list=ls(all=TRUE))

#---------USER INPUTS-------------
## temperature column name
temperatureColName <- "Daily.Mean.Temperature"
## temperatureType: "mean", "max", "min"
temperatureType <- "mean" 
## rainfallType: "total", "max"
rainfallType <- "total"
minYear <- 2002
maxYear <- 2018
# if formula is specified, stepAIC will be skipped
formula <- NULL
location <- "CC"
#---------------------------------

source("../lib/getGLM.R")
model.glm <- getGLM(Ttype=temperatureType, Rtype=rainfallType,
                    location=location, temperatureColName=temperatureColName,
                    glmFormula=formula)

# model comparison example
model.glm.ex1 <- getGLM(Ttype=temperatureType, Rtype=rainfallType,
                        location=location, temperatureColName=temperatureColName,
                        glmFormula=CASES ~ T3 + T4 + R4 + R6)

model.glm.ex2 <- getGLM(Ttype=temperatureType, Rtype=rainfallType,
                        location=location, temperatureColName=temperatureColName,
                        glmFormula=CASES ~ T3 + T4 + T7 + R4 + R6 + R8)

compareGLMs(model.glm, model.glm.ex1, model.glm.ex2, rank=AICc)
