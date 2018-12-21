isAICValid <- function(val) {
  return(!(is.null(val) | is.na(val) | !is.finite(val)))
}

getAICorAICc <- function(model, isAICc) {
  if (isAICc) {
    return(AICc(model))
  }
  return(AIC(model))
}

stepAIC <- function(df, model=glm, responseVar="", explanatoryVars=c(), randomFormula="", isAICc=FALSE, ...) {
  if (!require("MuMIn")) install.packages("MuMIn")
  maxPosAIC <- 1500 # maximum possible AIC
  minAIC <- maxPosAIC
  notPickedVars <- explanatoryVars
  # temporary formula
  tempFormula <- paste(responseVar, " ~ ", randomFormula, sep="")
  
  # base case
  finalModel <- model(as.formula(tempFormula), data=df, ...)
  minAIC <- getAICorAICc(finalModel, isAICc)
  if (!isAICValid(minAIC)) {
    minAIC <- maxPosAIC
    finalModel <- NULL
  }
  while (length(notPickedVars) > 0) {
    tempAIC <- minAIC
    tempNextVarIdx <- NULL
    tempModel <- NULL
    for (i in 1:length(notPickedVars)) {
      testFormula <- paste(tempFormula, notPickedVars[i], sep=" + ")
      testModel <- NULL
      try <- tryCatch({
        testModel <- model(as.formula(testFormula), data=df, ...)
      }, warning=function(war) {
        return(NULL)
      }, error=function(err) {
        return(NULL)
      })
      if (is.null(testModel)) next
      testAIC <- getAICorAICc(testModel, isAICc)
      if (!isAICValid(testAIC)) next
      if (testAIC < tempAIC) {
        tempAIC <- testAIC
        tempNextVarIdx <- i
        tempModel <- testModel
        testModel <- NULL
      }
    }
    # no more variable picked
    if (is.null(tempNextVarIdx)) break
    print(summary(tempModel))
    finalModel <- tempModel
    minAIC <- tempAIC
    print(minAIC)
    tempFormula <- paste(tempFormula, notPickedVars[tempNextVarIdx], sep=" + ")
    notPickedVars <- notPickedVars[-tempNextVarIdx]
  }
  return(finalModel)
}
