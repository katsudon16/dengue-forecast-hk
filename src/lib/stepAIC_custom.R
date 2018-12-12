isAICValid <- function(val) {
  return(!(is.null(val) | is.na(val) | !is.finite(val)))
}

stepAIC <- function(df, model=glm, explanatoryVars=c(), randomFormula="", ...) {
  maxPosAIC <- 1500 # maximum possible AIC
  minAIC <- maxPosAIC
  notPickedVars <- explanatoryVars
  # temporary formula
  tempFormula <- paste("RISK ~ ", randomFormula, sep="")
  
  # base case
  finalModel <- model(as.formula(tempFormula), data=df, ...)
  minAIC <- AIC(finalModel)
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
      try <- tryCatch({
        testModel <- model(as.formula(testFormula), data=df, ...)
      }, warning=function(war) {
        print(paste("Warning occurs:", war))
        return(NULL)
      }, error=function(err) {
        print(paste("Error occurs:", err))
        return(NULL)
      })
      if (is.null(testModel)) next
      testAIC <- AIC(testModel)
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
    tempFormula <- paste(tempFormula, notPickedVars[tempNextVarIdx], sep=" + ")
    notPickedVars <- notPickedVars[-tempNextVarIdx]
  }
  return(finalModel)
}
