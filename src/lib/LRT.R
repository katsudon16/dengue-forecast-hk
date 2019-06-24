# a function to run likelihood ratio test (LRT)
runLRT <- function(responseVar, fixedVarsList, randomVarsList, data, func, ...) {
  
  # build the random predictors formula in a string
  buildRandomVarsFormulaStr <- function(varsList) {
    formula <- ""
    for (var in varsList) {
      formula <- paste(formula,
                       ifelse(nchar(formula) == 0, "", " +"),
                       sep="")
      formula <- paste(formula, " (1|", var, ")", sep="")
    }
    return(formula)
  }
  
  # build the fixed predictors formula in a string
  buildFixedVarsFormulaStr <- function(varsList, isRandomFormulaEmpty=T) {
    formula <- ""
    for (var in varsList) {
      formula <- paste(formula,
                       ifelse(nchar(formula) == 0 && isRandomFormulaEmpty, "", " +"),
                       sep="")
      formula <- paste(formula, " ", var, sep="")
    }
    return(formula)
  }
  
  # build a full formula
  buildFullFormula <- function(responseVarStr, fixedFormulaStr, randomFormulaStr="") {
    formula <- as.formula(paste(responseVarStr, " ~",
                                randomFormulaStr,
                                fixedFormulaStr,
                                sep=""))
    return(formula)
  }
  
  formula.random.str <- buildRandomVarsFormulaStr(randomVarsList)
  
  isRandomFormulaEmpty <- nchar(formula.random.str) == 0
  formula.fixed.str <- buildFixedVarsFormulaStr(fixedVarsList, isRandomFormulaEmpty)
  
  formula.full <- buildFullFormula(responseVar, formula.fixed.str, formula.random.str)
  model.full  <- func(formula.full, data=data, ...)
  
  for (fixedVar in fixedVarsList) {
    cmpFixedVarsList <- fixedVarsList[fixedVarsList != fixedVar]
    formula.fixed.str <- buildFixedVarsFormulaStr(cmpFixedVarsList, isRandomFormulaEmpty)
    formula.cmp <- buildFullFormula(responseVar, formula.fixed.str, formula.random.str)
    # model.cmp <- func(formula.cmp, data=data, ...)
    model.cmp <- func(formula.cmp, data=data, family=poisson, REML=F)
    anova.res <- anova(model.full, model.cmp)
    
    cat("removing", fixedVar, "...\n")
    cat("chi-square", anova.res$Chisq[2], "\n")
    cat("p-value", anova.res$`Pr(>Chisq)`[2], "\n")
    cat("-------------------------------------\n\n")
  }
}