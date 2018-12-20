# For summerizing  and comparing differnt models 

 # Model selection approach summerizes and compare  models based on their AICc values 

# Inputes : Models names,as Model1, Model2, ..... 

getGLMSummary <- function(rank=rank, ...) {
  library(MuMIn)
  selectionSummary <- model.sel(rank=AICc, ...)
  return(selectionSummary)
}





