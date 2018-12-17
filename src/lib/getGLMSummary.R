# For summerizing  differnt models in to one table

# method 1 : Model selection approach summerizes and compare  models based on their AICc values 

# Inputes : Models names,as Model1, Model2, ..... 

library(MuMIn)
msAICc <- model.sel(Model1,Model2)
model.sel(msAICc, rank = AIC, rank.args = alist(k = log(nobs(x))))




# Method 2 ,gives summary of coeffcients, doesnot compare 

# Inputes : Models names,as Model1, Model2, ..... 

library(broom)

models <- list(Model1,Model2)
names(models) <- paste0("MODEL", 1:2)

all_coefs <- plyr::ldply(models, tidy, .id = "model")
head(all_coefs)

library(dplyr)
library(tidyr)
results <- all_coefs %>% select(-(std.error:p.value)) %>%
  spread(term, estimate)


