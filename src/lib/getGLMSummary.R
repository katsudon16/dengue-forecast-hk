

# 1
library(MuMIn)
(msAICc <- model.sel(Model2,Model3))
model.sel(msAICc, rank = AIC, rank.args = alist(k = log(nobs(x))))



# 2
library(broom)

models <- list(Model1,Model2)
names(models) <- paste0("MODEL", 1:2)

all_coefs <- plyr::ldply(models, tidy, .id = "model")
head(all_coefs)

library(dplyr)
library(tidyr)
results <- all_coefs %>% select(-(std.error:p.value)) %>%
  spread(term, estimate)


#3 

library(MuMIn)
#R2 <- function(x) summary(x)$r.squared
ms <- model.sel(Model1,Model2)

i <- 1:2 # indices of columns with model terms
response <- "a"

res <- as.data.frame(ms)
v <- names(ms)[i]
v[v == "(Intercept)"] <- 1

# create formula-like model names:
mnames <- apply(res[, i], 1, function(x) 
  deparse(simplify.formula(reformulate(v[!is.na(x)], response = response))))
## OR
#   mnames <- apply(res[, i], 1, function(x)
#          sapply(attr(ms, "modelList"), function(x) deparse(formula(x)))

res <- cbind(model = mnames, res[, -i])
Hmisc::latex(res, file = "")