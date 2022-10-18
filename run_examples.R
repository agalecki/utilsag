 

devtools::install_github("agalecki/utilsag")
path <- "C:/Users/agalecki/Documents/GitHub/utilsag/examples"

library(tidymodels)
library(utilsag)

# Ex1. mytidy for cva object (glmnetUtils)
ex1 <- paste0(path,"/ex1-glmnet.R")
source(ex1)
mytidy(fit_cox, what = "dev")
mytidy(fit_cox)
