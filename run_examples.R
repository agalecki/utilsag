 

devtools::install_github("agalecki/utilsag")
rm(list = ls())

path <- "C:/Users/agalecki/Documents/GitHub/utilsag/examples"

library(tidymodels)
library(utilsag)

ex0_data <- paste0(path,"/ex0-data.R")
source(ex0_data)

# Ex1. mytidy for cva object (glmnetUtils)
ex1 <- paste0(path,"/ex1-glmnet.R")
source(ex1,echo = TRUE)
