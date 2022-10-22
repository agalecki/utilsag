 

devtools::install_github("agalecki/utilsag")
rm(list = ls())

path <- "C:/Users/agalecki/Documents/GitHub/utilsag"

library(tidymodels)
library(utilsag)

ex0_data <- paste0(path,"/examples/ex0-data.R")
source(ex0_data)

# Ex1. mytidy for glmnet object
ex1 <- paste0(path,"/examples/ex1-glmnet.R")
#source(ex1,echo = TRUE)

ex5 <- paste0(path,"/examples/ex5-penAFT.R")
dt5 <- paste0(path,"/examples/penAFT_object.Rdata")
load(file =dt5)
source(ex5,echo = TRUE)


