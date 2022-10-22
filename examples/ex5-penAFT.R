
dt5 <- paste0(path,"/examples/penAFT_object.Rdata")
load(file =dt5)      # penAFT_object

library(penAFT)
library(tidymodels)
library(utilsag)
ls()

myglance(penAFT_object)
mytidy(penAFT_object)



