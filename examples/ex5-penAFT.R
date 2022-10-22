
dt5 <- paste0(path,"/examples/penAFT_object.Rdata")
load(file =dt5)      # penAFT_object

library(penAFT)
library(tidymodels)
library(utilsag)
ls()


stepx <- c(4,5)
myglance(penAFT_object)
(tt <- mytidy(penAFT_object) %>% filter( step %in% stepx))
tt %>% unnest(beta)


