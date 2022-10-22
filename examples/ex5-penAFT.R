
dt5 <- paste0(path,"/examples/penAFT_object.Rdata")
load(file =dt5)      # penAFT_object

library(penAFT)
library(tidymodels)
library(utilsag)
ls()

load(paste0(path, "/examples/penAFT_objects.Rdata"))
str(penAFT_object)

### penAFT
stepx <- c(4,5)
myglance(penAFT_object)
(tt <- mytidy(penAFT_object) %>% filter(step %in% stepx))
tt %>% unnest(beta)

str(penAFTcv_object)




