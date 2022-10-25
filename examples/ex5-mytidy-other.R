
dt5 <- paste0(path,"/examples/penAFT_objects.Rdata")
load(file =dt5)      # penAFT_object

library(penAFT)
library(tidymodels)
library(utilsag)
ls()

#str(penAFT_object)
#str(penAFTcv_object)

### penAFT
stepx <- c(4,5)
myglance(penAFT_object)
(tt <- mytidy(penAFT_object) %>% filter(step %in% stepx))
tt %>% unnest(beta)

mytidy(penAFT_object)

myglance(penAFTcv_object)

mytidy(penAFTcv_object)


### cva.glmnet

myglance(cvaglmnet_fit_cox)
mytidy(cvaglmnet_fit_cox)
