# utilsag

Utility functions for internal use

## Installation

* Development version from Github:
```
detach(package:utilsag)
library("devtools"); install_github("agalecki/utilsag")
```

## Examples

# Path to `examples` folder (choose)
#path <- system.file("examples", package = "utilsag")
path <- "./examples"

# Ex1. mytidy for cva object (glmnetUtils)
ex1 <- paste0(path,"/ex1-glmnet.R")
source(ex1)
mytidy(cva)
mytidy(cva, extract = "mod")
mytidy(cva, extract = "cva.summ")
mytidy(cva, extract = "mod.summ")




## syntax

```
library(utilsag)
Dropbox.path()                  # returns Dropbox path. Windows only.
write.Rd(cars)                  # generates .Rd file for cars data frame
univariateStat(cars, min)       # Returns min for every variable in cars
```
