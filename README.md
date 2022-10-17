# utilsag

Utility functions for internal use

## Installation

* Development version from Github:
```
library("devtools"); install_github("agalecki/utilsag")
```

## Examples

# Path to `examples` folder
path <- system.file("examples", package = "utilsag")
path <- "./examples"

# Ex1. mytidy for cva object (glmnetUtils)
ex1 <- paste0(path,"/ex1-cva.R")
source(ex1)
utilsag:::mytidy.cva.glmnet(cva)
utilsag:::mytidy.cva.glmnet(cva, extract = "mod")
utilsag:::mytidy.cva.glmnet(cva, extract = "cva.summ")
utilsag:::mytidy.cva.glmnet(cva, extract = "mod.summ")

detach(package:utilsag)


## syntax

```
library(utilsag)
Dropbox.path()                  # returns Dropbox path. Windows only.
write.Rd(cars)                  # generates .Rd file for cars data frame
univariateStat(cars, min)       # Returns min for every variable in cars
```
