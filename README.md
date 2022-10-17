# utilsag

Utility functions for internal use

## Installation

* Development version from Github:
```
library("devtools"); install_github("agalecki/utilsag")
```
## syntax

```
library(utilsag)
Dropbox.path()                  # returns Dropbox path. Windows only.
write.Rd(cars)                  # generates .Rd file for cars data frame
univariateStat(cars, min)       # Returns min for every variable in cars
```

## Examples

path <- system.file("examples", package = "utilsag")
path <- "./examples"
ex1 <- paste0(path,"/ex1-cva.R")
source(ex1)
detach(package:utilsag)