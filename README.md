# utilsag 

Utility functions for internal use

## Installation

* Development version from Github:

```
detach(package:utilsag)
library("devtools"); install_github("agalecki/utilsag")

library(utilsag)
methods(myglance)
methods(mytidy)

```

## Examples

```
# Path to `examples` folder (choose)
#path <- system.file("examples", package = "utilsag")
path <- "./examples"

library(tidymodels)
library(utilsag)

# Ex1. mytidy for cva object (glmnetUtils)
ex1 <- paste0(path,"/ex1-glmnet.R")
source(ex1)
mytidy(fit_cox, what = "dev")
mytidy(fit_cox)
```

## syntax

```
library(utilsag)
Dropbox.path()                  # returns Dropbox path. Windows only.
```
