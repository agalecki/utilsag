# utilsag

Utility functions for internal use

## Installation

* Development version from Github:
```
library("devtools"); install_github("agalecki/utilsag")
```
## Examples

```
library(utilsag)
Dropbox.path()                  # returns Dropbox path. Windows only.
write.Rd(cars)                  # generates .Rd file for cars data frame
univariateStat(cars, min)       # Returns min for every variable in cars
```
