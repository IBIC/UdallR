# UdallR
R functions to analyze data from the Udall Project 2 REDCap database. 

## Loading the library functions
See this very helpful tutorial on creating an R library. https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

You should probably have packages `devtools` and `roxygen` installed to work with this package. From the parent working directory that contains the UdallR folder, you should do the following:

```R
setwd("..")
install("UdallR")
```

## Using the library functions
You would use them using code that looks something like this, where you substitute your REDCap token where it says `INSERTTOKENHERE`. Don't commit this token to github. 

```R
require(REDCapR)
library(REDCapR)
dat <- as.data.frame(redcap_read(redcap_uri="https://redcap.iths.org/api/", token="INSERTTOKENHERE"))

cdat <- udallCleanREDCapDataWide(dat)
```
