# UdallR
R functions to analyze data from the Udall Project 2 REDCap database. 


## Requesting REDCap token
You must generate a token through the REDCap website (https://redcap.iths.org/) in order to export the Udall data from REDCap.  

After logging in to REDCap, select "MyProjects" > "UDALL_P2" and then "API" under the "Applications" menu on the left-hand side.

<img src="./images/api_demo_1.png" height="400px" width="300px" />

If the "API" option is missing from the "Applications" menu, then someone in the project must give you privileges to see this page. If you are able to access the "API" page, there will be an option to request a new Application Programming Interface (API) token from the REDCap administrator. When your token request has successfully processed, your token should be printed out in green text on this page. A new token can be regenerated at any point by selecting "Regenerate token".


<img src="./images/api_demo_2.png" height="500px" width="700px" />


For more information, see REDCap's documentation of API tokens: https://redcap.iths.org/redcap_v7.1.2/API/help.php?content=default


## Loading the library functions

From the parent working directory that contains the UdallR folder, you should do the following:

```R
install.packages("UdallR", repos=NULL, type="source")
library(UdallR)
```
Note that if you try to use the `install` function from the `devtools`
package you will get an error. Install as above.

## Using the library functions
You would use them using code that looks something like this, where you substitute your REDCap token where it says `INSERTTOKENHERE`. Don't commit this token to github. 

```R
require(REDCapR)
library(REDCapR)
library(UdallR)

dat <- as.data.frame(redcap_read(redcap_uri="https://redcap.iths.org/api/", token="INSERTTOKENHERE"))

cdat <- udallCleanREDCapDataWide(dat)
```

## Important notes

A few important notes regarding `UdallR` and its presence on GitHub.

+ Make sure not to commit your REDCap API key to GitHub, your key is for you and you only. See `example.R` for an exxample of how to save your key locally and avoid uploading it to GitHub.
+ Do not distribute this repo. The `data/` directory has two data files (`.rda` extensions) that contain sensitive and protected information.
