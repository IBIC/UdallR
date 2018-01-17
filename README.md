# UdallR
R functions to analyze data from the Udall Project 2 REDCap database. 


## Requesting REDCap token
You must generate a token through the REDCap website (https://redcap.iths.org/) in order to export the Udall data from REDCap.  

After logging in to REDCap, select "MyProjects" > "UDALL_P2" and then "API" under the "Applications" menu on the left-hand side.

<img src="./images/api_demo_1.png" height="400px" width="300px" />

If the "API" option is missing from the "Applications" menu, then someone in the project must give you privileges to see this page. If you are able to access the "API" page, there will be an option to request a new Application Programming Interface (API) token from the REDCap administrator. When your token request has successfully processed, your token should be printed out in green text on this page. A new token can be regenerated at any point by selecting "Regenerate token."

<img src="./images/api_demo_2.png" height="500px" width="700px" />

For more information, see REDCap's documentation of API tokens: https://redcap.iths.org/redcap_v7.1.2/API/help.php?content=default

## Loading the library functions

From the parent working directory that contains the UdallR folder, you should do the following (you can also provide an absolute path):

```R
install.packages("UdallR", repos=NULL, type="source")
library(UdallR)
```

Note that if you try to use the `install` function from the `devtools` package you will get an error. Install as above.

## Using the library functions

You would use them using code that looks something like this, where you substitute your REDCap token where it says `INSERTTOKENHERE`. Don't commit this token to github. 

```R
require(REDCapR)
library(REDCapR)
library(UdallR)

dat <- as.data.frame(redcap_read(redcap_uri="https://redcap.iths.org/api/", token="INSERTTOKENHERE"))

cdat <- udallCleanREDCapDataWide(dat)
```

## A guide to calculated variables

The program `udallCleanREDCapDataWide` calculates or converts a number of measures from the REDCap data automatically. H1re is a summary:

 + `sex`        Converts `on_health_demo_sex` to a named factor ("male" or "female").
 + `ethnicity`  Converts `on_health_demo_ethnic` to a named factor ("hispanic" or "not_hispanic").
 + `group`      Converts `on_health_demo_group` to a named factor ("pd", "control").
 + `scage`      Age at scan. Calculated `on_mri_date - on_mri_dob` divided by 365.
 + `educ`       Identical to `on_health_demo_years_educ`, just with an abbreviated column name for easier reference.
 + `dx_dominant_side` Names `dx_dominant_side` "left" and "right."
 + `on_fog_total` Sums freezing of gait questions.
 + UPDRS calculations:
    + `{on,off}_updrs_{3,4}_total` Sums the values for the UPDRS III or IV connected to the ON and OFF states.
    + `{on,off}_{left,right}_symptoms` Sums the following UPDRS III side-specific measures to create a lateralized symptom score: 3.4-3.8, 3.15-3.16, 3.17 (UE and LE).
    + `{on,off}_RminusL_symptoms` Subtracts the left-sided UPDRS III scores from the right-sided scores to create a laterality measure. PD symptoms tend to be left-biased (hence, negative).
+ `on_sai_sai`  Calculates the unconditioned amplitude for the subject's SAI run (only conducted on-medication).
+ Dual task cost:
    * The dual task cost measurements are calculated in the function `udallCalculateCost` and calculate the difference between the dual-task trial and the single-task trial (always `DT - ST`). Hence, the values for each of these can be speeds, distances, angles, etc.
    * The single-task trials match the regex `{on,off}_st_*`, the dual task `{on_off}_dt_*` and the cost columns `{on,off}_dtcost_*`.
    * As there are 203 numeric measurements (plus the completeness value, discussed below) in that range, it wouldn't do to summarize them all here.
    * Note that `{on,off}_{st,dt}_{gait,sway}_complete` are originally numeric (0 = incomplete, 1 = unverified, 2 = complete), and so the variables `{on,off}_dtcost_{gait,sway}_complete` are automatically calculated. Any non-zero value here means either original completeness measure is incomplete or unverified. 

## Important notes

A few important notes regarding `UdallR` and its presence on GitHub.

+ Make sure not to commit your REDCap API key to GitHub, your key is for you and you only. See `example.R` for an example of how to save your key locally and avoid uploading it to GitHub.
+ Do not distribute this repo. The `data/` directory has data files (`.rda` extensions) that contain sensitive and protected information.
