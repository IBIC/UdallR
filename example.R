# Load libraries required to download from REDCap and install custom packages.
# This loop installs missing pacakges.

setwd("~")

for (package in c("REDCapR", "devtools"))
{
  if (!require(package, character.only = TRUE))
  {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Install the custom UdallR library. You may have to change the path, depending
## on where you downloaded UdallR to.
install("UdallR", quiet = TRUE)
library(UdallR, quietly = TRUE,  warn.conflicts = FALSE)

# It isn't secure to upload access tokens to GitHub, so store your access token
## in a file like this so R can read it.
rc.token <- readChar("UdallR/access-token.txt", nchars = 32)

# Read the data from the REDCap API. "stringsAsFactors" stops R from treating
## important strings as factors (which when written to file, are written as the
## underlying numeric representation.
dat <- as.data.frame(redcap_read(redcap_uri = "https://redcap.iths.org/api/",
                                    token = rc.token),
                     stringsAsFactors = FALSE)

# Test the UdallR cleaning function
# The visit variable doesn't do much right now.
cdat <- udallCleanREDCapDataWide(dat, visit = 1)

# Test the UdallR data-checking function
checked <- udallCheckDataWide(cdat, error.file = "REDCap-errors.txt")

# Test the UdallR cross-checking function that checks for inconsistencies in
## data uploaded to REDCap by IBIC and the data in the MS Access database
## maintained by the Udall team.
udallCrosscheckEntries(cdat, error.file = "mismatch-errors.txt")

closest.visits <- getClosestACVisit(cdat,
                                    multivis.df = panuc_multivis_2017_07_17)

demographics <- c("agevisit", "education_years", "gender", "handedness")
pd.symptoms <- c("hoehn_and_yahr_m0", "dx_dominant_side", "updrs_new_1_total",
                 "updrs_new_2_total", "updrs_new_3_total", "updrs_new_4_total",
                 "ageatonset")
cog <- c("moca_score", "trails_b_seconds")

closest.visits[closest.visits < 0] <- NA
closest.visits$handedness <- as.factor(closest.visits$handedness)
levels(closest.visits$handedness) <- c("right", "left", "mixed")

closest.visits$dx_dominant_side <- as.factor(closest.visits$dx_dominant_side)

udallDemographicTable(closest.visits, c(demographics, pd.symptoms, cog))

# checked <- checked[, !grepl("redcap_event_name", colnames(checked))]
# checked$redcap_event_name <- "visit_for_mri_1_arm_3"
#
# write.csv(checked, file = paste0("upload-", Sys.Date(), ".csv"),
#           row.names = FALSE)