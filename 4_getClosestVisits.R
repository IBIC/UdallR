# Load libraries required to download from REDCap and install custom packages.
# This loop installs missing pacakges.

library(REDCapR)

# Install the custom UdallR library. You may have to change the path, depending
# on where you downloaded UdallR to. Installation has to be done each time to
# pick up the new data file.

library(devtools)
install("~/UdallR", quiet = FALSE)
library(UdallR, quietly = TRUE,  warn.conflicts = FALSE)

today <- gsub("[^0-9]", "", Sys.Date())
outfile <- paste0("closest-visits/closestVisits-", today, ".csv")

dir.create("closest-visits", showWarnings = FALSE)

# It isn't secure to upload access tokens to GitHub, so store your access token
## in a file like this so R can read it.
rc.token <- readChar("~/UdallR/access-token.txt", nchars = 32)

# Read the data from the REDCap API. "stringsAsFactors" stops R from treating
## important strings as factors (which when written to file, are written as the
## underlying numeric representation.
dat <- as.data.frame(redcap_read(redcap_uri = "https://redcap.iths.org/api/",
                                 token = rc.token, batch_size = 150),
                     stringsAsFactors = FALSE)

# Test the UdallR cleaning function
# The visit variable doesn't do much right now.
cdat <- udallCleanREDCapDataWide(dat, visit = 1, drop.excluded = TRUE)

PD.cdat <- cdat[cdat$group == "pd", ]
HC.cdat <- cdat[cdat$group == "control", ]

# Get the closest visits, comparing to the most recent multivis
## UPDATE THIS LINE WHEN NECESSARY
closest.visits  <- getClosestACVisit(cdat,
                                     multivis.df = panuc_multivis_2019_05_06)

# Get the numeric id from the PWAXX-XXXX string
idnum <- gsub("[^0-9]", "", closest.visits$summary_id)

# Add arm name for upload
redcap_event_name <- "visit_for_mri_1_arm_3"

# The columns for upload are just those that are present in both data frames
cols.for.upload <- colnames(closest.visits) %in% colnames(cdat)
closest.visits <- closest.visits[, cols.for.upload]

message("We have the closest visits")

# Add the idnum and event name first (REDCap is particular)
out <- cbind(idnum, redcap_event_name, closest.visits)
# dim(closest.visits)

# Use incomplete if there's no agevisit (an easy sanity check), otherwise
## mark entries as complete.
complete <- ifelse(!is.na(out$agevisit), 2, 1)
out$ms_access_database_complete <- complete

# Remove rows with missing idnums
out$idnum[out$idnum == ""] <- NA
out <- out[!is.na(out$idnum), ]

# Save to file
write.csv(out, outfile, row.names = FALSE)

message(outfile, " saved")
