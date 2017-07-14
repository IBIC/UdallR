# Load libraries required to download from REDCap and install custom packages.
# This loop installs missing pacakges.

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
install("~/UdallR", quiet = TRUE)
library(UdallR, quietly = TRUE,  warn.conflicts = FALSE)

today <- gsub("[^0-9]", "", Sys.Date())
outfile <- paste0("closestVisits-", today, ".csv")

# It isn't secure to upload access tokens to GitHub, so store your access token
## in a file like this so R can read it.
rc.token <- readChar("access-token.txt", nchars = 32)

# Read the data from the REDCap API. "stringsAsFactors" stops R from treating
## important strings as factors (which when written to file, are written as the
## underlying numeric representation.
dat <- as.data.frame(redcap_read(redcap_uri = "https://redcap.iths.org/api/",
                                 token = rc.token),
                     stringsAsFactors = FALSE)

# Test the UdallR cleaning function
# The visit variable doesn't do much right now.
cdat <- udallCleanREDCapDataWide(dat, visit = 1)

closest.visits  <- getClosestACVisit(cdat,
                                     multivis.df = panuc_multivis_2017_07_07)

idnum <- gsub("[^0-9]", "", closest.visits$summary_id)
redcap_event_name <- "visit_for_mri_1_arm_3"

closest.visits <- closest.visits[, colnames(closest.visits) %in% colnames(cdat)]

out <- cbind(idnum, redcap_event_name, closest.visits)

write.csv(out, outfile, row.names = FALSE)

cat(paste(outfile, "saved"), fill = TRUE)
