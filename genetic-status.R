# Load libraries required to download from REDCap and install custom packages.
# This loop installs missing pacakges.

# setwd("~")

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

# It isn't secure to upload access tokens to GitHub, so store your access token
## in a file like this so R can read it.
rc.token <- readChar("~/UdallR/access-token.txt", nchars = 32)

# Read the data from the REDCap API. "stringsAsFactors" stops R from treating
## important strings as factors (which when written to file, are written as the
## underlying numeric representation.
dat <- as.data.frame(redcap_read(redcap_uri = "https://redcap.iths.org/api/",
                                 token = rc.token),
                     stringsAsFactors = FALSE)

# Test the UdallR cleaning function
# The visit variable doesn't do much right now.
cdat <- udallCleanREDCapDataWide(dat, visit = 1)

pd <- cdat[cdat$group == "pd", ]
con <- cdat[cdat$group == "control", ]

# Number of PD people who have an e4 allele
pd.apoe4 <- grepl("4", pd$apoe)
pd.gbacarrier <- pd$gbastatus != "Non-Carrier"
pd.noncarrier <- pd$gbastatus == "Non-Carrier" & !grepl("4", pd$apoe)

con.apoe4 <- grepl("4", con$apoe)
con.gbacarrier <- con$gbastatus != "Non-Carrier"
con.noncarrier <- con$gbastatus == "Non-Carrier" & !grepl("4", con$apoe)


sum.NA <- function(v)
{
  name <- deparse(substitute(v))
  message(paste0(name, ": ", sum(v, na.rm = TRUE), " (", sum(is.na(v)), " NA)"))
}

message("")
message(paste0("N PD: ", nrow(pd)))
message(paste0("N Control: ", nrow(con)))

sum.NA(pd.apoe4)
sum.NA(pd.gbacarrier)
sum.NA(pd.noncarrier)
sum.NA(pd.apoe4 & pd.gbacarrier)

sum.NA(con.apoe4)
sum.NA(con.gbacarrier)
sum.NA(con.noncarrier)
sum.NA(con.apoe4 & con.gbacarrier)
