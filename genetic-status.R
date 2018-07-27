# Load libraries required to download from REDCap and install custom packages.
# This loop installs missing pacakges.

# setwd("~")

if (!require("optparse")) install.packages("optparse")
library(optparse)

option_list = list(
  make_option(c("-a", "--all"), action = "store_true", dest = "all",
              default = FALSE, help = "Include excluded subjects."),
  make_option(c("-d", "--date"), type = "character", default = Sys.Date(),
              help = "What day to compare against, default = today")
);

opt_parser = OptionParser(option_list = option_list);
opt = parse_args(opt_parser);

# Load these packages after parsing args because if the program is asked for
# help, it will load help faster this way.
for (package in c("REDCapR", "devtools"))
{
  if (!require(package, character.only = TRUE))
  {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

the.date <- opt$date

message(paste("Using date", as.character(the.date)))

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
cdat <- udallCleanREDCapDataWide(dat, visit = 1, drop.excluded = !opt$all)

cdat <- cdat[!is.na(cdat$idnum), c("idnum", "group", "on_mri_date", "apoe",
                                   "gbastatus")]

# Were they scanned before the given date?
scanned.before <- cdat$on_mri_date < as.Date(the.date)

message(paste(sum(!scanned.before), "participants have been scanned since",
                  the.date))

cdat <- cdat[scanned.before, ]

na.g <- sum(is.na(cdat$group))
if (na.g > 0) {
  message(paste(cdat$idnum[is.na(cdat$group)], collapse = ", "))
  stop(paste(na.g, "subjects missing group status! Cannot proceed"))
}

pd  <- cdat[cdat$group == "pd", ]
con <- cdat[cdat$group == "control", ]

# Number of PD people who have an e4 allele and are GBA-
pd.apoe4 <- grepl("4", pd$apoe) & pd$gbastatus == "Non-Carrier"
pd.apoe4[is.na(pd$apoe)] <- NA

# Anyone who isn't GBA-
pd.gbacarrier <- pd$gbastatus != "Non-Carrier"

# GBA+ and APOE e4-
pd.noncarrier <- pd$gbastatus == "Non-Carrier" & !grepl("4", pd$apoe)

con.apoe4 <- grepl("4", con$apoe) & con$gbastatus == "Non-Carrier"
con.apoe4[is.na(con$apoe)] <- NA

con.gbacarrier <- con$gbastatus != "Non-Carrier"
con.noncarrier <- con$gbastatus == "Non-Carrier" & !grepl("4", con$apoe)

sum.NA <- function(v, category)
{
  name <- deparse(substitute(v))

  # return(c(sum(v, na.rm = TRUE)), sum(is.na(v)))

  message(paste0(category, ":\t", sum(v, na.rm = TRUE), " ("), sum(is.na(v)),
          ")")
}

message("")
message(paste0("N PD: ", nrow(pd)))
message(paste0("N Control: ", nrow(con)))
message(paste0("Total: ", nrow(cdat)))

message("")
sum.NA(pd.apoe4,      "PD APOE e4+    ")
sum.NA(pd.gbacarrier, "PD GBA+        ")
sum.NA(pd.noncarrier, "PD non-carriers")

message("")
sum.NA(con.apoe4,      "Control APOE e4+")
# sum.NA(con.gbacarrier, "PD GBA+")
sum.NA(con.noncarrier, "Control non-carriers")

# WHICH IDS ARE MISSING INFO?
message("")

message(paste("No APOE status (PD):\t",
              paste(pd$idnum[is.na(pd.apoe4)], collapse = " ")))
message(paste("No GBA status (PD):\t",
              paste(pd$idnum[is.na(pd.gbacarrier)], collapse = " ")))

message(paste("No APOE status (C):\t",
              paste(con$idnum[is.na(con.apoe4)], collapse = " ")))
message(paste("No GBA status (C):\t",
              paste(con$idnum[is.na(con.gbacarrier)], collapse = " ")))

