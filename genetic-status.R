# Load libraries required to download from REDCap and install custom packages.
# This loop installs missing pacakges.

# setwd("~")

# Use Python-like optparse library (note: Python syntax for this)
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

# Get date out of the opt object
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
# If opt$all is true, the user wants to keep all subjects. The clean function
## wants to know if we are dropping the excluded file, so reverse opt$all.
cdat <- udallCleanREDCapDataWide(dat, visit = 1, drop.excluded = !opt$all)

# The first four variables should be fairly self-explanatory.
# apoe, gbastatus are the actual values, and redcap_* is a simple bool
short <- cdat[!is.na(cdat$idnum), c("idnum", "group",
                                   "off_mri_date", "on_mri_date",
                                   "apoe", "redcap_apoe4",
                                   "gbastatus", "redcap_gbacarrier")]

na.g <- sum(is.na(short$group))
if (na.g > 0) {
  message(paste(short$idnum[is.na(short$group)], collapse = ", "))
  stop(paste(na.g, "subjects missing group status! Cannot proceed"))
}

# Controls don't have off dates, make them NA
short$off_mri_date[short$group == "control"] <- NA

# Is the subject an APOE carrier?
# First, get the values from the apoe (the actual value) field
short$is.apoe <- grepl("4", short$apoe)
short$is.apoe[is.na(short$apoe)] <- NA
# If it's still NA, then use the redcap_apoe4 field
short$is.apoe[is.na(short$is.apoe)] <- as.logical(short$redcap_apoe4[is.na(short$is.apoe)])
message(paste("After checking,", as.character(sum(is.na(short$is.apoe))),
              "subjects are missing APOE records"))

# Is the subject an GBA carrier?
# First, get the values from the apoe (the actual value) field
short$is.gba <- !grepl("Non", short$gbastatus)
# If it's still NA, then use the redcap_apoe4 field
short$is.gba[is.na(short$is.gba)] <- as.logical(short$redcap_gbacarrier[is.na(short$is.gba)])
message(paste("After checking,", as.character(sum(is.na(short$is.gba))),
              "subjects are missing GBA records"))

# Okay, but now APOE e4+ subjects don't count as APOE if they are also GBA+
both <- short$is.apoe & short$is.gba
short$is.apoe[both] <- FALSE

# Use the first date they were scanned, ignoring any repeats, and drop NA to
## use the single visit date for controls
short$date <- apply(short[, c("on_mri_date", "off_mri_date")], 1, min,
                    na.rm = TRUE)

na.dates <- sum(is.na(as.Date(short$date, format = "%Y-%m-%d")))
if (na.dates > 0)
  stop(paste("There are", na.dates, "subjects with missing dates.",
             "Cannot proceed."))


# Were they scanned before the given date?
scanned.before <- short$date < as.Date(the.date)
short <- short[scanned.before, ]

message(paste(sum(!scanned.before), "participants have been scanned since",
                  as.character(the.date)))

# Split up into groups
pd  <- short[short$group == "pd", ]
con <- short[short$group == "control", ]

pd.apoe4 <- sum(pd$is.apoe, na.rm = TRUE)
pd.gbacarrier <- sum(pd$is.gba, na.rm = TRUE)
pd.noncarrier <- sum(!pd$is.gba & !pd$is.apoe, na.rm = TRUE)

con.apoe4 <- sum(con$is.apoe, na.rm = TRUE)
con.gbacarrier <- sum(con$is.gba, na.rm = TRUE)
con.noncarrier <- sum(!con$is.gba & !con$is.apoe, na.rm = TRUE)

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
message(paste0("Total: ", nrow(short)))

message("")
sum.NA(pd.apoe4,      "PD APOE e4+    ")
sum.NA(pd.gbacarrier, "PD GBA+        ")
sum.NA(pd.noncarrier, "PD non-carriers")

message("")
sum.NA(con.apoe4,      "Control APOE e4+")
sum.NA(con.gbacarrier, "-- Control GBA+")
sum.NA(con.noncarrier, "Control non-carriers")

# WHICH IDS ARE MISSING INFO?
message("")

if (sum(is.na(pd.apoe4)) > 0)
  message(paste("No APOE status (PD):\t",
                paste(pd$idnum[is.na(pd.apoe4)], collapse = " ")))

if (sum(is.na(pd.gbacarrier)) > 0)
  message(paste("No GBA status (PD):\t",
                paste(pd$idnum[is.na(pd.gbacarrier)], collapse = " ")))

if (sum(is.na(con.apoe4)) > 0)
  message(paste("No APOE status (C):\t",
                paste(con$idnum[is.na(con.apoe4)], collapse = " ")))

if (sum(is.na(con.gbacarrier)) > 0)
message(paste("No GBA status (C):\t",
              paste(con$idnum[is.na(con.gbacarrier)], collapse = " ")))

