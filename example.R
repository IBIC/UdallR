# Load libraries required to download from REDCap and install custom packages.
# This loop installs missing pacakges.

# setwd("~")

for (package in c("REDCapR", "devtools", "ggplot2"))
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

# Get a table with which data can be analyzed for which participants
analyzing <- udallGetAnalyze(cdat)

# Test the UdallR data-checking function
checked <- udallCheckDataWide(cdat, error.file = "REDCap-errors.txt")

# Test the UdallR cross-checking function that checks for inconsistencies in
## data uploaded to REDCap by IBIC and the data in the MS Access database
## maintained by the Udall team.
udallCrosscheckEntries(cdat, error.file = "mismatch-errors.txt")

closest.visits <- getClosestACVisit(cdat,
                                    multivis.df = panuc_multivis_2018_08_15)

demographics <- c("agevisit", "education_years", "gender", "handedness")
pd.symptoms <- c("hoehn_and_yahr_m0", "dx_dominant_side", "updrs_new_1_total",
                 "updrs_new_2_total", "updrs_new_3_total", "updrs_new_4_total",
                 "ageatonset")
cog <- c("moca_score", "trails_b_seconds")

closest.visits[closest.visits < 0] <- NA
closest.visits$handedness <- as.factor(closest.visits$handedness)
levels(closest.visits$handedness) <- c("right", "left", "mixed")

closest.visits$dx_dominant_side <- as.factor(closest.visits$dx_dominant_side)

udallDemographicTable(closest.visits, c(demographics, pd.symptoms, cog),
                      group.by = "parkinsonism_status",
                      group.ok = c("PD", "Unaffected"))

checked <- checked[, !grepl("redcap_event_name", colnames(checked))]
checked$redcap_event_name <- "visit_for_mri_1_arm_3"

write.csv(checked, file = paste0("upload-", Sys.Date(), ".csv"),
          row.names = FALSE)


# Load in gait and balance data from Excel spreadsheet
gnb <- udallProcessGnB("~/UdallR/data/gaitbalance-170912.xlsx",
                       write = "~/UdallR")

# Calcuate task cost measures for sway and gait tasks.
# sway.cost <- udallCalculateCost(gnb$ST_Sway, gnb$DT_Sway)
# gait.cost <- udallCalculateCost(gnb$ST_Gait, gnb$DT_Gait)

# Get list of analyzeable subjects
controls <- cdat[cdat$group == "control", ]
pds <- cdat[cdat$group == "pd", ]

axcpt.controls <- controls$idnum[controls$on_analyze_axcpt_fmri == 1 &
                                   !(is.na(controls$on_analyze_axcpt_fmri))]
axcpt.pd.on <- pds$idnum[pds$on_analyze_axcpt_fmri == 1 &
                                !(is.na(pds$on_analyze_axcpt_fmri))]
axcpt.pd.off <- pds$idnum[pds$off_analyze_axcpt_fmri == 1 &
                           !(is.na(pds$off_analyze_axcpt_fmri))]
axcpt.pd.both <- intersect(axcpt.pd.on, axcpt.pd.off)

write.table(axcpt.controls, file = "axcpt-controls.txt", row.names = FALSE,
            col.names = FALSE)
write.table(axcpt.pd.on, file = "axcpt-pd-on.txt", row.names = FALSE,
            col.names = FALSE)
write.table(axcpt.pd.off, file = "axcpt-pd-off.txt", row.names = FALSE,
            col.names = FALSE)
write.table(axcpt.pd.both, file = "axcpt-pd-both.txt", row.names = FALSE,
            col.names = FALSE)

### Some plots

ggplot(subset(cdat, cdat$handedness < 3),
       aes(x = on_RminusL_symptoms, fill = as.factor(handedness))) +
  geom_density()

## PD

exclude.PD <- pds[, c("idnum", "on_analyze_rest_fmri",
                       "on_analyze_rest_fmri_why", "off_analyze_rest_fmri",
                       "off_analyze_rest_fmri_why", "on_mri_me_rs_notes",
                       "off_mri_me_rs_notes"), ]

both.OK <- exclude.PD$on_analyze_rest_fmri & exclude.PD$off_analyze_rest_fmri
OK <- exclude.PD[both.OK, ]

excluded <- exclude.PD[!both.OK, ]

# If they were excluded for motion in either scan, change the flag to OK
on.excess.motion <- excluded$on_analyze_rest_fmri_why == "Excessive motion"
excluded$on_analyze_rest_fmri[on.excess.motion] <- 1
off.excess.motion <- excluded$off_analyze_rest_fmri_why == "Excessive motion"
excluded$off_analyze_rest_fmri[off.excess.motion] <- 1

# Subset out people who were excluded for motion
secondary.OK <- excluded$on_analyze_rest_fmri == 1 & excluded$off_analyze_rest_fmri == 1
OK2 <- excluded[secondary.OK, ]

# Everyone who is OK or just had high motion
OKmo <- rbind(OK, OK2)

other.exclusions <- exclude.PD$idnum[!exclude.PD$idnum %in% OKmo$idnum]

other <- exclude.PD[exclude.PD$idnum %in% other.exclusions, ]

cat(OKmo$idnum, fill = TRUE)
cat(other$idnum, fill = TRUE)

out <- other
out$on_mri_me_rs_notes <- gsub("\n", "; ", out$on_mri_me_rs_notes)
out$off_mri_me_rs_notes <- gsub("\n", "; ", out$off_mri_me_rs_notes)
out$on_mri_me_rs_notes <- gsub(",", " ", out$on_mri_me_rs_notes)
out$off_mri_me_rs_notes <- gsub(",", " ", out$off_mri_me_rs_notes)

write.csv(out, "exclusions-why.csv", quote = FALSE, row.names = FALSE)
