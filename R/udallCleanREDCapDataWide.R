#' Clean up the data that come from Udall REDCap database
#'
#' Takes a data frame downloaded directly from REDCap using the redcap_read()
#' function, and prunes it, to remove NAs and empty columns, and appends "on"
#' or "off where necessary."
#'
#' @param dat Data frame that comes directly from redcap_read.
#' @param visit Which visit to use (1 or 2). Defaults to 1.
#'
#' @return A cleaned data frame, with NA and empty columns removed, on or off
#' prepended to the names, and all subject data on a single row (wide format).
#' Removes all subjects who have been flagged in REDCap as unanalyzable (for
#' whatever reason).
#'
#' @examples dat <- as.data.frame(redcap_read(
#'    redcap_uri="https://redcap.iths.org/api/",
#'    token=rc.token), stringsAsFactors = FALSE)
#'
#' cdat <- udallCleanREDCapDatawide(dat, 1)
#'
#' @export
#'
udallCleanREDCapDataWide <- function(dat, visit = 1) {

  if (!is.numeric(visit) | ! visit %in% 1:2)
    stop(paste(visit, "is not a valid visit ID. Choose 1 or 2."))

  # remove the leading "data" from the names
  names <- colnames(dat)
  newnames <- gsub("^data.", "", names)
  colnames(dat) <- newnames

  # Select the on/off/behavioral columns depending on which visit.
  arm <- paste0("arm_", visit)

  on <- subset(dat, redcap_event_name == paste0("on_", arm))
  off <- subset(dat, redcap_event_name == paste0("off_", arm))
  beh <- subset(dat, redcap_event_name == paste0("behavioral_", arm))

  # Analyze column is the same either way
  analyze <- subset(dat, redcap_event_name == "analyze_arm_4")

  # just throw in some assertions
  stopifnot(dim(on)[1] >= dim(off)[1])

  # remove missing or empty columns from data frames
  off <- off[, colSums(is.na(off)) < nrow(off)]
  blank <- colSums(off == "") <= nrow(off)
  blank[is.na(blank)] <- TRUE
  off <- off[,blank]

  on <- on[,colSums(is.na(on)) < nrow(on)]
  blank <- colSums(on=="") <= nrow(on)
  blank[is.na(blank)] <- TRUE
  on <- on[,blank]

  axcpt.cols <- c("off_axcpt_correctdetection",
                  "off_axcpt_falsealarm", "off_axcpt_correctnontarget",
                  "off_axcpt_rawdiff", "off_axcpt_dprime", "off_axcpt_rtcd",
                  "off_axcpt_rtcntd", "on_axcpt_correctdetection",
                  "on_axcpt_falsealarm", "on_axcpt_correctnontarget",
                  "on_axcpt_rawdiff","on_axcpt_dprime", "on_axcpt_rtcd",
                  "on_axcpt_rtcntd")

  ant.cols <- c("ant_acc", "ant_rt", "ant_alerting_all",
                "ant_alerting_correct", "ant_orienting_all",
                "ant_orienting_correct", "ant_conflict_correct",
                "ant_conflict_all")


  behcolnames <- c("idnum", axcpt.cols, ant.cols)

  beh <- beh[, behcolnames]
  # rename columns for off and on conditions
  colnames(off) <- paste("off_", colnames(off),sep="")
  colnames(on) <- paste("on_", colnames(on),sep="")
  # fix double on and double off and idnum variables
  colnames(on) <- gsub("on_on", "on", colnames(on))
  colnames(off) <- gsub("off_off", "off", colnames(off))

  # fix freesurfer names
  on <- renameFreeSurfer(on)

  # rename subject id from each of these - I like it to be idnum
  names(on)[names(on)=="on_idnum"] <- "idnum"
  names(off)[names(off)=="off_idnum"]  <- "idnum"

  # merge these data frames together from idnum
  merged <- merge(on, off, by="idnum", all.x=TRUE, all.y=TRUE)
  # merge in the behavioral data
  merged <- merge(merged, beh, by="idnum", all.x=TRUE, all.y=TRUE)

  # Add information about whether to analyze a subject
  analyze.cols <- c("analyze_visit_1", "analyze_visit_2")
  analyze.df <- analyze[, c("idnum", analyze.cols)]

  merged <- merge(merged, analyze.df, by = "idnum", all.x = TRUE, all.y = TRUE)

  # We will create a couple of useful variables here
  # Create sex variable
  merged$sex <- merged$on_health_demo_sex
  merged$sex[ ! merged$sex %in% 1:2 ] <- NA
  merged$sex <- as.factor(merged$sex)
  levels(merged$sex) <- c("male", "female")

  #create ethnicity variable
  merged$ethnicity <- merged$on_health_demo_ethnic
  merged$ethnicity[ ! merged$ethnicity %in% 1:2 ] <- NA
  merged$ethnicity <- as.factor(merged$ethnicity)
  levels(merged$ethnicity) <- c("hispanic", "not_hispanic")

  #create group variable
  merged$group <- merged$on_health_demo_group
  merged$group[merged$group > 2] <- NA
  merged$group <- as.factor(merged$group)
  # PD is 1, control is 2
  levels(merged$group) <- c("pd", "control")

  # score various assessments
  merged <- scoreFOG(merged)

  # convert UPDRS scoring columns to numeric if not already
  merged <- numericColumnsUPDRS(merged)

  # score UPDRS total
  merged <- scoreUPDRS(merged)

  # education - right now coming from health/demo
  merged$educ <- merged$on_health_demo_years_educ

  # compute scan age in years
  merged$scage <- as.numeric((as.Date(merged$on_mri_date) - as.Date(merged$on_mri_dob))/365)

  # score SAI
  merged <- scoreSAI(merged)

  # TODO:
  ### 1 ) ADD NEW CLEANING ITEMS TO data/test.csv

  # cross with !is.na so no NAs are introduced.
  if (visit == 1)
  {
    merged <- merged[merged$analyze_visit_1 & !is.na(merged$analyze_visit_1), ]
  }
  else if (visit == 2)
  {
    merged <- merged[merged$analyze_visit_2 & !is.na(merged$analyze_visit_2), ]
  }


  return(merged)
}
