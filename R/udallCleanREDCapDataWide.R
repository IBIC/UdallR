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
  genetic <- subset(dat, redcap_event_name == "genetic_data_arm_5")

  include <- subset(dat, redcap_event_name == "study_inclusion_arm_4")
  include <- include[, c("idnum", "include")]
  include$include <- as.logical(include$include)

  # Arm 3, which has the closests visit that has been reuploaded to REDCap
  closest.visit <- subset(dat, redcap_event_name == "visit_for_mri_1_arm_3")

  # just throw in some assertions
  stopifnot(dim(on)[1] >= dim(off)[1])

  # remove missing or empty columns from data frames
  off <- off[, colSums(is.na(off)) < nrow(off)]
  # blank <- colSums(off == "") == nrow(off)
  # blank[is.na(blank)] <- TRUE
  # off <- off[, !blank]

  on <- on[, colSums(is.na(on)) < nrow(on)]
  # blank <- colSums(on == "") == nrow(on)
  # blank[is.na(blank)] <- FALSE
  # on <- on[, !blank]

  # Select a subset of columns for behavior and genetic axcpt.cols <-
  axcpt.cols <- c("on_axcpt_correctdetection", "on_axcpt_falsealarm",
                  "on_axcpt_correctnontarget" ,"on_axcpt_rawdiff", "on_axcpt_dprime",
                  "on_axcpt_rtcd" ,"on_axcpt_rtcntd" ,"on_ax_rawcorrect",
                  "on_bx_rawcorrect" ,"on_by_rawcorrect" ,"on_ay_rawcorrect",
                  "on_ax_percent" ,"on_bx_percent" ,"on_by_percent" ,"on_ay_percent",
                  "on_ax_rt" ,"on_bx_rt" ,"on_by_rt" ,"on_ay_rt" ,"on_ax_sd","on_bx_sd", "on_by_sd" ,"on_ay_sd",
                  "off_axcpt_correctdetection",
                  "off_axcpt_falsealarm" ,"off_axcpt_correctnontarget","off_axcpt_rawdiff", "off_axcpt_dprime" ,"off_axcpt_rtcd"
                  ,"off_axcpt_rtcntd" ,"off_ax_rawcorrect", "off_bx_rawcorrect","off_by_rawcorrect" ,"off_ay_rawcorrect", "off_ax_percent","off_bx_percent" ,"off_by_percent" ,"off_ay_percent", "off_ax_rt"
                  ,"off_bx_rt" ,"off_by_rt" ,"off_ay_rt" ,"off_ax_sd" ,"off_bx_sd",
                  "off_by_sd" ,"off_ay_sd")


  ant.cols <- c("ant_acc", "ant_rt", "ant_alerting_all",
                "ant_alerting_correct", "ant_orienting_all",
                "ant_orienting_correct", "ant_conflict_correct",
                "ant_conflict_all")

  genetic.colnames <- c("idnum", "redcap_apoe", "redcap_apoe4", "redcap_gba",
                        "redcap_gbacarrier", "genetic_data_complete")

  dx.colnames <- grep("dx", colnames(dat))

  # # There are so many closest column names, load them from file.
  # data("Codebook_PaNUC_2017_07_07")
  # closest.colnames <- as.character(Codebook_PaNUC_2017_07_07$Stata_Variable_Name)


  closest.colnames <- tolower(colnames(panuc_multivis_2018_03_30))
  closest.colnames <- closest.colnames[closest.colnames != ""]
  closest.colnames <- c("idnum",
                        closest.colnames[closest.colnames %in% colnames(dat)])

  behcolnames <- c("idnum", axcpt.cols, ant.cols)

  beh <- beh[, behcolnames]
  genetic <- genetic[, genetic.colnames]
  closest.visit <- data.frame(closest.visit[, closest.colnames])

  # rename columns for off and on conditions
  colnames(off) <- paste0("off_", colnames(off))
  colnames(on) <- paste0("on_", colnames(on))

  # fix double on and double off and idnum variables
  colnames(on) <- gsub("on_on", "on", colnames(on))
  colnames(off) <- gsub("off_off", "off", colnames(off))

  # fix freesurfer names
  on <- renameFreeSurfer(on)

  # rename subject id from each of these - I like it to be idnum
  names(on)[names(on) == "on_idnum"] <- "idnum"
  names(off)[names(off) == "off_idnum"]  <- "idnum"

  # merge these data frames together from idnum
  # Note that there will be a problem if we "grow" the number of records by merging additional arms -
  # that would mean that there is behavioral or analysis records for subjects that we haven't entered.
  # This is a crude check on that! If you get this kind of problem, go back to the database and figure out what records you've added incorrectly.
  merged <- merge(on, off, by = "idnum", all.x = TRUE, all.y = TRUE)

  # Merged data with inclusion status
  merged <- merge(merged, include, by = "idnum", all.x = TRUE, all.y = TRUE)

  # Include by default people with no inclusion status (it's most likely they're
  # OK, just haven't been processed yet.)
  merged$include[is.na(merged$include)] <- TRUE

  if (sum(!merged$include) > 0)
  {
    message(paste("Dropping", sum(!merged$include),
                  "subjects from whole study: ",
                  paste(merged$idnum[!merged$include], collapse = " ")))

    merged <- merged[merged$include, ]
  }

  n <- dim(merged)[1]

  # merge in the behavioral data
  merged <- merge(merged, beh, by = "idnum", all.x = TRUE)
  stopifnot(n == dim(merged)[1])

  #merge in the genetic data
  merged <- merge(merged, genetic, by = "idnum", all.x = TRUE)
  stopifnot(n == dim(merged)[1])

  # merge in the analysis flag
  # merged <- merge(merged, include, by = "idnum", all.x = TRUE)
  # stopifnot(n == dim(merged)[1])

  # merge in clinical core visit
  merged <- merge(merged, closest.visit, by = "idnum", all.x = TRUE)
  stopifnot(n == dim(merged)[1])

  merged <- merged[, !grepl("on_off", colnames(merged))]

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

  # Replace why columns with strings
  fmri.reasons <- c("Motion", "Did not understand task", "Sleep", "Recon error",
                    "Poor performance", "Excluded", "Other", "Behavior issues")
  for (variable in c("on_analyze_axcpt_fmri_why", "off_analyze_axcpt_fmri_why",
                     "on_analyze_rest_fmri_why", "off_analyze_rest_fmri_why"))
  {
    merged[, variable] <- fmri.reasons[merged[, variable]]
  }

  behavioral.reasons <- c("Did not understand task", "Sleep", "Other")
  merged$on_analyze_behavior_why <- behavioral.reasons[merged$on_analyze_behavior_why]
  merged$off_analyze_behavior_why <- behavioral.reasons[merged$off_analyze_behavior_why]

  # Add DTI when it shows up in the data

  # Add SAI when analyze_sai_why is converted from a text entry box to a factor

  merged$dx_dominant_side <- as.factor(merged$dx_dominant_side)
  levels(merged$dx_dominant_side) <- c("left", "right")

  # score various assessments
  merged <- scoreFOG(merged)

  # convert UPDRS scoring columns to numeric if not already
  merged <- numericColumnsUPDRS(merged)

  # score UPDRS total
  merged <- scoreUPDRS(merged)

  merged <- udallReplaceMissing(merged)

  # education - right now coming from health/demo
  merged$educ <- merged$on_health_demo_years_educ

  # compute scan age in years
  merged$scage <- as.numeric((as.Date(merged$on_mri_date) - as.Date(merged$on_mri_dob))/365)

  # score SAI
  merged <- scoreSAI(merged)

  # Calculate sway
  dtcost <- udallCalculateCost(merged, grep("_st_", colnames(merged),
                                            value = TRUE),
                                       grep("_dt_", colnames(merged),
                                            value = TRUE))
  merged <- cbind(merged, dtcost)


  # TODO:
  ### 1 ) ADD NEW CLEANING ITEMS TO data/test.csv

  # Identify subjects who should be included in the study only
  # If there are NAs present let us count them as true - perhaps missing data means they have not yet been evaluated.
  # if (visit == 1)
  # {
  #   if (sum(is.na(merged$analyze_visit_1)) > 0)
  #   {
  #     logError(merged, is.na(merged$analyze_visit_1),
  #              "No visit 1 analyze entry. Including by default",
  #              "analyze_visit_1", NA)
  #   }
  #
  #   merged$analyze_visit_1[is.na(merged$analyze_visit_1)] <- TRUE
  #   merged <- merged[merged$analyze_visit_1 ==TRUE, ]
  # }
  # else if (visit == 2)
  # {
  #   if (sum(is.na(merged$analyze_visit_2)) > 0)
  #   {
  #     logError(merged, is.na(merged$analyze_visit_2),
  #              "No visit 2 analyze entry. Including by default",
  #              "analyze_visit_2", NA)
  #   }
  #
  #   merged$analyze_visit_2[is.na(merged$analyze_visit_2)] <- TRUE
  #   merged <- merged[merged$analyze_visit_2 ==TRUE, ]
  # }

  # This is dangerous - do not assume blanket error codes. Use udallReplaceMissing function
  # Replace error codes ([-800, -900]) with NAs

  #  for (c in 1:ncol(merged))
  #  {
  #    if (is.numeric(merged[, c]))
  #      merged[, c]  <- ifelse(merged[, c] < -799, NA, merged[, c])
  #  }

  return(merged)
}
