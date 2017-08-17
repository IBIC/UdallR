#' Transform gait and balance data
#'
#' Cleans up column names, and process gait and balance data for upload to
#' REDCap.
#'
#' @param dat A data frame with the gait and balance data
#'
#' @export


transformGnB <- function(dat) {

  # Process column names
  colnames(dat) <- tolower(colnames(dat))
  colnames(dat) <- gsub("\\s", "_", colnames(dat))
  colnames(dat) <- gsub("_-_", "_", colnames(dat))

  # Sway
  colnames(dat) <- gsub("postural_sway_acc_", "postsway_acc_",
                        colnames(dat))
  colnames(dat) <- gsub("postural_sway_angles_", "postsway_ang_",
                        colnames(dat))
  colnames(dat) <- gsub("95%_", "95", colnames(dat))
  colnames(dat) <- gsub("axis_", "axis", colnames(dat))
  colnames(dat) <- gsub("radius", "rad", colnames(dat))
  colnames(dat) <- gsub("rotation", "rot", colnames(dat))
  colnames(dat) <- gsub("centroidal_frequency", "centfreq", colnames(dat))
  colnames(dat) <- gsub("frequency_dispersion", "freqdisp", colnames(dat))
  colnames(dat) <- gsub("mean_velocity", "meanvelocity", colnames(dat))
  colnames(dat) <- gsub("path_length", "pathlength", colnames(dat))
  colnames(dat) <- gsub("rms_sway", "rmssway", colnames(dat))
  colnames(dat) <- gsub("sway_area", "swayarea", colnames(dat))

  colnames(dat) <- gsub("\\(coronal\\)", "cor", colnames(dat))
  colnames(dat) <- gsub("\\(sagittal\\)", "sag", colnames(dat))

  colnames(dat) <- gsub("_\\(.*\\)", "", colnames(dat))

  # Gait
  colnames(dat) <- gsub("anticipatory_postural_adjustment", "antposadj",
                        colnames(dat))
  colnames(dat) <- gsub("apa_", "", colnames(dat))
  colnames(dat) <- gsub("duration", "dur", colnames(dat))
  colnames(dat) <- gsub("first_step", "firststep", colnames(dat))
  colnames(dat) <- gsub("range_of_motion", "range", colnames(dat))
  colnames(dat) <- gsub("_at_", "_", colnames(dat))
  colnames(dat) <- gsub("gait_cycle", "gaitcycle", colnames(dat))
  colnames(dat) <- gsub("gait_speed", "gaitspeed", colnames(dat))
  colnames(dat) <- gsub("gait_lower_limb", "gait_lower_limb",
                        colnames(dat))
  colnames(dat) <- gsub("foot_strike", "footstrike", colnames(dat))
  colnames(dat) <- gsub("angle", "ang", colnames(dat))
  colnames(dat) <- gsub("toe_", "toe", colnames(dat))
  colnames(dat) <- gsub("single_limb_support", "singlelimbsupport",
                        colnames(dat))
  colnames(dat) <- gsub("^step_dur", "stepdur", colnames(dat))
  colnames(dat) <- gsub("stride_length\\(m\\)", "stridelength", colnames(dat))
  colnames(dat) <- gsub("terminal_double_support", "terminaldoublesupport",
                        colnames(dat))
  colnames(dat) <- gsub("coronal", "cor", colnames(dat))
  colnames(dat) <- gsub("sagittal", "sag", colnames(dat))
  colnames(dat) <- gsub("transverse", "trans", colnames(dat))
  colnames(dat) <- gsub("upper_limb_arm_swing_velocity",
                        "upperlimb_armswingvel", colnames(dat))
  colnames(dat) <- gsub("upper_limb_arm_range", "upperlimb_armswingrange",
                        colnames(dat))
  colnames(dat) <- gsub("steps_in_turn", "stepsinturn", colnames(dat))
  colnames(dat) <- gsub("_turn", "", colnames(dat))
  colnames(dat) <- gsub("std", "sd", colnames(dat))

  colnames(dat) <- gsub("_std$", "_sd", colnames(dat))
  colnames(dat) <- gsub("_n$", "_num", colnames(dat))
  colnames(dat) <- gsub("\\[|\\]", "", colnames(dat))


  # Other information
  idnum <- gsub("[^0-9]", "", dat$subject_public_id)
  task <- tolower(substr(dat$condition[1], 7, 8))
  session_XXX <- ifelse(dat$session == "Second" | is.na(dat$session), 1, 2)
  group_XXX <- dat$groups
  date_XXX <- as.Date(gsub("-.*$", "", dat$record_date), format="%Y%m%d")

  redcap_event_name <- paste0(ifelse(session_XXX == 1, "on", "off"), "_arm_1")
  complete <- 2

  # This selects the Trial Notes column and all the columns after the last
  ## "header" information
  output1 <- cbind(session_XXX, group_XXX, date_XXX, dat[, c(7, 10:ncol(dat))])
  colnames(output1) <- paste0(task, "_", colnames(output1))

  output2 <- cbind(idnum, redcap_event_name, complete, output1)

  type <- ifelse(grepl("Stance", dat$condition[1]), "sway", "gait")
  colnames(output2) <- gsub("XXX", type, colnames(output2))

  colnames(output2)[3] <- paste0(task, "_", type, "_complete") # Complete
  colnames(output2)[7] <- paste0(task, "_trialnotes_", type)  # Trial notes

  return(as.data.frame(output2))
}
