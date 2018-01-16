#' scoreUPDRS; Modifies original data.frame by appending total UPDRS columns for
#'  UPDRS Part 3 & 4.
#'
#' @param dat Input data frame with items to be scored.
#' @return dat Output data frame with items scored.
#'
#' @export

# return modified dataframe of dataframe passed in with calcuated totals of
## updrs items
scoreUPDRS <- function(dat) {

  # replace missing data codes with NA values - but see below where we handle
  ## missing data coded as pull downs for updrs part 3
  dat <- udallReplaceMissing(dat)

  #### On UPDRS Part III
  # create vector of UPDRS III items to be scored
  on_updrs_3_colnames <- paste0("on_updrs_", c("3_1", "3_2",
                           "3_3_neck", "3_3_rue", "3_3_lue", "3_3_rle",
                           "3_3_lle", "3_4_r", "3_4_l", "3_5_r",
                           "3_5_l", "3_6_r", "3_6_l", "3_7_r",
                           "3_7_l", "3_8_r", "3_8_l", "3_9", "3_10",
                           "3_11", "3_12", "3_13", "3_14", "3_15_r",
                           "3_15_l", "3_16_r", "3_16_l", "3_17_rue",
                           "3_17_lue", "3_17_rle", "3_17_lle", "3_17_lipjaw",
                           "3_18"))

  # remove missing data - in these values, numbers greater than 4 are missing
  ## data codes; set them to NA
  on <- dat[, on_updrs_3_colnames]
  on[on > 4] <- NA

  # calculate total score of on UPDRS III; append to end of data frame
  on_updrs_3_total <- apply(on[,on_updrs_3_colnames], 1, sum, na.rm = TRUE)

  # calculate left and right hemispheric symptoms
  on_left_symptoms <- dat$on_updrs_3_4_l + dat$on_updrs_3_5_l +
                          dat$on_updrs_3_6_l + dat$on_updrs_3_7_l +
                          dat$on_updrs_3_8_l + dat$on_updrs_3_15_l +
                          dat$on_updrs_3_16_l + dat$on_updrs_3_17_lue +
                          dat$on_updrs_3_17_lle

  on_right_symptoms <- dat$on_updrs_3_4_r + dat$on_updrs_3_5_r +
                            dat$on_updrs_3_6_r + dat$on_updrs_3_7_r +
                            dat$on_updrs_3_8_r + dat$on_updrs_3_15_r +
                            dat$on_updrs_3_16_r + dat$on_updrs_3_17_rue +
                            dat$on_updrs_3_17_rle

  # create vector of ON UPDRS IV items to be scored
  on_updrs_4_colnames <- c("on_updrs_4_1", "on_updrs_4_2", "on_updrs_4_3",
                           "on_updrs_4_4", "on_updrs_4_5", "on_updrs_4_6")

  # calculate total score of on UPDRS IV; append to end of data frame
  on_updrs_4_total <- apply(dat[, on_updrs_4_colnames], 1, sum, na.rm = TRUE)

  #### off UPDRS Part III
  # create vector of UPDRS III items to be scored
  off_updrs_3_colnames <- paste0("off_updrs_", c("3_1", "3_2",
                            "3_3_neck", "3_3_rue", "3_3_lue", "3_3_rle",
                            "3_3_lle", "3_4_r", "3_4_l", "3_5_r",
                            "3_5_l", "3_6_r", "3_6_l", "3_7_r",
                            "3_7_l", "3_8_r", "3_8_l", "3_9", "3_10",
                            "3_11", "3_12", "3_13", "3_14", "3_15_r",
                            "3_15_l", "3_16_r", "3_16_l", "3_17_rue",
                            "3_17_lue", "3_17_rle", "3_17_lle", "3_17_lipjaw",
                            "3_18"))

  # remove missing data - in these values, numbers greater than 4 are missing
  ## data codes; set them to NA
  off <- dat[, off_updrs_3_colnames]
  off[off > 4] <- NA

  # calculate total score of off UPDRS III; append to end of data frame
  off_updrs_3_total <- apply(off[, off_updrs_3_colnames], 1, sum, na.rm = TRUE)

  # calculate left and right hemispheric symptoms
  off_left_symptoms <- dat$off_updrs_3_4_l + dat$off_updrs_3_5_l +
                          dat$off_updrs_3_6_l + dat$off_updrs_3_7_l +
                          dat$off_updrs_3_8_l + dat$off_updrs_3_15_l +
                          dat$off_updrs_3_16_l + dat$off_updrs_3_17_lue +
                          dat$off_updrs_3_17_lle

  off_right_symptoms <- dat$off_updrs_3_4_r + dat$off_updrs_3_5_r +
                          dat$off_updrs_3_6_r + dat$off_updrs_3_7_r +
                          dat$off_updrs_3_8_r + dat$off_updrs_3_15_r +
                          dat$off_updrs_3_16_r + dat$off_updrs_3_17_rue +
                          dat$off_updrs_3_17_rle


  # create vector of off UPDRS IV items to be scored
  off_updrs_4_colnames <- c("off_updrs_4_1", "off_updrs_4_2", "off_updrs_4_3",
                            "off_updrs_4_4", "off_updrs_4_5", "off_updrs_4_6")

  # calculate total score of off UPDRS IV; append to end of data frame
  off_updrs_4_total <- apply(dat[,off_updrs_4_colnames], 1, sum, na.rm = TRUE)

  # Calculate laterality scores
  on_RminusL_symptoms <- on_right_symptoms - on_left_symptoms
  off_RminusL_symptoms <- off_right_symptoms - off_left_symptoms

  # Add calculated values to result data frame for returning it
  result <- cbind(dat, on_updrs_3_total, on_updrs_4_total, on_left_symptoms,
                          on_right_symptoms, on_RminusL_symptoms,
                  off_updrs_3_total, off_updrs_4_total, off_left_symptoms,
                          off_right_symptoms, off_RminusL_symptoms)

  # return modified data.frame
  return(result)
}

