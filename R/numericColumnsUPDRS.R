#' Returns modified version of passed data.frame with UPDRS elements converted to numeric.
#' @param dat data.frame to change UPDRS columns to numeric
#' @export
#'

numericColumnsUPDRS <- function(dat) {

  # column names of UPDRS items
  updrs_colnames <- c("idnum", "on_updrs_3_1", "on_updrs_3_2",
                      "on_updrs_3_3_neck", "on_updrs_3_3_rue", "on_updrs_3_3_lue", "on_updrs_3_3_rle",
                      "on_updrs_3_3_lle", "on_updrs_3_4_r", "on_updrs_3_4_l", "on_updrs_3_5_r",
                      "on_updrs_3_5_l", "on_updrs_3_6_r", "on_updrs_3_6_l", "on_updrs_3_7_r",
                      "on_updrs_3_7_l", "on_updrs_3_8_r", "on_updrs_3_8_l", "on_updrs_3_9", "on_updrs_3_10",
                      "on_updrs_3_11", "on_updrs_3_12", "on_updrs_3_13", "on_updrs_3_14", "on_updrs_3_15_r",
                      "on_updrs_3_15_l", "on_updrs_3_16_r", "on_updrs_3_16_l", "on_updrs_3_17_rue",
                      "on_updrs_3_17_lue", "on_updrs_3_17_rle", "on_updrs_3_17_lle", "on_updrs_3_17_lipjaw",
                      "on_updrs_3_18", "on_updrs_3_hoehn_yahr", "off_updrs_3_1", "off_updrs_3_2",
                      "off_updrs_3_3_neck", "off_updrs_3_3_rue", "off_updrs_3_3_lue", "off_updrs_3_3_rle",
                      "off_updrs_3_3_lle", "off_updrs_3_4_r", "off_updrs_3_4_l", "off_updrs_3_5_r",
                      "off_updrs_3_5_l", "off_updrs_3_6_r", "off_updrs_3_6_l", "off_updrs_3_7_r",
                      "off_updrs_3_7_l", "off_updrs_3_8_r", "off_updrs_3_8_l", "off_updrs_3_9", "off_updrs_3_10",
                      "off_updrs_3_11", "off_updrs_3_12", "off_updrs_3_13", "off_updrs_3_14", "off_updrs_3_15_r",
                      "off_updrs_3_15_l", "off_updrs_3_16_r", "off_updrs_3_16_l", "off_updrs_3_17_rue",
                      "off_updrs_3_17_lue", "off_updrs_3_17_rle", "off_updrs_3_17_lle", "off_updrs_3_17_lipjaw",
                      "off_updrs_3_18", "off_updrs_3_hoehn_yahr", "on_updrs_4_1",
                      "on_updrs_4_2", "on_updrs_4_3", "on_updrs_4_4", "on_updrs_4_5",
                      "on_updrs_4_6", "off_updrs_4_1", "off_updrs_4_2", "off_updrs_4_3", "off_updrs_4_4", "off_updrs_4_5",
                      "off_updrs_4_6")

  # change to columns to numeric
  dat[updrs_colnames] <- lapply(dat[updrs_colnames], as.numeric)

  # return modified data.frame
  return(dat)
}
