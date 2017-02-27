#' scoreUPDRS; Modifies original data.frame by appending total UPDRS columns for UPDRS Part 3 & 4.
#' @param dat Input data frame with items to be scored.
#' @return dat Output data frame with items scored.

# return modified dataframe of dataframe passed in with calcuated totals of updrs items
scoreUPDRS <- function(dat) {
  # replace missing data codes with NA values
  dat <- udallReplaceMissing(dat)
  
  #### On UPDRS Part III
  # create vector of UPDRS III items to be scored
  on_updrs_3_colnames <- c("on_updrs_3_1", "on_updrs_3_2", 
                        "on_updrs_3_3_neck", "on_updrs_3_3_rue", "on_updrs_3_3_lue", "on_updrs_3_3_rle", 
                        "on_updrs_3_3_lle", "on_updrs_3_4_r", "on_updrs_3_4_l", "on_updrs_3_5_r", 
                        "on_updrs_3_5_l", "on_updrs_3_6_r", "on_updrs_3_6_l", "on_updrs_3_7_r", 
                        "on_updrs_3_7_l", "on_updrs_3_8_r", "on_updrs_3_8_l", "on_updrs_3_9", "on_updrs_3_10", 
                        "on_updrs_3_11", "on_updrs_3_12", "on_updrs_3_13", "on_updrs_3_14", "on_updrs_3_15_r", 
                        "on_updrs_3_15_l", "on_updrs_3_16_r", "on_updrs_3_16_l", "on_updrs_3_17_rue", 
                        "on_updrs_3_17_lue", "on_updrs_3_17_rle", "on_updrs_3_17_lle", "on_updrs_3_17_lipjaw", 
                        "on_updrs_3_18", "on_updrs_3_hoehn_yahr")
  
  # calculate total score of on UPDRS III; append to end of data frame
  dat[, "on_updrs_3_total"] <- apply(dat[,on_updrs_3_colnames],1,sum,na.rm=TRUE)
  
  # create vector of on UPDRS IV items to be scored
  on_updrs_4_colnames <- c("on_updrs_4_1", "on_updrs_4_2", "on_updrs_4_3", "on_updrs_4_4", "on_updrs_4_5", 
                        "on_updrs_4_6")
  
  # calculate total score of on UPDRS IV; append to end of data frame 
  dat[, "on_updrs_4_total"] <- apply(dat[,on_updrs_4_colnames],1,sum,na.rm=TRUE)
  
  #### off UPDRS Part III
  # create vector of UPDRS III items to be scored
  off_updrs_3_colnames <- c("off_updrs_3_1", "off_updrs_3_2", 
                           "off_updrs_3_3_neck", "off_updrs_3_3_rue", "off_updrs_3_3_lue", "off_updrs_3_3_rle", 
                           "off_updrs_3_3_lle", "off_updrs_3_4_r", "off_updrs_3_4_l", "off_updrs_3_5_r", 
                           "off_updrs_3_5_l", "off_updrs_3_6_r", "off_updrs_3_6_l", "off_updrs_3_7_r", 
                           "off_updrs_3_7_l", "off_updrs_3_8_r", "off_updrs_3_8_l", "off_updrs_3_9", "off_updrs_3_10", 
                           "off_updrs_3_11", "off_updrs_3_12", "off_updrs_3_13", "off_updrs_3_14", "off_updrs_3_15_r", 
                           "off_updrs_3_15_l", "off_updrs_3_16_r", "off_updrs_3_16_l", "off_updrs_3_17_rue", 
                           "off_updrs_3_17_lue", "off_updrs_3_17_rle", "off_updrs_3_17_lle", "off_updrs_3_17_lipjaw", 
                           "off_updrs_3_18", "off_updrs_3_hoehn_yahr")
  
  # calculate total score of off UPDRS III; append to end of data frame
  dat[, "off_updrs_3_total"] <- apply(dat[,off_updrs_3_colnames],1,sum,na.rm=TRUE)
  
  # create vector of off UPDRS IV items to be scored
  off_updrs_4_colnames <- c("off_updrs_4_1", "off_updrs_4_2", "off_updrs_4_3", "off_updrs_4_4", "off_updrs_4_5", 
                           "off_updrs_4_6")
  
  # calculate total score of off UPDRS IV; append to end of data frame 
  dat[, "off_updrs_4_total"] <- apply(dat[,off_updrs_4_colnames],1,sum,na.rm=TRUE)
  
  # return modified data.frame
  return(dat)
}
