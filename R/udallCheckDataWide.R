#' Check the data that have been processed by udallCleanREDCapDataWide
#' This function contains arbitrary sanity checks to make sure that there
#' are no obvious errors in the data. It will issue warnings, but currently
#' will not stop if there is a problem.
#' @param dat Data frame that has been processed and cleaned (missing codes replaced with NA
#' @export

# passed a data.frame, min and max; will print out warning and id numbers that are not within the specified 
# min and max
check_range <- function(dat, min, max) {
  idnum <- dat$idnum
  dat$idnum <- NULL
  outOfRange <- which(names(which(rowSums(dat < min | max < dat, na.rm = TRUE) != 0))
                      == rownames(dat))
  rowsNotInRange <- length(outOfRange)
  warningIfNot(rowsNotInRange == 0, paste("Incorrect data range for following subjects:", 
                                          paste(idnum[outOfRange], collapse = ", ")))
}

udallCheckDataWide <- function(dat) {
    # make sure subjects have unique ids
    # would be useful to print out any duplicates if there is an issue
    dupl_id <- sort(unique(dat$idnum[duplicated(dat$idnum)]))
    warningIfNot(sum(duplicated(dat$idnum)) == 0, paste("Duplicate IDs for ", length(dupl_id), 
                                                        "subjects:", paste(dupl_id, collapse = " ")))
                                        # make sure we have basic demographic data
    miss_sex <- sort(dat$idnum[is.na(dat$sex)])
    warningIfNot(sum(is.na(dat$sex)) == 0, paste("Missing sex for ", length(miss_sex), 
                                                 "subjects: ", paste(miss_sex,collapse = " ")))
    
    # column names of on_updrs_3 scoring items that are not hoens and yahr
    on_updrs_3_colnames <- c("idnum", "on_updrs_3_1", "on_updrs_3_2", 
                             "on_updrs_3_3_neck", "on_updrs_3_3_rue", "on_updrs_3_3_lue", "on_updrs_3_3_rle", 
                             "on_updrs_3_3_lle", "on_updrs_3_4_r", "on_updrs_3_4_l", "on_updrs_3_5_r", 
                             "on_updrs_3_5_l", "on_updrs_3_6_r", "on_updrs_3_6_l", "on_updrs_3_7_r", 
                             "on_updrs_3_7_l", "on_updrs_3_8_r", "on_updrs_3_8_l", "on_updrs_3_9", "on_updrs_3_10", 
                             "on_updrs_3_11", "on_updrs_3_12", "on_updrs_3_13", "on_updrs_3_14", "on_updrs_3_15_r", 
                             "on_updrs_3_15_l", "on_updrs_3_16_r", "on_updrs_3_16_l", "on_updrs_3_17_rue", 
                             "on_updrs_3_17_lue", "on_updrs_3_17_rle", "on_updrs_3_17_lle", "on_updrs_3_17_lipjaw", 
                             "on_updrs_3_18")
    
    # column names of off_updrs_3 scoring items that are not hoens and yahr
    off_updrs_3_colnames <- c("idnum", "off_updrs_3_1", "off_updrs_3_2", 
                              "off_updrs_3_3_neck", "off_updrs_3_3_rue", "off_updrs_3_3_lue", "off_updrs_3_3_rle", 
                              "off_updrs_3_3_lle", "off_updrs_3_4_r", "off_updrs_3_4_l", "off_updrs_3_5_r", 
                              "off_updrs_3_5_l", "off_updrs_3_6_r", "off_updrs_3_6_l", "off_updrs_3_7_r", 
                              "off_updrs_3_7_l", "off_updrs_3_8_r", "off_updrs_3_8_l", "off_updrs_3_9", "off_updrs_3_10", 
                              "off_updrs_3_11", "off_updrs_3_12", "off_updrs_3_13", "off_updrs_3_14", "off_updrs_3_15_r", 
                              "off_updrs_3_15_l", "off_updrs_3_16_r", "off_updrs_3_16_l", "off_updrs_3_17_rue", 
                              "off_updrs_3_17_lue", "off_updrs_3_17_rle", "off_updrs_3_17_lle", "off_updrs_3_17_lipjaw", 
                              "off_updrs_3_18")
    
    # create data frame for each both control and pd groups and for off/on conditions 
    on_3_updrs_control <- data.frame(dat[dat$group == "control", on_updrs_3_colnames])
    on_3_updrs_pd <- data.frame(dat[dat$group == "pd", on_updrs_3_colnames])
    off_3_updrs_control <- data.frame(dat[dat$group == "control", off_updrs_3_colnames])
    off_3_updrs_pd <- data.frame(dat[dat$group == "pd", off_updrs_3_colnames])
    
    # check ranges between 0 and 4
    check_range(on_updrs_control, 0, 4)
    check_range(on_updrs_pd, 0, 4) 
    check_range(off_updrs_control, 0, 4)
    check_range(off_updrs_pd, 0, 4)
    
    # extract column names of hoehn and yahr scoring items and idnumbers for both on and off conditions
    on_updrs_3_hoehn_yahr_column_names <- c("idnum", "on_updrs_3_hoehn_yahr")
    off_updrs_3_hoehn_yahr_column_names <- c("idnum", "off_updrs_3_hoehn_yahr")
    
    # create data frames for both pd and control groups and off/on conditions
    on_updrs_3_hoehn_yahr_control <- data.frame(dat[dat$group == "control", on_updrs_3_hoehn_yahr_column_names])
    on_updrs_3_hoehn_yahr_pd <- data.frame(dat[dat$group == "pd", on_updrs_3_hoehn_yahr_column_names])
    off_updrs_3_hoehn_yahr_control <- data.frame(dat[dat$group == "control", off_updrs_3_hoehn_yahr_column_names])
    off_updrs_3_hoehn_yahr_pd <- data.frame(dat[dat$group == "pd", off_updrs_3_hoehn_yahr_column_names])
    
    # check ranges between 0 and 5
    check_range(on_updrs_3_hoehn_yahr_control, 0, 5)
    check_range(on_updrs_3_hoehn_yahr_pd, 0, 5) 
    check_range(off_updrs_3_hoehn_yahr_control, 0, 5)
    check_range(off_updrs_3_hoehn_yahr_pd, 0, 5)
    
    # extract column names of both off and on updrs 4 scoring items 
    on_updrs_4_colnames <- c("on_updrs_4_1", "on_updrs_4_2", "on_updrs_4_3", "on_updrs_4_4", "on_updrs_4_5", 
                             "on_updrs_4_6")
    
    off_updrs_4_colnames <- c("off_updrs_4_1", "off_updrs_4_2", "off_updrs_4_3", "off_updrs_4_4", "off_updrs_4_5", 
                              "off_updrs_4_6")
    
    # create data frames for both control and pd and off/on conditions
    on_updrs_4_control <- data.frame(dat[dat$group == "control", on_updrs_4_colnames ])
    on_updrs_4_pd <- data.frame(dat[dat$group == "pd", on_updrs_4_colnames ])
    off_updrs_4_control <- data.frame(dat[dat$group == "control", off_updrs_4_colnames])
    off_updrs_4_pd <- data.frame(dat[dat$group == "pd", off_updrs_4_colnames])
    
    # check ranges between 0 and 4
    check_range(on_updrs_4_control, 0, 4)
    check_range(on_updrs_4_pd, 0, 4) 
    check_range(off_updrs_4_control, 0, 4)
    check_range(off_updrs_4_pd, 0, 4)
}

