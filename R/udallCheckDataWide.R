#' Check the data that have been processed by udallCleanREDCapDataWide
#' This function contains arbitrary sanity checks to make sure that there
#' are no obvious errors in the data. It will issue warnings, but currently
#' will not stop if there is a problem.
#'
#' @param dat Data frame that has been processed and cleaned missing codes
#' replaced with NA
#' @param error.file A string that has the location of a file to save error
#' descriptions too.
#'
#' @export


udallCheckDataWide <- function(dat, error.file) {

    if (file.exists(error.file))
    {
      warning(paste("Deleting old", error.file))
      file.remove(error.file)
    }

    write(paste("subject", "status", "description", "variable.name",
                sep = "\t"),
          file = error.file)

    # make sure subjects have unique ids
    # would be useful to print out any duplicates if there is an issue
    dupl_id <- sort(unique(dat$idnum[duplicated(dat$idnum)]))
    # warningIfNot(sum(duplicated(dat$idnum)) == 0,
    #              paste("Duplicate IDs for ", length(dupl_id), "subjects:",
    #                    paste(dupl_id, collapse = " ")))

    logError(dat, duplicated(dat$idnum), problem = "duplicate ID",
             variable = "idnum", save.file = error.file)

    # make sure we have basic demographic data
    miss_sex <- sort(dat$idnum[is.na(dat$sex)])
    # warningIfNot(sum(is.na(dat$sex)) == 0,
    #              paste("Missing sex for ", length(miss_sex), "subjects: ",
    #                    paste(miss_sex,collapse = " ")))

    logError(dat, is.na(dat$sex), problem = "missing sex", variable = "sex",
             save.file = error.file)

    ##### Check UPDRS scoring items column range values
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

    if(sum(is.na(dat$group)))
      warning(paste("Group is missing for", sum(is.na(dat$group)),
                    "subjects. This causes a lot of errors; resolve missing",
                    "groups first."))

    # Log subjects who have no group assignment.

    logError(dat, is.na(dat$group), "missing group", variable = "group",
             error.file)

    # create data frame for each both control and pd groups and for off/on conditions
    on_updrs_3_control <- data.frame(dat[dat$group == "control",
                                         on_updrs_3_colnames])
    on_updrs_3_pd <- data.frame(dat[dat$group == "pd", on_updrs_3_colnames])

    # Controls don't do a UPDRS OFF
    # off_updrs_3_control <- data.frame(dat[dat$group == "control",
    #                                       off_updrs_3_colnames])
    off_updrs_3_pd <- data.frame(dat[dat$group == "pd", off_updrs_3_colnames])

    # check ranges between 0 and 4
    checkNumericRange(on_updrs_3_control, 0, 4)
    checkNumericRange(on_updrs_3_pd, 0, 4)
    # checkNumericRange(off_updrs_3_control, 0, 4)
    checkNumericRange(off_updrs_3_pd, 0, 4)

    # extract column names of hoehn and yahr scoring items and idnumbers for both on and off conditions
    on_updrs_3_hoehn_yahr_column_names <- c("idnum", "on_updrs_3_hoehn_yahr")
    off_updrs_3_hoehn_yahr_column_names <- c("idnum", "off_updrs_3_hoehn_yahr")

    # create data frames for both pd and control groups and off/on conditions
    on_updrs_3_hoehn_yahr_control <- data.frame(dat[dat$group == "control",
                                                    on_updrs_3_hoehn_yahr_column_names])
    on_updrs_3_hoehn_yahr_pd <- data.frame(dat[dat$group == "pd",
                                               on_updrs_3_hoehn_yahr_column_names])
    # off_updrs_3_hoehn_yahr_control <- data.frame(dat[dat$group == "control",
    #                                                   off_updrs_3_hoehn_yahr_column_names])
    off_updrs_3_hoehn_yahr_pd <- data.frame(dat[dat$group == "pd",
                                                off_updrs_3_hoehn_yahr_column_names])

    # check ranges between 0 and 5
    checkNumericRange(on_updrs_3_hoehn_yahr_control, 0, 5)
    checkNumericRange(on_updrs_3_hoehn_yahr_pd, 0, 5)
    # checkNumericRange(off_updrs_3_hoehn_yahr_control, 0, 5)
    checkNumericRange(off_updrs_3_hoehn_yahr_pd, 0, 5)

    # extract column names of both off and on updrs 4 scoring items
    on_updrs_4_colnames <- c("on_updrs_4_1", "on_updrs_4_2", "on_updrs_4_3",
                             "on_updrs_4_4", "on_updrs_4_5", "on_updrs_4_6")

    off_updrs_4_colnames <- c("off_updrs_4_1", "off_updrs_4_2", "off_updrs_4_3",
                              "off_updrs_4_4", "off_updrs_4_5", "off_updrs_4_6")

    # UPDRS 4 was added around the end of 2016; not all subjects will have it.

    # create data frames for both control and pd and off/on conditions
    on_updrs_4_control <- data.frame(dat[dat$group == "control",
                                         on_updrs_4_colnames])
    on_updrs_4_pd <- data.frame(dat[dat$group == "pd", on_updrs_4_colnames ])

    # UPDRS 4 is never done in the off state.

    # off_updrs_4_control <- data.frame(dat[dat$group == "control",
    #                                       off_updrs_4_colnames])
    # off_updrs_4_pd <- data.frame(dat[dat$group == "pd", off_updrs_4_colnames])

    # check ranges between 0 and 4
    checkNumericRange(on_updrs_4_control, 0, 4)
    checkNumericRange(on_updrs_4_pd, 0, 4)
    # checkNumericRange(off_updrs_4_control, 0, 4)
    # checkNumericRange(off_updrs_4_pd, 0, 4)


    # TODO:
    # ADD TO udallCheckDataWideTest.R

    # check for missing age values
    missing_age <- is.na(dat$scage)
    # warningIfNot(sum(missing_age) == 0, paste(sum(missing_age),
    #                                           "subjects missing age:",
    #                                           paste(dat[which(missing_age),
    #                                                     "idnum"],
    #                                                 collapse = ", ")))

    logError(dat, is.na(dat$scage), "missing age", variable = "scage", error.file)

    ## Check behavioral data

    axcpt.correct <- grep("axcpt_correct", colnames(dat), value = TRUE)

    for (col in axcpt.correct)
    {
      # If they scored exactly 0 on any of the AXCPT tasks in the on sesion,
      ## report them.
      if (grepl("on", col))
        logError(dat, dat[, col] == 0, "0% accuracy", variable = col,
                 warning = TRUE, error.file)
      else if (grepl("off", col))
      {
        # If they scored exactly 0 on the AXCPT off tasks; and are PD (the
        ## only group to do them), report them.
        is.pd <- dat$group == "pd" & !is.na(dat$group)
        logError(dat, dat[, col] == 0 & is.pd,
                 "0% AXCPT accuracy", variable = col, warning = TRUE,
                 error.file)
      }
    }

    # Log all subjects who got a 0 on their ant.
    logError(dat, dat$ant_acc == 0 & !is.na(dat$ant_acc), "0% ANT accuracy",
             variable = "ant_acc", warning = TRUE, error.file)

    # Log all subjects who have an NA in the rt column if accuracy is above 0
    logError(dat,
             dat$off_axcpt_correctdetection > 0 & is.na(dat$off_axcpt_rtcd),
             "RT <NA>:", variable = "off_axcpt_rtcd", error.file)
    logError(dat,
             dat$off_axcpt_correctnontarget > 0 & is.na(dat$off_axcpt_rtcntd),
             "RT <NA>:", variable = "off_axcpt_rtcntd", error.file)
    logError(dat,
             dat$on_axcpt_correctdetection > 0 & is.na(dat$on_axcpt_rtcd),
             "RT <NA>:", variable = "on_cpt_rtcd", error.file)
    logError(dat,
             dat$on_axcpt_correctnontarget > 0 & is.na(dat$on_axcpt_rtcntd),
             "RT <NA>:", variable = "on_axcpt_rtcntd", error.file)

    # Report missing off in pd
    is.pd <- dat$group == "pd" & !is.na(dat$group)

    # If they are PD, and are missing axcpt off data, report them
    logError(dat, is.pd & is.na(dat$off_axcpt_correctdetection),
             "AXCPT OFF missing for PD",
             variable = "off_axcpt_correctdetection", error.file)

    # If they are controls, and have OFF data, report them.
    logError(dat, !is.pd & !is.na(dat$off_axcpt_correctdetection),
             "AXCPT OFF for control",
             variable = "off_axcpt_correctdetection", error.file)


    logError(dat, is.na(dat$on_sai_sai), "unable to calculate SAI",
             variable = "on_sai_sai", warning = TRUE, error.file)

    logError(dat, is.na(dat$on_fog_q1), "missing FOG Q1 entry",
             variable = "on_fog_q1", error.file)

    logError(dat,
             dat$on_fog_q1 == 1 & !is.na(dat$on_fog_q1) & is.na(dat$on_fog_total),
             "Indicated FOG; but total score is NA", variable = "on_fog_q1",
             error.file)

    logError(dat,
             dat$led < 100 & is.pd,
             "Levadopa-equivalent dose is < 100", variable = "led",
             warning = TRUE, error.file)

    ## TODO:
    # ADD TO udallCheckDataWideTest.R

    return(dat)

}

