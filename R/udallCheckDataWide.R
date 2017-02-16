#' Check the data that have been processed by udallCleanREDCapDataWide
#' This function contains arbitrary sanity checks to make sure that there
#' are no obvious errors in the data. It will issue warnings, but currently
#' will not stop if there is a problem.
#' @param dat Data frame that has been processed and cleaned (missing codes replaced with NA
#' @export
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
}

