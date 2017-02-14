#' Check the data that have been processed by udallCleanREDCapDataWide
#' This function contains arbitrary sanity checks to make sure that there
#' are no obvious errors in the data. It will issue warnings, but currently
#' will not stop if there is a problem.
#' @param dat Data frame that has been processed and cleaned (missing codes replaced with NA
#' @export
udallCheckDataWide <- function(dat) {
    # make sure subjects have unique ids
    # would be useful to print out any duplicates if there is an issue
    warningIfNot(length(dat$idnum) == length(unique(dat$idnum)), "Duplicate ids found in data set")
                                        # make sure we have basic demographic data
    warningIfNot(sum(is.na(dat$sex)) ==0, paste("Missing sex for ", sum(is.na(dat$sex)), "subjects"))
}

