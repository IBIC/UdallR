#' Score Unified Parkinson's Disease Rating Scale.
#' @param dat Input data frame with items to be scored.
#' @return dat Output data frame with items scored.

score <- function(dat, column) {
  # capture column names in vector
  names <- colnames(dat) 
  # concatenate suffix to "total" column
  new_column <- paste(column, "_total", sep = "") 
  # create vector of columns names 
  updrs <- dat[,grep(column,names)]
  # calculate measure total and append "total" column to end of data frame 
  dat[, new_column] <- apply(updrs,1,sum,na.rm=TRUE)
  return(dat)
}

scoreUPDRS <- function(dat) {
  dat <- score(dat, "on_updrs_3") # calculate updrs_3 total
  dat <- score(dat, "on_updrs_4") # calculate updrs_4 total
  return(dat)
}