#' Compares each value in passed data.frame to specified minimum and maximum values.
#' Subject IDs and associated column names are printed out for values that are not
#' within specified range.
#' @param dat_cols data.frame with columns of items to be checked for specified range
#' @param min minimum value in range
#' @param max maximum value in range
#' @export
#'

# passed a data.frame, min and max; will print out warning and id numbers that are not within the specified range
# for each observation in each column
checkNumericRange <- function(dat_cols, min, max) {
  # total number of columns
  col_total <- length(dat_cols)

  # test how many columns are numeric
  col_numeric <- sum(apply(dat_cols, 2, is.numeric))

  # warning if not all columns are numeric
  warningIfNot(col_total == col_numeric, "All columns must be numeric.")

  # total number of rows (observations)
  row_total <- nrow(dat_cols)

  # row numbers will be consecutive from 1 to number of total rows of data.frame
  rownames(dat_cols) <- 1:row_total

  # ID numbers in "idnum" are extracted into a vector and column is removed from data.frame
  #   otherwise, the ID value will be processed along with the measured dat_colsa
  idnum <- dat_cols$idnum
  dat_cols$idnum <- NULL

  # produces an index number for unique observation in each column, equivalent to product of row and column,
  # that are not in the specifed range, extracted as vector
  row_x_col <- which(dat_cols < min | max < dat_cols)

  # calculates column numbers of observations not in specifed range, extracted as vector
  col <- ceiling(row_x_col / row_total) # used to index column names in "colnames(dat_cols[col])"

  # calculates row numbers of observations not in specified range, extracted as vector
  row <- row_total - (col * row_total - row_x_col) # used to index subject ID numbers in "idnum[row]"

  # warning is produced if data.are out of specified range
  #    column and subject ID of incorrect data.are printed out
  warningIfNot(length(row) == 0, paste("Incorrect data.range(s) for",
                                       length(unique(row)), "subject(s):",
                                       paste("\n", "column: ",
                                             gsub("[.][0-9]", "",
                                                  colnames(dat_cols[col])), # remove ".#" if repeated column
                                             "; subject: ", idnum[row],
                                             collapse = "", sep = "")))
}
