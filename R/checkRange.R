#' This is a function that checks each value in a data frame and will print out the subject ID and column
#' name of element that is not in specified range. 
#' @param dat data.frame with columns of items to be checked for specified range
#' @param min minimum value in range
#' @param max maximum value in range
#' @export
#' 

# passed a data.frame, min and max; will print out warning and id numbers that are not within the specified range
checkRange <- function(dat, min, max) {
  # total number of columns
  col_total <- length(dat)
  
  # test how many columns are numeric
  col_numeric <- sum(apply(dat, 2, is.numeric))
  
  # warning if not all columns are numeric
  warningIfNot(col_total == col_numeric, paste("All columns must be numeric."))
  
  # total number of rows (observations)
  row_total <- nrow(dat)
  
  # row numbers will be consecutive from 1 to number of total rows of data.frame
  rownames(dat) <- 1:row_total
  
  # ID numbers in "idnum" are extracted into a vector and column is removed from data.frame
  #   otherwise, the ID value will be processed along with the measured data
  idnum <- dat$idnum
  dat$idnum <- NULL
  
  # produces an index number for unique observation in each column, equivalent to product of row and column,
  # that are not in the specifed range, extracted as vector
  row_x_col <- which(dat < min | max < dat)
  
  # calculates column numbers of observations not in specifed range, extracted as vector
  col <- ceiling(row_x_col / row_total) # used to index column names in "colnames(dat[col])"
  
  # calculates row numbers of observations not in specified range, extracted as vector
  row <- row_total - (col * row_total - row_x_col) # used to index subject ID numbers in "idnum[row]"
  
  # warning is produced if data are out of specified range 
  #    column and subject ID of incorrect data are printed out 
  warningIfNot(length(row) == 0, paste("Incorrect data range(s) for", length(unique(row)), "subject(s):", paste("\n",
                                    "column: ", gsub("[.][0-9]", "", colnames(dat[col])), # remove ".#" if repeated column
                                    "; subject: ", idnum[row],
                                     collapse = "", sep = "")))
}