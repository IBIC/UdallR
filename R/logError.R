#'
#' Log errors in data frame.
#'
#' Takes a data frame and logs the idnums of subjects who fit a given criteria
#' and logs them to a given file.
#'
#' @param dat Data file containing an ID column.
#' @param column A logical vector the same length as dat.
#' @param problem A short string describing the problem at the TRUE values
#' of the string.
#' @param save.file A string describing the location of the file to save to.
#' Always appends.
#' @param id.number The name of the column that contains the ID values.
#' Defaults to "idnum".
#'
#' @examples
#'
#' Take dat$age: [70, 88, 71, NA]
#'
#' > logError(dat, is.na(dat$age), "Age value is NA", "errors.txt")
#'
#' errors.txt would read something like:
#'
#' 0004 Age value is NA
#'
#' @export

logError <- function(dat, column, problem, variable,
                     save.file = NA, warning = FALSE, id.column = "idnum")
{
  # Make sure the colunn fits in dat
  if (length(column) != nrow(dat))
    stop("Column length doesn't match dat dimensions.")

  # Conjunct w/ !is.na to account for NAs in logical column
  bad.ids <- as.character(dat[column & !is.na(column), id.column])

  # If problem isn't an atomic vector, select the matching subset.
  if (length(problem) > 1)
    problem <- problem[column & !is.na(column)]

  # Change warning text to add WARNING or ERROR
  warning.text <- ifelse(warning, "WARNING", "ERROR")
  problem <- paste(warning.text, problem, variable, sep = "\t")

  if (length(bad.ids) > 0 )
  {
    if (!is.na(save.file))
    {
      out <- as.data.frame(cbind(bad.ids, problem))

      write.table(out, file = save.file, append = TRUE, sep = "\t",
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
    }
    else
      cat(paste(bad.ids, problem), fill = TRUE)
  }
}
