#' Get the closest analytic site visits.
#'
#' Takes a cleaned data frame downloaded from REDCap and a file to save the
#' output to, if wanted, and outputs a data frame containing the analytical
#' core visit that was nearest to the scan date (preferably within 6 months).
#'
#' @param dat Cleaned REDCap data
#' @param file.name CSV file to save output to. Defaults to NULL. If none is
#' given, returns data frame.
#' @param multivis.file CSV file to read multivis information from, if you need
#' data other than the default (currently 20174/04/25).
#' @return closest.visit A data table with the information from the analytic
#' center visit nearest to scan date.
#'
#' @examples getClosestACVisit(visit1, "visit1.txt")
#'
#' @export
#'

getClosestACVisit <- function(dat, file.name = NULL, multivis.file = NULL)
{

  if (is.null(multivis.file))
  {
    # This must be updated manually if a newer file is added.
    # data() command attaches frame under the name given
    data("panuc_multivis_2017_04_25")

    # Move default name to other name.
    multivis.dat <- panuc_multivis_2017_04_25
  }
  else
  {
    multivis.dat <- read.csv(multivis.file, header = TRUE,
                              stringsAsFactors = FALSE)
    colnames(multivis.dat) <- tolower(colnames(multivis.dat))
  }

  # Get the nearest row from the multivis based on the REDCap data (dat)
  closest.visit <- as.data.frame(t(apply(dat, 1, getNearestRow,
                                         multivis = multivis.dat)))
  colnames(closest.visit) <- colnames(multivis.dat)

  # If a value was passed to file.name, write out the csv, otherwise return it.
  if (!is.null(file.name))
      write.csv(closest.visit, file = file.name, row.names = FALSE,
                quote = FALSE)
  else
    return(closest.visit)
}
