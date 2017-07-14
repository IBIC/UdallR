#' Get the closest analytic site visits.
#'
#' Takes a cleaned data frame downloaded from REDCap and a file to save the
#' output to, if wanted, and outputs a data frame containing the analytical
#' core visit that was nearest to the scan date (preferably within 6 months).
#'
#' @param dat Cleaned REDCap data
#' @param file.name CSV file to save output to. Defaults to NULL. If none is
#' given, returns data frame.
#' @param mulitivis.df Data frame with multivis information (e.g. the
#' lazy-loaded panuc_multivis_*_* datasets). Mutually exclusive with
#' multivis.file
#' @param multivis.file CSV file to read multivis information from, if you need
#' data other than the default (currently 20174/04/25).
#'
#' @return closest.visit A data table with the information from the analytic
#' center visit nearest to scan date.
#'
#' @examples getClosestACVisit(visit1, multivis.df = panuc_multivis_07_07)
#'
#' @export
#'

getClosestACVisit <- function(dat, multivis.df = NULL, multivis.file = NULL,
                              file.name = NULL)
{

  if (is.null(multivis.file) & is.null(multivis.df))
  {
    stop("Either multivis file or multivis data frame must be set.")
  }
  else if (!is.null(multivis.df))
  {
    multivis.dat <- multivis.df
  }
  else if (!is.null(multivis.file))
  {
    multivis.dat <- read.csv(multivis.file, header = TRUE,
                              stringsAsFactors = FALSE)
    colnames(multivis.dat) <- tolower(colnames(multivis.dat))
  }
  else if (!is.null(multivis.df) & !is.null(multivis.file))
  {
    stop("Cannot set both multivis file and multivis data frame.")
  }

  # Get the nearest row from the multivis based on the REDCap data (dat)
  closest.visit <- as.data.frame(t(apply(X = dat, MARGIN = 1,
                                         FUN = UdallR::getNearestRow,
                                         multivis = multivis.dat)))
  colnames(closest.visit) <- colnames(multivis.dat)

  # If a value was passed to file.name, write out the csv, otherwise return it.
  if (!is.null(file.name))
      write.csv(closest.visit, file = file.name, row.names = FALSE,
                quote = FALSE)
  else
    return(closest.visit)
}
