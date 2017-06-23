#' Takes a cleaned data frame downloaded from REDCap and a file to save the
#' output to, if wanted, and outputs a data frame containing the analytical
#' core visit that was nearest to the scan date (preferably within 6 months).
#' @param dat Cleaned REDCap data
#' @param file CSV file to save output to. Defaults to NA - no write out.
#' @return closest.visit A data table with the information from the analytic
#' center visit nearest to scan date.
#' @export
#'

getClosestACVisit <- function(dat, file.name = NA)
{
  # Find the most recent multivis file in UdallR/data.
  # Relies on the multivis file having the date in the format yyyy_mm_dd,
  # And read that it.
  multivis.file <- sort(list.files(path = "../data/",
                                   pattern = "panuc_multivis_.*.csv",
                                   full.names = TRUE),
                        decreasing = TRUE)[1]
  multivis.dat <- read.csv(multivis.file, header = TRUE,
                           stringsAsFactors = FALSE)
  colnames(multivis.dat) <- tolower(colnames(multivis.dat))

  # Get the nearest row from the multivis based on the REDCap data (dat)
  closest.visit <- as.data.frame(t(apply(dat, 1, get.nearest.row,
                                         multivis = multivis.dat)))
  colnames(closest.visit) <- colnames(multivis)

  # If a value was passed to file.name, write out the csv, otherwise return it.
  if (!is.na(file.name))
      write.csv(closest.visit, file = file.name, row.names = FALSE)
  else
    return(closest.visit)
}
