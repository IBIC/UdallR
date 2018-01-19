#' Get version number and date of last update.
#'
#' Returns the version number and the date of the last update.
#'
#' @return result Data framme containing the version # and last updated.
#'
#' @export
#'

udallTimestamp <- function()
{
  version = 0.3.0.0000
  date = "01/19/18"

  result <- as.data.frame(cbind(version,date))

  return(result)
}

