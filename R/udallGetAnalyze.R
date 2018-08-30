#' Extracts which data collection measures are to be included in the analysis
#'
#' udallGetAnalyze returns a dataframe of dimensions n x 9, where n is the
#' number of subjects. The first column contains idnums and the last eight
#' logical values. If the `whys` option is selected, the function will return
#' a list of two data frames, where the second has the explanations downloaded
#' frome REDCap.
#'
#' @param cdat A cleaned data frame
#' @param whys Do you need the explanations? Default FALSE
#'
#' @return A data frame with the analyze values, or a list of two data frames
#' with the analyze values and the corresponding whys
#'
#' @export
udallGetAnalyze <- function(dat, whys = FALSE)
{
  # All the analyze columns, excluding the why and complete columns
  analyze.columns <- grep("(complete|why|notes)$", invert = TRUE, value = TRUE,
                          x = grep("^o.*_analyze", colnames(dat), value = TRUE))

  # All the why columns
  why.columns <- grep("^o.*_analyze_.*_why$", colnames(dat), value = TRUE)

  analyze.status <- dat[, c("idnum", analyze.columns)]
  analyze.status[, analyze.columns] <- apply(analyze.status[, analyze.columns],
                                               2, as.logical)

  # If the user wants the whys, return them as a list with the actual values
  if (whys)
  {
    why <- dat[, c("idnum", why.columns)]
    result <- list(analyze.status, why)
  }
  else
    result <- analyze.status

  return(result)
}
