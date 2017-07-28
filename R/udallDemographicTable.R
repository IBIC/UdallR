#' Create a demographic table for Udall P2
#'
#' Creates a demographic table from the given data frame and columns. Make sure
#' to type columns appropriately before inputting them into
#' udallDemographicTable (e.g. factor conversions).
#'
#' @param data Data from which to select the columns.
#' @param columns List of columns that exist in data.
#'
#' @return A data frame with the inputted columns on the left and seven columns:
#' PD group mean/count; PD group SD/percent; control group mean/count; control
#' group SD/percent; p-value (when appropriate); total mean/count; total
#' SD/percent
#'
#' @export
#'

udallDemographicTable <- function(data, columns)
{
  if (sum(data$parkinsonism_status %in% c("PD", "Unaffected")) > 0)
  {
    warning(paste("Subjects have PD status besides \"PD\" or \"Unaffected\".",
                  "Skipping them."))

    data <- data[data$parkinsonism_status %in% c("PD", "Unaffected"), ]
  }

  # Use sapply instead of a loop to go over the columns, t() because the
  ## resulting data table is rotated 90 degrees over what we'd prefer.
  results <- t(sapply(columns, testVariable, all.subjects = data))
  colnames(results) <- c("PD.m", "PD.sd", "C.m", "C.sd", "p", "T.m", "T.sd")

  return(results)
}

# data <- read.csv("closestVisits-20170724.csv")
# data[data < 0] <- NA
#


# test.var("agevisit")
# test.var("education_years")
# test.var("updrs_new_3_total")
# test.var("hoehn_and_yahr_m0")
# test.var("gender")
# test.var("handedness")

# round(results, 3)

