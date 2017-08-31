#' Create a demographic table for Udall P2
#'
#' Creates a demographic table from the given data frame and columns. Make sure
#' to type columns appropriately before inputting them into
#' udallDemographicTable (e.g. factor conversions).
#'
#' @param data Data from which to select the columns.
#' @param columns List of columns to summarize, all must exist in data.
#' @param group.by Variable to group subjects by.
#' @param group.ok List of groups to include in the sorting; defaults to all
#' options in group.by if not given.
#'
#' @return A data frame with the inputted columns on the left and seven columns:
#' PD group mean/count; PD group SD/percent; control group mean/count; control
#' group SD/percent; p-value (when appropriate); total mean/count; total
#' SD/percent
#'
#' @export
#'

udallDemographicTable <- function(data, columns, group.by, group.ok = NULL)
{
  if (is.null(group.ok))
  {
    group.ok <- unique(data[, group.by])
  }
  else
  {
    group.ok <- group.ok[group.ok %in% data[, group.by]]
  }

  # Use sapply instead of a loop to go over the columns, t() because the
  ## resulting data table is rotated 90 degrees over what we'd prefer.
  results <- as.data.frame(t(sapply(columns, testVariable, all.subjects = data,
                                    grouping = group.by, groups = group.ok)))

  return(results)
}

