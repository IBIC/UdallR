#' Calculate task cost for gait and balance
#'
#' @param single.task Data from single task run, as a data frame.
#' @param dual.task Data from dual task run, as a data.frame
#'
#' @return Cost for all measures over all subjects
#'
#' @export

udallCalculateCost <- function(single.task, dual.task)
{
  # Take the data columns from the single and dual task data frames.
  # If more "header" information is added or removed, n will need to be
  ## changed.
  n <- 8

  single.task.data <- single.task[, n:ncol(single.task)]
  dual.task.data <- dual.task[, n:ncol(dual.task)]

  output <- (dual.task.data - single.task.data) / single.task.data

  colnames(output) <- gsub("^.t", "cost", colnames(output))

  return(output)
}
