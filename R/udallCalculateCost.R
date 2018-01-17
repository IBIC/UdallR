#' Calculate task cost for gait and balance
#'
#' @param data.frame Data frame that contains both the single task and dual
#' task cost columsn.
#' @param st.cols    Which columns have information on single-task performance.
#' @param dt.cols    Which columns have information on dual-task performance.
#' Length must match st.cols.
#'
#' @return Data frame with #cols = length of st.cols and
#' nrows = nrows(data.frame). Cost measurements for
#'
#' @export


udallCalculateCost <- function(data.frame, st.cols, dt.cols)
{
  # Make sure columns match
  if (length(st.cols) != length(dt.cols))
  {
    stop("Single/dual task columns not equal length")
  }

  if (!all.equal(gsub("_st_", "_dt_", st.cols), dt.cols))
  {
    stop("Sing/dual task name mismatch.")
  }

  # Create data frame with DT - ST values
  output <- as.data.frame(matrix(NA, nrow = nrow(data.frame),
                                 ncol = length(st.cols)))
  colnames(output) <- gsub("_st_", "_dtcost_", st.cols)
  for (n in 1:ncol(output))
  {
    if (is.numeric(data.frame[, st.cols[n]]))
      output[, n] <- data.frame[, dt.cols[n]] - data.frame[, st.cols[n]]
    else
      output[, n] <- NA
  }

  # Remove all-NA columns (e.g. columns that were originally not numeric.)
  output.clean <- output[, colSums(is.na(output)) != nrow(output)]

  # Remove irrelevant columns that have session, group info (stored elsewhere)
  output.drop <- output.clean[, grep("dtcost_(session|group)",
                                     colnames(output.clean),
                                     invert = TRUE)]

  return(output.drop)
}

# udallCalculateCost <- function(single.task, dual.task)
# {
#   # Take the data columns from the single and dual task data frames.
#   # If more "header" information is added or removed, n will need to be
#   ## changed.
#   n <- 8
#
#   single.task.data <- single.task[, n:ncol(single.task)]
#   dual.task.data <- dual.task[, n:ncol(dual.task)]
#
#   output <- (dual.task.data - single.task.data) / single.task.data
#
#   colnames(output) <- gsub("^.t", "cost", colnames(output))
#
#   return(output)
# }
