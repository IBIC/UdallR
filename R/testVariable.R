#' Test demographic variable
#'
#' Given a column and data frame, compares values of column for PD and
#' control/unaffected participants.
#'
#' @param col Column name
#' @param all.subjects Data frame containing information for all subjects.
#'
#' @return Vector of length 7 with PD group mean/count; PD group SD/percent;
#' control group mean/count; control group SD/percent; p-value (when
#' appropriate); total mean/count; total SD/percent
#'
#' @export

testVariable <- function(col, all.subjects){

  pd <- all.subjects[all.subjects$parkinsonism_status=="PD", ]
  ctrl <- all.subjects[all.subjects$parkinsonism_status=="Unaffected", ]

  pd.col <- pd[, col]
  ctrl.col <- ctrl[, col]
  total.col <- all.subjects[, col]

  if (is.numeric(pd.col)) # Both vectors should be the same type
  {
    if (all(is.na(ctrl.col)))
    {
      warning(paste("Control col", col, "is all NA"))

      ctrl.mean <- NA
      ctrl.sd <- NA

      p.val <- NA
    }
    else
    {
      ctrl.mean <- mean(ctrl.col, na.rm = TRUE)
      ctrl.sd <- sd(ctrl.col, na.rm = TRUE)

      p.val <- t.test(pd.col, ctrl.col)$p.value
    }

    pd.mean <- mean(pd.col, na.rm = TRUE)
    pd.sd <- sd(pd.col, na.rm = TRUE)

    if (is.numeric(p.val) & p.val < 0.05)
      warning(paste("p val is < .05 for col", col))

    return.vec <- c(pd.mean, pd.sd, ctrl.mean, ctrl.sd, p.val,
                    mean(total.col, na.rm = TRUE), sd(total.col, na.rm = TRUE))
  }
  else if (is.factor(pd.col) | is.character(pd.col))
  {
    if (length(levels(pd.col)) != 2)
    {
      warnings("Error: More than two factor levels in column.")
    }

    pd.col <- as.factor(as.character(pd.col))
    ctrl.col <- as.factor(as.character(ctrl.col))

    pd.c <- sum(pd.col == levels(pd.col)[1], na.rm = TRUE)
    ctrl.c <- sum(ctrl.col == levels(ctrl.col)[1], na.rm = TRUE)

    pd.p <- pd.c / length(pd.col)
    ctrl.p <- ctrl.c / length(ctrl.col)

    total.c <- sum(total.col == levels(pd.col)[1], na.rm = TRUE)
    total.p <- total.c / length(total.col)

    message(paste(col, "is a factor vector. Returning count for",
                  levels(pd.col)[1]))

    return.vec <- c(pd.c, pd.p, ctrl.c, ctrl.p, NA, total.c, total.p)
  }

  if (all(is.na(pd.col)))
  {
    warning(paste("PD col", col, "is NAs"))
    return.vec[1:2] <- NA
  }

  if (all(is.na(ctrl.col)))
  {
    warning(paste("Control col", col, "is NAs"))
    return.vec[3:4] <- NA
  }


  return(return.vec)
}
