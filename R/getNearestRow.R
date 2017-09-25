#' Get analytic site visit nearest MRI visit
#'
#' Takes a row of the REDCap data and compares it to the multivis data that
#' contains infromation for each subject's analytic site visit.
#'
#' @param x Row of REDCap data to use to identify analytic site visit.
#' @param multivis The data frame containing the analytic site information.
#'
#' @export

getNearestRow <- function(x, multivis)
{
  # Parse input
  # v <- as.vector(unlist(x))
  ID <- unlist(unname(x["idnum"]))
  age <- as.numeric(unlist(unname(x["scage"])))

  # csv stores IDs in the format PWA00-0000, input is 000000
  new.id <- paste0("PWA", substr(ID, 1, 2), "-", substr(ID, 3, 6))

  # Row for when there's errors
  na.row <- rep(NA, ncol(multivis))
  na.row[1] <- new.id

  if (is.na(age))
  {
    warning(paste(ID, "has no visit age."))

    return(na.row)
  }

  # Dates for subject
  subj.subset <- multivis[multivis$subject_id == new.id, ]

  # Ages at different visits
  ages <- subj.subset$agevisit
  ages[ages <= 0] <- NA

  # Convert string to date objects, and compare to fMRI scan date
  if (sum(is.na(ages)) < length(ages))
  {
    dates.diff <- abs(ages - age)

    if (sum(is.na(subj.subset$agevisit)) > 0)
    {
      warning(paste(sum(is.na(dates.diff))), " NAs in ", ID,
              " analytic site ages.")
    }

    # Check whether any vists are within 6 months (= half a year)
    if (all(dates.diff > 0.5, na.rm = TRUE))
    {
      warning(paste(ID, "doesn't have any visits within ±6 months.",
                    "Choosing nearest visit (", round(dates.diff * 12, 3),
                    " months)"))
    }

    # Get the row closest in age to the patient's MRI visit.
    nearest.row <- subj.subset[which.min(dates.diff), ]

  } else {
    warning(paste(ID, " has no valid ages to compare to. Returning NA row"))

    nearest.row <- na.row
  }

  # Turn factors into strings, otherwise they get discombobulated and written
  ## to csv as numerics instead, which isn't at all valuable.
  nr.factor <- sapply(nearest.row, is.factor)
  nearest.row[nr.factor] <- lapply(nearest.row[nr.factor], as.character)

  return(unlist(nearest.row))
}
