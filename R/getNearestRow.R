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
  ID.with.prefix <- unlist(unname(x["on_mri_subject_id"]))
  ID <- gsub("[^0-9]", "", ID.with.prefix)
  age <- as.numeric(unlist(unname(x["scage"])))

  # Row for when there's errors
  na.row <- rep(NA, ncol(multivis))

  # idnum is always the stripped, digits-only version
  na.row[1] <- ID

  if (is.na(age))
  {
    warning(paste(ID.with.prefix, "has no visit age."))

    return(na.row)
  }

  # Dates for subject
  # subj.subset <- multivis[multivis$subject_id == new.id, ]
  # subj.subset <- multivis[grep(dash.regex, multivis$subject_id), ]
  subj.subset <- multivis[multivis$summary_id == ID.with.prefix, ]

  # Ages at different visits
  ages <- subj.subset$agevisit
  ages[ages <= 0] <- NA

  # Convert string to date objects, and compare to fMRI scan date
  if (sum(is.na(ages)) < length(ages))
  {
    dates.diff <- abs(ages - age)
    closest <- which.min(dates.diff)

    if (sum(is.na(subj.subset$agevisit)) > 0)
    {
      warning(paste(sum(is.na(dates.diff))), " NAs in ", ID,
              " analytic site ages.")
    }

    # Check whether any vists are within 6 months
    if (all(dates.diff > 0.5, na.rm = TRUE))
    {
      warning(paste0(ID.with.prefix,
                    " doesn't have any visits within ±6 months. ",
                    "Choosing nearest visit (",
                    round((ages - age)[closest] * 12, 3),
                    " months). "))
    }

    # Get the row closest in age to the patient's MRI visit.
    nearest.row <- subj.subset[which.min(dates.diff), ]

  } else {
    warning(paste(ID.with.prefix,
                  " has no valid ages to compare to. Returning NA row"))

    nearest.row <- na.row
  }

  # Turn factors into strings, otherwise they get discombobulated and written
  ## to csv as numerics instead, which isn't at all valuable.
  nr.factor <- sapply(nearest.row, is.factor)
  nearest.row[nr.factor] <- lapply(nearest.row[nr.factor], as.character)

  return(unlist(nearest.row))
}
