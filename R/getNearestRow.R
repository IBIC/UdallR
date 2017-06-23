#' Takes an ID number and scan date, and returns
#' @param x Row from cleaned to examine.
#' @param multivis Database from analytic sites (MS ACCESS).
#' @param verbose Display informational messages
#' @return nearest.row Row containing appropriate information for the visit
#' nearest the scan date given.
#' @export
#'

get.nearest.row <- function(x, multivis, verbose = FALSE)
{
  # Parse input
  # v <- as.vector(unlist(x))
  ID <- unlist(unname(x["idnum"]))
  age <- as.numeric(unlist(unname(x["scage"])))

  # csv stores IDs in the format PWA00-0000, input is 000000
  new.id <- paste0("PWA", substr(ID, 1, 2), "-", substr(ID, 3, 6))

  # Row for when there's errors
  na.row <- rep(NA, ncol(subj.subset))
  na.row[1] <- new.id

  if (is.na(age))
  {
    warning(paste(ID, "has no visit age."))

    return(na.row)
  }

  # Dates for subject
  subj.subset <- multivis[multivis$subject_id==new.id, ]

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
      warning(paste(ID, "doesn't have any visits within 6 months.",
                    "Choosing nearest visit."))
    }

    # Get the row closest in age to the patient's MRI visit.
    nearest.row <- subj.subset[which.min(dates.diff), ]

    if (verbose)
    {
      print(paste0(ID, "'s visit was ",
                   round(min(dates.diff, na.rm = TRUE) * 12,digits = 2),
                 " months away."))
    }
  } else {
    warning(paste(ID, " has no valid ages to compare to. Returning NA row"))

    nearest.row <- na.row
  }

  return(unlist(unname(nearest.row)))
}
