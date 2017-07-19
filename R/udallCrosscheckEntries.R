#'
#' Crosscheck entries
#'
#' Crosschecks the double-uploaded data and between the REDCap data and the
#' MS Access/Analytic Core/mulitvis data.
#'
#' @param cdat Cleaned data frame (producet of udallCleanDataWide)
#' @param error.file File to save all errors to.
#'
#' @examples udallCrosscheckEntries(cdat, "mismatch-errors.txt")
#'
#' @export

udallCrosscheckEntries <- function(cdat, error.file)
{
  g <- "group"
  ps <- "parkinsonism_status"

  if (file.exists(error.file))
  {
    warning(paste("Deleting old", error.file))
    file.remove(error.file)
  }

  write(paste("subject", "status", "description", "variable.name",
              sep = "\t"),
        file = error.file)

  comparison <- cdat[, c("idnum", g, ps)]

  for (r in 1:nrow(comparison))
  {
    row <- comparison[r, ]

    if (comparison[r, g] == "pd" & !is.na(comparison[r, g]))
      if (comparison[r, ps] != "PD" | is.na(comparison[r, ps]))
        logError(row, TRUE, problem = paste0("group mismatch: ",
                                             comparison[r, g], ",",
                                             comparison[r, ps]),
                 variable = paste0(g, ",", ps),
                 error.file)

    if (comparison[r, g] == "control" & !is.na(comparison[r, g]))
      if (comparison[r, ps] != "Unaffected" | is.na(comparison[r, ps]))
        logError(row, TRUE,  problem = paste0("group mismatch: ",
                                              comparison[r, g], ",",
                                              comparison[r, ps]),
                 variable = paste0(g, ",", ps),
                 error.file)

    if (comparison[r, ps] == "Other" & !is.na(comparison[r, ps]))
      logError(row, TRUE, problem = "group is OTHER", variable = ps,
               warning = TRUE, error.file)
  }

  # Age difference
  diffs <- cdat$scage - cdat$agevisit
  diffs.days <- diffs * 365

  logError(cdat, diffs.days > 90,
           problem = paste0("wide visit gap: ", round(diffs.days), " days"),
           variable = "scage,agevisit", warning = TRUE, error.file)

  # Compare double-entered UPDRS data

  if (sum(is.na(c(cdat$on_updrs_3_total,
                  cdat$off_updrs_3_total,
                  cdat$updrs_new_3_total_m1,
                  cdat$updrs_new_3_total_m2))) > 0)
    warning(paste("NAs in one or more UPDRS total column.",
                  "This will cause other errors. Resolve these first."))


  logError(cdat, is.na(cdat$on_updrs_3_total),
           "UPDRS ON in REDCap is NA", variable = "on_updrs_3_total",
           error.file)

  logError(cdat,
           is.na(cdat$off_updrs_3_total) & cdat$group == "pd",
           "patient UPDRS OFF in REDCap is NA", variable = "off_updrs_3_total",
           error.file)

  logError(cdat, is.na(cdat$updrs_new_3_total_m1),
           "closest UPDRS ON is NA", variable = "updrs_new_3_total_m1",
           error.file)

  logError(cdat,
           is.na(cdat$updrs_new_3_total_m2) & cdat$group == "pd",
           "patient closest UPDRS OFF is NA",
           variable = "updrs_new_3_total_m2", error.file)

  on.mismatches <- !sapply(cdat$on_updrs_3_total ==
                             cdat$updrs_new_3_total_m1,
                           isTRUE)

  logError(cdat, on.mismatches,
           paste0("UPDRS ON mismatch:", cdat$on_updrs_3_total, "/",
                  cdat$updrs_new_3_total_m1),
           variable = "on_updrs_3_total,updrs_new_3_total_m1",
           error.file)

  off.mismatches <- !sapply(cdat$off_updrs_3_total ==
                              cdat$updrs_new_3_total_m2,
                            isTRUE) & cdat$group == "pd"

  logError(cdat, off.mismatches,
           paste0("patient UPDRS OFF mismatch:", cdat$off_updrs_3_total, "/",
                  cdat$updrs_new_3_total_m2),
           variable = "off_updrs_3_total,updrs_new_3_total_m2",
           error.file)
}
