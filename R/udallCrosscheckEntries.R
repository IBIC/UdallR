
#' @export

udallCrosscheckEntries <- function(redcap.data, error.file)
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

  comparison <- redcap.data[, c("idnum", g, ps)]

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
  diffs <- redcap.data$scage - redcap.data$agevisit
  diffs.days <- diffs * 365

  logError(redcap.data, diffs.days > 90,
           problem = paste0("wide visit gap: ", round(diffs.days), " days"),
           variable = "scage,agevisit", warning = TRUE, error.file)

  # Compare double-entered UPDRS data

  if (sum(is.na(c(redcap.data$on_updrs_3_total,
                  redcap.data$off_updrs_3_total,
                  redcap.data$updrs_new_3_total_m1,
                  redcap.data$updrs_new_3_total_m2))) > 0)
    warning(paste("NAs in one or more UPDRS total column.",
                  "This will cause other errors. Resolve these first."))


  logError(redcap.data, is.na(redcap.data$on_updrs_3_total),
           "UPDRS ON in REDCap is NA", variable = "on_updrs_3_total",
           error.file)

  logError(redcap.data,
           is.na(redcap.data$off_updrs_3_total) & redcap.data$group == "pd",
           "patient UPDRS OFF in REDCap is NA", variable = "off_updrs_3_total",
           error.file)

  logError(redcap.data, is.na(redcap.data$updrs_new_3_total_m1),
           "closest UPDRS ON is NA", variable = "updrs_new_3_total_m1",
           error.file)

  logError(redcap.data,
           is.na(redcap.data$updrs_new_3_total_m2) & redcap.data$group == "pd",
           "patient closest UPDRS OFF is NA",
           variable = "updrs_new_3_total_m2", error.file)

  on.mismatches <- !sapply(redcap.data$on_updrs_3_total ==
                             redcap.data$updrs_new_3_total_m1,
                           isTRUE)

  logError(redcap.data, on.mismatches,
           paste0("UPDRS ON mismatch:", redcap.data$on_updrs_3_total, "/",
                  redcap.data$updrs_new_3_total_m1),
           variable = "on_updrs_3_total,updrs_new_3_total_m1",
           error.file)

  off.mismatches <- !sapply(redcap.data$off_updrs_3_total ==
                              redcap.data$updrs_new_3_total_m2,
                            isTRUE)

  logError(redcap.data, off.mismatches,
           paste0("UPDRS OFF mismatch:", redcap.data$off_updrs_3_total, "/",
                  redcap.data$updrs_new_3_total_m2),
           variable = "off_updrs_3_total,updrs_new_3_total_m2",
           error.file)
}
