
#' @export

udallCrosscheckEntries <- function(redcap.data, error.file)
{
  g <- "group"
  cps <- "closest_parkinsonism_status"

  if (file.exists(error.file))
  {
    warning(paste("Deleting old", error.file))
    file.remove(error.file)
  }

  write(paste("subject", "status", "description", "variable.name",
              sep = "\t"),
        file = error.file)

  comparison <- redcap.data[, c("idnum", g, cps)]

  for (r in 1:nrow(comparison))
  {
    row <- comparison[r, ]

    if (comparison[r, g] == "pd" & !is.na(comparison[r, g]))
      if (comparison[r, cps] != "PD" | is.na(comparison[r, cps]))
        logError(row, TRUE, problem = paste0("group mismatch: ",
                                             comparison[r, g], ",",
                                             comparison[r, cps]),
                 variable = paste0(g, ",", cps),
                 error.file)

    if (comparison[r, g] == "control" & !is.na(comparison[r, g]))
      if (comparison[r, cps] != "Unaffected" | is.na(comparison[r, cps]))
        logError(row, TRUE,  problem = paste0("group mismatch: ",
                                              comparison[r, g], ",",
                                              comparison[r, cps]),
                 variable = paste0(g, ",", cps),
                 error.file)

    if (comparison[r, cps] == "Other" & !is.na(comparison[r, cps]))
      logError(row, TRUE, problem = "group is OTHER", variable = cps,
               warning = TRUE, error.file)
  }

  # Age difference
  diffs <- redcap.data$scage - redcap.data$closest_agevisit
  diffs.days <- diffs * 365

  logError(redcap.data, diffs.days > 90,
           problem = paste0("wide visit gap: ", round(diffs.days), " days"),
           variable = "scage,closest_agevisit", warning = TRUE, error.file)

  # Compare double-entered UPDRS data

  if (sum(is.na(c(redcap.data$on_updrs_3_total,
                  redcap.data$off_updrs_3_total,
                  redcap.data$closest_updrs_new_3_total_m1,
                  redcap.data$closest_updrs_new_3_total_m2))) > 0)
    warning(paste("NAs in one or more UPDRS total column.",
                  "This will cause other errors. Resolve these first."))


  logError(redcap.data, is.na(redcap.data$on_updrs_3_total),
           "UPDRS ON in REDCap is NA",variable = "on_updrs_3_total", error.file)

  logError(redcap.data, is.na(redcap.data$off_updrs_3_total),
           "UPDRS OFF in REDCap is NA", variable = "off_updrs_3_total",
           error.file)

  logError(redcap.data, is.na(redcap.data$closest_updrs_new_3_total_m1),
           "closest UPDRS ON is NA", variable = "closest_updrs_new_3_total_m1",
           error.file)

  logError(redcap.data, is.na(redcap.data$closest_updrs_new_3_total_m2),
           "closest UPDRS OFF is NA", variable = "closest_updrs_new_3_total_m2",
            error.file)

  on.mismatches <- !sapply(redcap.data$on_updrs_3_total ==
                            redcap.data$closest_updrs_new_3_total_m1,
                            isTRUE)

  logError(redcap.data, on.mismatches,
           paste0("UPDRS ON mismatch:", redcap.data$on_updrs_3_total, "/",
                  redcap.data$closest_updrs_new_3_total_m1),
           variable = "on_updrs_3_total,closest_updrs_new_3_total_m1",
           error.file)

  off.mismatches <- !sapply(redcap.data$off_updrs_3_total ==
                        redcap.data$closest_updrs_new_3_total_m2,
                        isTRUE)

  logError(redcap.data, off.mismatches,
           paste0("UPDRS OFF mismatch:", redcap.data$off_updrs_3_total, "/",
                  redcap.data$closest_updrs_new_3_total_m2),
           variable = "off_updrs_3_total,closest_updrs_new_3_total_m2",
           error.file)
}
