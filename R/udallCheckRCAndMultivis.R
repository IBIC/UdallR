
#' @export

udallCrosscheckEntries <- function(redcap.data, error.file)
{
  if (file.exists(error.file))
  {
    warning(paste("Deleting old", error.file))
    file.remove(error.file)
  }

  # Check group status
  comparison <- as.matrix(cbind(redcap.data$idnum,
                              as.character(redcap.data$group),
                              as.character(multivis$parkinsonism_status)))
  colnames(comparison) <- c("idnum", "group","parkinsonism_status")

  comparison[is.na(comparison)] <- "NA"

  for (row in 1:nrow(comparison))
  {
    if (comparison[row, "group"] == "pd")
    {
      if (comparison[row, "parkinsonism_status"] != "PD")
        write(paste(comparison[row, "idnum"], "\tvalue mismatch:",
                      comparison[row, "group"], "/",
                      comparison[row, "parkinsonism_status"]),
              append = TRUE, file = error.file)
    }
    else if (comparison[row, "group"] == "control")
    {
      if (comparison[row, "parkinsonism_status"] != "Unaffected")
        write(paste(comparison[row, "idnum"], "\tvalue mismatch:",
                      comparison[row, "group"], "/",
                      comparison[row, "parkinsonism_status"]),
              append = TRUE, file = error.file)
    }

    if (comparison[row, "parkinsonism_status"] == "Other")
      write(paste(comparison[row, "idnum"],
                    "\tparkinsonism status is \"Other\""),
            append = TRUE, file = error.file)
  }

  # Age difference
  diffs <- redcap.data$scage - as.numeric(as.character(multivis$agevisit))
  diffs.days <- diffs * 365

  more.than.3mo <- abs(diffs.days) > 90

  write(paste(redcap.data$idnum[more.than.3mo & !is.na(more.than.3mo)],
              "\twide visit gap:",
              round(diffs.days[more.than.3mo & !is.na(more.than.3mo)]),
              "days"),
        append = TRUE, file = error.file)

  # Compare double-entered UPDRS data

  if (sum(is.na(c(redcap.data$on_updrs_3_total,
                  redcap.data$off_updrs_3_total,
                  multivis$updrs_new_3_total_m1,
                  multivis$updrs_new_3_total_m2))) > 0)
    warning(paste("NAs in one or more UPDRS total column.",
                  "This will cause other erros. Resolve these first."))


  logError(redcap.data, is.na(redcap.data$on_updrs_3_total),
           "UPDRS ON in REDCap is NA", error.file)

  logError(redcap.data, is.na(redcap.data$off_updrs_3_total),
           "UPDRS OFF in REDCap is NA", error.file)

  logError(multivis, unname(is.na(multivis$updrs_new_3_total_m1)),
           "UPDRS ON in multivis is NA", error.file, id.column = "subject_id")

  logError(multivis, unlist(is.na(multivis$updrs_new_3_total_m2)),
           "UPDRS OFF in multivis is NA", error.file, column = "subject_id")

  on.mismatches <- !sapply(redcap.data$on_updrs_3_total ==
                        as.numeric(as.character(multivis$updrs_new_3_total_m1)),
                        isTRUE)

  write(paste(redcap.data$idnum[on.mismatches], "\tUPDRS ON mismatch:",
              redcap.data$on_updrs_3_total[on.mismatches], "/",
              as.character(multivis$updrs_new_3_total_m1))[on.mismatches],
        append = TRUE, file = error.file)

  off.mismatches <- !sapply(redcap.data$off_updrs_3_total ==
                       as.numeric(as.character(multivis$updrs_new_3_total_m2)),
                       isTRUE)

  write(paste(redcap.data$idnum[off.mismatches], "\tUPDRS OFF mismatch:",
              redcap.data$off_updrs_3_total[off.mismatches], "/",
              as.character(multivis$updrs_new_3_total_m2)[off.mismatches]),
        append = TRUE, file = error.file)



}
