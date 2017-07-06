# Load the multivis (built into UdallR package) that has the information from
## the analytic site visits.
attach(panuc_multivis_2017_04_25)

# Load which columns are uploaded to arm 3
columns.for.upload <- colnames(read.csv("data/panuc_headers.csv"))

# Get from the multivis file the closest analytic site visit to the MRI visits
closestVisits <- getClosestACVisit(checked,
                                   multivis.df = panuc_multivis_2017_04_25)

closestVisits <- closestVisits[, columns.for.upload]

closestVisits$idnum <- gsub("[^0-9]", "", closestVisits$summary_id)
closestVisits$redcap_event_name <- "visit_for_mri_1_arm_3"

closestVisits <- closestVisits[, c(688, 689, 1:687)]

write.csv(closestVisits, file = "visitForMRI1.csv", row.names = FALSE)
