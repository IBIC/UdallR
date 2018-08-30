setwd("~/UdallR/data")

args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]

if (length(args) == 0) dir <- "180826"

if (!dir.exists(dir))
  stop("No such directory")

# Read input files
files <- list.files(dir, pattern = "panuc-0110-.*.csv", full.names = TRUE)

if (length(files) != 2)
  stop(paste0("There must be two and only two CSV files in data/", dir))

updrs <- read.csv(files[1])
main  <- read.csv(files[2])

# These are the columns shared by both data frames, minus the ones were actually
# going to merge on
in.common <- intersect(colnames(updrs), colnames(main))[-(1:2)]

# Drop those columns since the data already exists and merging on it just
# creates more problems
main.merge <- main[, !(colnames(main) %in% in.common)]

# Actually merge the data. We want to keep everyone from the main file, even
# if they don't have UPDRS data.
merged <- merge(updrs, main.merge, by = c("summary_id", "visit_number"),
                all.y = TRUE)

# Output file name, extract date from original file, not today
output.date <- gsub(".*/panuc-0110-", "", gsub(".csv", "", files[2]))
output.file <- paste0("~/UdallR/data/panuc_multivis_", output.date, ".csv")

write.csv(merged, file = output.file, row.names = FALSE)
