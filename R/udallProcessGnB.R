#' Process gait and balance data
#'
#' Loads gait and balance data from an .xlsx file with (minimally) sheets
#' {ST,DT}_{Sway,Gait}. Runs through transformGnB() to clean up and writes to
#' file if directed to.
#'
#' @param xlsx Location of the xlsx file with the relevant gait and balance
#' data, relative to the current working directory.
#' @param write Location to save output CSV files to. Defaults to NA, which
#' signals no write-out.
#'
#' @return data.l A list of data frames with the transformed data.
#'
#' @export


udallProcessGnB <- function(xlsx, write = NA) {
  library(readxl)
  today <- format(Sys.Date(), format = "%Y%m%d")

  sheets <- c("ST_Sway", "DT_Sway", "ST_Gait", "DT_Gait")

  data.l <- as.list(rep(NA, 4))
  names(data.l) <- sheets

  output <- as.list(rep(NA, 4))
  names(output) <- sheets

  for (i in 1:length(sheets))
  {
    s <- sheets[i]
    data.l[[i]] <- read_excel(xlsx, sheet = s)
    data.l[[i]] <- data.l[[i]][, !grepl("^X__", colnames(data.l[[i]]))]
    output[[i]] <- transformGnB(data.l[[i]])
  }

  merged <- merge(merge(merge(output[[1]], output[[2]]), output[[3]]),
                output[[4]])

  if (!is.na(write))
      write.csv(merged, file = paste0(write, "/udall-SG-", today, ".csv"),
                row.names = FALSE)

  return(output)
}


