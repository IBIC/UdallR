#' Process gait and balance data
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

  sheets <- c("ST_Sway", "DT_Sway", "ST_Gait", "DT_Gait")

  data.l <- as.list(rep(NA, 4))
  names(data.l) <- sheets

  for (s in sheets)
    data.l[[s]] <- read_excel(xlsx, sheet = s)

  if (!is.na(write))
    for (i in 1:length(data.l))
      write.csv(transformGnB(data.l[[i]]), file = paste0(write, "/",
                                                         names(data.l)[i],
                                                         ".csv"),
                row.names = FALSE)

  return(data.l)
}


