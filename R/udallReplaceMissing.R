#' Replace missing data codes with NA for analysis
#' @param dat Data frame with missing data codes
#' @return dat Data frame with missing data codes assigned to NA
#' @export
udallReplaceMissing <- function(dat) {
  codes <- c(-800:-804, -810:-814, -888,
             -900:-906, -994:-999)

  dat[dat %in% codes] <- NA
  dat[dat %in% as.character(codes)] <- NA


  return(dat)
}
