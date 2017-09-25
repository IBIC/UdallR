#' Replace missing data codes with NA for analysis
#' @param dat Data frame with missing data codes
#' @return dat Data frame with missing data codes assigned to NA
#' @export
udallReplaceMissing <- function(dat) {
  codes <- c(-800:-804, -810:-814, -888,
             -900:-906, -994:-999)

  # n.strip <- as.data.frame(apply(dat, 1, function(x) {x[x %in% codes] <- NA}))
  #
  # dat[dat %in% codes] <- NA
  # dat[dat %in% as.character(codes)] <- NA

  new <- dat

  if (is.data.frame(dat))
  {
    for (col in colnames(new))
    {
      temp <- new[, col]
      temp[temp %in% codes] <- NA
      temp[temp %in% temp %in% as.character(codes)] <- NA
      new[, col] <- temp
    }
  }
  else
  {
    temp <- new
    temp[temp %in% codes] <- NA
    temp[temp %in% temp %in% as.character(codes)] <- NA
    new <- temp
  }

  return(new)
}
