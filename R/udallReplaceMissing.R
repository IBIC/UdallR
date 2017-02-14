#' Replace missing data codes with NA for analysis
#' @param dat Data frame with missing data codes
#' @return dat Data frame with missing data codes assigned to NA
#' @export
udallReplaceMissing <- function(dat) {
    dat[dat == -900] <- NA
    dat[dat== -901] <- NA
    dat[dat == -902] <- NA
    dat[dat == -903] <- NA
    dat[dat == -904] <- NA
    dat[dat == -905] <- NA

    dat[dat == "-900"] <- NA
    dat[dat== "-901"] <- NA
    dat[dat == "-902"] <- NA
    dat[dat == "-903"] <- NA
    dat[dat == "-904"] <- NA
    dat[dat == "-905"] <- NA    

    return(dat)
}
