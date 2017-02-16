#'  Clean up the data that come from redcap Udall
#' @param dat Data frame that comes directly from redcap_read
#' @return A cleaned data frame, with NA and empty columns removed, on or off prepended to the names, and all subject data on a single row (wide format)
#' @export
#' 
udallCleanREDCapDataWide <- function(dat) {
    # remove the leading "data" from the names
    names <- colnames(dat)
    newnames <- gsub("data.", "", names)
    colnames(dat) <- newnames
                                        # merge on and off
    on <- subset(dat, redcap_event_name=="on_arm_1")
    off <- subset(dat, redcap_event_name=="off_arm_1")
    # just throw in some assertions
    stopifnot(dim(on)[1] >= dim(off)[1])
    # remove missing or empty columns from data frames
    off <- off[,colSums(is.na(off)) <nrow(off)]
    blank <- colSums(off=="") <= nrow(off)
    blank[is.na(blank)] <- TRUE
    off <- off[,blank]

    on <- on[,colSums(is.na(on)) < nrow(on)]
    blank <- colSums(on=="") <= nrow(on)
    blank[is.na(blank)] <- TRUE
    on <- on[,blank]

    # rename columns for off and on conditions
    colnames(off) <- paste("off_", colnames(off),sep="")
    colnames(on) <- paste("on_", colnames(on),sep="")
    # fix double on and double off and idnum variables
    colnames(on) <- gsub("on_on", "on", colnames(on))
    colnames(off) <- gsub("off_off", "off", colnames(off))    

    # rename subject id from each of these - I like it to be idnum
    names(on)[names(on)=="on_idnum"] <- "idnum"
    names(off)[names(off)=="off_idnum"]  <- "idnum"    

    # merge these data frames together from idnum
    merged <- merge(on, off, by="idnum", all.x=TRUE, all.y=TRUE)

    # We will create a couple of useful variables here
    #create sex variable
    merged$sex <-merged$on_health_demo_sex
    merged$sex[ merged$sex > 2] <- NA
    merged$sex <- as.factor(merged$sex)
    levels(merged$sex) <- c("male", "female")

    #create ethnicity variable
    merged$ethnicity <- merged$on_health_demo_ethnic
    merged$ethnicity[merged$ethnicity > 2] <- NA
    merged$ethnicity <- as.factor(merged$ethnicity)
    levels(merged$ethnicity) <- c("hispanic", "not_hispanic")

    #create group variable
    merged$group <- merged$on_health_demo_group
    merged$group[merged$group > 2] <- NA
    merged$group <- as.factor(merged$group)
    # PD is 1, control is 2
    levels(merged$group) <- c("pd", "control")    

    #score various assessments
    merged <- scoreFOG(merged)

    return(merged)
}
