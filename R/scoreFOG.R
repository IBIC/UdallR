#' Score Freezing of Gait Instrument
#' @param dat Input data frame with fog variables
#' @return dat New data frame with FOG score added to it 

scoreFOG <-function(dat) {
    names <- colnames(dat)
    fog <-dat[,grep("on_fog_q",names)]
    dat$on_fog_total <- apply(fog,1,sum,na.rm=TRUE)
    return(dat)
}
                
