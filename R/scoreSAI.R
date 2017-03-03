#' Score SAI
#' @param dat Input data frame with SAI variables
#' @return dat New data frame with SAI score added to it 

scoreSAI <-function(dat) {
    names <- colnames(dat)

    sai <- data.frame(
        (dat$on_sai_n20_amp/dat$on_sai_n20_uncond_amp)*100,
        (dat$on_sai_n20_1_amp/dat$on_sai_n20_uncond_amp)*100,
        (dat$on_sai_n20_2_amp/dat$on_sai_n20_uncond_amp)*100,
        (dat$on_sai_n20_3_amp/dat$on_sai_n20_uncond_amp)*100,
        (dat$on_sai_n20_4_amp/dat$on_sai_n20_uncond_amp)*100,
        (dat$on_sai_n20_5_amp/dat$on_sai_n20_uncond_amp)*100)
    dat$on_sai_sai <- apply(sai, 1, mean)
    
    return(dat)
}
                
