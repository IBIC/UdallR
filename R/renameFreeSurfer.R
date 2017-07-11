#' Score Freezing of Gait Instrument
#' @param dat Input data frame with freesurfer variables
#' @return dat New data frame with freesurfer variables renamed
#' @export
#'

renameFreeSurfer <- function(dat) {
  library(data.table)
  FreeSurferNames <- c("leftlateralventricle", "leftinflatvent",
                       "leftcerebellumwhitematter", "leftcerebellumcortex",
                       "leftthalamusproper", "leftcaudate", "leftputamen", "leftpallidum",
                       "thirdventricle", "fourthventricle", "brainstem", "lefthippocampus",
                       "leftamygdala", "csf", "leftaccumbensarea", "leftventraldc",
                       "leftvessel", "leftchoroidplexus", "rightlateralventricle",
                       "rightinflatvent", "rightcerebellumwhitematter",
                       "rightcerebellumcortex", "rightthalamusproper", "rightcaudate",
                       "rightputamen", "rightpallidum", "righthippocampus", "rightamygdala",
                       "rightaccumbensarea", "rightventraldc", "rightvessel",
                       "rightchoroidplexus", "fifthventricle", "wmhypointensities",
                       "leftwmhypointensities", "rightwmhypointensities",
                       "nonwmhypointensities", "leftnonwmhypointensities",
                       "rightnonwmhypointensities", "opticchiasm", "cc_posterior",
                       "cc_mid_posterior", "cc_central", "cc_mid_anterior", "cc_anterior",
                       "lh_g_and_s_frontomargin", "lh_g_and_s_occipital_inf",
                       "lh_g_and_s_paracentral", "lh_g_and_s_subcentral",
                       "lh_g_and_s_transv_frontopol", "lh_g_and_s_cingulant",
                       "lh_g_and_s_cingulmidant", "lh_g_and_s_cingulmidpost",
                       "lh_g_cingulpostdorsal", "lh_g_cingulpostventral", "lh_g_cuneus",
                       "lh_g_front_infopercular", "lh_g_front_inforbital",
                       "lh_g_front_inftriangul", "lh_g_front_middle", "lh_g_front_sup",
                       "lh_g_ins_lg_and_s_cent_ins", "lh_g_insular_short",
                       "lh_g_occipital_middle", "lh_g_occipital_sup",
                       "lh_g_octemp_latfusifor", "lh_g_octemp_medlingual",
                       "lh_g_octemp_medparahip", "lh_g_orbital", "lh_g_pariet_infangular",
                       "lh_g_pariet_infsupramar", "lh_g_parietal_sup", "lh_g_postcentral",
                       "lh_g_precentral", "lh_g_precuneus", "lh_g_rectus",
                       "lh_g_subcallosal", "lh_g_temp_supg_t_transv", "lh_g_temp_suplateral",
                       "lh_g_temp_supplan_polar", "lh_g_temp_supplan_tempo",
                       "lh_g_temporal_inf", "lh_g_temporal_middle", "lh_lat_fisanthorizont",
                       "lh_lat_fisantvertical", "lh_lat_fispost", "lh_pole_occipital",
                       "lh_pole_temporal", "lh_s_calcarine", "lh_s_central",
                       "lh_s_cingulmarginalis", "lh_s_circular_insula_ant",
                       "lh_s_circular_insula_inf", "lh_s_circular_insula_sup",
                       "lh_s_collat_transv_ant", "lh_s_collat_transv_post", "lh_s_front_inf",
                       "lh_s_front_middle", "lh_s_front_sup", "lh_s_interm_primjensen",
                       "lh_s_intrapariet_and_p_trans", "lh_s_oc_middle_and_lunatus",
                       "lh_s_oc_sup_and_transversal", "lh_s_occipital_ant",
                       "lh_s_octemp_lat", "lh_s_octemp_med_and_lingual",
                       "lh_s_orbital_lateral", "lh_s_orbital_medolfact",
                       "lh_s_orbitalh_shaped", "lh_s_parieto_occipital", "lh_s_pericallosal",
                       "lh_s_postcentral", "lh_s_precentralinfpart",
                       "lh_s_precentralsuppart", "lh_s_suborbital", "lh_s_subparietal",
                       "lh_s_temporal_inf", "lh_s_temporal_sup", "lh_s_temporal_transverse",
                       "rh_g_and_s_frontomargin", "rh_g_and_s_occipital_inf",
                       "rh_g_and_s_paracentral", "rh_g_and_s_subcentral",
                       "rh_g_and_s_transv_frontopol", "rh_g_and_s_cingulant",
                       "rh_g_and_s_cingulmidant", "rh_g_and_s_cingulmidpost",
                       "rh_g_cingulpostdorsal", "rh_g_cingulpostventral", "rh_g_cuneus",
                       "rh_g_front_infopercular", "rh_g_front_inforbital",
                       "rh_g_front_inftriangul", "rh_g_front_middle", "rh_g_front_sup",
                       "rh_g_ins_lg_and_s_cent_ins", "rh_g_insular_short",
                       "rh_g_occipital_middle", "rh_g_occipital_sup",
                       "rh_g_octemp_latfusifor", "rh_g_octemp_medlingual",
                       "rh_g_octemp_medparahip", "rh_g_orbital", "rh_g_pariet_infangular",
                       "rh_g_pariet_infsupramar", "rh_g_parietal_sup", "rh_g_postcentral",
                       "rh_g_precentral", "rh_g_precuneus", "rh_g_rectus",
                       "rh_g_subcallosal", "rh_g_temp_supg_t_transv", "rh_g_temp_suplateral",
                       "rh_g_temp_supplan_polar", "rh_g_temp_supplan_tempo",
                       "rh_g_temporal_inf", "rh_g_temporal_middle", "rh_lat_fisanthorizont",
                       "rh_lat_fisantvertical", "rh_lat_fispost", "rh_pole_occipital",
                       "rh_pole_temporal", "rh_s_calcarine", "rh_s_central",
                       "rh_s_cingulmarginalis", "rh_s_circular_insula_ant",
                       "rh_s_circular_insula_inf", "rh_s_circular_insula_sup",
                       "rh_s_collat_transv_ant", "rh_s_collat_transv_post", "rh_s_front_inf",
                       "rh_s_front_middle", "rh_s_front_sup", "rh_s_interm_primjensen",
                       "rh_s_intrapariet_and_p_trans", "rh_s_oc_middle_and_lunatus",
                       "rh_s_oc_sup_and_transversal", "rh_s_occipital_ant",
                       "rh_s_octemp_lat", "rh_s_octemp_med_and_lingual",
                       "rh_s_orbital_lateral", "rh_s_orbital_medolfact",
                       "rh_s_orbitalh_shaped", "rh_s_parieto_occipital", "rh_s_pericallosal",
                       "rh_s_postcentral", "rh_s_precentralinfpart",
                       "rh_s_precentralsuppart", "rh_s_suborbital", "rh_s_subparietal",
                       "rh_s_temporal_inf", "rh_s_temporal_sup", "rh_s_temporal_transverse")

  oldnames <- paste("on_", FreeSurferNames, sep="")
  setnames(dat, oldnames, FreeSurferNames)
  dat$hippocampus <- dat$lefthippocampus+dat$righthippocampus
  dat$precuneus <- dat$rh_g_precuneus+dat$lh_g_precuneus
  dat$putamen <- dat$rightputamen+dat$leftputamen
  dat$caudate <- dat$rightcaudate+dat$leftcaudate
  dat$acc <- dat$lh_g_and_s_cingulant+dat$rh_g_and_s_cingulant
  dat$ips <- dat$rh_s_intrapariet_and_p_trans+dat$lh_s_intrapariet_and_p_trans
  dat
}

