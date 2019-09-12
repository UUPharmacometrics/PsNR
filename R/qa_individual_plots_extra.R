#' Observations (DV), individual predictions (IPRED) and population predictions (PRED) versus time for subjects identified as influential.
#'
#' @param values All cdd dofv values. Missing values will be excluded.
#' @param quiet A logical indicating whether function should not write the warning message if some file not found. By default quiet=FALSE.
#' 
#' @return A ggplot object.
#' @export
individual_plots_extra <- function(file_name,ID_nr,dvid_name,quiet=F) {
  if(file.exists(file_name) && (length(ID_nr)!=0) ) {
    if(dvid_name=="") {
      p <- xpose::xpose_data(file=file_name) %>%
        xpose::mutate(IPRED=CIPREDI) %>%
        xpose::set_var_types(ipred="IPRED") %>% 
        dplyr::filter(ID %in% !!ID_nr) %>%
        xpose::ind_plots(nrow=4, ncol=3, title="Individual plots",subtitle="",caption="") + labs(color = NULL, linetype = NULL, alpha = NULL) + theme_bw()
    } else {
      p <- xpose::xpose_data(file=file_name) %>%
        xpose::mutate(IPRED=CIPREDI) %>% 
        xpose::set_var_types(ipred="IPRED",dvid=dvid_name) %>%
        dplyr::filter(ID %in% !!ID_nr) %>%
        xpose::ind_plots(nrow=4, ncol=3, facets = c('ID',dvid_name), title="Individual plots",subtitle="",caption="") + labs(color = NULL, linetype = NULL, alpha = NULL) + theme_bw()
    }
    return(p)
  } else {
    if(!file.exists(file_name) && !quiet) {
      message("WARNING: File ",file_name," not found!")
    }
    if(length(ID_nr)==0 && !quiet) {
      message("WARNING: Individual plots will not be made, ID_nr vector is empty!")
    }
  }
}
