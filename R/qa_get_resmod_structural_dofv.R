#' Get dofv value from the resmod.csv file from the idv_varying_theta model for the specific dvid value.
#'    
#' @param directory A path to the qa run directory.
#' @param idv The string of the idv name.
#' @param dvid A dvid value. By default dvid='NA'.
#' 
#' @return A numeric resmod structural dofv value. If resmod.csv file does not exist will return string "ERROR".

get_resmod_structural_dofv <- function(directory, idv, dvid="NA") {
  resmod_table_list <- get_resmod_table(directory, idv)
  resmod_file_exists <- resmod_table_list$resmod_file_exists
  if(resmod_file_exists) {
    if(dvid=='NA') {
      resmod_structural_dofv <- resmod_table_list$resmod_table %>%
        dplyr::filter(model=="idv_varying_theta") %>%
        dplyr::select(dOFV) %>%
        as.numeric()
    } else {
      resmod_structural_dofv <- resmod_table_list$resmod_table %>%
        dplyr::filter(dvid==!!dvid,model=="idv_varying_theta") %>%
        dplyr::select(dOFV) %>%
        as.numeric()
    }
  } else {
    resmod_structural_dofv <- "ERROR"
  }
  return(resmod_structural_dofv)
}