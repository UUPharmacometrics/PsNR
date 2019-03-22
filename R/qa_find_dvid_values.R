#' Finds DVID values based on the DVID column name.
#' 
#' @param directory A path to the qa run directory.
#' @param idv The string of the idv name from the -idv option in the qa run.
#' @param dvid_name The string of the idv name from the -idv option in the qa run.
#' 
#' @return A numeric vector of all DVID values. If no DVIDs then will be set to string 'NA'.
find_dvid_values <- function(directory,idv,dvid_name) {
  #check if dvid exist
  resmod_table_list <- get_resmod_table(directory, idv)
  resmod_file_exists_idv <- resmod_table_list$resmod_file_exists
  if(resmod_file_exists_idv && dvid_name!='') {
    resmod_table <- resmod_table_list$resmod_table
    if(any(resmod_table$dvid!="NA")) {
      dvid_nr <- unique(resmod_table$dvid)
      if(any(dvid_nr=="sum")) {
        dvid_nr <- as.numeric(dvid_nr[-which(dvid_nr=="sum")])
      } else {
        dvid_nr <- as.numeric(dvid_nr)
      }
    } else {
      dvid_nr <- 'NA'
    }
  } else {
    dvid_nr <- 'NA'
  }
  return(dvid_nr)
}