read_psn_rawres <- function(path){
  if(!file.exists(path)) rlang::cnd_signal(cnd_file_not_found(path))
  # the suppressWarnings is necessary as PsN does not alway provide the right number of commas
  suppressWarnings(tab <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_guess()), na = "NA"))
  return(tab)
}

#' Get a ofv value from the csv file.
#' 
#' @param path Name of the file.
#' @param row The row number from which ofv value will be taken. By default is 1.
#' @return A ofv value of specific row.
#' @export
get_rawres_ofv <- function(path, row = 1){
  read_psn_rawres(path) %>%
    dplyr::slice(row) %>%
    dplyr::select(ofv) %>% 
    as.numeric()
}