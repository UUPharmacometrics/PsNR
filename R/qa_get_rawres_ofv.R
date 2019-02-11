#' Get a ofv value from the csv file column ofv.
#'
#' @param rawres_file A csv file.
#' @param row Number of row from which ofv value should be selected. By default row = 1.
#' 
#' @return Objective function value.
.get_rawres_ofv <- function(rawres_file, row = 1){
  read.csv(rawres_file) %>%
    dplyr::slice(row) %>%
    dplyr::select(ofv) %>% 
    as.numeric()
}