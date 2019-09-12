#' Get a ofv value from the csv file.
#' 
#' @param filename Name of the file.
#' @param row The row number from which ofv value will be taken. By default is 1.
#' @return A ofv value of specific row.
#' @export
get_rawres_ofv <- function(filename, row = 1){
  read.csv(filename) %>%
    dplyr::slice(row) %>%
    dplyr::select(ofv) %>% 
    as.numeric()
}