#' Gets objective function value from the specific iteration.
#'   
#' @param ext_file A string of the ext file name.
#' @param iteration A numeric value from the ITERATION column in the ext file. By default iteration=-1000000000.
#' 
#' @return An objective function value from specific iteration.
#' @export
.get_ext_ofv <- function(ext_file,iteration=-1000000000){
  read.table(ext_file,header=TRUE,skip=1,stringsAsFactors = F) %>%
    dplyr::filter(ITERATION==iteration) %>%
    dplyr::select(OBJ) %>% 
    as.numeric()
}