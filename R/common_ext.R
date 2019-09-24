
#' Get final OFVs
#'
#' @param ext_list A data structure as returned by \code{\link{read_nm_ext}}
#' @param table_selection A vector to select the list entries in ext_list
#'
#' @return A vector of OFV values
#' @export
get_final_ofvs <- function(ext_list, table_selection = 1){
  ext_list[table_selection] %>% 
    purrr::map_dbl(~dplyr::filter(.x, ITERATION == -1e+09) %>% dplyr::pull(OBJ)) %>% 
    purrr::set_names(NULL)
}