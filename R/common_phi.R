#' Get sum of iOFV
#'
#' @param phi_list  A data structure as returned by \code{\link{read_nm_phi}}
#' @param table_selection  A vector to select the list entries in phi_list
#'
#' @return A numeric vector of length table_selection
#' @export
get_iofv_sum <- function(phi_list, table_selection = 1){
  phi_list[table_selection] %>%
    purrr::map_dbl(~sum(.x[["OBJ"]])) %>%
    purrr::set_names(NULL)
}

