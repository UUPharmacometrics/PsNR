
select_tables <- function(ext_list, ...){
  tables <- tidyselect::vars_select(names(ext_list), ...)
  ext_list[tables]
}

tab_table <- function(table, vars = tidyselect::peek_vars()){
  regex <- sprintf("TABLE NO.\\s+(%s)\\b", paste(table, collapse = "|"))
  grep(regex, vars)
}

tab_problem <- function(problem, vars = tidyselect::peek_vars()){
  regex <- sprintf("Problem=(%s)\\b", paste(problem, collapse = "|"))
  grep(regex, vars)
}

tab_subproblem <- function(subproblem, vars = tidyselect::peek_vars()){
  regex <- sprintf("Subproblem=(%s)\\b", paste(subproblem, collapse = "|"))
  grep(regex, vars)
}

tab_superproblem <- function(superproblem, vars = tidyselect::peek_vars()){
  regex <- sprintf("Superproblem=(%s)\\b", paste(superproblem, collapse = "|"))
  grep(regex, vars)
}

select_iterations <- function(ext_list, ...){
  iterations <- c(...)
  purrr::map(ext_list, ~dplyr::filter(., .data$ITERATION %in% iterations))
}

iter_first <- function() return(0)


iter_final <- function() return(-1000000000)


iter_se <- function() return(-1000000001)

select_columns <- function(ext_list, ...){
  purrr::map(ext_list, function(tab){
    dplyr::select(tab, ...)
  })
}



cols_omega_all <- function(vars = tidyselect::peek_vars()){
  grep("^OMEGA", vars)
}

cols_omega_diag <- function(vars = tidyselect::peek_vars()){
  grep("^OMEGA\\((\\d+),\\1\\)", vars)
}

cols_omega_off_diag <- function(vars = tidyselect::peek_vars()){
  grep("^OMEGA\\((\\d+),(?!\\1)\\d+\\)", vars, perl = TRUE) # using negative lookahead to discard identical numbers
}


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
#' @export
get_initial_ofvs <- function(ext_list, table_selection = 1){
  ext_list[table_selection] %>%
    purrr::map_dbl(~dplyr::filter(.x, ITERATION == 0) %>% dplyr::pull(OBJ)) %>%
    purrr::set_names(NULL)
}

get_final_omegas <- function(ext_list){
  ext_list %>%
    select_iterations(iter_final()) %>%
    select_columns(cols_omega_all()) %>%
    purrr::simplify_all()
}

assert_one_result <- function(ext_list){
  if(length(ext_list)!=1) rlang::cnd_signal(cnd_unexpected_result_structure("The result was expected to have length 1."))
  return(ext_list[[1]])
}