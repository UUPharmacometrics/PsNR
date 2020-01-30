result <- function(x = NULL, error = NULL, warnings = NULL){
  structure(
    list(
      result = x,
      error = error,
      warnings = warnings
    ),
    class = "psn_result"
  )
}

as_result <- function(x) UseMethod("as_result")

as_result.error <- function(x){
  return(result(error = x))
}

as_result.default <- function(x){
  return(result(x))
}

get_result <- function(x) UseMethod("get_result")

get_result.psn_result <- function(x){
  return(x$result)
}

has_result <- function(x) UseMethod("has_result")

has_result.psn_result <- function(x){
  return(!is.null(x$result))
}

get_error <- function(x) UseMethod("get_error")

get_error.psn_result <- function(x){
  return(x$error)
}

has_errors <- function(x) UseMethod("has_errors")

has_errors.psn_result <- function(x){
  return(!is.null(x$error))
}

has_errors.psn_result_df <- function(x){
  return(any(has_errors(x$error)))
}

has_errors.list <- function(x){
  return(purrr::map_lgl(x, rlang::is_condition))
}

result_df <- function(df){
  df <- tibble::as_tibble(df)
  if(!exists("error", df)) df <- tibble::add_column(df, error = list(NULL))
  if(!exists("warnings", df)) df <- tibble::add_column(df, warnings = list(NULL))
  structure(df, class = c("psn_result_df", class(df)))
}

collect_results <- function(..., .id = "id", .result = "result"){
  l <- rlang::dots_list(...)
  if(!all(purrr::map_lgl(l, ~inherits(.x, "psn_result")))) rlang::abort("all function arguments need to be psn_result objects.")
  if(rlang::is_named(l)) l <- purrr::imap(l, ~purrr::update_list(.x, !!.id := .y))
  purrr::transpose(l) %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(~rlang::is_atomic(.[[1]]), ~purrr::simplify(.)) %>%
    dplyr::mutate(!!.result:=.data$result)
}

