#' Run expression and retain errors
#' 
#' The function allows to evaluates an expression and store eventual errors in the results data structure. 
#' 
#' @param expr Expression to evaluate
#'
#' @return A list with entries results and error (one of them is NA)
#' @export
#'
#' @examples
#' do_safely("a"/1)
do_safely <- function(expr){
  res <- try(rlang::with_abort(expr), silent = TRUE)
  if(inherits(res, "try-error")) return(list(result = NA, error = res))
  else return(list(result = res, error = NA))
}

contains_error <- function(result){
  return(!is.na(result$error))
}

cnd_file_not_found <- function(path)
  rlang::error_cnd("file_not_found", path = path,
                   message = paste0("File '",path,"' not found."))

cnd_unexpected_file_format <- function(path)
  rlang::error_cnd("unexpected_file_format", path = path,
                   message = paste0("The file '",path,"' had an unexpected format."))