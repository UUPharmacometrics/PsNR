cnd_file_not_found <- function(path)
  rlang::error_cnd("file_not_found", path = path,
                   message = paste0("File '",path,"' not found."))

cnd_unexpected_file_format <- function(path)
  rlang::error_cnd("unexpected_file_format", path = path,
                   message = paste0("The file '",path,"' had an unexpected format."))