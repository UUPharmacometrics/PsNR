read_psn_rawres <- function(path){
  if(!file.exists(path)) rlang::cnd_signal(cnd_file_not_found(path))
  tab <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_guess()), na = "NA")
  return(tab)
}
