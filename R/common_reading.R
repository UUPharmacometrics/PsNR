#' Read NONMEM table in standard format
#' 
#' This function reads a NONMEM output table that was generated with the default format option. 
#' The function should only be used for PsN generated tables with known format and not for "user defined" tables
#' that might have particular format options. 
#'
#' @param path Path for the table file to read
#' @param skip Number of lines to skip before the header
#' 
#' @export
#'
#' @return A data.frame
read_nm_std_table <- function(path, skip = 1){
  readr::read_table2(path, col_names = TRUE, 
                    col_types = readr::cols(.default = readr::col_double()), 
                    skip = skip)
}


#' Read NONMEM simulation table in standard format
#' 
#' This function reads $TABLE output from a simulation step (with possibly repeating headers). This function can 
#' can in general be used instead of \code{\link{read_nm_std_table}} but will be slightly slower due to the 
#' processing of the intermediate header rows. 
#'
#' @param path Path for the table file to read
#'
#' @return A data.frame
#' @export
read_nm_std_sim_table <- function(path){
  file_content <- readr::read_lines(path)
  intro_rows <- grepl("TABLE", file_content, fixed = TRUE)
  first_header <- file_content[[2]]
  header_rows <- grepl(first_header, file_content, fixed = TRUE)
  header <- scan(text = first_header, what = character(), quiet = TRUE)
  tab_data <- scan(text = file_content[!header_rows&!intro_rows], what = double(),  quiet = TRUE)
  tab_matrix <- matrix(tab_data, nrow = sum(!header_rows&!intro_rows), 
         ncol = length(header), byrow = TRUE, 
         dimnames = list(NULL, header)) 
  return(dplyr::as_tibble(tab_matrix))
}