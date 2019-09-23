#' Read NONMEM table in standard format
#' 
#' This function reads a NONMEM output table that was generated with the default format option. 
#' The function should only be used for PsN generated tables with known format and not for "user defined" table
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