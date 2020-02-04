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

#' Read NONMEM ext and phi files
#'
#' The function reads NONMEM ext and phi files and returns a list of data.frame with one data.frame for each
#' table (i.e. estimation). The element names are the intro lines and, hence, can be used to extract
#' problem number etc. The current implementation assumes that all headers are identical.
#'
#' @param path
#'
#' @return A list of data.frame
#'
#' @export
read_nm_ext <- function(path){
  read_nm_tab(path, "ext", "ITERATION")
}

#' Return name of .ext or .phi file
#'
#' @param path Model or lst file for which to return the file
#'
#' @return character
#' @export
#'
#' @examples
#' ext_file("run4.mod")
#' phi_file("run4.lst")
ext_file <- function(path) {
  sub_file_ext(path, "ext")
}

#' @rdname read_nm_ext
#' @export
read_nm_phi <- function(path){
  read_nm_tab(path, "phi", "SUBJECT_NO")
}

#' @export
#' @rdname ext_file
phi_file <- function(path) {
  sub_file_ext(path, "phi")
}

sub_file_ext <- function(path, ext){
  return(gsub("\\.[^.]+$",paste0("\\.", ext), path))
}

read_nm_tab <- function(path, file_type, header_start){
  if(!file.exists(path)) rlang::cnd_signal(cnd_file_not_found(path))
  file_content <- readr::read_lines(path)
  # find important rows
  intro_rows <- grepl("TABLE", file_content, fixed = TRUE)
  header_rows <- grepl(header_start, file_content, fixed = TRUE)
  inter_rows <- !intro_rows & !header_rows
  if(!any(intro_rows) || !any(header_rows))
    rlang::cnd_signal(cnd_unexpected_file_format(path))
  # parse header (assumes that all subsequent headers are identical)
  header <- scan(text = file_content[header_rows][1], what = character(), quiet = TRUE)
  # determine number of lines between headers
  rle_iter <- rle(intro_rows|header_rows)
  nlines <- rle_iter[["lengths"]][!rle_iter[["values"]]]
  ncols <- length(header)
  ntabs <- sum(intro_rows)
  # parse all lines
  values <- scan(text = file_content[inter_rows], what = double(),  quiet = TRUE)
  df <- matrix(values, ncol = ncols, byrow = TRUE, dimnames  = list(NULL, header)) %>%
    as.data.frame()
  tab_indicator <- inverse.rle(list(lengths = nlines, values = seq_len(ntabs)))
  lst <- split(df, tab_indicator)
  names(lst) <- file_content[intro_rows]
  return(lst)
}

