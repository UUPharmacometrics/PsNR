#' Count number of thetas in the ext file.
#'   
#' @param filename A string of the ext file name.
#' @param iteration A numeric value from the ITERATION column in the ext file. By default iteration=-1000000000.
#' @param keep_na A logical indicating whether thetas with missing values should be counted. By default keep_na=FALSE.
#' 
#' @return Number of thetas in the ext file.
count_thetas <- function(filename,iteration=-1000000000,keep_na=FALSE) {
  ext_file <- read.table(filename,header=TRUE,skip=1,stringsAsFactors = F) %>%
    dplyr::filter(ITERATION==iteration)
  TH_values <- ext_file[grep("^THETA+[0-9]$",colnames(ext_file))]
  nr_of_thetas <- length(TH_values)
  if(!keep_na) {
    nr_of_thetas <- length(TH_values[!is.na(TH_values)])
  }
  return(nr_of_thetas)
}